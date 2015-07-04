// 
// Copyright 2015 Jeff Bush
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

import Chisel._

//
// Tile Buffer
// This stores rendered depth, stencil, and color information for a small
// square portion of the framebuffer (a tile).  It performs alpha blending,
// stencil checks, and depth checks. It has a three stage pipeline and 
// can accept one 2x2 quad per cycle without stalling. When rendering is finished
// for the tile, writing the reg_start_resolve register will cause this to write
// the contents of the color buffer to main memory via the arbiter.
//
// Constraints:
// - Assumes pre-multiplied alpha
// - 32 bpp output
// - The same quad location cannot be written twice within 1 cycle
// - There must be 3 cycles after the last write before a resolve
//
// Open Questions/To do:
// - How to clear the framebuffer efficiently?  Do it during resolve?
// - Stencil and depth buffers are not implemented yet
// - Use advanced parameterization for tile size and burstByteCount
//   (https://chisel.eecs.berkeley.edu/2.2.0/chisel-parameters.pdf)
// - How does early-Z connect with this module?
//

class RGBAColor extends Bundle {
	val red = UInt(width = 8)
	val green = UInt(width = 8)
	val blue = UInt(width = 8)
	val alpha = UInt(width = 8)
}

// tileSize is pixels
class TileBuffer(tileSize : Int, burstByteCount : Int) extends Module {
	val tileSizeQuads = tileSize / 2
	val tileCoordBits = log2Up(tileSizeQuads)	// Divide by two to use the quad bits
	val colorChannelBits = 8
	val depthBufferBits = 24
	val stencilBufferBits = 8
	val colorBufferBits = colorChannelBits * 4
	val bytesPerPixel = 4

	val io = new Bundle {
		// Pixel update interface.  This colors a 2x2 quad per cycle
		val pixelX = UInt(INPUT, tileCoordBits)
		val pixelY = UInt(INPUT, tileCoordBits)
		val pixelMask = UInt(INPUT, 4)
		val pixelColors = Vec.fill(4){ (new RGBAColor).asInput }
		val resolveArbPort = new ArbiterWritePort(burstByteCount).flip
		val registerUpdate = new RegisterUpdate().flip
	}
	
	// Handle configuration updates from the command processor
	val resolveBaseAddress = Reg(UInt(INPUT, 32))
	val resolveStride = Reg(UInt(INPUT, 32))
	val enableAlpha = Reg(Bool(), init = Bool(false))

	when (io.registerUpdate.update) {
		switch (io.registerUpdate.address) {
			is (regids.reg_resolve_base_addr) {
				resolveBaseAddress := io.registerUpdate.value
			}

			is (regids.reg_resolve_stride) {
				resolveStride := io.registerUpdate.value
			}

			is (regids.reg_enable_alpha) {
				enableAlpha := io.registerUpdate.value(0)
			}
		}
	}

	//
	// Each entry in color memory stores a 2x2 block of pixels:
	// 0 1
	// 2 3
	//
	val colorMemory = Mem(Vec.fill(4){ new RGBAColor }, tileSizeQuads * tileSizeQuads, seqRead = true)

	val s_resolve_not_active :: s_resolve_collect_first :: s_resolve_collect_rest :: s_resolve_writeback :: Nil = Enum(UInt(), 4)
	val resolveStateReg = Reg(init = s_resolve_not_active)
	val resolveInternalAddr = UInt(width = tileCoordBits * 2)

	// 
	// Stage 1: read existing color at locations
	//
	val pixelAddressIn = Cat(io.pixelY, io.pixelX)
	val pixelAddressStage1Reg = Reg(UInt(width = tileCoordBits * 2))
	val pixelMaskStage1Reg = Reg(UInt(width = 4))
	val newPixelColorStage1Reg = Reg(Vec.fill(4){ new RGBAColor })
	val readColorValue = Vec.fill(4){ new RGBAColor }
	val readAddressReg = Reg(UInt(width = tileCoordBits * 2))
	
	when (resolveStateReg === s_resolve_collect_first || resolveStateReg === s_resolve_collect_rest) {
		// When in resolve mode, we use this read port to get the pixel
		// values
		readAddressReg := resolveInternalAddr
	}
	.elsewhen (io.pixelMask != UInt(0)) {
		readAddressReg := pixelAddressIn
	}

	readColorValue := colorMemory(readAddressReg)
	val oldPixelColorStage1 = readColorValue
	pixelMaskStage1Reg := io.pixelMask;
	pixelAddressStage1Reg := pixelAddressIn
	newPixelColorStage1Reg := io.pixelColors

	//
	// Stage 2:
	// - XXX Perform visibility checks (stencil, depth)
	// - Multiply destination pixel by one minus alpha
	//
	val newPixelColorStage2Reg = Vec.fill(4){ Reg(new RGBAColor) }
	val pixelAddressStage2Reg = Reg(UInt(width = tileCoordBits * 2))
	val updatePixelStage2Reg = Reg(UInt(width = 4))
	val oldWeightedColorStage2Reg = Vec.fill(4){ Reg(new RGBAColor) }
	val oldPixelColorStage2Reg = Vec.fill(4) { Reg(new RGBAColor) }

	pixelAddressStage2Reg := pixelAddressStage1Reg
	for (i <- 0 until 4) {
		val oneMinusAlpha = UInt(0xff) - newPixelColorStage1Reg(i).alpha
		oldWeightedColorStage2Reg(i).red := (oldPixelColorStage1(i).red * oneMinusAlpha)(15, 8)
		oldWeightedColorStage2Reg(i).green := (oldPixelColorStage1(i).green * oneMinusAlpha)(15, 8)
		oldWeightedColorStage2Reg(i).blue := (oldPixelColorStage1(i).blue * oneMinusAlpha)(15, 8)
		oldWeightedColorStage2Reg(i).alpha := (oldPixelColorStage1(i).alpha * oneMinusAlpha)(15, 8)
	}

	oldPixelColorStage2Reg := oldPixelColorStage1
	newPixelColorStage2Reg := newPixelColorStage1Reg

	// XXX perform depth and stencil checks here
	updatePixelStage2Reg := pixelMaskStage1Reg
	
	//
	// Stage 3:
	// - Blend pixel values, clamp, and writeback
	// - Write new pixel value (if visible)
	//
	def clamp = (x : UInt) => Mux(x < UInt(255), x, UInt(255))
	
	val blendedColor = Vec.fill(4){ new RGBAColor }
	for (i <- 0 until 4) {
		blendedColor(i).red := clamp(oldWeightedColorStage2Reg(i).red + newPixelColorStage2Reg(i).red)
		blendedColor(i).blue := clamp(oldWeightedColorStage2Reg(i).blue + newPixelColorStage2Reg(i).blue)
		blendedColor(i).green := clamp(oldWeightedColorStage2Reg(i).green + newPixelColorStage2Reg(i).green)
		blendedColor(i).alpha := clamp(oldWeightedColorStage2Reg(i).alpha + newPixelColorStage2Reg(i).alpha)
	}
	
	val writebackColor = Mux(enableAlpha, blendedColor, newPixelColorStage2Reg)

	// XXX chisel doesn't support per lane writeback selection. Mux in old value instead
	val writebackMasked = Vec.tabulate(4) { i => Mux(updatePixelStage2Reg(3 - i), writebackColor(3 - i), 
		oldPixelColorStage2Reg(3 - i)) }
	when (updatePixelStage2Reg != UInt(0)) {
		colorMemory.write(pixelAddressStage2Reg, writebackMasked)
	}
	
	// 
	// Resolve logic. We need to send the bytes to the arbiter in scanline
	// order, but they are stored in 2x2 quads. Shuffle and accumulate into
	// a burst buffer.
	//
	val numLanes = burstByteCount / 8	// 8 bytes per memory read
	val laneBits = log2Up(numLanes)
	val writeBufferReg = Reg(Vec.fill(numLanes) { UInt(width = 64) })
	val resolveXQuadReg = Reg(UInt(width = tileCoordBits))
	val resolveYCoordReg = Reg(UInt(width = tileCoordBits + 1)) // scanline, not quad
	val resolveWriteAddressReg = Reg(UInt(width = 32))
	resolveInternalAddr := Cat(resolveYCoordReg(tileCoordBits, 1), resolveXQuadReg)
	io.resolveArbPort.request := resolveStateReg === s_resolve_writeback
	io.resolveArbPort.address := resolveWriteAddressReg
	
	switch (resolveStateReg) {
		is (s_resolve_not_active) {
			when (io.registerUpdate.update && io.registerUpdate.address === regids.reg_start_resolve) {
				// Start resolve
				resolveStateReg := s_resolve_collect_first
				resolveXQuadReg := UInt(0)
				resolveYCoordReg := UInt(0)
				resolveWriteAddressReg := resolveBaseAddress
			}
		}

		is (s_resolve_collect_first) {
			// Send first address, but don't latch a result yet (memory has one
			// cycle of latency)
			resolveStateReg := s_resolve_collect_rest
			resolveXQuadReg := resolveXQuadReg + UInt(1)
		}

		is (s_resolve_collect_rest) {
			// Latch result of last color memory into writeback buffer and 
			// increment address
			when (resolveXQuadReg(laneBits - 1, 0) === UInt(0)) {
				// Write buffer is full, push to memory arbiter
				resolveStateReg := s_resolve_writeback
			}
			.otherwise {
				resolveXQuadReg := resolveXQuadReg + UInt(1)
			}

			val writeLane = ~(resolveXQuadReg(laneBits - 1, 0) - UInt(1))
			writeBufferReg(writeLane) := Mux(resolveYCoordReg(0), 
				Cat(readColorValue(2).toBits, readColorValue(3).toBits),
				Cat(readColorValue(0).toBits, readColorValue(1).toBits))
		}

		is (s_resolve_writeback) {
			// Write data to the bus arbiter
			when (io.resolveArbPort.ready) {
				when (resolveYCoordReg === UInt(tileSize) && resolveXQuadReg === UInt(0)) {
					// Tile is finished
					resolveStateReg := s_resolve_not_active
				}
				.otherwise {
					resolveStateReg := s_resolve_collect_first
				}

				when (resolveXQuadReg === UInt(0)) {
					// The column addr has wrapped; we reached the end of the line.
					resolveWriteAddressReg := resolveWriteAddressReg + UInt(burstByteCount) + 
						resolveStride
					resolveYCoordReg := resolveYCoordReg + UInt(1)
				}
				.otherwise {
					// Next burst in same row
					resolveWriteAddressReg := resolveWriteAddressReg + UInt(burstByteCount)
				}
			}
		}
	}
	
	io.resolveArbPort.data := writeBufferReg.toBits
}



