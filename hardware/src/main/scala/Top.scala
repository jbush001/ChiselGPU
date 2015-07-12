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
// Top level placeholder. This is test code now.
//

class Top (dataWidth : Int) extends Module {
	val io = new Bundle {
		val axiBus = new Axi4Master(dataWidth) 
		val halt = Bool(OUTPUT)
	}

	val tileSize = 64

	val busArbiter = Module(new MemoryArbiter(2, 2, 32))
	val tileBuffer = Module(new TileBuffer(tileSize))
	val commandListProcessor = Module(new CommandListProcessor())
	val rasterizer = Module(new Rasterizer(tileSize / 2))

	busArbiter.io.axiBus <> io.axiBus
	busArbiter.io.readPorts(0) <> commandListProcessor.io.arbiterPort
	busArbiter.io.writePorts(0) <> tileBuffer.io.resolveArbPort
	commandListProcessor.io.registerUpdate <> tileBuffer.io.registerUpdate

	busArbiter.io.readPorts(1).address := UInt(0)
	busArbiter.io.readPorts(1).request := Bool(false)
	busArbiter.io.writePorts(1).request := Bool(false)
	busArbiter.io.writePorts(1).address := UInt(0)

	//
	// XXX test code
	//
	val rastXStep = Vec(SInt(1), SInt(-1), SInt(0))
	val rastYStep = Vec(SInt(-1), SInt(-1), SInt(1))
	val rastInitialXCoord = UInt(0)	
	val rastInitialYCoord = UInt(8)
	val rastInitialVal0 = SInt(-17)
	val rastInitialVal1 = SInt(14)
	val rastInitialVal2 = SInt(-16)
	
	tileBuffer.io.pixelX := rasterizer.io.outputX
	tileBuffer.io.pixelY := rasterizer.io.outputY
	tileBuffer.io.pixelMask := rasterizer.io.outputMask

	val s_start_rasterize :: s_wait_rasterize :: s_start_resolve :: s_wait_resolve :: s_done :: Nil = Enum(UInt(), 5)
	val stateReg = Reg(init = s_start_rasterize)

	commandListProcessor.io.HACK_resolve := stateReg === s_start_resolve
	rasterizer.io.xStep := rastXStep
	rasterizer.io.yStep := rastYStep
	rasterizer.io.initialXCoord := rastInitialXCoord
	rasterizer.io.initialYCoord := rastInitialYCoord
	rasterizer.io.start := stateReg === s_start_rasterize
	rasterizer.io.initialValue(0) := rastInitialVal0
	rasterizer.io.initialValue(1) := rastInitialVal1
	rasterizer.io.initialValue(2) := rastInitialVal2

	for (i <- 0 until 4) {
		tileBuffer.io.pixelColors(i).red := UInt(0xff)
		tileBuffer.io.pixelColors(i).blue := UInt(0)
		tileBuffer.io.pixelColors(i).green := UInt(0)
		tileBuffer.io.pixelColors(i).alpha := UInt(0xff)
	}
	
	io.halt := stateReg === s_done

	switch (stateReg) {
		is (s_start_rasterize) {
			when (rasterizer.io.busy) {
				stateReg := s_wait_rasterize
			}
		}

		is (s_wait_rasterize) {
			when (!rasterizer.io.busy) {
				stateReg := s_start_resolve
			}
		}
		
		is (s_start_resolve) {
			when (tileBuffer.io.resolveActive) {
				stateReg := s_wait_resolve
			}
		}
		
		is (s_wait_resolve) {
			when (!tileBuffer.io.resolveActive) {
				stateReg := s_done
			}
		}
	}
}
