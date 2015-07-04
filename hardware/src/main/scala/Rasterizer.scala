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

object StepDir {
	val step_none :: step_right :: step_down :: step_left :: Nil = Enum(UInt(), 4)
}

//
// Rasterizer. This sweeps over the triangle in a fashion similar to that
// described by Pineda "A parallel algorithm for polygon rasterization"
// (SIGGRAPH 88), Figure 4. It outputs 2x2 aligned quads with coverage masks.
//

class StepControl extends Bundle {
	val xStep = Vec.fill(3)(SInt(INPUT, 32))
	val yStep = Vec.fill(3)(SInt(INPUT, 32))
	val stepDir = UInt(INPUT, 2)
}

class EdgeFunction(edgeIndex : Int) extends Module {
	val io = new Bundle {
		val stepControl = new StepControl
		val setEdgeValue = Bool(INPUT)
		val newEdgeValue = SInt(INPUT, 32)
		val inside = Bool(OUTPUT)
	}

	val edgeValue = Reg(SInt(width = 32))
	when (io.setEdgeValue) {
		edgeValue := io.newEdgeValue
	}
	.elsewhen (io.stepControl.stepDir === StepDir.step_down) {
		edgeValue := edgeValue + io.stepControl.yStep(edgeIndex)
	} 
	.elsewhen (io.stepControl.stepDir === StepDir.step_left) {
		edgeValue := edgeValue - io.stepControl.xStep(edgeIndex)
	}
	.elsewhen(io.stepControl.stepDir === StepDir.step_right) {
		edgeValue := edgeValue + io.stepControl.xStep(edgeIndex)
	}

	io.inside := edgeValue(31)
}

class TriangleFunction extends Module {
	val io = new Bundle {
		val stepControl = new StepControl
		val setEdgeValue = Bool(INPUT)
		val newEdgeValue = Vec.fill(3)(SInt(INPUT, 32))
		val inside = Bool(OUTPUT)
	}

	val edges = Vec.tabulate(3)(i => Module(new EdgeFunction(i)).io)
	var i = 0
	for (i <- 0 until 3) {
		edges(i).stepControl := io.stepControl
		edges(i).setEdgeValue := io.setEdgeValue
		edges(i).newEdgeValue := io.newEdgeValue(i)
	}

	io.inside := edges.map(_.inside).reduceLeft(_ && _)
}
	

// 0 1
// 2 3
class QuadTriangleFunctions extends Module {
	val io = new Bundle {
		val stepControl = new StepControl
		val setEdgeValue = Bool(INPUT)
		val newEdgeValue = Vec.fill(3)(SInt(INPUT, 32))
		val coverageMask = UInt(OUTPUT, 4)
	}

	val pixels = Vec.fill(4){ Module(new TriangleFunction()).io }
	var i = 0
	for (i <- 0 until 4) {
		pixels(i).stepControl := io.stepControl
		pixels(i).setEdgeValue := io.setEdgeValue
	}

	io.coverageMask := Cat(pixels.map(_.inside))
	
	for (i <- 0 until 3) {
		pixels(0).newEdgeValue(i) := io.newEdgeValue(i)
		pixels(1).newEdgeValue(i) := io.newEdgeValue(i) + io.stepControl.xStep(i)
		pixels(2).newEdgeValue(i) := io.newEdgeValue(i) + io.stepControl.yStep(i)
		pixels(3).newEdgeValue(i) := io.newEdgeValue(i) + io.stepControl.xStep(i) + 
			io.stepControl.yStep(i)
	}
}

class Rasterizer(tileSize : Int) extends Module {
	val io = new Bundle {
		val xStep = Vec.fill(3){ SInt(INPUT, 32) }
		val yStep = Vec.fill(3){ SInt(INPUT, 32) }
		val initialXCoord = UInt(INPUT, 16)
		val initialYCoord = UInt(INPUT, 16)
		val initialValue = Vec.fill(3){ SInt(INPUT, 32) }
		val start = Bool(INPUT)
		val busy = Bool(OUTPUT)
		val outputX = UInt(OUTPUT, 16)
		val outputY = UInt(OUTPUT, 16)
		val outputMask = UInt(OUTPUT, 4) 
	}
	
	val (s_idle ::
		s_initial_scan ::
		s_scan_right ::
		s_find_right_edge ::
		s_scan_left ::
		s_find_left_edge :: Nil) = Enum(UInt(), 6)

	val stateReg = Reg(UInt(), init = s_idle)
	val xPos = Reg(UInt(width = 16))
	val yPos = Reg(UInt(width = 16))
	val pixelValid = Reg(Bool(), init = Bool(false))
	val stepDir = UInt(width = 2)

	val quad = Module(new QuadTriangleFunctions)
	quad.io.stepControl.xStep := io.xStep
	quad.io.stepControl.yStep := io.yStep
	quad.io.stepControl.stepDir := stepDir
	quad.io.setEdgeValue := io.start && stateReg === s_idle
	quad.io.newEdgeValue := io.initialValue

	io.outputMask := quad.io.coverageMask & Fill(4, pixelValid)	

	io.outputX := xPos
	io.outputY := yPos
	io.busy := stateReg != s_idle

	stepDir := StepDir.step_none
	
	switch (stepDir) {
		is (StepDir.step_left) {
			xPos := xPos - UInt(1)
		}
		
		is (StepDir.step_down) {
			yPos := yPos + UInt(1)
		}
		
		is (StepDir.step_right) {
			xPos := xPos + UInt(1)
		}
	}
	
	switch (stateReg) {
		is (s_idle) {
			when (io.start) {
				stateReg := s_initial_scan
				xPos := io.initialXCoord
				yPos := io.initialYCoord
			}
		}
		
		is (s_initial_scan) {
			when (quad.io.coverageMask != UInt(0)) {
				// Found a pixel
				stateReg := s_scan_right
				pixelValid := Bool(true)
			}
			.elsewhen (xPos === UInt(tileSize)) {
				// Couldn't find first pixel, give up
				stateReg := s_idle
				pixelValid := Bool(false)
			}
			.otherwise {
				// Walk right looking for left edge
				stepDir := StepDir.step_right
			}
		}
		
		is (s_scan_right) {
			when (xPos === UInt(tileSize)) {
				// Hit edge, step down
				stepDir := StepDir.step_down
				stateReg := s_scan_left
				pixelValid := Bool(false)
			}
			.elsewhen (quad.io.coverageMask != UInt(0)) {
				// Step to the right
				stepDir := StepDir.step_right
			}
			.otherwise {
				// Right edge of triangle, step down
				stateReg := s_find_right_edge
				stepDir := StepDir.step_down
				pixelValid := Bool(false)
			}
		}
		
		is (s_find_right_edge) {
			when (quad.io.coverageMask != UInt(0) && xPos != UInt(tileSize)) {
				// Walk to the right until we are past the edge
				stepDir := StepDir.step_right
			}
			.otherwise {
				// Begin stepping backward
				// XXX this will fail if the left is not visible
				stateReg := s_scan_left
				stepDir := StepDir.step_left
				pixelValid := Bool(true)
			}
		}
		
		is (s_scan_left) {
			when (quad.io.coverageMask != UInt(0) && xPos != UInt(0)) {
				// Step left, filling pixels
				stepDir := StepDir.step_left
			}
			.elsewhen (yPos === UInt(tileSize - 1)) {
				// Needed to step down, but at the bottom of tile
				stateReg := s_idle
				pixelValid := Bool(false)
			} .otherwise {
				// Step to next line
				stateReg := s_find_left_edge
				pixelValid := Bool(false)
				stepDir := StepDir.step_down
			}
		}
		
		is (s_find_left_edge) {
			when (quad.io.coverageMask != UInt(0) && xPos != UInt(0)) {
				stepDir := StepDir.step_left
			}
			.otherwise {
				stepDir := StepDir.step_right
				stateReg := s_scan_right
				pixelValid := Bool(true)
			}
		}
	}
}

