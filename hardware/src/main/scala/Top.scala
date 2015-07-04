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
	}

	val tileSize = 64

	val busArbiter = Module(new MemoryArbiter(2, 2, 32, 32))
	val tileBuffer = Module(new TileBuffer(tileSize, 32))
	val commandListProcessor = Module(new CommandListProcessor(32))
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
	val x0 = 5
	val y0 = 7
	val x1 = 62
	val y1 = 61
	val x2 = 10
	val y2 = 40

	val rastXStep = Vec.fill(3){ SInt(1) }
	val rastYStep = Vec.fill(3){ SInt(-1) }
	val rastInitialXCoord = UInt(0)	// Should be top most point
	val rastInitialYCoord = UInt(0)
	val rastInitialVal0 = SInt(-1)
	val rastInitialVal1 = SInt(-30)
	val rastInitialVal2 = SInt(-30)
	
	tileBuffer.io.pixelX := rasterizer.io.outputX
	tileBuffer.io.pixelY := rasterizer.io.outputY
	tileBuffer.io.pixelMask := rasterizer.io.outputMask

	val s_start_rasterize :: s_wait_rasterize :: s_start_resolve :: s_done :: Nil = Enum(UInt(), 4)
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

	var i = 0;
	for (i <- 0 until 4) {
		tileBuffer.io.pixelColors(i).red := UInt(0xff)
		tileBuffer.io.pixelColors(i).blue := UInt(0)
		tileBuffer.io.pixelColors(i).green := UInt(0)
		tileBuffer.io.pixelColors(i).alpha := UInt(0xff)
	}

	switch (stateReg) {
		is (s_start_rasterize) {
			stateReg := s_wait_rasterize
		}

		is (s_wait_rasterize) {
			stateReg := s_start_resolve
		}
		
		is (s_start_resolve) {
			stateReg := s_done
		}
	}
}
