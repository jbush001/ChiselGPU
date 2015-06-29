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

	val busArbiter = Module(new BusArbiter(2, 2, 32, 32))
	val tileBuffer = Module(new TileBuffer(64, 32))
	val commandListProcessor = Module(new CommandListProcessor(32))

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

	val pixelXReg = Reg(UInt(width = 6), init = UInt(0))
	var pixelYReg = Reg(UInt(width = 6), init = UInt(0))

	val s_fill :: s_start_resolve :: s_done :: Nil = Enum(UInt(), 3)
	val stateReg = Reg(init = s_fill)

	tileBuffer.io.pixelX := pixelXReg
	tileBuffer.io.pixelY := pixelYReg
	commandListProcessor.io.HACK_resolve := stateReg === s_start_resolve

	tileBuffer.io.pixelValid := stateReg === s_fill
	tileBuffer.io.pixelColor := Vec.fill(4) { UInt(0xff) }

	switch (stateReg) {
		is (s_fill) {
			pixelXReg := pixelXReg + UInt(1)
			pixelYReg := pixelYReg + UInt(1)
			when (pixelXReg === UInt(32)) {
				stateReg := s_start_resolve
			}
		}
		
		is (s_start_resolve) {
			stateReg := s_done
		}
	}
}
