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
// SRAM with AXI bus interface
//

class AxiSram(dataWidth : Int, size : Int) extends Module {
	val io = new Axi4Master(dataWidth).flip
	
	val memory = Mem(UInt(width = 32), size, seqRead = true)
	
	val s_idle :: s_read_burst :: s_write_burst :: s_write_ack :: Nil = Enum(UInt(), 4)
	val state = Reg(init = s_idle)
	val burstAddress = Reg(UInt(width = 32))
	val burstCount = Reg(UInt(width = 8))
	io.wready := state === s_write_burst;
	io.bvalid := state === s_write_ack;

	val memoryReadValid = Reg(Bool(), init=Bool(false))
	io.rvalid := memoryReadValid;
	val memoryReadValue = Reg(UInt(width = 32))
	memoryReadValue := memory(burstAddress)
	io.rdata := memoryReadValue

	val writeLatched = Reg(Bool())
	val writeAddress = Reg(UInt(width=32))
	val readLatched = Reg(Bool())
	val readAddress = Reg(UInt(width=32))

	io.awready := !writeLatched
	io.arready := !readLatched

	when (!writeLatched && io.awvalid) {
		writeLatched := Bool(true)
		writeAddress := io.awaddr
	}
	
	when (!readLatched && io.arvalid) {
		readLatched := Bool(true)
		readAddress := io.awaddr
	}

	switch (state) {
		is (s_idle) {
			when (writeLatched) { 
				state := s_write_burst
				burstAddress := io.awaddr(31, 2)
				burstCount := io.awlen
				writeLatched := Bool(false)
			}
			.elsewhen (readLatched) {
				state := s_read_burst
				burstAddress := io.araddr(31, 2)
				burstCount := io.arlen
				readLatched := Bool(false)
			}
		} 
		is (s_read_burst) {
			memoryReadValid := Bool(true)
			when (io.rready) {
				when (burstCount === UInt(0)) {
					state := s_idle
					memoryReadValid := Bool(false)
				}
				.otherwise {
					burstAddress := burstAddress + UInt(1)
					burstCount := burstCount - UInt(1)
				}
			}
		} 
		is (s_write_burst) {
			when (io.wvalid) {
				memory(burstAddress) := io.wdata
				when (burstCount === UInt(0)) {
					state := s_write_ack
				}
				.otherwise {
					burstAddress := burstAddress + UInt(1)
					burstCount := burstCount - UInt(1)
				}
			}
		} 
		is (s_write_ack) {
			when (io.bready) {
				state := s_idle
			}
		}
	}
}


