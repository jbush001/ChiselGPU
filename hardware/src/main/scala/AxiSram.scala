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

	val s_idle :: s_read_burst :: s_write_burst :: s_write_ack :: Nil = Enum(UInt(), 4)

	val memory = Mem(UInt(width = dataWidth), size, seqRead = true)
	val stateReg = Reg(init = s_idle)
	val burstAddressReg = Reg(UInt(width = 32))
	val burstCountReg = Reg(UInt(width = 8))
	val writeLatchedReg = Reg(Bool(), init = Bool(false))
	val writeAddressReg = Reg(UInt(width=32))
	val readLatchedReg = Reg(Bool(), init = Bool(false))
	val readAddressReg = Reg(UInt(width=32))

	io.wready := stateReg === s_write_burst
	io.bvalid := stateReg === s_write_ack
	io.rvalid := stateReg === s_read_burst
	io.rdata := memory(burstAddressReg)

	io.awready := !writeLatchedReg
	when (io.awready && io.awvalid) {
		writeLatchedReg := Bool(true)
		writeAddressReg := io.awaddr
	}
	
	io.arready := !readLatchedReg
	when (io.arready && io.arvalid) {
		readLatchedReg := Bool(true)
		readAddressReg := io.awaddr
	}

	switch (stateReg) {
		is (s_idle) {
			when (writeLatchedReg) { 
				stateReg := s_write_burst
				burstAddressReg := io.awaddr(31, 2)
				burstCountReg := io.awlen
				writeLatchedReg := Bool(false)
			}
			.elsewhen (readLatchedReg) {
				stateReg := s_read_burst
				burstAddressReg := io.araddr(31, 2)
				burstCountReg := io.arlen
				readLatchedReg := Bool(false)
			}
		} 
		is (s_read_burst) {
			when (io.rready) {
				when (burstCountReg === UInt(0)) {
					stateReg := s_idle
				}
				.otherwise {
					burstAddressReg := burstAddressReg + UInt(1)
					burstCountReg := burstCountReg - UInt(1)
				}
			}
		} 
		is (s_write_burst) {
			when (io.wvalid) {
				memory(burstAddressReg) := io.wdata
				when (burstCountReg === UInt(0)) {
					stateReg := s_write_ack
				}
				.otherwise {
					burstAddressReg := burstAddressReg + UInt(1)
					burstCountReg := burstCountReg - UInt(1)
				}
			}
		} 
		is (s_write_ack) {
			when (io.bready) {
				stateReg := s_idle
			}
		}
	}
}

class AxiSramTest(c : AxiSram) extends Tester(c) {
	for (burstCount <- 0 until 2) {
		val baseDataValue = burstCount * 12345
		val burstAddress = burstCount * 256
		
		// Write Burst
		poke(c.io.awvalid, 1)
		expect(c.io.awready, 1)
		poke(c.io.awaddr, burstAddress)
		poke(c.io.awlen, 7)
		step(1)
		poke(c.io.awvalid, 0)
		step(1)
		for (x <- 0 until 8) {
			poke(c.io.wvalid, 1)
			poke(c.io.wdata, x + baseDataValue)
			expect(c.io.wready, 1)
			step(1)
		}

		poke(c.io.wvalid, 0)
		poke(c.io.bready, 1)
		step(1)
	
		// Read burst. Validate value we just wrote
		poke(c.io.arvalid, 1)
		expect(c.io.arready, 1)
		poke(c.io.araddr, burstAddress)
		poke(c.io.arlen, 7)
		step(2)
		poke(c.io.arvalid, 0)
		poke(c.io.rready, 1)
		for (x <- 0 until 8) {
			expect(c.io.rvalid, 1)
			expect(c.io.rdata, x + baseDataValue)
			step(1)
		}

		poke(c.io.rready, 0)
		step(2)
	}
}
