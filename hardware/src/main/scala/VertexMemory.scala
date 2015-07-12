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
// VertexMemory is used to store vertex attributes read from memory
// and parameters computed by vertex shaders. These are processed by
// the shader in batches of 16. Each memory location is 16 32-bit values
// wide. They are packed by batch, with incoming attributes first, then
// outgoing parameters:
//
//  Batch 0 -------------------------
//  Incoming     0  |  0  |   1  |  2  | ... | 15  |
//  Attributes   1  |  0  |   1  |  2  | ... | 15  |
//               2  |  0  |   1  |  2  | ... | 15  |
//              ... |
//               n  |  0  |   1  |  2  | ... | 15  |
//  Outgoing     0  |  0  |   1  |  2  | ... | 15  |
//  Parameters   1  |  0  |   1  |  2  | ... | 15  |
//               2  |  0  |   1  |  2  | ... | 15  |
//              ... |
//               n  |  0  |   1  |  2  | ... | 15  |
// -------------------------------------
//
//
//
// The ports use request/ack handshaking. When a unit wants access, it asserts
// request and any other control signals. These must remain asserted until ack 
// is asserted one or more cycles later. Request should be deasserted on the 
// next cycle. There should be no combinational dependency between request and 
// ack.


// Data is valid in the same cycle ack is asserted.
class VertexMemoryReadPort extends Bundle {
	val request = Bool(INPUT)
	val address = UInt(INPUT, width = 16)	
	val ack = Bool(OUTPUT)
	val data = UInt(OUTPUT, width = 32 * 16)
}

class VertexMemoryWritePort extends Bundle {
	val request = Bool(INPUT)
	val data = UInt(INPUT, width = 32 * 16)
	val mask = UInt(INPUT, width = 16)
	val address = UInt(INPUT, width = 16)
	val ack = Bool(OUTPUT)
}

//
// Currently VertexMemory has one read port and one write port. If it were 
// advantageous for area, these could be combined into one read/write port.
//
class VertexMemory(numReadPorts : Int, numWritePorts : Int, size : Int) extends Module {
	val addrWidth = log2Up(size)
	
	val io = new Bundle {
		val readPorts = Vec.fill(numReadPorts){ new VertexMemoryReadPort }
		val writePorts = Vec.fill(numWritePorts){ new VertexMemoryWritePort }
	}
	
	val memory = Mem(UInt(width = 32 * 16), size, seqRead = true)

	//
	// Read handling
	//
	val readAddressReg = Reg(UInt(width = addrWidth))
	val readValue = memory(readAddressReg)
	val readArbiter = Module(new Arbiter(numReadPorts))
	val readAckReg = Reg(UInt(width = numReadPorts), init=UInt(0))

	readArbiter.io.request := UInt(0)
	for (i <- 0 until numReadPorts) {
		io.readPorts(i).data := readValue
		io.readPorts(i).ack := readAckReg(i)
	}

	readArbiter.io.request := Cat((0 until numReadPorts).map(i => io.readPorts(numReadPorts - 1 - i).request && 
		!readAckReg(numReadPorts - 1 - i)))

	val readAddressNext = Mux1H(readArbiter.io.grantOneHot, io.readPorts.map(_.address))
	readAckReg := readArbiter.io.grantOneHot
	readAddressReg := readAddressNext

	// 
	// Write handling
	//
	val writeArbiter = Module(new Arbiter(numWritePorts))
	writeArbiter.io.request := Cat((0 until numWritePorts).map(i => io.writePorts(numWritePorts - 1 - i).request))
	val writeAckReg = Reg(UInt(width = numWritePorts), init=UInt(0))
	writeAckReg := writeArbiter.io.grantOneHot
	
	val writeMask = Mux1H(writeArbiter.io.grantOneHot, io.writePorts.map(_.mask))
	val writeAddress = Mux1H(writeArbiter.io.grantOneHot, io.writePorts.map(_.address))
	val writeData = Mux1H(writeArbiter.io.grantOneHot, io.writePorts.map(_.data))

	for (i <- 0 until numReadPorts) {
		io.writePorts(i).ack := writeAckReg(i)
	}

	when (writeArbiter.io.grantOneHot != UInt(0)) {
		memory.write(writeAddress, writeData, Cat((0 until 16).map(i => Fill(32, writeMask(15 - i)))))
	}
}

// Test bench creates with 2 read ports, 2 write ports, and 32 words of memory
class VertexMemoryTest(c : VertexMemory) extends Tester(c) {
	// Two simultaneous writes
	poke(c.io.writePorts(0).request, 1)
	poke(c.io.writePorts(0).address, 10)
	poke(c.io.writePorts(0).mask, 1)
	poke(c.io.writePorts(0).data, 1234)
	poke(c.io.writePorts(1).request, 1)
	poke(c.io.writePorts(1).address, 10)
	poke(c.io.writePorts(1).mask, 2)
	poke(c.io.writePorts(1).data, BigInt(5678) << 32)
	step(1)
	expect(c.io.writePorts(0).ack, 1)
	expect(c.io.writePorts(1).ack, 0)
	poke(c.io.writePorts(0).request, 0)
	step(1)
	expect(c.io.writePorts(0).ack, 0)
	expect(c.io.writePorts(1).ack, 1)
	poke(c.io.writePorts(1).request, 0)
	step(1)
	expect(c.io.writePorts(0).ack, 0)
	expect(c.io.writePorts(1).ack, 0)
	
	// Read back values
	poke(c.io.readPorts(0).request, 1)
	poke(c.io.readPorts(0).address, 10)
	poke(c.io.readPorts(1).request, 1)
	poke(c.io.readPorts(1).address, 10)
	step(1)
	expect(c.io.readPorts(0).ack, 1)
	expect(c.io.readPorts(1).ack, 0)
	poke(c.io.readPorts(0).request, 0)
	poke(c.io.readPorts(0).data, (BigInt(5678) << 32) | 1234)
	step(1)
	expect(c.io.readPorts(0).ack, 0)
	expect(c.io.readPorts(1).ack, 1)
	poke(c.io.readPorts(1).request, 0)
	poke(c.io.readPorts(1).data, (BigInt(5678) << 32) | 1234)
	step(1)
	expect(c.io.readPorts(0).ack, 0)
	expect(c.io.readPorts(1).ack, 0)
}

