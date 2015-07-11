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
// Memory arbiter. Performs the following functions:
// - Serializes requests from multiple masters for the external system
//   memory bus.
// - Hides external bus protocol behind a simpler request/response
//   interface.
//
// Caveats:
// - Does not attempt to perform caching or read/write combining
// - Will not consolidate requests for the same address
// It is expected that individual masters will take care of this
// Usually these don't end up being optimized cases. If they are, the
// interfaces can optimized more efficiently for their own use pattern.
//

class ArbiterReadPort(burstByteCount : Int) extends Bundle {
	val request = Bool(INPUT)
	val ack = Bool(OUTPUT) 
	val address = UInt(INPUT, 32)
	val data = UInt(OUTPUT, burstByteCount * 8)
	
	override def clone = new ArbiterReadPort(burstByteCount).asInstanceOf[this.type]
}

class ArbiterWritePort(burstByteCount : Int) extends Bundle {
	val request = Bool(INPUT)
	val ready = Bool(OUTPUT) 
	val address = UInt(INPUT, 32)
	val data = UInt(INPUT, burstByteCount * 8)

	override def clone = new ArbiterWritePort(burstByteCount).asInstanceOf[this.type]
}

class MemoryArbiter(numReadPorts : Int, numWritePorts : Int, axiDataWidthBits : Int, 
	burstByteCount : Int) extends Module {

	val io = new Bundle {
		val readPorts = Vec.fill(numReadPorts)(new ArbiterReadPort(burstByteCount))
		val writePorts = Vec.fill(numWritePorts)(new ArbiterWritePort(burstByteCount))
		val axiBus = new Axi4Master(axiDataWidthBits)
	}

	val burstTransferCount = burstByteCount / (axiDataWidthBits / 8)

	def convertToVec(bitArray : UInt, bitsPerElement : Int) = Vec.tabulate(bitArray.getWidth() /
		bitsPerElement)((i : Int) => bitArray((i + 1) * bitsPerElement - 1, i * bitsPerElement))

	//
	// Read Logic
	//
	val s_read_idle :: s_send_read_addr :: s_read_burst_active :: s_read_burst_complete :: Nil = Enum(UInt(), 4)
	val readStateReg = Reg(init = s_read_idle)
	val readDataReg = Reg(UInt(width = burstByteCount * 8))
	val activeReaderReg = Reg(UInt(width = log2Up(numReadPorts)))
	val readBurstCountReg = Reg(UInt(width = log2Up(burstTransferCount)))

	for (i <- 0 until numReadPorts ) {
		io.readPorts(i).data := readDataReg
		io.readPorts(i).ack := (readStateReg === s_read_burst_complete) && 
			(activeReaderReg === UInt(i))
	}

	io.axiBus.arvalid := readStateReg === s_send_read_addr
	io.axiBus.araddr := io.readPorts(activeReaderReg).address
	io.axiBus.arlen := UInt(burstTransferCount - 1)
	io.axiBus.arsize := UInt(log2Down(burstByteCount))
	io.axiBus.rready := readStateReg === s_read_burst_active
	val readLanes = convertToVec(readDataReg, axiDataWidthBits)
	val readArbiter = Module(new Arbiter(numReadPorts))
	var readRequestBitmap = UInt(io.readPorts(0).request && readStateReg === s_read_idle)
	for (i <- 1 until numReadPorts)
		readRequestBitmap = Cat(io.readPorts(i).request && readStateReg === s_read_idle, readRequestBitmap)
	
	readArbiter.io.request := readRequestBitmap
	readArbiter.io.enableUpdate := Bool(true)

	switch (readStateReg) {
		is (s_read_idle) {
			when (readArbiter.io.grantOneHot != UInt(0)) {
				// Choose a master to begin a read transaction
				activeReaderReg := OHToUInt(readArbiter.io.grantOneHot)
				readStateReg := s_send_read_addr
			}
		}
		
		is (s_send_read_addr) {
			when (io.axiBus.arready) {
				readStateReg := s_read_burst_active
				readBurstCountReg := UInt(0)
			}
		}

		is (s_read_burst_active) {
			when (io.axiBus.rvalid) {
				readLanes(~readBurstCountReg) := io.axiBus.rdata
				readBurstCountReg := readBurstCountReg + UInt(1)
				when (readBurstCountReg === UInt(burstTransferCount - 1)) {
					readStateReg := s_read_burst_complete
				}
			}
		}

		is (s_read_burst_complete) {
			readStateReg := s_read_idle
		}
	}

	//
	// Write Logic
	//
	val s_write_idle :: s_send_write_addr :: s_write_burst_active :: s_write_burst_complete :: Nil = Enum(UInt(), 4)
	val writeStateReg = Reg(init = s_write_idle)
	val activeWriterReg = Reg(UInt(width = log2Up(numWritePorts)))
	val writeBurstCountReg = Reg(UInt(width = log2Up(burstTransferCount)))

	class WriteBuffer extends Bundle {
		val latched = Bool()
		val address = UInt(width = 32)
		val data = UInt(width = burstByteCount * 8)

		override def clone = (new WriteBuffer).asInstanceOf[this.type]
	}

	val writeBuffers = Vec.fill(numWritePorts) { Reg(new WriteBuffer) }
	for (i <- 0 until numWritePorts) {
		io.writePorts(i).ready := !writeBuffers(i).latched
		when (io.writePorts(i).request && io.writePorts(i).ready) {
			writeBuffers(i).latched := Bool(true)
			writeBuffers(i).address := io.writePorts(i).address
			writeBuffers(i).data := io.writePorts(i).data
		}
		.elsewhen (writeStateReg === s_write_burst_complete && io.axiBus.bvalid
			&& activeWriterReg === UInt(i)) {
			assert(writeBuffers(i).latched, "Write burst completed on inactive write buffer")
			writeBuffers(i).latched := Bool(false)
		}
	}

	io.axiBus.awvalid := writeStateReg === s_send_write_addr
	io.axiBus.awaddr := writeBuffers(activeWriterReg).address
	io.axiBus.awlen := UInt(burstTransferCount - 1)
	io.axiBus.awsize := UInt(log2Down(burstByteCount))
	io.axiBus.wvalid := writeStateReg === s_write_burst_active

	// Select the appropriate write data
	val writeData = writeBuffers(activeWriterReg).data
	val writeLanes = convertToVec(writeData, axiDataWidthBits)
	io.axiBus.wdata := writeLanes(~writeBurstCountReg)
	io.axiBus.wlast := (writeBurstCountReg === UInt(burstTransferCount - 1)) && 
		(writeStateReg === s_write_burst_active)
	io.axiBus.bready := writeStateReg === s_write_burst_complete
	
	val writeArbiter = Module(new Arbiter(numWritePorts))
	var writeRequestBitmap = UInt(writeBuffers(0).latched && writeStateReg === s_write_idle)
	for (i <- 1 until numWritePorts)
		writeRequestBitmap = Cat(writeBuffers(i).latched && writeStateReg === s_write_idle, writeRequestBitmap)

	writeArbiter.io.request := writeRequestBitmap
	writeArbiter.io.enableUpdate := Bool(true)

	switch (writeStateReg) {
		is (s_write_idle) {
			when (writeArbiter.io.grantOneHot != UInt(0)) {
				// Choose a master to begin a write transaction
				activeWriterReg := OHToUInt(writeArbiter.io.grantOneHot)
				writeStateReg := s_send_write_addr
			}
		}
		
		is (s_send_write_addr) {
			when (io.axiBus.awvalid) {
				writeStateReg := s_write_burst_active
				writeBurstCountReg := UInt(0)
			}
		}
		
		is (s_write_burst_active) {
			when (io.axiBus.wready) {
				writeBurstCountReg := writeBurstCountReg + UInt(1)
				when (writeBurstCountReg === UInt(burstTransferCount - 1)) {
					writeStateReg := s_read_burst_complete
				}
			}
		}

		is (s_write_burst_complete) {
			when (io.axiBus.bvalid) {
				writeStateReg := s_write_idle
			}
		}
	}
}

class MemoryArbiterTest(c : MemoryArbiter) extends Tester(c) {
	def makeBurstBuffer(start : Int) : BigInt = (start until start + 8).foldLeft(BigInt(0))((x, y) => (x << 32) + BigInt(y))
		
	print(makeBurstBuffer(19))
		
	poke(c.io.axiBus.awready, 0)
	poke(c.io.axiBus.arready, 0)
	poke(c.io.axiBus.wready, 0)

	// Send four requests simultaneously
	poke(c.io.readPorts(0).request, 1)
	poke(c.io.readPorts(0).address, 320)
	poke(c.io.readPorts(1).request, 1)
	poke(c.io.readPorts(1).address, 352)
	poke(c.io.writePorts(0).request, 1)
	poke(c.io.writePorts(0).address, 96)
	poke(c.io.writePorts(0).data, makeBurstBuffer(29))
	poke(c.io.writePorts(1).request, 1)
	poke(c.io.writePorts(1).address, 128)
	poke(c.io.writePorts(1).data, makeBurstBuffer(51))
	expect(c.io.writePorts(0).ready, 1)
	expect(c.io.writePorts(1).ready, 1)
	poke(c.io.writePorts(0).request, 1)
	poke(c.io.writePorts(1).request, 1)
	step(1)
	poke(c.io.writePorts(0).request, 0)
	poke(c.io.writePorts(1).request, 0)
	step(1)
	expect(c.io.axiBus.arvalid, 1)
	expect(c.io.axiBus.awvalid, 1)

	def expectWriteBurst(address : BigInt, dataStart : BigInt) = {
		// Address
		expect(c.io.axiBus.awaddr, address) 
		poke(c.io.axiBus.awready, 1)
		step(1)
		poke(c.io.axiBus.awready, 0)

		// Wait four cycles before asserting wready to ensure 
		// the arbiter waits
		step(4)
		poke(c.io.axiBus.wready, 1)
		for (i <- 0 until 8) {
			expect(c.io.axiBus.wvalid, 1)
			expect(c.io.axiBus.wdata, i + dataStart)
			step(1)
		}

		poke(c.io.axiBus.wready, 0)
		expect(c.io.axiBus.wvalid, 0)
		step(1)

		// Write acknowledgement. Wait four cycles before
		// acknowledging.
		expect(c.io.axiBus.bready, 1)
		step(4)
		expect(c.io.axiBus.bready, 1)
		poke(c.io.axiBus.bvalid, 1)
		step(1)
		poke(c.io.axiBus.bvalid, 0)
		step(1)
	}
	
	expectWriteBurst(96, 29)
	expectWriteBurst(128, 51)
	
	def expectReadBurst(address : BigInt, dataStart : BigInt) = {
		// Address
		expect(c.io.axiBus.araddr, address) 
		poke(c.io.axiBus.arready, 1)
		step(1)
		poke(c.io.axiBus.arready, 0)

		//
		// Wait four cycles before asserting rvalid
		//
		step(4)
		poke(c.io.axiBus.rvalid, 1)
		for (i <- 0 until 8) {
			expect(c.io.axiBus.rready, 1)
			poke(c.io.axiBus.rdata, i + dataStart)
			step(1)
		}

		poke(c.io.axiBus.rvalid, 0)
	}

	expectReadBurst(320, 97)
	expect(c.io.readPorts(0).ack, 1)
	expect(c.io.readPorts(0).data, makeBurstBuffer(97))
	step(1)
	expect(c.io.readPorts(0).ack, 0)
	step(1)

	expectReadBurst(352, 192)
	expect(c.io.readPorts(1).ack, 1)
	expect(c.io.readPorts(1).data, makeBurstBuffer(192))
	step(1)
	expect(c.io.readPorts(1).ack, 0)
	step(1)
}
