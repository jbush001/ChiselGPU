import Chisel._

class Top (dataWidth : Int) extends Module {
	val io = new Bundle {
		val axiBus = new Axi4Master(dataWidth) 
	}

	val busArbiter = Module(new BusArbiter(2, 2, 32, 32))
	busArbiter.io.axiBus <> io.axiBus
	busArbiter.io.readPorts(1).address := UInt(0)
	busArbiter.io.readPorts(1).request := Bool(false)
	busArbiter.io.writePorts(1).request := Bool(false)
	busArbiter.io.writePorts(1).address := UInt(0)

	// Fill memory with checkerboard pattern
	val checker0 = UInt("hffffffffffffffffffffffffffffffffff000000ff000000ff000000ff000000")
	val checker1 = UInt("hff000000ff000000ff000000ff000000ffffffffffffffffffffffffffffffff")

	val writeEnable0 = Reg(Bool(), init=Bool(false))
	val writeAddress0 = Reg(UInt(width=32), init=UInt(0))
	val writeData0 = Mux(writeAddress0(10) != UInt(0), checker1, checker0)
	
	busArbiter.io.writePorts(0).request := writeEnable0
	busArbiter.io.writePorts(0).address := writeAddress0
	busArbiter.io.writePorts(0).data := writeData0
	
	val readEnable0 = Reg(Bool(), init=Bool(false))
	val readAddress0 = Reg(UInt(width = 32), init = UInt(0))

	busArbiter.io.readPorts(0).request := readEnable0
	busArbiter.io.readPorts(0).address := readAddress0
	
	when (writeAddress0 < UInt(64 * 64 * 4)) {
		writeEnable0 := Bool(true)
		when (busArbiter.io.writePorts(0).ack) {
			writeAddress0 := writeAddress0 + UInt(32)
		}
	}
	.otherwise {
		writeEnable0 := Bool(false)
		
		// Read four bursts back after writing
		when (readAddress0 < UInt(32 * 4 * 4)) {
			readEnable0 := Bool(true)
			when (busArbiter.io.readPorts(0).ack) {
				printf("read %x\n", busArbiter.io.readPorts(0).data);
				readAddress0 := readAddress0 + UInt(32 * 4)
			}
		}
		.otherwise {
			readEnable0 := Bool(false)
		}
	}
}
