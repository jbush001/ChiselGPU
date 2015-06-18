import Chisel._

class Top (dataWidth : Int) extends Module {
	val io = new Bundle {
		val axiBus = new Axi4Master(dataWidth) 
	}

	def makeColor(red : UInt, blue : UInt, green : UInt, alpha : UInt) = Cat(UInt(alpha, 8), 
		UInt(blue, 8), UInt(green, 8), UInt(red, 8))

	val writeAddress = Reg(UInt(width=32), init=UInt(0))
	val burstCount = Reg(UInt(width = 8), init=UInt(0))

	io.axiBus.bready := Bool(true)
	io.axiBus.arvalid := Bool(false)
	io.axiBus.rready := Bool(false)
	
	val s_send_addr :: s_write_burst :: s_write_ack :: Nil = Enum(UInt(), 3)
	val state = Reg(init = s_send_addr)

	io.axiBus.awvalid := state === s_send_addr
	io.axiBus.awaddr := writeAddress
	io.axiBus.awlen := UInt(8)
	io.axiBus.awsize := UInt(4)
	io.axiBus.wvalid := state === s_write_burst
	io.axiBus.wdata := Mux((writeAddress(5) ^ writeAddress(11)) != UInt(0),
		UInt("hff0000ff"), UInt("hff000000"))
	io.axiBus.bready := state === s_write_ack

	switch (state) {
		is (s_send_addr) {
			when (writeAddress < UInt(64 * 64 * 4)) {
				burstCount := UInt(8)
				when (io.axiBus.awready) {
					state := s_write_burst
				}
			}
		}
		
		is (s_write_burst) {
			when (io.axiBus.wready) {
				burstCount := burstCount - UInt(1)
				writeAddress := writeAddress + UInt(4)
				when (burstCount === UInt(1)) {
					state := s_write_ack
				}
			}
		}
		
		is (s_write_ack) {
			when (io.axiBus.bvalid) {
				state := s_send_addr
			}
		}
	}
}
