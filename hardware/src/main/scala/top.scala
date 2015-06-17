import Chisel._

class Top (dataWidth : Int) extends Module {
	val io = new Bundle {
		val axiBus = new Axi4Master(dataWidth) 
	}

	val writeAddress = Reg(UInt(width=32))
	val writeCount = Reg(UInt(width = 8))
	val totalBursts = Reg(UInt(width = 8), init = UInt(8))

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
	io.axiBus.wdata := writeAddress
	io.axiBus.bready := state === s_write_ack

	switch (state) {
		is (s_send_addr) {
			when (totalBursts != UInt(0)) {
				writeCount := UInt(8)
				when (io.axiBus.awready) {
					state := s_write_burst
					totalBursts := totalBursts - UInt(1)
				}
			}
		}
		
		is (s_write_burst) {
			when (io.axiBus.wready) {
				writeCount := writeCount - UInt(1)
				writeAddress := writeAddress + UInt(4)
				when (writeCount === UInt(1)) {
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
