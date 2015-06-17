import Chisel._

class Top (dataWidth : Int) extends Module {
	
	val io = new Bundle {
		val countOut = UInt(OUTPUT, dataWidth)
	}
	
	val counter = Reg(UInt(width = dataWidth), init = UInt(0))
	counter := counter + UInt(1)
	printf("counter = %d\n", counter);
	io.countOut := counter
}
