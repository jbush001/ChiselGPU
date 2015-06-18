import Chisel._

class Testbench extends Module {
	val io = new Bundle {}
	val top = Module(new Top(32))
	val systemMemory = Module(new AxiSram(32, 64 * 64))
	top.io.axiBus <> systemMemory.io
}

object main {
	def main(args: Array[String]): Unit = {
		// Generate different modules depending if we are in simulation or synthesis
		if (args.contains("v")) {
			chiselMain(args, () => Module(new Top(32)))
		} else {
			chiselMain(args, () => Module(new Testbench()))
		}
	}
}
