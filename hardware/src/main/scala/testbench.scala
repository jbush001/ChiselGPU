import Chisel._

class Testbench extends Module {
	val io = new Bundle {}
	val top = Module(new Top(32))
	val systemMemory = Module(new AxiSram(32, 64 * 64))
	top.io.axiBus <> systemMemory.io
}

object main {
	def main(args: Array[String]): Unit = {
		if (args.contains("v")) {
			// For synthesis, generate only 'Top' module
			chiselMain(args, () => Module(new Top(32)))
		} else {
			// For simulation, create Testbench wrapper
			chiselMain(args, () => Module(new Testbench()))
		}
	}
}
