import Chisel._

class Testbench extends Module {
	val io = new Bundle {}
	val top = Module(new Top(32))
}

object main {
	def main(args: Array[String]): Unit = {
		// Generate different modules depending if we are in simulation or synthesis
		print(args)
		if (args.contains("v")) {
			chiselMain(args, () => Module(new Top(32)))
		} else {
			chiselMain(args, () => Module(new Testbench()))
		}
	}
}
