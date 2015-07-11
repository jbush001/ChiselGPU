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
// Top level simulation testbench, and main function for both simulation and
// synthesis.
//

class Testbench extends Module {
	val io = new Bundle {
		val halfClock = UInt(OUTPUT)	// Allows seeing clock in waveform
		val halt = Bool(OUTPUT)
	}

	val top = Module(new Top(32))
	val systemMemory = Module(new AxiSram(32, 64 * 64))
	top.io.axiBus <> systemMemory.io
	io.halt := top.io.halt

	val DEBUG_halfClockReg = Reg(Bool())
	DEBUG_halfClockReg := !DEBUG_halfClockReg
	io.halfClock := DEBUG_halfClockReg
}

object main {
	def main(args: Array[String]): Unit = {
		val chiselMainArgs = args.slice(1, args.length)
		args(0) match {
			case "verilog" => chiselMain(chiselMainArgs, () => Module(new Top(32)))
			case "simulation" => chiselMain(chiselMainArgs, () => Module(new Testbench()))
			case "AxiSram" => chiselMainTest(chiselMainArgs, () => Module(new AxiSram(32, 1024))) {
				c => new AxiSramTest(c)}
			case "Arbiter" => chiselMainTest(chiselMainArgs, () => Module(new Arbiter(4))) {
				c => new ArbiterTest(c)}
		}
	}
}
