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
// Round robin arbiter. Given a bitmap of requestors, set one bit
// in the a grant signal to indicate which should receive access.  
// There is already RRArbiter in the standard library, but I couldn't 
// figure out how to get it to do quite what I wanted. Should investigate 
// switching to that.
//
class Arbiter(numInputs : Int) extends Module {
	
	val io = new Bundle {
		val request = UInt(INPUT, width = numInputs)
		val grantOneHot = UInt(OUTPUT, width = numInputs)
		val enableUpdate = Bool(INPUT)
	}

	var priorityOneHotReg = Reg(UInt(width = numInputs), init=UInt(1))

	var grantOneHot = UInt(0)
	for (grantIndex <- 0 until numInputs) {
		var isGranted0 = Bool(false)
		for (priorityIndex <- 0 until numInputs) {
			var isGranted1 = io.request(grantIndex) & priorityOneHotReg(priorityIndex)
			var bitIndex = priorityIndex
			while (bitIndex != grantIndex) {
				isGranted1 = isGranted1 & !io.request(bitIndex)
				bitIndex = (bitIndex + 1) % numInputs
			}

			isGranted0 = isGranted0 | isGranted1
		}
		
		if (grantIndex == 0)
			grantOneHot = isGranted0
		else
			grantOneHot = Cat(isGranted0, grantOneHot)
	}
	
	io.grantOneHot := grantOneHot
	
	// Rotate left
	when (io.enableUpdate && io.request != UInt(0)) {
		if (numInputs > 2)
			priorityOneHotReg := Cat(io.grantOneHot(numInputs - 2, 0), io.grantOneHot(numInputs - 1))
		else
			priorityOneHotReg := ~io.grantOneHot	// One bit priority
	}
}

// Top level testbench configures with four inputs
class ArbiterTest(c : Arbiter) extends Tester(c) {
	poke(c.io.enableUpdate, 1)

	// All inputs requesting
	poke(c.io.request, 15)
	step(1)
	expect(c.io.grantOneHot, 2)
	step(1)
	expect(c.io.grantOneHot, 4)
	step(1)
	expect(c.io.grantOneHot, 8)
	step(1)
	expect(c.io.grantOneHot, 1)

	// Only input 3
	poke(c.io.request, 8)
	step(1)
	expect(c.io.grantOneHot, 8)
	step(1)
	expect(c.io.grantOneHot, 8)

	// Inputs 0 and 2
	poke(c.io.request, 5)
	step(1)
	expect(c.io.grantOneHot, 4)
	step(1)
	expect(c.io.grantOneHot, 1)
	step(1)
	expect(c.io.grantOneHot, 4)
	step(1)
	expect(c.io.grantOneHot, 1)

	// Nobody requesting
	poke(c.io.request, 0)
	step(1)
	expect(c.io.grantOneHot, 0)
	step(1)
	expect(c.io.grantOneHot, 0)
}

