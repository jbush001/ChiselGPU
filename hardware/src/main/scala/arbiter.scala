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

class Arbiter(numInputs : Int) extends Module {
	
	val io = new Bundle {
		val request = UInt(INPUT, width = numInputs)
		val grantOneHot = UInt(OUTPUT, width = numInputs)
		val enableUpdate = Bool(INPUT)
	}

	var priorityOneHot = Reg(UInt(width = numInputs), init=UInt(1))

	var grantIndex = 0
	var priorityIndex = 0
	var bitIndex = 0
	var grantOneHot = UInt(0)
	for (grantIndex <- 0 until numInputs) {
		var isGranted0 = Bool(false)
		for (priorityIndex <- 0 until numInputs) {
			var isGranted1 = io.request(grantIndex) & priorityOneHot(priorityIndex)
			for (bitIndex <- 0 until numInputs - 1) {
				isGranted1 = isGranted1 & !io.request((priorityIndex + bitIndex + 1) % numInputs)
			}
			
			isGranted0 = isGranted0 | isGranted1
		}
		
		if (grantIndex == 0)
			grantOneHot = isGranted0
		else
			grantOneHot = Cat(isGranted0, grantOneHot)
	}
	
	io.grantOneHot := grantOneHot
	
	when (io.enableUpdate && io.request != UInt(0)) {
		if (numInputs > 2)
			priorityOneHot := Cat(priorityOneHot(numInputs - 2, 1), priorityOneHot(numInputs - 1))
		else
			priorityOneHot := ~priorityOneHot
	}
}
