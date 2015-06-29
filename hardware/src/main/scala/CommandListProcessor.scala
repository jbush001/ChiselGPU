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
// The command list processor reads commands put in system memory by the host
// processor. It broadcasts updates of internal control registers for other
// functional units.
//

// Broadcasts register updates to other internal function units
class RegisterUpdate extends Bundle {
	val update = Bool(OUTPUT)
	val address = UInt(OUTPUT, 8)
	val value = UInt(OUTPUT, 32)
}

object regids {
	val reg_start_resolve :: reg_resolve_base_addr :: reg_resolve_stride :: reg_enable_alpha :: Nil = Enum(UInt(), 4)
}

class CommandListProcessor(burstByteCount : Int) extends Module {
	val io = new Bundle {
		val registerUpdate = new RegisterUpdate
		val arbiterPort = new ArbiterReadPort(burstByteCount).flip
		val HACK_resolve = Bool(INPUT)
	}

	val updateReg = Reg(Bool(), init = Bool(false))
	
	io.arbiterPort.request := Bool(false)
	io.arbiterPort.address := UInt(0)
	
	// XXX placeholder, not currently connected.
	io.registerUpdate.update := io.HACK_resolve
	io.registerUpdate.address := regids.reg_start_resolve
	io.registerUpdate.value := UInt(0)
}
