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
// AMBA AXI and ACE Protocol Specification, revision E
// http://www.arm.com/products/system-ip/amba-specifications.php
//
class Axi4Master( dataWidth : Int ) extends Bundle {
    // Write address channel (Table A2-2)
    val awvalid = Bool(OUTPUT)
    val awready = Bool(INPUT)
    val awaddr = UInt(OUTPUT, 32)
    val awlen = UInt(OUTPUT, 8)
    val awsize = UInt(OUTPUT, 3)

    // Write data channel (Table A2-3)
    val wvalid = Bool(OUTPUT)
    val wready = Bool(INPUT)
    val wdata = UInt(OUTPUT, dataWidth)
    val wlast = Bool(OUTPUT)

    // Write response channel (Table A2-4)
    val bvalid = Bool(INPUT)
    val bready = Bool(OUTPUT)

    // Read address channel (Table A2-5)
    val arvalid = Bool(OUTPUT)
    val arready = Bool(INPUT)
    val araddr = UInt(OUTPUT, 32)
    val arlen = UInt(OUTPUT, 8)
    val arsize = UInt(OUTPUT, 3)

    // Read data channel (Table A2-6)
    val rvalid = Bool(INPUT)
    val rready = Bool(OUTPUT)
    val rdata = UInt(INPUT, dataWidth)
}
