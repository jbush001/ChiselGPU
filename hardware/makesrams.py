#!/usr/bin/python
# 
# Copyright 2015 Jeff Bush
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 

import sys
import math

#
# Create SRAM blocks based on .conf file emitted by Chisel
# Each line is:
# name TileBuffer_colorMemory  depth 4096 width 32 ports write,read

def emitMemory(file, name, dataWidth, depth):
	addrWidth = int(math.ceil(math.log(depth, 2)))
	
	file.write('module ' +  name + '''(
  	input CLK,
	input[''' + str(addrWidth - 1) + ''':0] W0A,
	input W0E,
	input[''' + str(dataWidth - 1) + ''':0] W0I,
	input[''' + str(addrWidth - 1) + ''':0] R1A,
	input R1E,
	output[''' + str(dataWidth - 1) + ''':0] R1O);
	
	ALTSYNCRAM #(
		.OPERATION_MODE("DUAL_PORT"),
		.READ_DURING_WRITE_MIXED_PORTS("DONT_CARE"),
		.WIDTH_A(''' + str(dataWidth) + '''),
		.WIDTHAD_A(''' + str(addrWidth) + '''),
		.WIDTH_B(''' + str(dataWidth) + '''),
		.WIDTHAD_B(''' + str(addrWidth) + ''')
	) data0(
		.clock0(CLK),
		.clock1(CLK),
		// Write port
		.rden_a(1'b0),
		.wren_a(W0E),	
		.address_a(W0A),
		.data_a(W0I),
		.q_a(),
		// Read port
		.rden_b(R1E),
		.wren_b(1'b0),
		.address_b(R1A),
		.q_b(R1O));
		
endmodule

''')

with open(sys.argv[1], 'r') as inputFile, open(sys.argv[2], 'w') as outputFile:
	for line in inputFile:
		fields = [ x.strip() for x in line.split(' ') if x != '' ]
		if len(fields) > 0 and fields[0] == 'name':
			emitMemory(outputFile, fields[1], int(fields[5]), int(fields[3])) 
