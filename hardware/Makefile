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

# Generate C++ simulator
simulator: FORCE
	sbt "run simulation --Wall --backend c --debug --assert --vcd --targetDir generated_cpp"
	g++ -Os -std=c++11 -o simulator src/main/cpp/simmain.cpp generated_cpp/Testbench.cpp -Igenerated_cpp

run: simulator
	./simulator -d memory.bin,0,4000 -c 20000 -w
	convert -depth 8 -size 64x64 rgba:memory.bin output.png

# Generate verilog sources
verilog:
	sbt "run verilog --Wall --backend v --assert --targetDir generated_verilog --noInlineMem"
	./makesrams.py generated_verilog/Top.conf generated_verilog/altera_memories.v

tests: arbitertest axisramtest memoryarbitertest tilebuffertest

# Unit tests
axisramtest:
	sbt "run AxiSram --Wall --backend c --debug --assert --vcd --targetDir generated_cpp --genHarness --compile --test"

memoryarbitertest:
	sbt "run MemoryArbiter --Wall --backend c --debug --assert --vcd --targetDir generated_cpp --genHarness --compile --test"

arbitertest:
	sbt "run Arbiter --Wall --backend c --debug --assert --vcd --targetDir generated_cpp --genHarness --compile --test"

tilebuffertest:
	sbt "run TileBuffer --Wall --backend c --debug --assert --vcd --targetDir generated_cpp --genHarness --compile --test"

clean:
	rm -rf generated_cpp
	rm -rf generated_verilog
	sbt clean
	rm simulator

FORCE: