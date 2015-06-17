
# Generate C++ simulator
simulator: FORCE
	sbt "run --Wall --backend c --debug --assert --vcd --targetDir generated_cpp"
	g++ -std=c++11 -o simulator src/main/cpp/simmain.cpp generated_cpp/Testbench.cpp -Igenerated_cpp

# Generate verilog sources
verilog:
	sbt "run --Wall --backend v --assert --targetDir generated_verilog"

clean:
	rm -rf generated_cpp
	rm -rf generated_verilog
	sbt clean
	rm simulator

FORCE: