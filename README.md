# Setup

This implementation uses the Chisel hardware construction language:

https://chisel.eecs.berkeley.edu/

## Required Software

- GCC-4.8+/Apple Clang 4.2+
- J2SE 7.0+
- SBT 0.13.0+ (http://www.scala-sbt.org/)
- ImageMagick

SBT (Scala build tool) will automatically download and install the remaining 
dependencies, including Scala and the Chisel library.

## Set up on Linux

sudo apt-get install gcc g++ openjdk-7-jre sbt imagemagick

## Set up on MacOS

Download and install Java from here:

https://java.com/en/download/help/mac_install.xml

If the system compiler is not already installed:

xcode-select --install

Finally:

sudo port install sbt imagemagick

# Running

To run the whole design in simualtion:

    cd hardware
    make run

Output framebuffer is written to 'output.png'
The simulator writes a waveform trace to 'trace.vcd'

To run unit tests:

    make tests
    
To synthesize:
 
    make verilog
    
The result will be in hardware/generated_verilog/

