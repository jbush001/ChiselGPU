
# Getting Started

https://chisel.eecs.berkeley.edu/

## Required Software

- GCC-4.8+/Apple Clang 4.2+
- J2SE 7.0+
- SBT 0.13.0+ (http://www.scala-sbt.org/)
- ImageMagick

SBT (Scala build tool) will automatically download and install remaining 
dependencies

## Set up on Linux

sudo apt-get install gcc g++ openjdk-7-jre sbt imagemagick

## Set up on MacOS

Download and install Java from here:

https://java.com/en/download/help/mac_install.xml

Then:

xcode-select --install
sudo port install sbt imagemagick

# Running

cd hardware
make run

Output framebuffer is written to 'output.png'
