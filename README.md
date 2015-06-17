
# Getting Started

https://chisel.eecs.berkeley.edu/

## Required Software

- GCC-4.8+/Apple Clang 4.2+
- J2SE 7.0+
- SBT 0.13.0+ (http://www.scala-sbt.org/)

SBT (Scala build tool) will automatically download and install remaining 
dependencies

## Building on Linux

sudo apt-get install gcc g++ openjdk-7-jre sbt
cd hardware
sbt run


## Building on MacOS

Download and install Java from here:

https://java.com/en/download/help/mac_install.xml

Then:

xcode-select --install
sudo port install sbt
cd hardware
sbt run


