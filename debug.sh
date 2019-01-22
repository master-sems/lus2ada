#!/bin/bash

scalac -d bin/ src/*.scala
scala -classpath bin $1

if [ "$1" == "ImperativePrinter" ]; then
  gnatmake stable_gen.adb
  ./stable_gen

  gnatmake leds_gen.adb
  ./leds_gen
fi