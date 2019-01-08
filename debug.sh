#!/bin/bash

scalac -d bin/ src/*.scala
scala -classpath bin $1