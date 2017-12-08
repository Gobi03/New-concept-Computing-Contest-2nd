#!/bin/sh

rm -f *.class

fsc CommonPackage.scala
fsc Entities.scala

fsc ResultPackage.scala
fsc ToolsPackage.scala

fsc EvaluatorPackage.scala

fsc Main.scala
