#!/bin/sh

rm -f *.class

fsc CommonPackage.scala
fsc Entities.scala

fsc ToolsPackage.scala

fsc Main.scala
