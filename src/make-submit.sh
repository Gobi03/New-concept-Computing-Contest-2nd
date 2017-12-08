#!/bin/sh

file=Submit.scala

rm -f ${file}
touch ${file}

# generate file
cat CommonPackage.scala >> ${file}
cat Entities.scala >> ${file}

cat ResultPackage.scala >> ${file}
cat ToolsPackage.scala >> ${file}

cat EvaluatorPackage.scala >> ${file}

cat Main.scala >> ${file}
