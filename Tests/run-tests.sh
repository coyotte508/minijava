#!/bin/bash

EXE=./Main.native

function ensure_good_run(){
    if [[ $? != 0 ]]; then
        echo "Tests failed :("
        exit 1
    fi
}

function ensure_bad_run(){
    if [[ $? == 0 ]]; then
        echo "Tests failed :("
        exit 1
    fi
}

echo "Running tests..."

echo "============================"
echo "Running tests valid examples"
echo "============================"

readarray <<HERE
Class.java
Class2.java
Variable.java
Variable2.java
Variable3.java
Variable4.java
Comments.java
Expressions.java
Expressions2.java
Expressions3.java
Function.java
Function2.java
Function3.java
Mixed1.java
FunctionCall.java
Conditions.java
Mixed2.java
HERE

for a in "${MAPFILE[@]}"; do
	echo "testing $a"
    $EXE Tests/$a > /dev/null
    ensure_good_run
done

echo "================================="
echo "Running tests on invalid examples"
echo "================================="

readarray <<HERE
BadClass.java
HERE

for a in "${MAPFILE[@]}"; do
	echo "testing $a"
    $EXE Tests/$a &> /dev/null
    ensure_bad_run
done

echo "Tests all good!"
