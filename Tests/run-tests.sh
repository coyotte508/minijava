#!/bin/bash

EXE=./Main.native


function ensure_good_run(){
    if [[ $? != 0 ]]; then
        echo "Tests failed :("
        exit 1
    fi
}

echo "Running tests..."

$EXE Tests/Basic.java
ensure_good_run

echo "Tests all good!"
