#!/bin/bash

set -e

tests=$(find . -depth 1 -name "*.test.js")

function print_step {
    echo -e "[\033[1;34m$1\033[0m]"
}

function build_rust {
    cd ..
    cargo build
    cd examples
}

function run_test_case {
    file_prefix=${1%.test.js}
    ../target/debug/codegen $1 > _build/$file_prefix.test.c
	gcc -I../runtime/include ../target/debug/libruntime.a _build/$file_prefix.test.c -o _build/$file_prefix.test
    _build/$file_prefix.test > $2
}

function run_all_tests {
    for test in $tests; do
        echo -e "\033[1;32m    Running test\033[0m $test"
        file_prefix=${test%.test.js}
        if [ -z "$SWC_TEST_BLESS" ]; then
            run_test_case "$test" "_build/actual/$file_prefix.test.actual"
            if ! diff "_build/actual/$file_prefix.test.actual" "expected/$file_prefix.test.expected"; then
                echo -e "\033[1;31mTest $test failed!\033[0m"
                exit 1
            fi
        else
            run_test_case "$test" "expected/$file_prefix.test.expected"
        fi
    done
}

function init {
    mkdir -p _build
    mkdir -p _build/actual
    if ! [ -z "$SWC_TEST_BLESS" ]; then
        rm -rf expected
    fi
    mkdir -p expected
}

function main {
    case $1 in
        "clean")
            print_step "Cleaning Test Files"
            rm -rf _build
            exit 0
            ;;
        "bless")
            print_step "Bless Enabled"
            export SWC_TEST_BLESS=1
            ;;
    esac

    init

    print_step "Building Compiler and Runtime"
    build_rust

    print_step "Running Tests"
    run_all_tests
}
main $@
