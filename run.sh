#!/bin/sh

# check if at least one argument is passed
if [ $# -eq 0 ]; then
    echo "[run.sh] - no arguments provided"
    exit 1
fi

FILE=$1
FILE=${FILE%.*}

shift 1

set -e

cargo run -- $FILE.fox "$@"
