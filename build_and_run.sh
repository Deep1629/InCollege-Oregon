#!/bin/bash
set -e

# Compile COBOL program (auto-detect free/fixed format)
cobc -x src/InCollege.cob -o InCollege || cobc -x -free src/InCollege.cob -o InCollege

# Run the program
./InCollege
