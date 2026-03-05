#!/bin/bash
set -e

cobc -x -free -I./src src/InCollege.cob -o InCollege

# Run the program
./InCollege
