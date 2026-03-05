#!/bin/bash
set -e

cobc -x -free -I./src src/InCollege.cob -o InCollege

touch users.dat
touch profiles.dat
touch profiles.tmp
touch connections.dat

# Run the program
./InCollege
