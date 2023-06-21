#!/bin/bash

find . -type f -name '*.smt2' -print0 | xargs --null -n 1 -P $(nproc) ./run1.sh
