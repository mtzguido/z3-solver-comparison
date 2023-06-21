#!/bin/bash

F=$1

ramon -o "${F}.z3-4.8.5.ramon" \
	z3 "${F}" > "${F}.z3-4.8.5.out"

ramon -o "${F}.z3-master-solver6.ramon" \
	z3-master smt.arith.solver=6 "${F}" > "${F}.z3-master-solver6.out"

ramon -o "${F}.z3-master-solver2.ramon" \
	z3-master smt.arith.solver=2 "${F}" > "${F}.z3-master-solver2.out"
