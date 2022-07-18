#!/bin/bash

# Helper script for building problem136.v

mkdir tmp
(
    cp problem136.v tmp/
    cd tmp/
    cat <<EOF >sim_main.cpp
#include "Vproblem136.h"
#include "verilated.h"
int main(int argc, char** argv, char** env) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);
  Vproblem136* top = new Vproblem136{contextp};
  while (!contextp->gotFinish()) { top->eval(); }
  delete top;
  delete contextp;
  return 0;
}
EOF
    verilator -Wall --cc --exe --build sim_main.cpp problem136.v && ./obj_dir/Vproblem136
)
rm -rf tmp/
