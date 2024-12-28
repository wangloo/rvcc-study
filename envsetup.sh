#!/bin/sh
# set -ev
export RISCV=$HOME/riscv

mm () {
  [ $# -ne 1 ] && echo "test file name is needed"

  make
  $RISCV/bin/riscv64-unknown-linux-gnu-gcc -g -o build/$1.c -E -P -C test/$1.c
  ./rvcc -o build/$1.s build/$1.c
  $RISCV/bin/riscv64-unknown-linux-gnu-gcc -g -static -o build/$1.exe build/$1.s -xc test/common
  echo "Generate debugged test execuable: build/$1.exe"
  $RISCV/bin/qemu-riscv64 -g 1234 -L $RISCV/sysroot build/$1.exe
}

mmgcc() {
  [ $# -ne 1 ] && echo "test file name is needed"

  make
  $RISCV/bin/riscv64-unknown-linux-gnu-gcc -g -S -o build/$1.s test/$1.c
  $RISCV/bin/riscv64-unknown-linux-gnu-gcc -g -static -o build/$1.exe build/$1.s -xc test/common
  echo "Generate debugged test execuable: build/$1.exe"
  $RISCV/bin/qemu-riscv64 -g 1234 -L $RISCV/sysroot build/$1.exe
}

gdb () {
  [ $# -ne 1 ] && echo "test file name is needed"

  gdb-multiarch -nx -x .gdbinit --symbols=build/$1.exe
}