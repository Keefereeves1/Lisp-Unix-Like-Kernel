#!/bin/bash

# Set the base directory
BASE_DIR="$(dirname "$0")/.."

# Directories
INCLUDE_DIR="$BASE_DIR/lisp_unix/include"
ARCH_DIR="$BASE_DIR/lisp_unix/arch/x86"
INIT_DIR="$BASE_DIR/lisp_unix/init"
DRIVERS_DIR="$BASE_DIR/lisp_unix/drivers"
MM_DIR="$BASE_DIR/lisp_unix/mm"
KERNEL_DIR="$BASE_DIR/lisp_unix/kernel"
FS_DIR="$BASE_DIR/lisp_unix/fs"
NET_DIR="$BASE_DIR/lisp_unix/net"

# Ensure that the required directories and files exist
required_dirs=("$INCLUDE_DIR" "$ARCH_DIR" "$INIT_DIR" "$DRIVERS_DIR" "$MM_DIR" "$KERNEL_DIR" "$FS_DIR" "$NET_DIR")
for dir in "${required_dirs[@]}"; do
  if [ ! -d "$dir" ]; then
    echo "Directory $dir does not exist."
    exit 1
  fi
done

# Compile the Lisp files and generate .fasl files
compile_lisp_file() {
  lisp_file=$1
  fasl_file="${lisp_file%.lisp}.fasl"
  sbcl --noinform --load "/home/keefe/quicklisp/setup.lisp" --eval "(progn
                (ql:quickload '(:split-sequence :cffi))
                (load \"$INCLUDE_DIR/packages.lisp\")
                (compile-file \"$lisp_file\")
                (quit))"
  if [ ! -f "$fasl_file" ]; then
    echo "Failed to generate $fasl_file"
    exit 1
  fi
}

lisp_files=(
  "$INCLUDE_DIR/packages.lisp"
  "$INCLUDE_DIR/kernel-package.lisp"
  "$INCLUDE_DIR/kernel.lisp"
  "$ARCH_DIR/boot.lisp"
  "$INIT_DIR/main.lisp"
  "$DRIVERS_DIR/vga.lisp"
  "$MM_DIR/memory.lisp"
  "$KERNEL_DIR/scheduler.lisp"
  "$KERNEL_DIR/interrupts.lisp"
  "$KERNEL_DIR/syscalls.lisp"
  "$FS_DIR/simplefs.lisp"
  "$NET_DIR/stack.lisp"
)

for lisp_file in "${lisp_files[@]}"; do
  compile_lisp_file "$lisp_file"
done

# Load the generated .fasl files
load_fasl_files() {
  sbcl --noinform --load "/home/keefe/quicklisp/setup.lisp" --eval "
    (progn
      (ql:quickload '(:split-sequence :cffi))
      (load \"$INCLUDE_DIR/packages.fasl\")
      (load \"$INCLUDE_DIR/kernel-package.fasl\")
      (load \"$INCLUDE_DIR/kernel.fasl\")
      (load \"$ARCH_DIR/boot.fasl\")
      (load \"$INIT_DIR/main.fasl\")
      (load \"$DRIVERS_DIR/vga.fasl\")
      (load \"$MM_DIR/memory.fasl\")
      (load \"$KERNEL_DIR/scheduler.fasl\")
      (load \"$KERNEL_DIR/interrupts.fasl\")
      (load \"$KERNEL_DIR/syscalls.fasl\")
      (load \"$FS_DIR/simplefs.fasl\")
      (load \"$NET_DIR/stack.fasl\")
      (quit))"
}

load_fasl_files

# Check if the kernel image was created successfully
if [ -f "kernel.img" ]; then
  echo "Kernel image created successfully."
else
  echo "Kernel image creation failed."
  exit 1
fi
