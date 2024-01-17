#!/usr/bin/sh
echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=opencl rangeQuery2d_grid.fut' "
futhark bench --backend=opencl rangeQuery2d_grid.fut
echo "--------------------------------------------------\n"
