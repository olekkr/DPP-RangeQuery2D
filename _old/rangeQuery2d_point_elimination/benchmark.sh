#!/usr/bin/sh
echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=opencl rangeQuery2d_pt_elim.fut' "
futhark bench --backend=opencl rangeQuery2d_pt_elim.fut
echo "--------------------------------------------------\n"
