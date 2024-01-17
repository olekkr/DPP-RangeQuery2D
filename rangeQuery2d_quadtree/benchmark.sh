#!/usr/bin/sh
echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=opencl rangeQuery2d_quadtree.fut' "
futhark bench --backend=opencl rangeQuery2d_quadtree.fut
echo "--------------------------------------------------\n"
