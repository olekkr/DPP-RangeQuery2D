#!/usr/bin/sh
echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=c rangeQuery2d_rect_sweep.fut' "
futhark bench --backend=c rangeQuery2d_rect_sweep.fut
echo "--------------------------------------------------\n"

echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=opencl rangeQuery2d_rect_sweep.fut' "
futhark bench --backend=opencl rangeQuery2d_rect_sweep.fut
echo "--------------------------------------------------\n"

# rm rangeQuery2d_rect_sweep.c rangeQuery2d_rect_sweep
# rm -r data
