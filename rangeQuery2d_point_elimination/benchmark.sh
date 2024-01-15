#!/usr/bin/sh
echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=c rangeQuery2d_point_elimination.fut' "
futhark bench --backend=c rangeQuery2d_point_elimination.fut
echo "--------------------------------------------------\n"

echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=opencl rangeQuery2d_point_elimination.fut' "
futhark bench --backend=opencl rangeQuery2d_point_elimination.fut
echo "--------------------------------------------------\n"

echo "\n--------------------------------------------------"
echo "Command line for benchmarking : 'futhark bench --backend=opencl Bench_Par_rangeQuery2d_point_elimination.fut' "
futhark bench --backend=opencl Bench_Par_rangeQuery2d_point_elimination.fut
echo "--------------------------------------------------\n"

rm rangeQuery2d_point_elimination.c rangeQuery2d_point_elimination
rm Bench_Par_rangeQuery2d_point_elimination.c Bench_Par_rangeQuery2d_point_elimination

# rm -r data
