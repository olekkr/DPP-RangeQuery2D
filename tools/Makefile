#include common/parallelDefs
CC?=cc
CFLAGS?=-Wall -Wextra -pedantic -O3


COMMON = common/IO.h common/parse_command_line.h common/geometry.h common/geometryIO.h parlay/parallel.h parlay/primitives.h

# converter
pbbs2fut: pbbs2fut.c
	$(CC) -o $@ $(CFLAGS) $^



# randpoints
randPoints : randPoints.C geometryData.h $(COMMON)
	g++ -DHOMEGROWN -pthread -mcx16 -O2 -g -std=c++17 -DNDEBUG -I . -DHOMEGROWN -pthread -ldl -L/usr/lib -ljemalloc -o  $@ $@.C


# datasets
2Dkuzmin_10M : randPoints
	./randPoints -k -d 2 10000000 $@
	mv $@ ../InputData/

2Dkuzmin_% : randPoints
	./randPoints -k -d 2 $(subst 2Dkuzmin_,,$@) $@
	mv $@ ../InputData/

2DinCube_10M : randPoints
	./randPoints -d 2 10000000 $@
	mv $@ ../InputData/

2DinCube_% : randPoints
	./randPoints -d 2  $(subst 2DinCube_,,$@) $@
	mv $@ ../InputData/


# fut datasets
fut_2Dkuzmin_10M.in : 2Dkuzmin_10M pbbs2fut
	cat ../InputData/2Dkuzmin_10M | ./pbbs2fut > ../InputData/$@

fut_2Dkuzmin_%.in : 2Dkuzmin_% pbbs2fut
	cat ../InputData/$< | ./pbbs2fut > ../InputData/$@

fut_2DinCube_10M.in : 2DinCube_10M pbbs2fut
	cat ../InputData/2DinCube_10M | ./pbbs2fut > ../InputData/$@

fut_2DinCube_%.in : 2DinCube_% pbbs2fut
	cat ../InputData/$< | ./pbbs2fut > ../InputData/$@


bench_all_c : fut_2DinCube_1000000.in fut_2Dkuzmin_1000000.in
	cd ../src; futhark bench -r 1 --backend=c benchmark_all.fut

bench_all_multicore : fut_2DinCube_1000000.in fut_2Dkuzmin_1000000.in
	cd ../src; futhark bench -r 1 --backend=multicore benchmark_all.fut
	
bench_all_opencl : fut_2DinCube_1000000.in fut_2Dkuzmin_1000000.in
	cd ../src; futhark bench -r 1 --backend=opencl benchmark_all.fut