# DPP-RangeQuery2D


### To use the generate needed data:

```
cd tools
make <needed input file> 
```
For example:
```
make fut_2Dkuzmin_10M.in
```


### To run benchmark (including generating required data)
```
cd tools
make bench_all
```
To specify settings such as which backend to use just do 
```
futhark bench --backend=opencl rangeQuery2d_grid.fut
```

### To clean directory 
**Important** ensure that all the needed files are added to git staging.  
Then use:
`git clean -fx <optionally target directory>`