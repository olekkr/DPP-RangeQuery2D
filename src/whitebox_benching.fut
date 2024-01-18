import "benchmarking"
import "rangeQuery2d_grid"



entry preprocess_bench [n_in] (points_in : [n_in][2]f64) : []Cell [] =
    let (rs, ps) = mk_inputs points_in
    let cells =  preprocess_create_grid_depth 3 ps -- choose gridleve
    in cells


-- ### BENCHMARKING UNIT ### --
-- ==
-- entry: preprocess_bench
-- "2DinCube (small) 1k" input @ ../InputData/fut_2DinCube_1000.in
-- "2DinCube (small) 10k" input @ ../InputData/fut_2DinCube_10000.in
-- "2DinCube (small) 1M" input @ ../InputData/fut_2DinCube_1000000.in

