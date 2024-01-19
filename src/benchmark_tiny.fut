import "benchmarking"

-- entry functions:
entry bench_brute [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in brute.rangeQuery2d rs ps

entry bench_simple_parallel [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in para.rangeQuery2d rs ps

entry bench_point_grid [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in grid.rangeQuery2d_grid 3 rs ps -- choose gridlevel

entry bench_point_grid_bis [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in grid_bis.rangeQuery2d_grid_bis 3 rs ps -- choose gridlevel


-- ### BENCHMARKING UNIT ### --
-- ==
-- entry: bench_simple_parallel  bench_point_grid bench_point_grid_bi
-- "2DinCube (small)" input @ ../InputData/fut_2DinCube_1000.in

