import "common"
import "benchmarking"

module brute = import "bruteforce"
module para = import "bruteforce_parallel"
module p_elim = import "rangeQuery2d_pt_elim"
module grid = import "rangeQuery2d_grid"


-- entry functions:
entry bench_brute [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in brute.rangeQuery2d rs ps

entry bench_simple_parallel [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in para.rangeQuery2d rs ps

entry bench_point_elimination [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in p_elim.rangeQuery2d rs ps

entry bench_point_grid [n_in] (points_in : [n_in][2]f64) : []i64 =
    let (rs, ps) = mk_inputs points_in
    in grid.rangeQuery2d_grid 3 rs ps -- choose gridlevel




-- ### BENCHMARKING UNIT ### --
-- ==
-- entry: bench_simple_parallel bench_point_elimination bench_point_grid
-- "2DinCube (small)" input @ ../InputData/fut_2DinCube_1000000.in
-- "2Dkuzmin (small)" input @ ../InputData/fut_2Dkuzmin_1000000.in

