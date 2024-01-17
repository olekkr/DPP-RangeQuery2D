import "common"

module brute = import "bruteforce"
module para = import "bruteforce_parallel"
module p_elim = import "rangeQuery2d_pt_elim"
module grid = import "rangeQuery2d_grid"


-- helper functions for data
def mk_rect (ll : Point) (ur : Point) : Rectangle = {ll = ll, ur = ur}
 
def mk_rects [n] (points_in : [n]Point) : []Rectangle =
    let is = map(\i -> 2*i) (iota n)
    
    let rs : []Rectangle = map(\i -> mk_rect points_in[i] points_in[i+1]) is[:n/2]
    in rs 

def mk_inputs [n_in] (points_in : [n_in][2]f64) : ([]Rectangle, []Point) = 
    let m = 2 * (n_in/3)
    let ps_in = map(\ps -> {x = ps[0], y = ps[1]})points_in

    let rs = mk_rects ps_in[:m]
    let ps = ps_in[m:]
    in (rs, ps)



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
 
-- ignore for now 
-- -- "2Dkuzmin (small)" input @ ../InputData/fut_2Dkuzmin_1000000.in

