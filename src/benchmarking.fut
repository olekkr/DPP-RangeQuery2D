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
