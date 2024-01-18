type Point = {x : f64, y : f64}
type Rectangle = {ll : Point, ur : Point}
type Cell [n] = {rectangle : Rectangle, p_in : [n]i64}

-- Returns an array of flags
-- 1 indicates that the point is not above the rectangle, -1 otherwise
def up_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].y < rectangle.ur.y) then 1 else -1) (iota n)

-- Returns an array of flags
-- 1 indicates that the point is not under the rectangle, -1 otherwise
def down_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].y > rectangle.ll.y) then 1 else -1) (iota n)

-- Returns an array of flags
-- 1 indicates that the point is not ont the left of the rectangle, -1 otherwise
def left_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].x < rectangle.ll.x) then 1 else -1) (iota n)

-- Returns an array of flags
-- 1 indicates that the point is not ont the right of the rectangle, -1 otherwise
def right_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].x > rectangle.ur.x) then 1 else -1 ) (iota n)

-- Returns an array of flags
-- 1 indicates that the point is in the rectangle, -1 otherwise
def points_in_rectangle [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =

    let P_up = up_elimination rectangle points
    let P_down = down_elimination rectangle points
    let P_left = left_elimination rectangle points
    let P_right = right_elimination rectangle points

    let P_in = map4(
                \u d l r -> 
                if u == -1 && d == -1 && l == -1 && r == -1 then 1
                else -1
                ) P_up P_down P_left P_right

    in P_in

-- Returns ne number of point of points that are within rectangle
def nb_points_in_rectangle [n] (rectangle : Rectangle) (points : [n]Point) : i64 =

    let P_up = up_elimination rectangle points
    let P_down = down_elimination rectangle points
    let P_left = left_elimination rectangle points
    let P_right = right_elimination rectangle points

    let P_in = map4(
                \u d l r -> 
                if u == -1 && d == -1 && l == -1 && r == -1 then 1 
                else 0
                ) P_up P_down P_left P_right

    let nb_point = reduce (+) 0 P_in
    in nb_point 





-- Given a depth and index of a cell <c>, returns c.rectangle
def cell_helper_rectangle_depth (depth : i64) (ind : i64) : Rectangle =

    let d = f64.i64 depth 
    -- let i = f64.i64 ind 
    let u = 1/(2**d)

    let x_subdiv = ind % (2**depth)
    let y_subdiv = ind / (2**depth)

    let fx_subdiv = f64.i64 x_subdiv 
    let fy_subdiv = f64.i64 y_subdiv 
    
    let ll : Point = {x = fx_subdiv * u, y = (fy_subdiv + 1) * u}
    let ur : Point = {x = (fx_subdiv + 1)* u, y = fy_subdiv * u}
    let r : Rectangle = {ll = ll, ur = ur}
    in r


-- This function returns a grid of cells for a given depths
def preprocess_create_grid_depth [n] (depth : i64) (points : [n]Point) : []Cell [n] = 

    let rect_grid : []Rectangle = map(\i -> cell_helper_rectangle_depth depth i) (iota (4**depth) )
    let p_in_grid : [][n]i64 = map(\r -> points_in_rectangle r points) rect_grid
    let grid : []Cell [n] = map2(\r p -> {rectangle = r, p_in = p}) rect_grid p_in_grid
    in grid







-- Returns 1 if the rectangle cross the cell, -1 otherwise
def rect_cross_cell [n] (r : Rectangle) (c : Cell [n]) : i64 = 
    
    let c_r = c.rectangle

    -- -1 indicate that the rectangle do not cross the rectangle for sure 
    let cross_u = if r.ll.y < c_r.ur.y then -1 else 1
    let cross_d = if r.ur.y > c_r.ll.y then -1 else 1
    let cross_l = if r.ur.x < c_r.ll.x then -1 else 1
    let cross_r = if r.ll.x > c_r.ur.x then -1 else 1

    let cross = if cross_u == -1 || cross_d == -1 || cross_l == -1 || cross_r == -1 
            then -1
            else 1 
    in cross


-- This function returns an array with 1 when a cell is crossed by r, -1 otherwise
def rect_cross_cells [n][m] (r : Rectangle) (cs : [m]Cell [n]) : [m]i64 = 
    let selected_cells = map(\c -> rect_cross_cell r c ) cs
    in selected_cells

-- Lifted operator to make the operation of gattering all point in c1 union c2
-- This way, we can compute that in //
-- Returns a cell with a dummy rectangle and a p_in = c1.p_in \cup c2.p_in
def points_in_cells_reduce [n] (c1 : Cell [n]) (c2 : Cell [n]) : Cell [n] = 
    
    let r_ll = { x = 0, y = 0}
    let r_ur = { x = 0, y = 0}
    let r = {ll = r_ll, ur = r_ur}

    let p_in = map2(\c1_p c2_p -> if c1_p == 1 || c2_p == 1 then 1 else -1) c1.p_in c2.p_in
    
    let c = {rectangle = r, p_in = p_in}
    in c


-- This function returns an array of flag with a 1 at each points in \cup cs, -1 otherwise
def points_in_cells [n][m] (cs : [m]Cell [n]) : [n]i64 =
    
    let r_ll = { x = 0, y = 0}
    let r_ur = { x = 0, y = 0}
    let r = {ll = r_ll, ur = r_ur}
    let ps_init = map(\_ -> -1) (iota n)
    let c_neutral = {rectangle = r, p_in = ps_init}

    let custom_cell = reduce (points_in_cells_reduce) c_neutral cs
    let ps_in = custom_cell.p_in
    in ps_in
    

def get_subarray_cells_bis [n][m] (r : Rectangle) (grid : [m]Cell [n]) : []Cell [n] =


    let cells_flags_to_consider = rect_cross_cells r grid

    let cells_scatter_sz_helper = map(\f -> if f == 1 then 1 else  0) cells_flags_to_consider
    let cells_scatter_sz = reduce (+) 0  cells_scatter_sz_helper

    let scatter_idx_offset = scan (+) 0 cells_scatter_sz_helper
    let scatter_idx = map(\i -> i - 1 ) scatter_idx_offset
    let scatter_idx_masked = map2(\f i -> if f == 1 then i else -1 ) cells_flags_to_consider scatter_idx

    let dest = map(\_ -> grid[0]) (iota cells_scatter_sz)
    let cells_to_consider = scatter dest scatter_idx_masked grid

    in cells_to_consider


-- This function returns the subarray of points to consider to then brute force
def get_subarray_point_bis [n][m] (cells : [m]Cell [n]) (P : [n]Point) : []Point =

    let points_flags_to_consider = points_in_cells cells  

    let points_scatter_sz_helper = map(\f -> if f == 1 then 1 else  0) points_flags_to_consider
    let points_scatter_sz = reduce (+) 0  points_scatter_sz_helper

    let scatter_idx_offset = scan (+) 0 points_scatter_sz_helper
    let scatter_idx = map(\i -> i - 1 ) scatter_idx_offset
    let scatter_idx_masked = map2(\f i -> if f == 1 then i else -1 ) points_flags_to_consider scatter_idx

    let dest = map(\_ -> P[0]) (iota points_scatter_sz)
    let subarray_point = scatter dest scatter_idx_masked P

    in subarray_point



-- This function returns an array with the number of points in the rectangle at this index
def rangeQuery2d_grid_bis [m] [n] (depth : i64) (rectangles : [m]Rectangle) (points : [n]Point) : [m]i64 =

    let d = depth
    let cells = preprocess_create_grid_depth d points


    let solution =  map(\r -> 
                        let cells_to_consider : []Cell[n] = get_subarray_cells_bis r cells
                        let subarray_pts : []Point = get_subarray_point_bis cells_to_consider points 
                        in nb_points_in_rectangle r subarray_pts 
                    ) rectangles
    in solution











-- ### BENCHMARKING UNIT ### --
-- ==
-- entry: bench_rangeQuery2d_grid_bis
-- "2DinCube (small), d=3" input @InputData/fut/fut_2DinCube_1000000.in
-- "2Dkuzmin (small), d=3" input @InputData/fut/fut_2Dkuzmin_1000000.in
-- "2DinCube (large), d=3" input @InputData/fut/fut_2DinCube_10M.in
-- "2Dkuzmin (large), d=3" input @InputData/fut/fut_2Dkuzmin_10M.in


def mk_rect (ll : Point) (ur : Point) : Rectangle = {ll = ll, ur = ur}
 
def mk_rects [n] (points_in : [n]Point) : []Rectangle =
    let is = map(\i -> 2*i) (iota n)
    
    let rs : []Rectangle = map(\i -> mk_rect points_in[i] points_in[i+1]) is[:n/2]
    in rs 

entry bench_rangeQuery2d_grid_bis [n_in] (points_in : [n_in][2]f64) : []i64 =

    let d = 1

    let m = 2 * (n_in/3)
    let ps_in = map(\ps -> {x = ps[0], y = ps[1]})points_in

    let rs = mk_rects ps_in[:m]
    let ps = ps_in[m:]

    in rangeQuery2d_grid_bis d rs ps