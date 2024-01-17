-- Note that the upper left corner of the space is the origin.
-- x is going to the right
-- y is going down

-- ### TYPE DEFINITIONS ### --
type Point = {x : f64, y : f64}
type Rectangle = {ll : Point, ur : Point}
type Cell [n] = {rectangle : Rectangle, p_in : [n]i64}

-- ### FUNCTIONS DEFINITIONS ### --

-- Name :   up_elimination
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points to eliminate, and -1 otherwise
--
-- This function returns an array of indices of points to eliminate.
-- Those points correspond to the point above the Rectangle (ie p.y < Rect.ur.y)
def up_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_up = map(
                    \i -> if (points[i].y < rectangle.ur.y) then i else -1 
                ) (iota n)

    in P_up


-- Name :   down_elimination
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points to eliminate, and -1 otherwise
--
-- This function returns an array of indices of points to eliminate.
-- Those points correspond to the point under the Rectangle (ie p.y > Rect.ll.y)
def down_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_down = map(
                    \i -> if (points[i].y > rectangle.ll.y) then i else -1 
                ) (iota n)

    in P_down


-- Name :   left_elimination
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points to eliminate, and -1 otherwise
--
-- This function returns an array of indices of points to eliminate.
-- Those points correspond to the point on the left of the Rectangle (ie p.x < Rect.ll.x)
def left_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_left = map(
                    \i -> if (points[i].x < rectangle.ll.x) then i else -1
                ) (iota n)

    in P_left


-- Name :   right_sweep
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points to eliminate, and -1 otherwise
--
-- This function returns an array of indices of points to eliminate.
-- Those points correspond to the point on the right of the Rectangle (ie p.x > Rect.ur.x)
def right_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_right = map(
                    \i -> if (points[i].x > rectangle.ur.x) then i else -1 
                ) (iota n)

    in P_right


-- Name :   points_in_rectangle
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points in the rectangle
--
-- This function returns an array of indices of the points in the rectangle
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

-- Name :   nb_points_in_rectangle
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Number of points in the rectangle
--
-- This function returns the number of points in the rectangle
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
























-- def cell_helper_rectangle_depth (depth : i64) (ind : i64) : Rectangle =
-- def subgrid_union [n] (grid : [4]Cell [n]) : Cell [n] =
-- def get_subgrid [n] (ind : i64) (depth : i64) (grid : []Cell [n]): [4]Cell [n] =
-- def preprocess_create_grid_depth [n] (depth : i64) (points : [n]Point) : []Cell [n] = 
-- def preprocess_create_grid_tree [n] (depth : i64) (points : [n]Point) : []Cell [n] =
-- def helper_rect_cross_cell_u [n] (r : Rectangle) (c : Cell [n]) : i64 = 
-- def helper_rect_cross_cell_d [n] (r : Rectangle) (c : Cell [n]) : i64 = 
-- def helper_rect_cross_cell_l [n] (r : Rectangle) (c : Cell [n]) : i64 = 
-- def helper_rect_cross_cell_r [n] (r : Rectangle) (c : Cell [n]) : i64 = 
-- def rect_cross_cell [n] (r : Rectangle) (c : Cell [n]) : i64 = 
-- def cell_in_rect [n] (r : Rectangle) (c : Cell [n]) : i64 = 
-- def cells_in_rect [n][m] (r : Rectangle) (cs : [m]Cell [n]) : [m]i64 =
-- def points_in_cells_reduce [n] (c1 : Cell [n]) (c2 : Cell [n]) : Cell [n] = 
-- def points_in_cells [n][m] (cs : [m]Cell [n]) : [n]i64 =
-- def points_flags_2_points_array [n] (points_flags : [n]i64) (P : [n]Point) : [n](Point) =
-- def get_subarray_point [n][m][z] (r : Rectangle) (grid : [m]Cell [n]): [z](Point) =
-- def rangeQuery2d_grid [m] [n] (rectangles : [m]Rectangle) (points : [n]Point) : [m]i64 =



-- ##########################################################################################################
-- Function that returns a rectangle according to its index and depth
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


-- Function that returns get 4 subcell and combine it to a single bigger one
def subgrid_union [n] (grid : [4]Cell [n]) : Cell [n] =

    let r_tr = grid[1].rectangle
    let r_bl = grid[2].rectangle

    let r = {ll = r_bl.ll , ur = r_tr.ur}


    let p_in_tl = grid[0].p_in 
    let p_in_tr = grid[1].p_in 
    let p_in_bl = grid[2].p_in 
    let p_in_br = grid[3].p_in

    let p_in = map4(\tl tr bl br -> 
                    if tl == 1 || tr == 1 || bl == 1 || br == 1 then 1 
                    else -1
                    ) p_in_tl p_in_tr p_in_bl p_in_br


    let cell : Cell [n] = {rectangle = r, p_in = p_in} 
    in cell



-- The formula is id, d -> (id /2^d).2^{d+1} + 2( id - (id % 2^d) )
-- This function returns the upper left subcell index of any cell given its depth and index
def get_subgrid [n] (ind : i64) (depth : i64) (grid : []Cell [n]): [4]Cell [n] =

    let tl = (ind /2**depth) * 2**(depth+1) + (2 * ( ind - (ind % 2**depth)))
    let tr = tl + 1
    let bl = tl + 2**depth
    let br = bl + 1

    in [grid[tl], grid[tr], grid[bl], grid[br]]


-- This function returns a grid of cells
def preprocess_create_grid_depth [n] (depth : i64) (points : [n]Point) : []Cell [n] = 

    let rect_grid : []Rectangle = map(\i -> cell_helper_rectangle_depth depth i) (iota (4**depth) )
    let p_in_grid : [][n]i64 = map(\r -> points_in_rectangle r points) rect_grid
    let grid : []Cell [n] = map2(\r p -> {rectangle = r, p_in = p}) rect_grid p_in_grid
    in grid


-- This function returns a flatten representation of somesort of a quadtree
def preprocess_create_grid_tree [n] (depth : i64) (points : [n]Point) : []Cell [n] = 

    let deep_grid = preprocess_create_grid_depth depth points

    let (cells, _) = loop (acc, d) = (deep_grid, depth-1) for i < depth-1 do
                
                -- As we append it to the beginning, and by construction we won't 
                -- get_subgrid.tl etc... are all in the [0 ... 4**(d+1)]
                -- 4**d : nb_point at depth d
                let subgrids = map(\i -> get_subgrid i d acc)(iota(4**d))
                let grid = map(\sg -> subgrid_union sg) subgrids
                let debug = i 
                in (grid ++ acc, d - 1)
    in cells








-- TODO : Double check these one. Might cause issue
-- This function return 1 if the r cross the c by the upper edge
def helper_rect_cross_cell_u [n] (r : Rectangle) (c : Cell [n]) : i64 = 
    
    let c_r = c.rectangle

    let s = if c_r.ur.y <= r.ll.y && c_r.ur.y >= r.ur.y then 1 else -1 
    in s

-- This function return 1 if the r cross the c by the lower edge
def helper_rect_cross_cell_d [n] (r : Rectangle) (c : Cell [n]) : i64 = 
    
    let c_r = c.rectangle

    let s = if c_r.ll.y <= r.ll.y && c_r.ll.y >= r.ur.y then 1 else -1 
    in s

-- This function return 1 if the r cross the c by the left edge
def helper_rect_cross_cell_l [n] (r : Rectangle) (c : Cell [n]) : i64 = 
    
    let c_r = c.rectangle

    let s = if c_r.ll.x >= r.ll.x && c_r.ll.x <= r.ur.x then 1 else -1 
    in s

-- This function return 1 if the r cross the c by the right edge
def helper_rect_cross_cell_r [n] (r : Rectangle) (c : Cell [n]) : i64 = 
    
    let c_r = c.rectangle

    let s = if c_r.ur.x >= r.ll.x && c_r.ur.x <= r.ur.x then 1 else -1 
    in s




-- This function return 1 if the r cross the c
def rect_cross_cell [n] (r : Rectangle) (c : Cell [n]) : i64 = 
    
    let c_u = helper_rect_cross_cell_u r c
    let c_d = helper_rect_cross_cell_d r c
    let c_r = helper_rect_cross_cell_r r c
    let c_l = helper_rect_cross_cell_l r c

    let cross = if c_u == 1 || c_d == 1 || c_r == 1 || c_l == 1 then 1 else -1 
    in cross

-- This function returns an array with 1 when a cell is crossed by r.
def rect_cross_cells [n][m] (r : Rectangle) (cs : [m]Cell [n]) : [m]i64 = 
    let selected_cells = map(\c -> rect_cross_cell r c ) cs
    in selected_cells

-- This function return 1 if the c is in r
def cell_in_rect [n] (r : Rectangle) (c : Cell [n]) : i64 = 
    
    let c_r = c.rectangle

    let ll_in = if c_r.ll.x >= r.ll.x && c_r.ll.y <= r.ll.y then 1 else -1
    let ur_in = if c_r.ur.x <= r.ur.x && c_r.ur.y >= r.ur.y then 1 else -1

    let s = if ll_in == 1 && ur_in == 1 then 1 else -1
    in s



-- This function return 1 if the r cross the c by the lower edge
def cells_in_rect [n][m] (r : Rectangle) (cs : [m]Cell [n]) : [m]i64 =

    let selected_cells = map(\c -> cell_in_rect r c ) cs
    in selected_cells

-- This function turns an flag_array of cells into an array of cells
def cells_flags_2_cells_array [n][m] (cs_flag : [m]i64) (grid : [m]Cell [n]) : []Cell [n] =

    let cs_f = map(\f -> if f == 1 then 1 else 0) cs_flag
    let scatter_helper_cs_f = scan (+) 0 cs_f
    let sz = reduce (+) 0 cs_f

    let dest = map(\_ -> grid[scatter_helper_cs_f[0]] ) (iota sz)
    in scatter dest scatter_helper_cs_f grid

-- Lifted operator to make the operation of gattering all point in c1 union c2
-- This way, we can compute that in //
def points_in_cells_reduce [n] (c1 : Cell [n]) (c2 : Cell [n]) : Cell [n] = 
    
    let r_ll = { x = 0, y = 0}
    let r_ur = { x = 0, y = 0}
    let r = {ll = r_ll, ur = r_ur}

    let p_in = map2(\c1_p c2_p -> if c1_p == 1 || c2_p == 1 then 1 else -1) c1.p_in c2.p_in
    
    let c = {rectangle = r, p_in = p_in}
    in c

-- This function returns an array of flag with a 1 at each points in union cs 
def points_in_cells [n][m] (cs : [m]Cell [n]) : [n]i64 =
    
    let r_ll = { x = 0, y = 0}
    let r_ur = { x = 0, y = 0}
    let r = {ll = r_ll, ur = r_ur}
    let ps_init = map(\_ -> -1) (iota n)
    let c_neutral = {rectangle = r, p_in = ps_init}

    let custom_cell = reduce (points_in_cells_reduce) c_neutral cs
    let ps_in = custom_cell.p_in
    in ps_in

-- This function returns an array Point constructed using there indices
def points_flags_2_points_array [n] (points_flags : [n]i64) (P : [n]Point) : []Point =
    
    let ps_f = map(\f -> if f == 1 then 1 else 0) points_flags
    let scatter_helper_ps_f = scan (+) 0 ps_f
    -- let scatter_helper_ps_f = map(\i -> i-1) scatter_helper_ps_f
    let sz = reduce (+) 0 ps_f
    
    let dest = map(\_ -> P[scatter_helper_ps_f[0]]) (iota sz)
    in scatter dest scatter_helper_ps_f P


-- This function returns the subarray to consider to brute force
def get_subarray_point [n][m] (r : Rectangle) (grid : [m]Cell [n]) (P : [n]Point) : []Point =

    let cells_flags_to_consider = rect_cross_cells r grid
    let cells_to_consider = cells_flags_2_cells_array cells_flags_to_consider grid

    let points_flags_to_consider = points_in_cells cells_to_consider    
    let subarray_point : []Point = points_flags_2_points_array points_flags_to_consider P
    in subarray_point


def rangeQuery2d_grid [m] [n] (depth : i64) (rectangles : [m]Rectangle) (points : [n]Point) : [m]i64 =

    let d = depth
    let cells = preprocess_create_grid_depth d points -- correct till there for sure


    let solution =  map(\r -> 
                        let subarray_pts : []Point = get_subarray_point r cells points
                        in nb_points_in_rectangle r subarray_pts 
                    ) rectangles
    in solution


-- def rangeQuery2d_quadtree [m] [n] (rectangles : [m]Rectangle) (points : [n]Point) : [m]i64 =

--     let d = 3i64

--     let cells_shp = map(\i -> 4**(i+1)) (iota d)
--     let cells = preprocess_create_grid_tree d points

--     -- loop on layers,
--     -- if cell is in rect, rect.p_in ++ cell.p_in
--     -- if cell intersect rect, rect.p_in ++ loop on sublayer -- recursion ...
--     -- else rect.p_in ++ []
    
--     in (iota m)


-- ### TEST UNIT : rangeQuery2d ### --

-- Types definitions
-- type Point = (f64, f64)                     -- (x, y)
-- type Rectangle = (Point, Point)             -- (ll, ur)

def p0 : Point = {x = 0.0,y = 0.0}
def p1 : Point = {x = 0.25,y = 0.5}
def p2 : Point = {x = 0.5, y = 0.25}
def p3 : Point = {x = 0.75, y = 0.5}
def p4 : Point = {x = 0.5, y = 0.75}
def p5 : Point = {x = 0.4, y = 0.5}
def p6 : Point = {x = 0.5, y = 0.4}
def p7 : Point = {x = 0.6, y = 0.5}
def p8 : Point = {x = 0.5, y = 0.6}
def p9 : Point = {x = 0.5, y = 0.5}

def p10 : Point = {x = 0.25, y = 0.25}
def p11 : Point = {x = 0.75, y = 0.25}
def p12 : Point = {x = 0.75, y = 0.75}
def p13 : Point = {x = 0.25, y = 0.75}
def p14 : Point = {x = 0.33, y = 0.33}

def ll : Point = {x = 0.4, y = 0.6}
def ur : Point = {x = 0.6, y = 0.4}
def r : Rectangle = {ll = ll, ur = ur}


def R : []Rectangle = [r]
def P : []Point = [p0, p1, p2, p3, p6, p7, p10, p11, p8, p9, p4, p5, p12, p13]

def P1 = P[:1]
def P2 = P[:2]
def P3 = P[:3]
def P4 = P[:4]
def P5 = P[:5]
def P6 = P[:6]
def P7 = P[:7]
def P8 = P[:8]
def P9 = P[:9]

-- rangeQuery2d_grid 3 R P
-- def expected_result : []i64 = [5]


def debug_1 = preprocess_create_grid_depth 1 P
def debug_2 = preprocess_create_grid_depth 2 P
def debug_3 = preprocess_create_grid_depth 3 P



-- ### BENCHMARKING UNIT ### --
-- ==
-- entry: bench_rangeQuery2d
-- "2DinCube (small)" input @ InputData/fut/fut_2DinCube_1000000.in
-- "2Dkuzmin (small)" input @ InputData/fut/fut_2Dkuzmin_1000000.in


def mk_rect (ll : Point) (ur : Point) : Rectangle = {ll = ll, ur = ur}
 
def mk_rects [n] (points_in : [n]Point) : []Rectangle =
    let is = map(\i -> 2*i) (iota n)
    
    let rs : []Rectangle = map(\i -> mk_rect points_in[i] points_in[i+1]) is[:n/2]
    in rs 

entry bench_rangeQuery2d [n_in] (points_in : [n_in][2]f64) : []i64 =
    let m = 2 * (n_in/3)
    let ps_in = map(\ps -> {x = ps[0], y = ps[1]})points_in

    let rs = mk_rects ps_in[:m]
    let ps = ps_in[m:]

    in rangeQuery2d_grid 1 rs ps