type Point = {x : f64, y : f64}
type Rectangle = {ll : Point, ur : Point}
type Cell [n] = {rectangle : Rectangle, p_in : [n]i64}

def p0 : Point = {x = 0.0, y = 0.0}
def p1 : Point = {x = 0.25, y = 0.5}
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
-- def P : []Point = [p0, p1, p2, p3, p6, p7, p10, p11, p8, p9, p4, p5, p12, p13]
def P : []Point = [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14]

def up_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].y < rectangle.ur.y) then i else -1) (iota n)

def down_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].y > rectangle.ll.y) then i else -1) (iota n)


def left_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].x < rectangle.ll.x) then i else -1) (iota n)

def right_elimination [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    map(\i -> if (points[i].x > rectangle.ur.x) then i else -1 ) (iota n)

def debug_points_in_rectangle [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =

    let P_up = up_elimination rectangle points
    let P_down = down_elimination rectangle points
    let P_left = left_elimination rectangle points
    let P_right = right_elimination rectangle points

    let P_in = map5(
                \u d l r id-> 
                if u == -1 && d == -1 && l == -1 && r == -1 then id
                else -1
                ) P_up P_down P_left P_right (iota n)

    in P_in






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
-- Correct
def preprocess_create_grid_depth [n] (depth : i64) (points : [n]Point) : []Cell [n] = 

    let rect_grid : []Rectangle = map(\i -> cell_helper_rectangle_depth depth i) (iota (4**depth) )
    let p_in_grid : [][n]i64 = map(\r -> debug_points_in_rectangle r points) rect_grid
    let grid : []Cell [n] = map2(\r p -> {rectangle = r, p_in = p}) rect_grid p_in_grid
    in grid



def cells = preprocess_create_grid_depth 2 P

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
