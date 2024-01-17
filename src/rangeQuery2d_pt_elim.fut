-- Note that the upper left corner of the space is the origin.
-- x is going to the right
-- y is going down
import "common"

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


-- Name :   rangeQuery2d
-- Input :  - Array of [m] Rectangle
--          - Array of [n] Point
--
-- Output : - Number of points in the rectangle
--
-- This function returns the number of points in the rectangle
def rangeQuery2d [m] [n] (rectangles : [m]Rectangle) (points : [n]Point) : [m]i64 =

    let solution = map(\r -> nb_points_in_rectangle r points) rectangles
    in solution


-- ### BENCHMARKING UNIT ### --
-- ==
-- entry: bench_rangeQuery2d
-- "2DinCube (small)" input @ InputData/fut/fut_2DinCube_1000000.in
-- "2Dkuzmin (small)" input @ InputData/fut/fut_2Dkuzmin_1000000.in
-- "2DinCube (large)" input @ InputData/fut/fut_2DinCube_10M.in
-- "2Dkuzmin (large)" input @ InputData/fut/fut_2Dkuzmin_10M.in

-- PBBS times (s)
-- 2DinCube (small):    '0.583', '0.584', '0.58', geomean = 0.583
-- 2Dkuzmin (small):    '0.582', '0.584', '0.578', geomean = 0.581
-- 2DinCube (large):    '9.737', '9.813', '9.845', geomean = 9.798
-- 2Dkuzmin (large):    '10.15', '10.673', '10.578', geomean = 10.465

-- Our times (us)
-- 2DinCube (small):    9221271μs (95% CI: [ 9112031.0,  9375027.1])
-- 2Dkuzmin (small):    9074049μs (95% CI: [ 9069187.4,  9078955.0])
-- 2DinCube (large):    No data, taking too long on my machine
-- 2Dkuzmin (large):    No data, taking too long on my machine

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

    in rangeQuery2d rs ps