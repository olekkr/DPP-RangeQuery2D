-- Note that the upper left corner of the space is the origin.
-- x is going to the right
-- y is going down

-- ### TYPE DEFINITIONS ### --
type Point = {x : f64, y : f64}
type Rectangle = {ll : Point, ur : Point}


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






-- ### TEST UNIT : rangeQuery2d ### --

-- Types definitions
-- type Point = (f64, f64)                     -- (x, y)
-- type Rectangle = (Point, Point)             -- (ll, ur)

-- Points outside the rectangle
def p1 : Point = {x = 2.5,y = 5}        -- on the left
def p2 : Point = {x = 5, y = 2.5}       -- above
def p3 : Point = {x = 7.5, y = 5}       -- on the right
def p4 : Point = {x = 5, y = 7.5}       -- under

-- Points on the edge of the rectangle
def p5 : Point = {x = 4, y = 5}         -- on the left edge
def p6 : Point = {x = 5, y = 4}         -- on the upper edge
def p7 : Point = {x = 6, y = 5}         -- on the right edge
def p8 : Point = {x = 5, y = 6}         -- on the bottom edge

-- Points in the rectangle
def p9 : Point = {x = 5, y = 5}         -- in the middle of the rectangle

def ll : Point = {x = 4, y = 6}         -- lower left corner
def ur : Point = {x = 6, y = 4}         -- upper right corner
def r : Rectangle = {ll = ll, ur = ur}


def R : []Rectangle = [r]
def P : []Point = [p1, p2, p3, p4, p5, p6, p7, p8, p9] 

def Pl = left_elimination r P
def Pr = right_elimination r P
def Pu = up_elimination r P
def Pd = down_elimination r P
def result = rangeQuery2d R P


def expected__Pl : []i64 = [0, -1, -1, -1, -1, -1, -1, -1, -1]
def expected__Pr : []i64 = [-1, -1, 2, -1, -1, -1, -1, -1, -1]
def expected__Pu : []i64 = [-1, 1, -1, -1, -1, -1, -1, -1, -1]
def expected__Pd : []i64 = [-1, -1, -1, 3, -1, -1, -1, -1, -1]
def expected_result : []i64 = [5]




-- ### BENCHMARKING UNIT : rangeQuery2d ### --

-- BENCHMARKING rangeQuery2d.
-- ==
-- entry: bench_rangeQuery2d
-- "n=10 m=10" compiled script input { mk_inputs 10i64 10i64 }
-- "n=100 m=100" compiled script input { mk_inputs 100i64 100i64 }
-- "n=1_000 m=1_000" compiled script input { mk_inputs 1000i64 1000i64 }
-- "n=10_000 m=10_000" compiled script input { mk_inputs 10000i64 10000i64 }


-- --------------------------------------------------
-- Command line for benchmarking : 'futhark bench --backend=c rangeQuery2d_point_elimination.fut' 
-- Compiling rangeQuery2d_point_elimination.fut...
-- Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
-- More runs automatically performed for up to 300s to ensure accurate measurement.

-- rangeQuery2d_point_elimination.fut:bench_rangeQuery2d (no tuning file):
-- n=10 m=10:                  0μs (95% CI: [       0.4,        0.4])
-- n=100 m=100:               21μs (95% CI: [      20.9,       21.0])
-- n=1_000 m=1_000:         2022μs (95% CI: [    2007.3,     2041.8])
-- n=10_000 m=10_000:     195190μs (95% CI: [  194830.0,   195905.5])
-- --------------------------------------------------

-- TODO? : Randomize this function
def mk_point : Point  = 
    let p = { x = 0.0, y = 0.0}
    in p

-- TODO? : Randomize this function
def mk_rect : Rectangle  = 
    let ll = { x = 0.0, y = 0.0}
    let ur = { x = 1.0, y = 1.0}
    in {ll = ll, ur = ur}

def mk_points (n : i64) : []Point = 
    let ps = map(\_ -> mk_point ) (iota n)
    in ps 

def mk_rects (m : i64) : []Rectangle = 
    let rs = map(\_ -> mk_rect ) (iota m)
    in rs 

entry mk_inputs (m : i64) (n : i64) : ([]Rectangle, []Point) = 
    let rs = mk_rects m
    let ps = mk_points n
    in (rs, ps)

    entry bench_rangeQuery2d [m] [n] (rectangles : [m]Rectangle) (points : [n]Point) : [m]i64 =
        let solution = map(\r -> nb_points_in_rectangle r points) rectangles
        in solution

