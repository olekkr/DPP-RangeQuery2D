-- Note that the upper left corner of the space is the origin.
-- x is going to the right
-- y is going down

-- ### TEST DEFINITIONS ### --

type Point = (f64, f64)                     -- (x, y)
type Rectangle = (Point, Point)             -- (ll, ur)


-- ### FUNCTIONS DEFINITIONS ### --

-- Name :   up_sweep
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points to eliminate, and -1 otherwise
--
-- This function returns an array of indices of points to eliminate.
-- Those points correspond to the point above the Rectangle (ie p.y < Rect.ur.y)
def up_sweep [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_up = map(
                    -- \i -> if (points[i].y < rectangle.ur.y) then i else -1 
                    \i -> if (points[i].1 < rectangle.1.1) then i else -1
                ) (iota n)

    in P_up


-- Name :   down_sweep
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points to eliminate, and -1 otherwise
--
-- This function returns an array of indices of points to eliminate.
-- Those points correspond to the point under the Rectangle (ie p.y > Rect.ll.y)
def down_sweep [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_down = map(
                    -- \i -> if (points[i].y > rectangle.ll.y) then i else -1 
                    \i -> if (points[i].1 > rectangle.0.1) then i else -1
                ) (iota n)

    in P_down


-- Name :   left_sweep
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Array of the indices of the points to eliminate, and -1 otherwise
--
-- This function returns an array of indices of points to eliminate.
-- Those points correspond to the point on the left of the Rectangle (ie p.x < Rect.ll.x)
def left_sweep [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_left = map(
                    -- \i -> if (points[i].x < rectangle.ll.x) then i else -1 
                    \i -> if (points[i].0 < rectangle.0.0) then i else -1
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
def right_sweep [n] (rectangle : Rectangle) (points : [n]Point) : [n](i64) =
    let P_right = map(
                    -- \i -> if (points[i].x > rectangle.ur.x) then i else -1 
                    \i -> if (points[i].0 > rectangle.1.0) then i else -1
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

    let P_up = up_sweep rectangle points
    let P_down = down_sweep rectangle points
    let P_left = left_sweep rectangle points
    let P_right = right_sweep rectangle points

    let P_sol = map4(
                \u d l r -> 
                if u == -1 && d == -1 && l == -1 && r == -1 then 1 
                else -1
                ) P_up P_down P_left P_right

    in P_sol


-- Name :   nb_points_in_rectangle
-- Input :  - A Rectangle
--          - Array of [n] Point
--
-- Output : - Number of points in the rectangle
--
-- This function returns the number of points in the rectangle
def nb_points_in_rectangle [n] (rectangle : Rectangle) (points : [n]Point) : i64 =

    let P_up = up_sweep rectangle points
    let P_down = down_sweep rectangle points
    let P_left = left_sweep rectangle points
    let P_right = right_sweep rectangle points

    let P_sol = map4(
                \u d l r -> 
                if u == -1 && d == -1 && l == -1 && r == -1 then 1 
                else 0
                ) P_up P_down P_left P_right

    let nb_point = reduce (+) 0 P_sol
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

def p1 : Point = (2.5, 5)
def p2 : Point = (5, 2.5)
def p3 : Point = (7.5, 5)
def p4 : Point = (5, 7.5)

def p5 : Point = (4, 5)
def p6 : Point = (5, 4)
def p7 : Point = (6, 5)
def p8 : Point = (5, 6)

def p9 : Point = (5, 5)

def ll : Point = (4,6)
def ur : Point = (6,4)
def r : Rectangle = (ll,ur)


def R : []Rectangle = [r]
def P : []Point = [p1, p2, p3, p4, p5, p6, p7, p8, p9] 
def result = rangeQuery2d R P


def debug_Pl = left_sweep r P
def debug_Pr = right_sweep r P
def debug_Pu = up_sweep r P
def debug_Pd = down_sweep r P


-- ### BENCHMARKING UNIT : rangeQuery2d ### --

-- BENCHMARKING rangeQuery2d.
-- ==
-- entry: bench_rangeQuery2d
-- "n=10 m=10" compiled script input { mk_inputs 10i64 10i64 }
-- "n=10 m=100" compiled script input { mk_inputs 10i64 100i64 }
-- "n=100 m=10" compiled script input { mk_inputs 100i64 10i64 }
-- "n=100 m=100" compiled script input { mk_inputs 100i64 100i64 }
-- "n=10 m=1_000" compiled script input { mk_inputs 10i64 1000i64 }
-- "n=1_000 m=10" compiled script input { mk_inputs 1000i64 10i64 }
-- "n=1_000 m=1_000" compiled script input { mk_inputs 1000i64 1000i64 }
-- "n=10 m=10_000" compiled script input { mk_inputs 10i64 10000i64 }
-- "n=10_000 m=10" compiled script input { mk_inputs 10000i64 10i64 }
-- "n=10_000 m=10_000" compiled script input { mk_inputs 10000i64 10000i64 }


-- TODO : Randomize this function
def mk_point : Point  = 
    let p = (0.0, 0.0)
    in p

-- TODO : Randomize this function
def mk_rect : Rectangle  = 
    let ll = (0.0, 0.0)
    let ur = (1.0, 1.0)
    in (ll, ur)

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

-- --------------------------------------------------
-- Command line for benchmarking : 'futhark bench --backend=c rangeQuery2d_rect_sweep.fut' 
-- Compiling rangeQuery2d_rect_sweep.fut...
-- Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
-- More runs automatically performed for up to 300s to ensure accurate measurement.

-- rangeQuery2d_rect_sweep.fut:bench_rangeQuery2d (no tuning file):
-- n=10 m=10:                  1μs (95% CI: [       0.6,        0.6])
-- n=10 m=100:                 4μs (95% CI: [       3.6,        3.6])
-- n=100 m=10:                 4μs (95% CI: [       3.5,        3.6])
-- n=100 m=100:               35μs (95% CI: [      34.6,       35.0])
-- n=10 m=1_000:              33μs (95% CI: [      33.3,       33.4])
-- n=1_000 m=10:              36μs (95% CI: [      35.6,       35.8])
-- n=1_000 m=1_000:         5257μs (95% CI: [    5255.0,     5261.0])
-- n=10 m=10_000:            629μs (95% CI: [     626.6,      633.3])
-- n=10_000 m=10:            593μs (95% CI: [     588.2,      598.7])
-- n=10_000 m=10_000:     329823μs (95% CI: [  329176.2,   331015.9])
-- --------------------------------------------------


-- --------------------------------------------------
-- Command line for benchmarking : 'futhark bench --backend=opencl rangeQuery2d_rect_sweep.fut' 
-- Compiling rangeQuery2d_rect_sweep.fut...
-- Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
-- More runs automatically performed for up to 300s to ensure accurate measurement.

-- rangeQuery2d_rect_sweep.fut:bench_rangeQuery2d (no tuning file):
-- n=10 m=10:                 33μs (95% CI: [      28.0,       39.3])
-- n=10 m=100:                39μs (95% CI: [      34.0,       45.0])
-- n=100 m=10:                38μs (95% CI: [      33.6,       44.2])
-- n=100 m=100:               43μs (95% CI: [      37.5,       49.3])
-- n=10 m=1_000:              38μs (95% CI: [      33.5,       45.0])
-- n=1_000 m=10:              41μs (95% CI: [      35.2,       48.5])
-- n=1_000 m=1_000:          186μs (95% CI: [     176.3,      198.0])
-- n=10 m=10_000:             52μs (95% CI: [      46.6,       58.7])
-- n=10_000 m=10:             64μs (95% CI: [      58.1,       71.6])
-- n=10_000 m=10_000:       9378μs (95% CI: [    8850.6,    10309.9])
-- --------------------------------------------------
