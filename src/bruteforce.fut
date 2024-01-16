
-- TODO: should maybe be moved to a common.fut along with within
type Point = (f64, f64)                     -- (x, y)
type Rectangle = (Point, Point)             -- (ll, ur)


def within (p:Point) (r:Rectangle) : bool =
    p.0 <= r.1.0 && 
            p.0 >= r.0.0 &&
            p.1 <= r.0.1 &&
            p.1 >= r.1.1

   
def get_counts [n] [m] (points: [n]Point) (rects: [m]Rectangle): [m]i64 = 
    let counts = replicate m 0 
    let res = loop (new_counts, r) = (counts, 0) for r < m do 
        let (count, _) = loop (acc, p) = (0, 0) for p < n do  
        -- loops flipped around for better temporal locality
            if within points[p] rects[r] 
                then (acc+1, p+1)
                else (acc, p+1)
        in
        (new_counts with [r] = count, r+1)
    let (final_counts, _) = res 
    in final_counts 

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
def result = get_counts P R


-- def debug_Pl = left_sweep r P
-- def debug_Pr = right_sweep r P
-- def debug_Pu = up_sweep r P
-- def debug_Pd = down_sweep r P


-- ### BENCHMARKING UNIT : rangeQuery2d ### --

-- BENCHMARKING rangeQuery2d.
-- ==
-- entry: bench_rangeQuery2d
-- "n=10 m=10" compiled script input { mk_inputs 10i64 10i64 }
-- "n=100 m=10" compiled script input { mk_inputs 100i64 10i64 }
-- "n=10 m=100" compiled script input { mk_inputs 10i64 100i64 }
-- "n=100 m=100" compiled script input { mk_inputs 100i64 100i64 }
-- "n=1_000 m=1_000" compiled script input { mk_inputs 1000i64 1000i64 }
-- "n=10 m=1_000" compiled script input { mk_inputs 10i64 1000i64 }
-- "n=1_000 m=10" compiled script input { mk_inputs 1000i64 10i64 }
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
    let solution = get_counts points rectangles
    in solution


-- --------------------------------------------------
-- Command line for benchmarking : 'futhark bench --backend=c rangeQuery2d_rect_sweep.fut' 
-- Compiling rangeQuery2d_rect_sweep.fut...
-- Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
-- More runs automatically performed for up to 300s to ensure accurate measurement.

-- rangeQuery2d_rect_sweep.fut:bench_rangeQuery2d (no tuning file):
-- n=10 m=10:                  1μs (95% CI: [       0.6,        0.7])
-- n=100 m=10:                 4μs (95% CI: [       3.7,        3.8])
-- n=10 m=100:                 4μs (95% CI: [       3.6,        3.6])
-- n=100 m=100:               35μs (95% CI: [      34.4,       34.7])
-- n=1_000 m=1_000:         5732μs (95% CI: [    5683.7,     5897.6])
-- n=10 m=1_000:              41μs (95% CI: [      40.6,       42.3])
-- n=1_000 m=10:              34μs (95% CI: [      33.8,       34.2])
-- n=10 m=10_000:            633μs (95% CI: [     629.1,      637.4])
-- n=10_000 m=10:            634μs (95% CI: [     627.4,      642.5])
-- n=10_000 m=10_000:     341101μs (95% CI: [  330622.5,   372403.2])
-- --------------------------------------------------


-- --------------------------------------------------
-- Command line for benchmarking : 'futhark bench --backend=opencl rangeQuery2d_rect_sweep.fut' 
-- Compiling rangeQuery2d_rect_sweep.fut...
-- Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
-- More runs automatically performed for up to 300s to ensure accurate measurement.

-- rangeQuery2d_rect_sweep.fut:bench_rangeQuery2d (no tuning file):
-- n=10 m=10:                 44μs (95% CI: [      38.5,       52.4])
-- n=100 m=10:                53μs (95% CI: [      47.1,       60.0])
-- n=10 m=100:                36μs (95% CI: [      31.4,       41.9])
-- n=100 m=100:               41μs (95% CI: [      36.2,       48.1])
-- n=1_000 m=1_000:          205μs (95% CI: [     192.7,      221.3])
-- n=10 m=1_000:              47μs (95% CI: [      41.7,       54.2])
-- n=1_000 m=10:              37μs (95% CI: [      32.3,       44.2])
-- n=10 m=10_000:             65μs (95% CI: [      57.7,       73.8])
-- n=10_000 m=10:             79μs (95% CI: [      71.6,       87.1])
-- n=10_000 m=10_000:       9321μs (95% CI: [    8877.6,     9846.1])
-- --------------------------------------------------
