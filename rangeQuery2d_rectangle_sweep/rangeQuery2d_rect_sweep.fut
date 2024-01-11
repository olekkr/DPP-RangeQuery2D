-- Note that the upper left corner of the space is the origin.
-- x is going to the right
-- y is going down

-- Types definitions
type Point = (f64, f64)                     -- (x, y)
type Rectangle = (Point, Point)             -- (ll, ur)


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


-- Functions definitions 

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
def rangeQuery2d [n] [m] (rectangles : [m]Rectangle) (points : [n]Point) : [m]i64 =

    let solution = map(\r -> nb_points_in_rectangle r points) rectangles
    in solution




-- Little Test for the functions

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


    