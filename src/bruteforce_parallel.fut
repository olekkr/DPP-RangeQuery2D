type Point = (f64, f64)                     -- (x, y)
type Rectangle = (Point, Point)             -- (ll, ur)



def within (p:Point) (r:Rectangle) : bool =
    p.0 <= r.1.0 && 
            p.0 >= r.0.0 &&
            p.1 <= r.0.1 &&
            p.1 >= r.1.1

-- counts the number of points in the rect 
def points_in_rect [n] (points: [n]Point) (rect: Rectangle): i64 = 
    reduce (+) 0 (map (\p -> if within p rect then 1 else 0) points)

def rangeQuery2d [n] [m] (points: [n]Point) (rects: [m]Rectangle): [m]i64 =
    map (points_in_rect points) rects

