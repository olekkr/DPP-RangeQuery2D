open import "common"

-- counts the number of points in the rect 
def points_in_rect [n] (points: [n]Point) (rect: Rectangle): i64 = 
    reduce (+) 0 (map (\p -> if within p rect then 1 else 0) points)

def rangeQuery2d [n] [m] (rects: [m]Rectangle)  (points: [n]Point): [m]i64 =
    map (points_in_rect points) rects

