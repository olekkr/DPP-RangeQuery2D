-- Note that the upper left corner of the space is the origin.
-- x is going to the right
-- y is going down

type Point = {x : f64, y : f64}
type Rectangle = {ll : Point, ur : Point}
type Cell [n] = {rectangle : Rectangle, p_in : [n]i64}

def within (p:Point) (r:Rectangle) : bool =
    p.x <= r.ur.x && 
            p.x >= r.ll.x &&
            p.y <= r.ll.y &&
            p.y >= r.ur.y

def within_i64 (p:Point) (r:Rectangle) : i64 =
    if p.x <= r.ur.x && 
            p.x >= r.ll.x &&
            p.y <= r.ll.y &&
            p.y >= r.ur.y
    then 1 else -1