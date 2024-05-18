use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }
}

fn orientation(p: Point, q: Point, r: Point) -> f64 {
    (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
}

fn polar_angle(p0: Point, p1: Point) -> f64 {
    (p1.y - p0.y).atan2(p1.x - p0.x)
}

fn graham_scan(points: &mut [Point]) -> Vec<Point> {
    // Step 1: Find the point with the lowest y-coordinate (and the leftmost in case of tie)
    let mut start = points[0];
    let mut start_index = 0;

    for (i, &point) in points.iter().enumerate() {
        if point.y < start.y || (point.y == start.y && point.x < start.x) {
            start = point;
            start_index = i;
        }
    }

    // Move the start point to the beginning of the array
    points.swap(0, start_index);

    // Step 2: Sort the points based on the polar angle with the start point
    let start = points[0];
    points[1..].sort_by(|&p1, &p2| {
        let angle1 = polar_angle(start, p1);
        let angle2 = polar_angle(start, p2);
        if angle1.partial_cmp(&angle2).unwrap() == Ordering::Equal {
            p1.y.partial_cmp(&p2.y).unwrap()
        } else {
            angle1.partial_cmp(&angle2).unwrap()
        }
    });

    // Step 3: Create a stack and push the start point
    let mut hull = Vec::new();
    hull.push(points[0]);
    hull.push(points[1]);

    for &point in points.iter().skip(2) {
        while hull.len() > 1 && orientation(hull[hull.len() - 2], hull[hull.len() - 1], point) <= 0.0 {
            hull.pop();
        }
        hull.push(point);
    }

    hull
}

fn is_inside_polygon(point: Point, hull: &[Point]) -> bool {
    if hull.len() < 3 {
        return false; // Convex hull needs at least 3 points
    }

    let mut wn = 0; // Winding number

    for i in 0..hull.len() {
        let p1 = hull[i];
        let p2 = hull[(i + 1) % hull.len()];

        if p1.y <= point.y {
            if p2.y > point.y && orientation(p1, p2, point) > 0.0 {
                wn += 1;
            }
        } else {
            if p2.y <= point.y && orientation(p1, p2, point) < 0.0 {
                wn -= 1;
            }
        }
    }

    wn != 0
}

fn points_outside_convex_hull(points: &[Point], hull: &[Point]) -> Vec<Point> {
    points
        .iter()
        .cloned()
        .filter(|&point| !is_inside_polygon(point, hull))
        .collect()
}



fn main() {
    // Sample points
    let mut points = vec![
        Point::new(0.0, 0.0),
        Point::new(-3.0, -3.0),
        Point::new(1.0, 1.0),
        Point::new(2.0, 2.0),
        Point::new(3.0, 1.0),
        Point::new(-3.0, 22.0),
        Point::new(0.0, 3.0),
        Point::new(3.0, 3.0),
        Point::new(3.0, 0.0),
        Point::new(1.0, 2.0),
        Point::new(4.0, 22.0),
    ];

    // Compute the convex hull
    let hull = graham_scan(&mut points);

    // Display the convex hull
    println!("Convex Hull Points:");
    for point in hull {
        println!("({:.1}, {:.1})", point.x, point.y);
    }

    // Compute the convex hull
    let mut points_clone = points.clone();
    let hull = graham_scan(&mut points_clone);

    // Find points outside the convex hull
    let outside_points = points_outside_convex_hull(&points, &hull);

    // Display points outside the convex hull
    println!("Points outside the convex hull:");
    for point in outside_points {
        println!("({:.1}, {:.1})", point.x, point.y);
    }
}
