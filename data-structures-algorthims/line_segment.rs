struct Point {
    x: f64,
    y: f64,
}

struct LineSegment {
    start: Point,
    end: Point,
}

impl LineSegment {
    fn new(start: Point, end: Point) -> LineSegment {
        LineSegment { start, end }
    }

    fn length(&self) -> f64 {
        ((self.end.x - self.start.x).powi(2) + (self.end.y - self.start.y).powi(2)).sqrt()
    }

    fn midpoint(&self) -> Point {
        Point {
            x: (self.start.x + self.end.x) / 2.0,
            y: (self.start.y + self.end.y) / 2.0,
        }
    }
}

fn main() {
    let start_point = Point { x: 1.0, y: 1.0 };
    let end_point = Point { x: 4.0, y: 5.0 };
    let line_segment = LineSegment::new(start_point, end_point);

    println!("Length of line segment: {}", line_segment.length());
    let midpoint = line_segment.midpoint();
    println!("Midpoint of line segment: ({}, {})", midpoint.x, midpoint.y);
}
