use std::cmp::Ordering;

#[derive(Debug)]
struct Point {
    coords: Vec<f64>,
}

impl Point {
    fn new(coords: Vec<f64>) -> Self {
        Point { coords }
    }
}

#[derive(Debug)]
struct Node {
    point: Point,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

impl Node {
    fn new(point: Point) -> Self {
        Node {
            point,
            left: None,
            right: None,
        }
    }
}

#[derive(Debug)]
pub struct KDTree {
    root: Option<Box<Node>>,
    dimensions: usize,
}

impl KDTree {
    pub fn new(dimensions: usize) -> Self {
        KDTree {
            root: None,
            dimensions,
        }
    }

    pub fn insert(&mut self, point: Point) {
        let root = self.root.take();
        self.root = self.insert_recursive(root, point, 0);
    }

    fn insert_recursive(&mut self, node: Option<Box<Node>>, point: Point, depth: usize) -> Option<Box<Node>> {
        match node {
            Some(mut n) => {
                let dim = depth % self.dimensions;
                let ordering = point.coords[dim].partial_cmp(&n.point.coords[dim]);

                match ordering {
                    Some(Ordering::Less) | Some(Ordering::Equal) => {
                        n.left = self.insert_recursive(n.left.take(), point, depth + 1);
                    }
                    Some(Ordering::Greater) => {
                        n.right = self.insert_recursive(n.right.take(), point, depth + 1);
                    }
                    None => {
                        // Handle NaNs or other unordered comparisons
                        panic!("NaNs or unordered comparison encountered!");
                    }
                }

                Some(n)
            }
            None => Some(Box::new(Node::new(point))),
        }
    }
}

fn main() {
    let mut tree = KDTree::new(2); // Creating a 2D KD tree

    tree.insert(Point::new(vec![3.0, 6.0]));
    tree.insert(Point::new(vec![17.0, 15.0]));
    tree.insert(Point::new(vec![13.0, 15.0]));
    tree.insert(Point::new(vec![6.0, 12.0]));

    println!("{:?}", tree);
}
