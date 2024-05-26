

// Define a rectangle structure
#[derive(Debug, Clone, Copy)]
struct Rectangle {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

impl Rectangle {
    // Function to check if a point is inside the rectangle
    fn contains(&self, x: f64, y: f64) -> bool {
        x >= self.x && x <= self.x + self.width && y >= self.y && y <= self.y + self.height
    }
}

// Define a quadtree node
#[derive(Debug)]
struct QuadTreeNode {
    boundary: Rectangle,
    objects: Vec<Rectangle>,
    children: Option<[Box<QuadTreeNode>; 4]>,
}

impl QuadTreeNode {
    // Function to create a new quadtree node
    fn new(boundary: Rectangle) -> Self {
        QuadTreeNode {
            boundary,
            objects: Vec::new(),
            children: None,
        }
    }

    // Function to subdivide the node into four children
    fn subdivide(&mut self) {
        let x = self.boundary.x;
        let y = self.boundary.y;
        let width = self.boundary.width / 2.0;
        let height = self.boundary.height / 2.0;

        let nw = Rectangle {
            x,
            y,
            width,
            height,
        };
        let ne = Rectangle {
            x: x + width,
            y,
            width,
            height,
        };
        let sw = Rectangle {
            x,
            y: y + height,
            width,
            height,
        };
        let se = Rectangle {
            x: x + width,
            y: y + height,
            width,
            height,
        };

        let children = [
            Box::new(QuadTreeNode::new(nw)),
            Box::new(QuadTreeNode::new(ne)),
            Box::new(QuadTreeNode::new(sw)),
            Box::new(QuadTreeNode::new(se)),
        ];

        self.children = Some(children);
    }

    // Function to insert a rectangle into the quadtree
    fn insert(&mut self, rect: Rectangle) {
        if !self.boundary.contains(rect.x, rect.y) {
            return;
        }

        if let Some(children) = &mut self.children {
            for child in children.iter_mut() {
                if child.boundary.contains(rect.x, rect.y) {
                    child.insert(rect);
                    return;
                }
            }
        }

        self.objects.push(rect);

        // Subdivide if necessary
        if self.objects.len() > 4 && self.children.is_none() {
            self.subdivide();
            let mut i = 0;
            while i < self.objects.len() {
                if let Some(children) = &mut self.children {
                    let mut j = 0;
                    while j < children.len() {
                        if children[j].boundary.contains(self.objects[i].x, self.objects[i].y) {
                            children[j].insert(self.objects.remove(i));
                            break;
                        }
                        j += 1;
                    }
                }
                i += 1;
            }
        }
    }
}

// Function to print a rectangle
fn print_rect(rect: &Rectangle) {
    println!(
        "Rectangle: x={}, y={}, width={}, height={}",
        rect.x, rect.y, rect.width, rect.height
    );
}

fn main() {
    // Define the boundary of the quadtree
    let boundary = Rectangle {
        x: 0.0,
        y: 0.0,
        width: 100.0,
        height: 100.0,
    };

    // Create a new quadtree
    let mut quadtree = QuadTreeNode::new(boundary);

    // Insert rectangles into the quadtree
    let rectangles = vec![
        Rectangle {
            x: 10.0,
            y: 10.0,
            width: 20.0,
            height: 20.0,
        },
        Rectangle {
            x: 30.0,
            y: 30.0,
            width: 15.0,
            height: 15.0,
        },
        Rectangle {
            x: 70.0,
            y: 70.0,
            width: 25.0,
            height: 25.0,
        },
    ];

    for rect in rectangles {
        quadtree.insert(rect);
    }

    // Print all rectangles stored in the quadtree
    println!("Rectangles stored in the quadtree:");
    for rect in &quadtree.objects {
        print_rect(rect);
    }
}
