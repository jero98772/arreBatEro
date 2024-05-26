#[derive(Debug, Clone, PartialEq)]
struct Interval {
    start: i32,
    end: i32,
}

impl Interval {
    fn new(start: i32, end: i32) -> Self {
        Interval { start, end }
    }
}

#[derive(Debug)]
struct Node {
    interval: Interval,
    max: i32,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

impl Node {
    fn new(interval: Interval) -> Self {
        Node {
            interval: interval.clone(),
            max: interval.end,
            left: None,
            right: None,
        }
    }
}


#[derive(Debug)]
struct IntervalTree {
    root: Option<Box<Node>>,
}

impl IntervalTree {
    fn new() -> Self {
        IntervalTree { root: None }
    }

    fn insert(&mut self, interval: Interval) {
        self.root = Self::insert_recursive(self.root.take(), interval);
    }

    fn insert_recursive(root: Option<Box<Node>>, interval: Interval) -> Option<Box<Node>> {
        match root {
            None => Some(Box::new(Node::new(interval))),
            Some(mut node) => {
                let node_interval = node.interval.clone();
                if interval.start < node_interval.start {
node.left = Self::insert_recursive(node.left.take(), interval.clone());
                } else {
node.right = Self::insert_recursive(node.right.take(), interval.clone());
                }
                node.max = node.max.max(interval.end);
                Some(node)
            }
        }
    }

    fn overlap_search(&self, interval: Interval) -> Vec<Interval> {
        let mut result = Vec::new();
        Self::overlap_search_recursive(&self.root, interval, &mut result);
        result
    }

    fn overlap_search_recursive(
        root: &Option<Box<Node>>,
        interval: Interval,
        result: &mut Vec<Interval>,
    ) {
        if let Some(node) = root {
            let node_interval = &node.interval;
            if node_interval.start <= interval.end && node_interval.end >= interval.start {
                result.push(node_interval.clone());
            }
            if let Some(ref left) = node.left {
                if left.max >= interval.start {
                    Self::overlap_search_recursive(&node.left, interval.clone(), result);
                }
            }
            if let Some(ref right) = node.right {
                if right.interval.start <= interval.end {
                    Self::overlap_search_recursive(&node.right, interval.clone(), result);
                }
            }
        }
    }
}

fn main() {
    let mut tree = IntervalTree::new();
    tree.insert(Interval::new(15, 20));
    tree.insert(Interval::new(10, 30));
    tree.insert(Interval::new(17, 19));
    tree.insert(Interval::new(5, 20));
    tree.insert(Interval::new(12, 15));
    tree.insert(Interval::new(30, 40));

    let search_interval = Interval::new(14, 16);
    let overlapping_intervals = tree.overlap_search(search_interval);
    println!("Overlapping Intervals: {:?}", overlapping_intervals);
}
