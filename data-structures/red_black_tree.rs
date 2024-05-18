use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Color {
    Red,
    Black,
}

#[derive(Debug)]
struct Node<T: Ord> {
    key: T,
    color: Color,
    left: Option<Box<Node<T>>>,
    right: Option<Box<Node<T>>>,
}

impl<T: Ord> Node<T> {
    fn new(key: T) -> Self {
        Node {
            key,
            color: Color::Red,
            left: None,
            right: None,
        }
    }
}

#[derive(Debug)]
pub struct RedBlackTree<T: Ord> {
    root: Option<Box<Node<T>>>,
}

impl<T: Ord> RedBlackTree<T> {
    pub fn new() -> Self {
        RedBlackTree { root: None }
    }

    pub fn insert(&mut self, key: T) {
        if let Some(ref mut root) = self.root {
            Self::insert_node(root, key);
            root.color = Color::Black; // Ensure root is always black
        } else {
            self.root = Some(Box::new(Node::new(key)));
        }
    }

    fn insert_node(root: &mut Box<Node<T>>, key: T) {
        match key.cmp(&root.key) {
            Ordering::Less => {
                if let Some(ref mut left) = root.left {
                    Self::insert_node(left, key);
                } else {
                    root.left = Some(Box::new(Node::new(key)));
                }
            }
            Ordering::Greater => {
                if let Some(ref mut right) = root.right {
                    Self::insert_node(right, key);
                } else {
                    root.right = Some(Box::new(Node::new(key)));
                }
            }
            Ordering::Equal => {} // Duplicates are not allowed
        }
    }
}

fn main() {
    let mut tree = RedBlackTree::new();
    tree.insert(10);
    tree.insert(5);
    tree.insert(15);
    tree.insert(3);
    tree.insert(7);
    println!("{:?}", tree);
}
