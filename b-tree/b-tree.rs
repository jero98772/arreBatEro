const MAX_KEYS: usize = 3;

#[derive(Debug)]
enum BTreeNode<K, V> {
    Internal {
        keys: Vec<K>,
        children: Vec<BTreeNode<K, V>>,
    },
    Leaf {
        keys: Vec<K>,
        values: Vec<V>,
    },
}

#[derive(Debug)]
pub struct BTree<K, V> {
    root: BTreeNode<K, V>,
}

impl<K: Ord, V> BTree<K, V> {
    pub fn new() -> Self {
        BTree { root: BTreeNode::Leaf { keys: vec![], values: vec![] } }
    }

    pub fn insert(&mut self, key: K, value: V) {
        let (new_key, new_child) = self.root.insert(key, value);
        if let Some(new_child) = new_child {
            let old_root = std::mem::replace(&mut self.root, BTreeNode::Internal {
                keys: vec![new_key.unwrap()],
                children: vec![],
            });
            if let BTreeNode::Internal { ref mut children, .. } = self.root {
                children.push(old_root);
                children.push(new_child);
            }
        }
    }
}

impl<K: Ord, V> BTreeNode<K, V> {
    fn insert(&mut self, key: K, value: V) -> (Option<K>, Option<BTreeNode<K, V>>) {
        match self {
            BTreeNode::Internal { keys, children } => {
                let mut idx = keys.len();
                for (i, k) in keys.iter().enumerate() {
                    if *k >= key {
                        idx = i;
                        break;
                    }
                }

                let (new_key, new_child) = children[idx].insert(key, value);
                if let Some(new_child) = new_child {
                    if keys.len() < MAX_KEYS {
                        keys.insert(idx, new_key.unwrap());
                        children.insert(idx + 1, new_child);
                        return (None, None);
                    } else {
                        let middle = keys.len() / 2;
                        let split_key = keys.remove(middle);
                        let mut right_keys = keys.split_off(middle);
                        right_keys.remove(0); // Remove the duplicate
                        let mut right_children = children.split_off(middle + 1);
                        let right_child = right_children.remove(0); // Remove the duplicate

                        return (Some(split_key), Some(BTreeNode::Internal {
                            keys: right_keys,
                            children: right_children,
                        }));
                    }
                }
            }
            BTreeNode::Leaf { keys, values } => {
                let mut idx = keys.len();
                for (i, k) in keys.iter().enumerate() {
                    if *k >= key {
                        idx = i;
                        break;
                    }
                }
                keys.insert(idx, key);
                values.insert(idx, value);
                if keys.len() > MAX_KEYS {
                    let middle = keys.len() / 2;
                    let split_key = keys.remove(middle);
                    let mut right_keys = keys.split_off(middle);
                    let right_values = values.split_off(middle);
                    return (Some(split_key), Some(BTreeNode::Leaf {
                        keys: right_keys,
                        values: right_values,
                    }));
                }
            }
        }
        (None, None)
    }
}

fn main() {
    let mut tree = BTree::new();
    tree.insert(3, "Three");
    tree.insert(1, "One");
    tree.insert(2, "Two");
    tree.insert(4, "Four");
    tree.insert(5, "Five");

    println!("{:#?}", tree);
}

