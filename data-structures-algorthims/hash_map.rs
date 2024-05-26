use std::collections::LinkedList;
use std::hash::{Hash, Hasher};
use std::fmt;

// Define a key-value pair struct
#[derive(Debug, Clone)]
struct KeyValuePair<K, V> {
    key: K,
    value: V,
}

// Implement Hash for KeyValuePair to enable hashing based on key
impl<K: Hash, V> Hash for KeyValuePair<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.hash(state);
    }
}

// Implement PartialEq for KeyValuePair to enable equality comparison based on key
impl<K: PartialEq, V> PartialEq for KeyValuePair<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

// Implement Eq for KeyValuePair to enable equality comparison based on key
impl<K: Eq, V> Eq for KeyValuePair<K, V> {}

// Define a hash map structure using separate chaining for collision resolution
#[derive(Debug)]
struct HashMap<K, V> {
    buckets: Vec<LinkedList<KeyValuePair<K, V>>>,
    capacity: usize,
}

impl<K, V> HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    // Function to create a new hash map with a specified capacity
    fn new(capacity: usize) -> Self {
        let mut buckets = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            buckets.push(LinkedList::new());
        }
        HashMap { buckets, capacity }
    }

    // Function to insert a key-value pair into the hash map
    fn insert(&mut self, key: K, value: V) {
        let index = self.hash(&key);
        let bucket = &mut self.buckets[index];
        let kv_pair = KeyValuePair {
            key: key.clone(),
            value: value.clone(),
        };

        if let Some(existing_kv_pair) = bucket.iter_mut().find(|kv| kv.key == key) {
            existing_kv_pair.value = value;
        } else {
            bucket.push_back(kv_pair);
        }
    }

    // Function to get a value associated with a key from the hash map
    fn get(&self, key: &K) -> Option<&V> {
        let index = self.hash(key);
        let bucket = &self.buckets[index];
        bucket.iter().find(|kv| kv.key == *key).map(|kv| &kv.value)
    }



    // Function to calculate the index for a key
    fn hash(&self, key: &K) -> usize {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        (hasher.finish() % (self.capacity as u64)) as usize
    }
}


// Implement fmt::Display for HashMap for pretty printing
impl<K: fmt::Debug, V: fmt::Debug> fmt::Display for HashMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, bucket) in self.buckets.iter().enumerate() {
            writeln!(f, "Bucket {}: {:?}", i, bucket)?;
        }
        Ok(())
    }
}

fn main() {
    // Create a new hash map with a capacity of 5
    let mut map: HashMap<&str, i32> = HashMap::new(5);

    // Insert some key-value pairs
    map.insert("apple", 10);
    map.insert("banana", 20);
    map.insert("orange", 30);
    map.insert("grape", 40);

    // Print the hash map
    println!("Initial HashMap:");
    println!("{}", map);

    // Get and print values associated with keys
    println!("Value of apple: {:?}", map.get(&"apple"));
    println!("Value of orange: {:?}", map.get(&"orange"));
    println!("Value of pineapple: {:?}", map.get(&"pineapple"));


}
