use rand::Rng;
use std::collections::HashMap;

fn generate_random_vector(size: usize, min: i32, max: i32) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen_range(min..=max)).collect()
}

fn all_two_sum_pairs(nums: Vec<i32>, target: i32) -> Vec<(usize, usize)> {
    let mut map = HashMap::new();
    let mut result = Vec::new();

    for (i, &num) in nums.iter().enumerate() {
        let complement = target - num;
        if let Some(indices) = map.get(&complement) {
            for &index in indices {
                result.push((index, i));
            }
        }
        map.entry(num).or_insert_with(Vec::new).push(i);
    }

    result
}

fn main() {
    let size = 100; // Size of the random vector
    let min = 1;   // Minimum value for the random numbers
    let max = 100; // Maximum value for the random numbers

    let random_vector = generate_random_vector(size, min, max);
    println!("Random Vector: {:?}", random_vector);

    let target = 50; // Example target sum
    let pairs = all_two_sum_pairs(random_vector, target);

    if pairs.is_empty() {
        println!("No solution found");
    } else {
        println!("Pairs of indices that sum to {}: {:?}", target, pairs);
    }
}
