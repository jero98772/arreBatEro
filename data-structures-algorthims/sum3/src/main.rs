use rand::Rng;
use std::collections::HashSet;

fn generate_random_vector(size: usize, min: i32, max: i32) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen_range(min..=max)).collect()
}

fn all_three_sum_tuples(nums: Vec<i32>, target: i32) -> Vec<(usize, usize, usize)> {
    let mut result = Vec::new();
    let mut seen = HashSet::new();

    for i in 0..nums.len() {
        let mut map = std::collections::HashMap::new();
        for j in (i + 1)..nums.len() {
            let complement = target - nums[i] - nums[j];
            if let Some(&k) = map.get(&complement) {
                let mut triplet = vec![i, j, k];
                triplet.sort();
                let triplet_tuple = (triplet[0], triplet[1], triplet[2]);
                if !seen.contains(&triplet_tuple) {
                    result.push(triplet_tuple);
                    seen.insert(triplet_tuple);
                }
            }
            map.insert(nums[j], j);
        }
    }

    result
}

fn main() {
    let size = 10; // Size of the random vector
    let min = 1;   // Minimum value for the random numbers
    let max = 50;  // Maximum value for the random numbers

    let random_vector = generate_random_vector(size, min, max);
    println!("Random Vector: {:?}", random_vector);

    let target = 50; // Example target sum
    let triplets = all_three_sum_tuples(random_vector, target);

    if triplets.is_empty() {
        println!("No solution found");
    } else {
        println!("Triplets of indices that sum to {}: {:?}", target, triplets);
    }
}
