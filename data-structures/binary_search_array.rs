fn binary_search<T: Ord>(array: &[T], target: &T) -> Option<usize> {
    let mut left = 0;
    let mut right = array.len() - 1;

    while left <= right {
        let mid = left + (right - left) / 2;

        if array[mid] == *target {
            return Some(mid);
        } else if array[mid] < *target {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    None
}

fn main() {
    let arr = [1, 2, 3, 4, 5, 7, 8, 9];
    let target = 6;
    
    match binary_search(&arr, &target) {
        Some(index) => println!("Element {} found at index {}", target, index),
        None => println!("Element {} not found in the array", target),
    }
}
