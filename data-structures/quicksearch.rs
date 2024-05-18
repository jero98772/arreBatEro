fn quick_select(arr: &mut [i32], k: usize) -> i32 {
    if arr.len() == 1 {
        return arr[0];
    }
    
    let pivot_index = partition(arr);
    
    if k == pivot_index {
        return arr[k];
    } else if k < pivot_index {
        return quick_select(&mut arr[0..pivot_index], k);
    } else {
        return quick_select(&mut arr[pivot_index + 1..], k - pivot_index - 1);
    }
}

fn partition(arr: &mut [i32]) -> usize {
    let pivot_index = arr.len() - 1;
    let pivot_value = arr[pivot_index];
    let mut i = 0;
    
    for j in 0..pivot_index {
        if arr[j] < pivot_value {
            arr.swap(i, j);
            i += 1;
        }
    }
    arr.swap(i, pivot_index);
    i
}

fn main() {
    let mut arr = [3, 2, 1, 5, 6, 4,24,53,14,42];
    let k = 6; // Index of the kth smallest element
    let result = quick_select(&mut arr, k);
    println!("The {}th smallest element is {}", k + 1, result);
}
