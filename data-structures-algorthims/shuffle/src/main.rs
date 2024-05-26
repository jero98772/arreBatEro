use rand::prelude::*;

fn shuffle<T>(deck: &mut [T]) {
    let mut rng = thread_rng();
    let mut n = deck.len();

    while n > 1 {
        let k = rng.gen_range(0..n);
        n -= 1;
        deck.swap(n, k);
    }
}

fn main() {
    let mut deck = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    println!("Before shuffling: {:?}", deck);
    shuffle(&mut deck);
    println!("After shuffling: {:?}", deck);
}
