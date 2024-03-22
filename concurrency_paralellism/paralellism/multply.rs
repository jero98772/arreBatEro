use std::thread;

fn main(){
	let numbers=vec![1,2,3,4,5];
	let handles: Vec<_>=numbers.into_iter().map(|num|{
		thread::spawn(move||{
			let result=num*num;
			println!("{} {}",num,result);
		})

	}).collect();

	for handle in handles{
		handle.join().unwrap();
	}
} 
