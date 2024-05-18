use std::fmt::Debug;

struct Queue<T> {
    items: Vec<T>,
}

impl<T> Queue<T> {
    fn new() -> Self {
        Queue { items: Vec::new() }
    }

    fn enqueue(&mut self, item: T) {
        self.items.push(item);
    }

    fn dequeue(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            Some(self.items.remove(0))
        }
    }

    fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    fn len(&self) -> usize {
        self.items.len()
    }

    fn peek(&self) -> Option<&T> {
        self.items.get(0)
    }

    fn print_queue(&self)
    where
        T: Debug,
    {
        println!("Queue: {:?}", self.items);
    }
}

fn main() {
    let mut queue: Queue<i32> = Queue::new();

    queue.enqueue(1);
    queue.enqueue(2);
    queue.enqueue(3);

    queue.print_queue();

    println!("Dequeue: {:?}", queue.dequeue());
    queue.print_queue();

    println!("Peek: {:?}", queue.peek());
    println!("Queue length: {}", queue.len());
}
