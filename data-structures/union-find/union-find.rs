// Implementation of Union-Find (Disjoint Set Union) data structure in Rust

struct UnionFind {
    parent: Vec<usize>,
    rank: Vec<usize>,
}

impl UnionFind {
    // Create a new Union-Find data structure with 'n' elements
    fn new(n: usize) -> UnionFind {
        let mut parent = vec![0; n];
        let mut rank = vec![0; n];
        for i in 0..n {
            parent[i] = i;
        }
        UnionFind { parent, rank }
    }

    // Find the root of the set to which 'x' belongs
    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]);
        }
        self.parent[x]
    }

    // Merge the sets containing elements 'x' and 'y'
    fn union(&mut self, x: usize, y: usize) {
        let root_x = self.find(x);
        let root_y = self.find(y);

        if root_x == root_y {
            return;
        }

        if self.rank[root_x] < self.rank[root_y] {
            self.parent[root_x] = root_y;
        } else if self.rank[root_x] > self.rank[root_y] {
            self.parent[root_y] = root_x;
        } else {
            self.parent[root_y] = root_x;
            self.rank[root_x] += 1;
        }
    }

    // Check if elements 'x' and 'y' belong to the same set
    fn same_set(&mut self, x: usize, y: usize) -> bool {
        self.find(x) == self.find(y)
    }
}

fn main() {
    let mut uf = UnionFind::new(10);

    uf.union(0, 1);
    uf.union(2, 3);
    uf.union(4, 5);
    uf.union(6, 7);

    println!("Is 1 and 2 in the same set? {}", uf.same_set(1, 2)); // Should print "false"
    println!("Is 3 and 4 in the same set? {}", uf.same_set(3, 4)); // Should print "false"

    uf.union(1, 2);
    uf.union(3, 4);

    println!("Is 1 and 2 in the same set? {}", uf.same_set(1, 2)); // Should print "true"
    println!("Is 3 and 4 in the same set? {}", uf.same_set(3, 4)); // Should print "true"
}
