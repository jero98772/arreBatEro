import steap_by_steap



print(dir(steap_by_steap))
uf = steap_by_steap.UnionFind(10)

uf.union(0, 1)
uf.union(2, 3)
uf.union(4, 5)
uf.union(6, 7)

print(f"Is 1 and 2 in the same set? {uf.same_set(1, 2)}")  # Should print "false"
print(f"Is 3 and 4 in the same set? {uf.same_set(3, 4)}")  # Should print "false"

uf.union(1, 2)
uf.union(3, 4)

print(f"Is 1 and 2 in the same set? {uf.same_set(1, 2)}")  # Should print "true"
print(f"Is 3 and 4 in the same set? {uf.same_set(3, 4)}")  # Should print "true"
