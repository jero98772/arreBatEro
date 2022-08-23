class Trie(object):
   def __init__(self):
      self.child = {}
   def insert(self, word):
      current = self.child
      for l in word:
         if l not in current:
            current[l] = {}
         current = current[l]
      current['#']=1
   def search(self, word):
      current = self.child
      for l in word:
         if l not in current:
            return False
         current = current[l]
      return '#' in current
   def startsWith(self, prefix):
      current = self.child
      for l in prefix:
         if l not in current:
            return False
         current = current[l]
      return True
ob1 = Trie()
ob1.insert("abb")
#print(ob1.search("apple"))
#print(ob1.search("app"))
#print(ob1.startsWith("app"))
ob1.insert("app")
print(ob1.search("app"))
Input