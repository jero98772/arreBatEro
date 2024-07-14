class TrieNode:
    def __init__(self):
        self.children = {}
        self.is_end_of_word = False

class Trie:
    def __init__(self):
        self.root = TrieNode()

    def insert(self, word):
        node = self.root
        for char in word:
            if char not in node.children:
                node.children[char] = TrieNode()
            node = node.children[char]
        node.is_end_of_word = True

    def search(self, prefix):
        node = self.root
        for char in prefix:
            if char not in node.children:
                return []
            node = node.children[char]
        return self._find_words(node, prefix)

    def _find_words(self, node, prefix):
        words = []
        if node.is_end_of_word:
            words.append(prefix)
        for char, next_node in node.children.items():
            words.extend(self._find_words(next_node, prefix + char))
        return words
def read_file_to_vector(file_path):
    # Open the file in read mode
    with open(file_path, 'r') as file:
        # Read the file contents and split by lines
        strings = file.read().splitlines()
    return strings


trie = Trie()
words = read_file_to_vector("texto.txt")#["hello", "hell", "heaven", "heavy"]
for word in words:
    trie.insert(word)

print(trie.search("he"))
