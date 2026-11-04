# A tree that Python's sum, max and "in" can use, for the chapter
# "Foldable and Traversable".
# sum や max や in が使える木。「Foldable・Traversable」の章


# ANCHOR: procedural
# Python (procedural style: walk the tree into a list by hand)
# Python（手続き型スタイル：手で木をたどって、リストにする）
class Node:
    def __init__(self, left, value, right):
        self.left, self.value, self.right = left, value, right

    def to_list(self):
        result = []
        if self.left is not None:
            result.extend(self.left.to_list())
        result.append(self.value)
        if self.right is not None:
            result.extend(self.right.to_list())
        return result
# ANCHOR_END: procedural


# ANCHOR: iter
# Python (functional style: one __iter__, and many tools work)
# Python（関数型スタイル：__iter__ を1つ書けば、たくさんの道具が使える）
class Tree:
    def __init__(self, left, value, right):
        self.left, self.value, self.right = left, value, right

    def __iter__(self):
        if self.left is not None:
            yield from self.left
        yield self.value
        if self.right is not None:
            yield from self.right


prices = Tree(Tree(None, 10, None), 50, Tree(None, 300, None))
total = sum(prices)      # 360
dearest = max(prices)    # 300
has_herb = 10 in prices  # True
# ANCHOR_END: iter

old = Node(Node(None, 10, None), 50, Node(None, 300, None))
assert old.to_list() == [10, 50, 300]
assert (total, dearest, has_herb) == (360, 300, True)
assert list(prices) == [10, 50, 300]
print("ok")
