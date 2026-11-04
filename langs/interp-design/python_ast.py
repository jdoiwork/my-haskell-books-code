# The Python code in the language design chapter.
# 言語を設計する章の Python のコード

# ANCHOR: ast
# Python (Python's own AST, with the ast module)
# Python（ast モジュールで、Python 自身の AST を見る）
import ast

tree = ast.parse("1 + 2 * x", mode="eval")
print(ast.dump(tree.body))
# ANCHOR_END: ast

assert isinstance(tree.body, ast.BinOp)
assert isinstance(tree.body.op, ast.Add)
assert isinstance(tree.body.right, ast.BinOp)
