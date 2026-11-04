# The Python code in the scikit-learn chapter.
# scikit-learn の章の Python のコード
import math

sizes = [i / 5 for i in range(50)]
prices = [3 * s + 5 + math.sin(1.7 * i) for i, s in enumerate(sizes)]

# ANCHOR: procedural
# Python (procedural style: gradient descent with a loop)
# Python（手続き型スタイル：ループで勾配降下法）
w, b = 0.0, 0.0
lr = 0.01
n = len(sizes)
for epoch in range(2000):
    grad_w = 0.0
    grad_b = 0.0
    for x, y in zip(sizes, prices):
        err = (w * x + b) - y
        grad_w += err * x / n
        grad_b += err / n
    w -= lr * grad_w
    b -= lr * grad_b
print(w, b)          # about 3.0056, 4.9847
# ANCHOR_END: procedural

assert abs(w - 3.0055619234664337) < 1e-9
assert abs(b - 4.984694819027008) < 1e-9

# ANCHOR: sklearn
# Python (scikit-learn: fit, then predict)
# Python（scikit-learn：fit してから predict）
import numpy as np
from sklearn.linear_model import LinearRegression, LogisticRegression

X = np.array(sizes).reshape(50, 1)
y = np.array(prices)
model = LinearRegression().fit(X, y)
print(model.coef_, model.intercept_)       # about [3.0012] 5.0135
print(model.score(X, y))                   # R², about 0.99339
print(model.predict(np.array([[4.0], [10.0]])))

points = [(i % 10, i // 10, i) for i in range(100)]
Xm = np.array([[a, s] for a, s, _ in points], dtype=float)
ym = np.array([1 if a + s + 0.8 * math.sin(2.3 * i) > 9 else 0
               for a, s, i in points])
clf = LogisticRegression().fit(Xm, ym)
print(clf.score(Xm, ym))                   # accuracy
print(clf.predict_proba(np.array([[2., 3.], [8., 9.]]))[:, 1])
# ANCHOR_END: sklearn

assert abs(model.coef_[0] - 3.00116745) < 1e-6
assert abs(model.intercept_ - 5.01347254) < 1e-6
assert abs(model.score(X, y) - 0.99338608) < 1e-6
assert abs(clf.score(Xm, ym) - 0.96) < 1e-9
