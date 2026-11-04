# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
n, k = map(int, input().split())


def digit_sum(number):
    return sum(map(int, str(number)))


real_boxes = filter(lambda box: digit_sum(box) == k, range(1, n + 1))
print(sum(real_boxes))
# ANCHOR_END: functional
