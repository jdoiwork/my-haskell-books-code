% Softmax of each row in Octave: whole-matrix steps (broadcasting).
% 行ごとの softmax を Octave で：行列まるごとの計算（ブロードキャスト）
1;
full = strcmp(getenv('BOOK_BENCH'), '1');

% ANCHOR: vector
% Octave (vectors)
% Octave（ベクトル）
function p = softmax_rows(x)
  e = exp(x - max(x, [], 2));
  p = e ./ sum(e, 2);
end
% ANCHOR_END: vector

x = rand(5, 7);
p = softmax_rows(x);
assert(sum(p, 2), ones(5, 1), 1e-12);

if full
  n = 1000;
else
  n = 50;
end
x = rand(n);
softmax_rows(x);
times = zeros(10, 1);
for r = 1:10
  tic; s = sum(softmax_rows(x)(:)); times(r) = toc;
end
printf('%-8s n=%-5d %-14s %10.2f ms\n', 'softmax', n, 'Octave', median(times) * 1000);
