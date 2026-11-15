% The matrix product in Octave (instead of MATLAB): three loops, and * (BLAS).
% Octave runs in Docker, in a Linux VM: its times do not go in the same
% table as the others.
% 行列の積を Octave（MATLAB の代わり）で：3重ループと、*（BLAS）。
% Octave は Docker の中（Linux の VM）で動くので、時間はほかと同じ表に並べない
1;
full = strcmp(getenv('BOOK_BENCH'), '1');

% ANCHOR: naive
% Octave (procedural style): three loops
% Octave（手続き型スタイル）：3重ループ
function c = naive_matmul(a, b)
  [n, k] = size(a);
  m = columns(b);
  c = zeros(n, m);
  for i = 1:n
    for j = 1:m
      s = 0;
      for t = 1:k
        s = s + a(i, t) * b(t, j);
      end
      c(i, j) = s;
    end
  end
end
% ANCHOR_END: naive

function ms = median_ms(reps, f)
  f();
  times = zeros(reps, 1);
  for r = 1:reps
    tic; f(); times(r) = toc;
  end
  ms = median(times) * 1000;
end

function report(task, n, how, ms)
  printf('%-8s n=%-5d %-14s %10.2f ms\n', task, n, how, ms);
end

rand('seed', 1);
a = rand(20); b = rand(20);
assert(naive_matmul(a, b), a * b, 1e-12);

if full
  sizes = [128 256 512 1024];
else
  sizes = [16];
end
for n = sizes
  a = rand(n); b = rand(n);
  if n <= 128
    report('matmul', n, 'Octave naive', median_ms(3, @() sum(naive_matmul(a, b)(:))));
  end
  report('matmul', n, 'Octave *', median_ms(10, @() sum((a * b)(:))));
end
