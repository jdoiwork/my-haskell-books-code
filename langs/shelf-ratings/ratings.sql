-- The ranking of the shelf in SQL (SQLite), for the chapter
-- "Gathering stars". ratings_sql.py makes the tables and runs these.
-- 「★を集める」の章の、本棚のランキングを SQL（SQLite）で。
-- 表を作って動かすのは ratings_sql.py

-- ANCHOR: avg
-- SQL (the plain mean: AVG, COUNT and GROUP BY)
-- SQL（ふつうの平均：AVG と COUNT と GROUP BY）
SELECT w.title,
       AVG(r.points) AS mean,
       COUNT(*)      AS votes
FROM works AS w
JOIN ratings AS r ON r.work_id = w.id
GROUP BY w.id
ORDER BY mean DESC, w.position;
-- ANCHOR_END: avg

-- ANCHOR: bayes
-- SQL (the Bayesian average: add 5 usual votes to each work)
-- SQL（ベイズ平均：各作品に、ふつうの点を5票足す）
WITH per_work AS (
  -- The votes and the sum of the points of each work.
  -- 作品ごとの票の数と点の合計
  SELECT work_id,
         COUNT(*)    AS votes,
         SUM(points) AS total
  FROM ratings
  GROUP BY work_id
),
usual AS (
  -- The mean of each work's mean (one work, one vote).
  -- 1.0 * makes it a real number, not an integer division.
  -- 作品ごとの平均の平均（1作品1票）。
  -- 1.0 * で、整数の割り算ではなく実数にする
  SELECT AVG(1.0 * total / votes) AS points
  FROM per_work
)
SELECT w.title,
       (5.0 * u.points + p.total) / (5 + p.votes) AS score
FROM works AS w
JOIN per_work AS p ON p.work_id = w.id
CROSS JOIN usual AS u
ORDER BY score DESC, w.position;
-- ANCHOR_END: bayes
