-- The two ways to keep my own attributes, for the chapter "Attributes
-- and states of my own". fields_sql.py runs these with SQLite.
-- 「自分で決める属性と状態」の章の、自分で決めた属性の2つのしまい方。
-- fields_sql.py が SQLite で動かす

-- ANCHOR: json
-- A JSON column: one object for each work.
-- JSON の列：作品ごとに、オブジェクトを1つ
SELECT title
  FROM works
 WHERE json_extract(attrs, '$.推し度') >= 3
   AND json_extract(attrs, '$.形') = 'Web'
 ORDER BY id;
-- ANCHOR_END: json

-- ANCHOR: eav
-- EAV: one row for each value (entity, attribute, value).
-- One join for each condition.
-- EAV：値ごとに1行（作品・属性・値）。条件ごとに、JOIN が1つ
SELECT w.title
  FROM works AS w
  JOIN work_attrs AS o ON o.work_id = w.id AND o.name = '推し度'
  JOIN work_attrs AS f ON f.work_id = w.id AND f.name = '形'
 WHERE CAST(o.value AS INTEGER) >= 3
   AND f.value = 'Web'
 ORDER BY w.id;
-- ANCHOR_END: eav

-- ANCHOR: index
-- A JSON column can have an index on one of its keys.
-- JSON の列にも、キーごとの索引を作れる
CREATE INDEX works_oshi ON works (json_extract(attrs, '$.推し度'));
-- ANCHOR_END: index

-- ANCHOR: rename
-- Renaming 推し度 to 推し: every JSON object is rewritten ...
-- 推し度を「推し」に改名する：JSON は、作品ごとのオブジェクトを全部書き直す……
UPDATE works
   SET attrs = json_set(json_remove(attrs, '$.推し度'),
                        '$.推し', json_extract(attrs, '$.推し度'))
 WHERE json_extract(attrs, '$.推し度') IS NOT NULL;

-- ... EAV changes the name in its rows.
-- ……EAV は、行の名前を変えるだけ
UPDATE work_attrs SET name = '推し' WHERE name = '推し度';
-- ANCHOR_END: rename
