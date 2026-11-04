-- The roster of adventurers.
-- 冒険者の名簿
CREATE TABLE adventurers
  ( id    INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , name  TEXT    NOT NULL UNIQUE
  , job   TEXT    NOT NULL
  , level INTEGER NOT NULL
  );
