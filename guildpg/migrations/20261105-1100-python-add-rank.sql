-- Python's change: the rank of each adventurer.
-- Python さんの変更：冒険者のランク
ALTER TABLE adventurers ADD COLUMN rank TEXT NOT NULL DEFAULT 'bronze';
