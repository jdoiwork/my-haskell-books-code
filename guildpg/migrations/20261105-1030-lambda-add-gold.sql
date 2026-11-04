-- Lambda's change: how much gold each adventurer has.
-- Lambda さんの変更：冒険者の所持金
ALTER TABLE adventurers ADD COLUMN gold INTEGER NOT NULL DEFAULT 0;
