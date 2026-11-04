-- Lambda's change: new adventurers start with 10 gold.
-- Lambda さんの変更：新しい冒険者は、所持金10から始める
ALTER TABLE adventurers ALTER COLUMN gold SET DEFAULT 10;
