ALTER TABLE users ADD COLUMN email TEXT NOT NULL DEFAULT '';

UPDATE users SET email = 'steve@stevelosh.com' WHERE NAME = 'sjl';
