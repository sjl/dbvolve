CREATE TABLE todos (
  id INTEGER PRIMARY KEY,
  user_id INTEGER NOT NULL,
  done BOOLEAN NOT NULL,
  content TEXT,

  FOREIGN KEY (user_id) REFERENCES users(id)
);

INSERT INTO todos (id, user_id, content, done) VALUES (0, 0, 'Write DBvolve skeleton.', TRUE);
INSERT INTO todos (id, user_id, content, done) VALUES (1, 0, 'Write DBvolve test suite.', FALSE);

