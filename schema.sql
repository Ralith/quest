CREATE TABLE users (
  id          serial     PRIMARY KEY,
  name        text       UNIQUE NOT NULL,
  email       text       NOT NULL,
  password    char(60)   NOT NULL,
  created     timestamp with time zone  NOT NULL DEFAULT now()
);

CREATE TABLE quests (
  id             serial   PRIMARY KEY,
  title          text,
  author         integer  NOT NULL REFERENCES users,
  chapter_count  integer  NOT NULL DEFAULT 0,
  created        timestamp with time zone  NOT NULL DEFAULT now()
);

CREATE TABLE chapters (
  id          serial     PRIMARY KEY,
  title       text,
  quest       integer    NOT NULL REFERENCES quests,
  ordinal     integer    NOT NULL,
  post_count  integer    NOT NULL DEFAULT 0,
  created     timestamp with time zone  NOT NULL DEFAULT now(),
  UNIQUE (quest, ordinal)
);

CREATE TABLE posts (
  id        serial     PRIMARY KEY,
  chapter   integer    NOT NULL REFERENCES chapters,
  ordinal   integer    NOT NULL,
  author    integer    REFERENCES users,
  address   inet       NOT NULL,
  suggestion boolean   NOT NULL DEFAULT TRUE,
  title     text,
  pen_name  text,
  body      text       NOT NULL,
  created   timestamp with time zone  NOT NULL DEFAULT now(),
  edited    timestamp with time zone,
  UNIQUE (chapter, ordinal)
);

CREATE FUNCTION get_chapter_ordinal(quest integer) RETURNS integer AS $$
DECLARE
  ordinal integer;
BEGIN
  UPDATE quests SET chapter_count = chapter_count + 1 WHERE id = quest RETURNING chapter_count INTO ordinal;
  RETURN ordinal;
END;
$$ LANGUAGE plpgsql;


CREATE FUNCTION get_post_ordinal(chapter integer) RETURNS integer AS $$
DECLARE
  ordinal integer;
BEGIN
  UPDATE chapters SET post_count = post_count + 1 WHERE id = chapter RETURNING post_count INTO ordinal;
  RETURN ordinal;
END;
$$ LANGUAGE plpgsql;
