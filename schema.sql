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
  id           serial     PRIMARY KEY,
  title        text,
  quest        integer    NOT NULL REFERENCES quests,
  ordinal      integer    NOT NULL,
  update_count integer    NOT NULL DEFAULT 0,
  created      timestamp with time zone  NOT NULL DEFAULT now(),
  UNIQUE (ordinal, quest)
);

CREATE TABLE updates (
  post integer PRIMARY KEY REFERENCES posts,
  chapter integer NOT NULL REFERENCES chapters,
  ordinal integer NOT NULL,
  suggestion_count integer NOT NULL DEFAULT 0,
  UNIQUE (ordinal, chapter)
);

CREATE TABLE suggestions (
  post integer PRIMARY KEY REFERENCES posts,
  update integer NOT NULL REFERENCES updates,
  ordinal integer NOT NULL,
  UNIQUE (ordinal, update)
);

CREATE TABLE posts (
  id        serial     PRIMARY KEY,
  author    integer    REFERENCES users,
  address   inet       NOT NULL,
  title     text,
  pen_name  text,
  body      text       NOT NULL,
  created   timestamp with time zone  NOT NULL DEFAULT now(),
  edited    timestamp with time zone
);

CREATE FUNCTION get_chapter_ordinal(quest integer) RETURNS integer AS $$
DECLARE
  ordinal integer;
BEGIN
  UPDATE quests SET chapter_count = chapter_count + 1 WHERE id = quest RETURNING chapter_count INTO ordinal;
  RETURN ordinal;
END;
$$ LANGUAGE plpgsql;


CREATE FUNCTION get_update_ordinal(chapter integer) RETURNS integer AS $$
DECLARE
  ordinal integer;
BEGIN
  UPDATE chapters SET update_count = update_count + 1 WHERE id = chapter RETURNING update_count INTO ordinal;
  RETURN ordinal;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION get_suggestion_ordinal(_update integer) RETURNS integer AS $$
DECLARE
  ordinal integer;
BEGIN
  UPDATE updates SET suggestion_count = suggestion_count + 1 WHERE post = _update RETURNING suggestion_count INTO ordinal;
  RETURN ordinal;
END;
$$ LANGUAGE plpgsql;
