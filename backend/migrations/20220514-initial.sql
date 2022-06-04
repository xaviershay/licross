CREATE TABLE games (
  id varchar not null PRIMARY KEY,
  created_at timestamp with time zone not null default NOW(),
  updated_at timestamp with time zone not null default NOW(),
  region varchar not null,
  version int not null,
  data jsonb not null
);

