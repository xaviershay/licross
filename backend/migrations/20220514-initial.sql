CREATE TABLE games (
  id varchar not null PRIMARY KEY,
  region varchar not null,
  version int not null,
  data jsonb not null
);

