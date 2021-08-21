-- Your SQL goes here
CREATE TABLE images (
    imageName TEXT NOT NULL PRIMARY KEY,
    postat TIMESTAMPTZ NOT NULL,
    main BYTEA NOT NULL,
    thumbnail BYTEA NOT NULL
)