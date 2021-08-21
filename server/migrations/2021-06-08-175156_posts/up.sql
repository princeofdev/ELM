CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    images TEXT[] NOT NULL,
    content TEXT NOT NULL,
    posttime TIMESTAMPTZ NOT NULL
)