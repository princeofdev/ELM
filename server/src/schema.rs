table! {
    images (imagename) {
        imagename -> Text,
        postat -> Timestamptz,
        main -> Bytea,
        thumbnail -> Bytea,
    }
}

table! {
    posts (id) {
        id -> Int4,
        title -> Text,
        images -> Array<Text>,
        content -> Text,
        posttime -> Timestamptz,
    }
}

allow_tables_to_appear_in_same_query!(images, posts,);
