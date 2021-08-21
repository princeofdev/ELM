#![feature(proc_macro_hygiene, decl_macro)]
use self::diesel::prelude::*;

#[macro_use]
extern crate rocket;
use rocket::response::NamedFile;
use std::path::{Path, PathBuf};
extern crate chrono;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate rocket_contrib;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
use rocket_contrib::json::Json;
pub mod cors;
pub mod models;
pub mod schema;
pub mod types;
use chrono::NaiveDateTime;
use diesel::insert_into;
use image::ImageFormat;
use models::{Image, Post};
use rocket::http::ContentType;
use rocket::http::Status;
use rocket::response::Content;
use rocket::response::Stream;
use rocket::Data;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::io::Cursor;
use std::time::{SystemTime, UNIX_EPOCH};
use types::{Admin, CachedFile, CachedImage, FileName};
extern crate base64;

#[database("newsroom")]
pub struct DbConn(diesel::PgConnection);

#[get("/")]
fn index() -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/index.html"))
        .ok()
        .map(|n| CachedFile(n, 0))
}

#[get("/<file..>", rank = 10)]
fn files(file: PathBuf) -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/").join(file))
        .ok()
        .map(|n| CachedFile(n, 31536000)) //  1 year (24*60*60*365)
        .or_else(|| {
            NamedFile::open(Path::new("server/public/index.html"))
                .ok()
                .map(|n| CachedFile(n, 0))
        })
}

#[post("/newsroom/posts?<i>&<range>")]
pub fn posts(conn: DbConn, i: i64, range: i64) -> Result<Json<Vec<Post>>, String> {
    use crate::schema::posts::dsl::*;

    posts
        .order((posttime.desc(), id.desc()))
        .offset(i)
        .limit(range.min(3))
        .load(&conn.0)
        .map_err(|_err| -> String { "Error querying page views from the database".into() })
        .map(Json)
}

#[post("/newsroom/posts?<linked_post>", rank = 2)]
pub fn linked_post(conn: DbConn, linked_post: i32) -> Result<Json<Vec<Post>>, String> {
    use crate::schema::posts::dsl::*;
    posts
        .filter(id.eq(linked_post))
        .load(&conn.0)
        .map_err(|_err| -> String { "Error querying page views from the database".into() })
        .map(Json)
}

#[post("/newsroom/getimages")]
pub fn get_images(conn: DbConn, _admin: Admin) -> Result<Json<Vec<String>>, String> {
    use crate::schema::images::dsl::*;
    match images.order(postat.desc()).load::<Image>(&conn.0) {
        Ok(imgs) => Ok(Json(
            imgs.into_iter()
                .map(|img| img.imagename)
                .collect::<Vec<String>>(),
        )),
        Err(_) => Err("Invalid request".to_string()),
    }
}

#[post("/newsroom/upload/image", data = "<upload>")]
pub fn upload_image(
    conn: DbConn,
    _admin: Admin,
    upload: Data,
    file_name: FileName,
) -> Result<Json<Vec<String>>, String> {
    use crate::schema::images::dsl::*;
    let mut vec = Vec::new();
    upload.stream_to(&mut vec).unwrap();
    let new_image = image::load_from_memory(&vec).unwrap();
    let mut thumbnail_data = Vec::new();
    let mut hasher = DefaultHasher::new();
    new_image.hash(&mut hasher);
    let mut hash_v = base64::encode(hasher.finish().to_le_bytes());
    hash_v.truncate(11);
    let new_name = format!("{}?v={}", file_name.0, hash_v);
    new_image
        .thumbnail(100, 100)
        .write_to(
            &mut thumbnail_data,
            if let Ok(format) = ImageFormat::from_path(&file_name.0) {
                image::ImageOutputFormat::from(format)
            } else {
                image::ImageOutputFormat::Unsupported("Invalid image in database".to_string())
            },
        )
        .expect("");
    insert_into(images)
        .values((
            imagename.eq(new_name),
            postat.eq(NaiveDateTime::from_timestamp(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs() as i64,
                0,
            )),
            main.eq(vec),
            thumbnail.eq(thumbnail_data),
        ))
        .execute(&conn.0)
        .unwrap();
    match images.order(postat.desc()).load::<Image>(&conn.0) {
        Ok(imgs) => Ok(Json(
            imgs.into_iter()
                .map(|img| img.imagename)
                .collect::<Vec<String>>(),
        )),
        Err(_) => Err("Invalid request".to_string()),
    }
}

#[post("/newsroom/upload/post", data = "<new_post>", rank = 2)]
pub fn upload_post(conn: DbConn, _admin: Admin, new_post: Json<Post>) -> Status {
    use crate::schema::posts::dsl::*;
    if new_post.id < 0 {
        println!(
            "{} {} {:?} {} {}",
            new_post.id, new_post.title, new_post.images, new_post.content, new_post.posttime
        );
        insert_into(posts)
            .values((
                title.eq(&new_post.title),
                images.eq(&new_post.images),
                content.eq(&new_post.content),
                posttime.eq(new_post.posttime),
            ))
            .execute(&conn.0)
            .unwrap();
    } else {
        let target = posts.filter(id.eq(new_post.id));
        diesel::update(target)
            .set((
                title.eq(&new_post.title),
                images.eq(&new_post.images),
                content.eq(&new_post.content),
                posttime.eq(&new_post.posttime),
            ))
            .execute(&conn.0)
            .unwrap();
    }
    Status::Accepted
}

#[post("/newsroom/delete/post?<post_id>")]
pub fn delete_post(conn: DbConn, _admin: Admin, post_id: i32) -> Status {
    use crate::schema::posts::dsl::*;
    match diesel::delete(posts.filter(id.eq(post_id))).execute(&conn.0) {
        Ok(_) => Status::Accepted,
        Err(_) => Status::BadRequest,
    }
}

#[get("/newsroom/thumbnail/<image>", rank = 2)]
pub fn thumbnails(conn: DbConn, image: String) -> CachedImage {
    use crate::schema::images::dsl::*;
    let buffer: Vec<u8> = Vec::new();
    let mut cursor = Cursor::new(buffer);

    match images.filter(imagename.eq(image)).load::<Image>(&conn.0) {
        Ok(imgs) => {
            match imgs.first() {
                Some(_) => {
                    image::load_from_memory(
                        &imgs[0].thumbnail, // either this is incorrect or my encoding into the DB is incorrect
                    )
                    .unwrap()
                    .write_to(
                        &mut cursor,
                        if let Ok(format) = ImageFormat::from_path(&imgs[0].imagename) {
                            image::ImageOutputFormat::from(format)
                        } else {
                            image::ImageOutputFormat::Unsupported(
                                "Invalid image in database".to_string(),
                            )
                        },
                    )
                    .expect("");
                }
                None => (),
            }
            cursor.set_position(0);
            CachedImage(Content(ContentType::Binary, Stream::from(cursor)), 31536000)
        }
        Err(_) => {
            cursor.set_position(0);
            CachedImage(Content(ContentType::Binary, Stream::from(cursor)), 0)
        }
    }
}
#[get("/newsroom/images/<image>", rank = 1)]
pub fn images(conn: DbConn, image: String) -> CachedImage {
    use crate::schema::images::dsl::*;
    let buffer: Vec<u8> = Vec::new();
    let mut cursor = Cursor::new(buffer);

    match images.filter(imagename.eq(image)).load::<Image>(&conn.0) {
        Ok(imgs) => {
            match imgs.first() {
                Some(_) => {
                    image::load_from_memory(&imgs[0].main)
                        .unwrap()
                        .write_to(
                            &mut cursor,
                            if let Ok(format) = ImageFormat::from_path(&imgs[0].imagename) {
                                image::ImageOutputFormat::from(format)
                            } else {
                                image::ImageOutputFormat::Unsupported(
                                    "Invalid image in database".to_string(),
                                )
                            },
                        )
                        .expect("");
                }
                None => (),
            }
            cursor.set_position(0);
            CachedImage(Content(ContentType::Binary, Stream::from(cursor)), 31536000)
        }
        Err(_) => {
            cursor.set_position(0);
            CachedImage(Content(ContentType::Binary, Stream::from(cursor)), 0)
        }
    }
}

fn main() {
    rocket::ignite()
        .mount(
            "/",
            routes![
                files,
                index,
                posts,
                images,
                thumbnails,
                get_images,
                upload_image,
                upload_post,
                delete_post,
                linked_post,
            ],
        )
        .attach(DbConn::fairing())
        .attach(cors::CorsFairing)
        .launch();
}
