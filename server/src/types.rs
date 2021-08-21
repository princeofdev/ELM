use google_signin;
use rocket::http::Status;
use rocket::request::{self, FromRequest, Request};
use rocket::response;
use rocket::response::Content;
use rocket::response::Stream;
use rocket::response::{NamedFile, Responder, Response};
use rocket::Outcome;
use std::io::Cursor;

const GOOGLE_CLIENT_ID: &str =
    "904165140417-upr6ca4hqgharv344ocq3dbrh7c3ns7k.apps.googleusercontent.com";
const ADMINS: &str = env!("ADMINS");

/// Returns sub identifier if `key` is a valid API key string.
fn is_valid(token: &str) -> Option<String> {
    let mut client = google_signin::Client::new();
    client.audiences.push(GOOGLE_CLIENT_ID.to_string()); // required
                                                         //client.hosted_domains.push(YOUR_HOSTED_DOMAIN);
    let id_info = client
        .get_slow_unverified(token)
        .expect("Expected token to exist");
    let mut admins = ADMINS.to_lowercase().to_string();
    admins.retain(|c| ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', ','].contains(&c));
    if id_info.verify(&client).is_ok()
        && admins
            .split(",")
            .into_iter()
            .any(|admin_sub| admin_sub == id_info.sub)
    {
        println!(
            "{}-(sub: {}) accessed admin only request",
            id_info.email.unwrap_or("No email provided".to_string()),
            id_info.sub
        );
        Some(id_info.sub)
    } else {
        println!(
            "{}-(sub: {}) failed to access admin only request",
            id_info.email.unwrap_or("No email provided".to_string()),
            id_info.sub
        );
        None
    }
}

#[derive(Debug)]
pub enum IdTokenError {
    Missing,
    Invalid,
}

#[derive(Debug)]
pub struct Admin(String);

impl<'a, 'r> FromRequest<'a, 'r> for Admin {
    type Error = IdTokenError;

    fn from_request(request: &'a Request<'r>) -> request::Outcome<Self, Self::Error> {
        if let Some(id_token) = request.headers().get_one("idToken") {
            if let Some(sub) = is_valid(id_token) {
                Outcome::Success(Admin(sub))
            } else {
                Outcome::Failure((Status::BadRequest, IdTokenError::Invalid))
            }
        } else {
            Outcome::Failure((Status::BadRequest, IdTokenError::Missing))
        }
    }
}

#[derive(Debug)]
pub struct FileName(pub String);

#[derive(Debug)]
pub enum FileNameError {
    MissingName,
}

impl<'a, 'r> FromRequest<'a, 'r> for FileName {
    type Error = FileNameError;

    fn from_request(request: &'a Request<'r>) -> request::Outcome<Self, Self::Error> {
        match request.headers().get_one("File-Name") {
            Some(name) => Outcome::Success(FileName(name.to_string())),
            None => Outcome::Failure((Status::BadRequest, FileNameError::MissingName)),
        }
    }
}

pub struct CachedFile(pub NamedFile, pub usize);

impl<'r> Responder<'r> for CachedFile {
    fn respond_to(self, req: &Request) -> response::Result<'r> {
        Response::build_from(self.0.respond_to(req)?)
            .raw_header("Cache-control", format!("max-age={}", self.1))
            .ok()
    }
}

pub struct CachedImage(pub Content<Stream<Cursor<Vec<u8>>>>, pub usize);

impl<'r> Responder<'r> for CachedImage {
    fn respond_to(self, req: &Request) -> response::Result<'r> {
        Response::build_from(self.0.respond_to(req)?)
            .raw_header("Cache-control", format!("max-age={}", self.1))
            .ok()
    }
}
