#[macro_use] extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    "Welcome to my website!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index])
}