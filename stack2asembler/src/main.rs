#[macro_use] extern crate rocket;

use rocket::fs::{FileServer, relative};
use rocket_dyn_templates::{Template, context};
use rocket::serde::{json::Json, Deserialize, Serialize};
use std::io::Write; // Required for file writing

mod vm_translator; // Assuming vm_translator contains your translation logic

// Define a struct for the incoming JSON data from the frontend
#[derive(Debug, Deserialize, Serialize)]
#[serde(crate = "rocket::serde")]
struct VmCodeInput {
    vm_code: String,
}

// The root route simply serves the main HTML page.
#[get("/")]
fn index() -> Template {
    Template::render("index", context! {})
}

#[post("/convert", data = "<input>")]
async fn convert(input: Json<VmCodeInput>) -> String {
    let vm_code = &input.vm_code; // Get the VM code from the JSON input

    // Print the frontend value to the backend console as requested
    println!("Received VM code from frontend:\n{}", vm_code);

    // Return "Hello, world!" to the frontend as requested
    String::from("Hello, world!")
}

#[launch]
fn rocket() -> _ {
    rocket::build()
        .mount("/", routes![index, convert]) // Mount both index and convert routes
        .mount("/static", FileServer::from(relative!("static")))
        .attach(Template::fairing())
}