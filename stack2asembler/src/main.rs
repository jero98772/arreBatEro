#[macro_use] extern crate rocket;
use std::fs;
use std::path::Path;
use rocket::fs::{FileServer, relative};
use rocket_dyn_templates::{Template, context};
use rocket::serde::{json::Json, Deserialize, Serialize};
use std::io::Write;

mod vm_translator;
use vm_translator::{CodeWriter, process_file};

#[derive(Deserialize)]
struct VmCodeInput {
    vm_code: String,
}

#[derive(Serialize)]
struct ConvertResponse {
    message: String,
    output_file: String,
}

#[get("/")]
fn index() -> Template {
    Template::render("index", context! {})
}

#[post("/convert", data = "<input>")]
async fn convert(input: Json<VmCodeInput>) -> Result<Json<ConvertResponse>, String> {
    let vm_code = &input.vm_code;
    
    // Print the frontend value to the backend console
    println!("Received VM code from frontend:\n{}", vm_code);
    
    // Create a temporary file with the VM code
    let input_file_path = "temp_input.vm";
    let output_file_path = "output.asm";
    
    // Write VM code to a text file
    match fs::write(input_file_path, vm_code) {
        Ok(_) => println!("Successfully wrote VM code to {}", input_file_path),
        Err(e) => {
            let error_msg = format!("Failed to write VM code to file: {}", e);
            eprintln!("{}", error_msg);
            return Err(error_msg);
        }
    }
    
    // Create CodeWriter instance
    let output_file = match fs::File::create(output_file_path) {
        Ok(file) => file,
        Err(e) => {
            let error_msg = format!("Failed to create output file: {}", e);
            eprintln!("{}", error_msg);
            return Err(error_msg);
        }
    };
    
    let mut code_writer = CodeWriter::new(Box::new(output_file));
    
    // Process the file
    let input_path = Path::new(input_file_path);
    match process_file(input_path, &mut code_writer) {
        Ok(_) => {
            println!("Successfully processed VM file and generated assembly");
            
            // Read the generated assembly code
            let assembly_output = match fs::read_to_string(output_file_path) {
                Ok(content) => content,
                Err(e) => {
                    let error_msg = format!("Failed to read output file: {}", e);
                    eprintln!("{}", error_msg);
                    return Err(error_msg);
                }
            };
            
            // Clean up temporary input file
            if let Err(e) = fs::remove_file(input_file_path) {
                eprintln!("Warning: Failed to remove temporary file {}: {}", input_file_path, e);
            }
            
            Ok(Json(ConvertResponse {
                message: "VM code successfully converted to assembly".to_string(),
                output_file: assembly_output,
            }))
        }
        Err(e) => {
            let error_msg = format!("Failed to process VM file: {}", e);
            eprintln!("{}", error_msg);
            
            // Clean up temporary input file on error
            if let Err(cleanup_err) = fs::remove_file(input_file_path) {
                eprintln!("Warning: Failed to remove temporary file {}: {}", input_file_path, cleanup_err);
            }
            
            Err(error_msg)
        }
    }
}

#[launch]
fn rocket() -> _ {
    rocket::build()
        .mount("/", routes![index, convert]) // Mount both index and convert routes
        .mount("/static", FileServer::from(relative!("static")))
        .attach(Template::fairing())
}