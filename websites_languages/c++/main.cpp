#include "crow.h"
#include "inja/inja.hpp"
#include <fstream>

std::string read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        return "File not found";
    }
    return {std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};
}

int main() {
    crow::SimpleApp app;

    // Dynamic HTML template
    CROW_ROUTE(app, "/profile/<string>")
    ([](const std::string& username) {
        inja::Environment env;
        std::string template_text = R"(
            <!DOCTYPE html>
            <html>
            <head><title>Profile</title></head>
            <body>
                <h1>Welcome, {{ username }}!</h1>
            </body>
            </html>
        )";

        nlohmann::json data;
        data["username"] = username;

        return env.render(template_text, data);
    });

    // Static files
    CROW_ROUTE(app, "/static/<string>")
    ([](const std::string& filename) {
        return crow::response(read_file("./static/" + filename));
    });

    // Form and input handling
    CROW_ROUTE(app, "/form")
    ([]() {
        return R"(
            <!DOCTYPE html>
            <html>
            <head><title>Form</title></head>
            <body>
                <form action="/submit" method="POST">
                    <label for="name">Name:</label>
                    <input type="text" id="name" name="name">
                    <button type="submit">Submit</button>
                </form>
            </body>
            </html>
        )";
    });

    CROW_ROUTE(app, "/submit").methods(crow::HTTPMethod::POST)([](const crow::request& req) {
        return "Received data: " + req.body;
    });

    app.port(8080).multithreaded().run();
    return 0;
}
