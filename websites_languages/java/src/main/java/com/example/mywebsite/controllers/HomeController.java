package com.example.mywebsite.controllers;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class HomeController {

    /**
     * Handles the root ("/") URL request.
     *
     * @param model The model to pass data to the view.
     * @return The name of the Thymeleaf template to render.
     */
    @GetMapping("/")
    public String home(Model model) {
        model.addAttribute("title", "Home Page");
        model.addAttribute("message", "Welcome to My Website! Explore, Learn, and Enjoy.");
        return "home"; // Refers to home.html in the templates folder.
    }

    /**
     * Handles the "/about" URL request.
     *
     * @param model The model to pass data to the view.
     * @return The name of the Thymeleaf template to render.
     */
    @GetMapping("/about")
    public String about(Model model) {
        model.addAttribute("title", "About Us");
        model.addAttribute("message", "This is the About Page. Learn more about our journey.");
        return "about"; // Refers to about.html in the templates folder.
    }
}
