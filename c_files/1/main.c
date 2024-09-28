// main.c
#include <stdio.h>
#include "rectangle.h"

int main() {
    double width, height;

    // Prompt user for input
    printf("Enter the width of the rectangle: ");
    scanf("%lf", &width);
    printf("Enter the height of the rectangle: ");
    scanf("%lf", &height);

    // Calculate the area
    double area = calculate_area(width, height);

    // Display the result
    printf("The area of the rectangle is: %.2f\n", area);

    return 0;
}
