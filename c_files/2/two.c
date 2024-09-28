// C Program to use code another
// file into this file
#include "one.c"
#include <stdio.h>
 
int main()
{
    // declared two variables
    int a = 4, b = 5;
 
    // sum function called
    int ans = sum(a, b);
    printf("Sum: %d", ans);
 
    // sub function called
    ans = sub(a, b);
    printf("Subtraction: %d", ans);
 
    // multiply function called
    ans = multiply(a, b);
    printf("Multiply: %d", ans);
 
    return 0;
}