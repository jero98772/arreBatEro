// Set up constants
@SCREEN
D=A
@R0
M=D  // Store SCREEN address in R0

// Infinite loop to read and display characters
(INFINITE_LOOP)
  // Read character from keyboard
  @KBD
  D=M  // Read character from keyboard into D

  // Display character on screen
  @R0
  A=M  // Load screen address
  M=D  // Display character on screen

  // Infinite loop to wait for another key press
  (WAIT_KEY_PRESS)
    @KBD
    D=M  // Read character from keyboard
  @WAIT_KEY_PRESS

// End of program
@INFINITE_LOOP
0;JMP  // Jump to infinite loop

