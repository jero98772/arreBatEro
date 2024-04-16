// Set up constants
@SCREEN
D=A
@R0
M=D  // Store SCREEN address in R0

// Set up big plus sign
@BIG_PLUS
D=A
@R1
M=D  // Store BIG_PLUS address in R1

// Infinite loop to read and display characters
(INFINITE_LOOP)
  // Read character from keyboard
  @KBD
  D=M  // Read character from keyboard into D

  // Check if the character is '+'
  @BIG_PLUS_CHECK
  D=D-M  // Compare character with '+'
  @DISPLAY_BIG_PLUS
  D;JEQ  // If character is '+', jump to display big plus sign

  // Display normal character on screen
  @R0
  A=M  // Load screen address
  M=D  // Display character on screen

  // Infinite loop to wait for another key press
  (WAIT_KEY_PRESS)
    @KBD
    D=M  // Read character from keyboard
  @WAIT_KEY_PRESS

// Display big plus sign
(DISPLAY_BIG_PLUS)
  @R1
  A=M  // Load big plus sign address
  M=0  // Display big plus sign on screen
  @INFINITE_LOOP
  0;JMP  // Jump to infinite loop

// Big plus sign data
(BIG_PLUS)
  @0b11111100000100001000010000000000
  D=A
  @R1
  M=D  // Store big plus sign data in memory

// End of program
@INFINITE_LOOP
0;JMP  // Jump to infinite loop

