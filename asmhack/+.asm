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

// Infinite loop to continuously check for input and display big plus sign
(INFINITE_LOOP)
  // Read character from keyboard
  @KBD
  D=M  // Read character from keyboard into D

  // Check if the character is '+'
  @BIG_PLUS_CHECK
  D=D-M  // Compare character with '+'
  @DISPLAY_BIG_PLUS
  D;JEQ  // If character is '+', jump to display big plus sign

  // Otherwise, display normal character on screen
  @R0
  A=M  // Load screen address
  M=D  // Display character on screen

  // Jump back to the beginning of the loop
  @INFINITE_LOOP
  0;JMP

// Display big plus sign
(DISPLAY_BIG_PLUS)
@16394
M=-1
@16395
M=-1
@16490
M=-1
@16491
M=-1
@16458
M=-1
@16459
M=-1
@16426
M=-1
@16427
M=-1
  @R1
  A=M  // Load big plus sign address
  M=0  // Display big plus sign on screen

  // Wait for key release before continuing
(WAIT_KEY_RELEASE)
  @KBD
  D=M  // Read character from keyboard
  @DISPLAY_BIG_PLUS
  D;JNE  // If character is not '+', jump back to display big plus sign
@16394
M=0
@16395
M=0
@16490
M=0
@16491
M=0
@16458
M=0
@16459
M=0
@16426
M=0
@16427
M=0
  @WAIT_KEY_RELEASE
  0;JMP

// Big plus sign data
(BIG_PLUS)
  @0b11111100000100001000010000000000
  D=A
  @R1
  M=D  // Store big plus sign data in memory

// End of program
@INFINITE_LOOP
0;JMP  // Jump to infinite loop
