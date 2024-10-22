// Simple program to add two numbers and store the result
// The numbers are stored in RAM addresses 0 and 1
// The result will be stored in RAM address 2

@0          // Load the address 0
D=M         // D = RAM[0]
@1          // Load the address 1
D=D+M       // D = D + RAM[1]
@2          // Load the address 2
M=D         // RAM[2] = D (store the result)
@END        // Jump to END label
0;JMP

(END)       // END label
@END        // Infinite loop to halt the program
0;JMP

