import jpype
import jpype.imports
import sys
import os



def main():
    jpype.startJVM(classpath=['.'])
    #assembleFile
    Hackasembler = jpype.JClass('HackAssembler')
    assembler = Hackasembler()

    test_code = """\
    // Simple program to add two numbers
    @2
    D=A
    @3
    D=D+A
    @0
    M=D

    // Loop example
    @i
    M=1
    (LOOP)
    @i
    D=M
    @100
    D=D-A
    @END
    D;JGT
    @i
    M=M+1
    @LOOP
    0;JMP
    (END)
    @END
    0;JMP"""


    binary_code = assembler.assemble(test_code)
    print("=== DEMO MODE ===")
    print("Assembly Code:")
    print(test_code)
    print("\nBinary Code:")
    print(binary_code)
    print("\n=== USAGE ===")
    print("To assemble your own files:")
    print("python hack_assembler.py yourfile.asm")
    print("python hack_assembler.py input.asm output.hack")

if __name__ == "__main__":
    main()
