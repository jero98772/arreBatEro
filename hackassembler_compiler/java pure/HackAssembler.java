import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

public class HackAssembler {
    
    // Symbol table for labels and variables
    private Map<String, Integer> symbolTable;
    
    // Next available RAM address for variables
    private int nextVariableAddress = 16;
    
    // Predefined symbols
    private static final Map<String, Integer> PREDEFINED_SYMBOLS = new HashMap<>();
    static {
        // Virtual registers
        PREDEFINED_SYMBOLS.put("R0", 0);
        PREDEFINED_SYMBOLS.put("R1", 1);
        PREDEFINED_SYMBOLS.put("R2", 2);
        PREDEFINED_SYMBOLS.put("R3", 3);
        PREDEFINED_SYMBOLS.put("R4", 4);
        PREDEFINED_SYMBOLS.put("R5", 5);
        PREDEFINED_SYMBOLS.put("R6", 6);
        PREDEFINED_SYMBOLS.put("R7", 7);
        PREDEFINED_SYMBOLS.put("R8", 8);
        PREDEFINED_SYMBOLS.put("R9", 9);
        PREDEFINED_SYMBOLS.put("R10", 10);
        PREDEFINED_SYMBOLS.put("R11", 11);
        PREDEFINED_SYMBOLS.put("R12", 12);
        PREDEFINED_SYMBOLS.put("R13", 13);
        PREDEFINED_SYMBOLS.put("R14", 14);
        PREDEFINED_SYMBOLS.put("R15", 15);
        
        // I/O pointers
        PREDEFINED_SYMBOLS.put("SP", 0);     // Stack pointer
        PREDEFINED_SYMBOLS.put("LCL", 1);    // Local
        PREDEFINED_SYMBOLS.put("ARG", 2);    // Argument
        PREDEFINED_SYMBOLS.put("THIS", 3);   // This
        PREDEFINED_SYMBOLS.put("THAT", 4);   // That
        
        // Screen and keyboard
        PREDEFINED_SYMBOLS.put("SCREEN", 16384);
        PREDEFINED_SYMBOLS.put("KBD", 24576);
    }
    
    // Computation codes
    private static final Map<String, String> COMP_CODES = new HashMap<>();
    static {
        COMP_CODES.put("0", "0101010");
        COMP_CODES.put("1", "0111111");
        COMP_CODES.put("-1", "0111010");
        COMP_CODES.put("D", "0001100");
        COMP_CODES.put("A", "0110000");
        COMP_CODES.put("!D", "0001101");
        COMP_CODES.put("!A", "0110001");
        COMP_CODES.put("-D", "0001111");
        COMP_CODES.put("-A", "0110011");
        COMP_CODES.put("D+1", "0011111");
        COMP_CODES.put("A+1", "0110111");
        COMP_CODES.put("D-1", "0001110");
        COMP_CODES.put("A-1", "0110010");
        COMP_CODES.put("D+A", "0000010");
        COMP_CODES.put("D-A", "0010011");
        COMP_CODES.put("A-D", "0000111");
        COMP_CODES.put("D&A", "0000000");
        COMP_CODES.put("D|A", "0010101");
        
        COMP_CODES.put("M", "1110000");
        COMP_CODES.put("!M", "1110001");
        COMP_CODES.put("-M", "1110011");
        COMP_CODES.put("M+1", "1110111");
        COMP_CODES.put("M-1", "1110010");
        COMP_CODES.put("D+M", "1000010");
        COMP_CODES.put("D-M", "1010011");
        COMP_CODES.put("M-D", "1000111");
        COMP_CODES.put("D&M", "1000000");
        COMP_CODES.put("D|M", "1010101");
    }
    
    // Destination codes
    private static final Map<String, String> DEST_CODES = new HashMap<>();
    static {
        DEST_CODES.put("", "000");
        DEST_CODES.put("M", "001");
        DEST_CODES.put("D", "010");
        DEST_CODES.put("MD", "011");
        DEST_CODES.put("A", "100");
        DEST_CODES.put("AM", "101");
        DEST_CODES.put("AD", "110");
        DEST_CODES.put("AMD", "111");
    }
    
    // Jump codes
    private static final Map<String, String> JUMP_CODES = new HashMap<>();
    static {
        JUMP_CODES.put("", "000");
        JUMP_CODES.put("JGT", "001");
        JUMP_CODES.put("JEQ", "010");
        JUMP_CODES.put("JGE", "011");
        JUMP_CODES.put("JLT", "100");
        JUMP_CODES.put("JNE", "101");
        JUMP_CODES.put("JLE", "110");
        JUMP_CODES.put("JMP", "111");
    }
    
    public HackAssembler() {
        symbolTable = new HashMap<>(PREDEFINED_SYMBOLS);
    }
    
    public String assemble(String assemblyCode) {
        List<String> lines = Arrays.asList(assemblyCode.split("\n"));
        
        // First pass: collect labels
        firstPass(lines);
        
        // Second pass: generate machine code
        return secondPass(lines);
    }
    
    private void firstPass(List<String> lines) {
        int instructionAddress = 0;
        
        for (String line : lines) {
            line = cleanLine(line);
            if (line.isEmpty()) continue;
            
            if (isLabel(line)) {
                String label = line.substring(1, line.length() - 1); // Remove parentheses
                symbolTable.put(label, instructionAddress);
            } else {
                instructionAddress++;
            }
        }
    }
    
    private String secondPass(List<String> lines) {
        StringBuilder result = new StringBuilder();
        
        for (String line : lines) {
            line = cleanLine(line);
            if (line.isEmpty() || isLabel(line)) continue;
            
            String binaryInstruction = translateInstruction(line);
            result.append(binaryInstruction).append("\n");
        }
        
        return result.toString().trim();
    }
    
    private String cleanLine(String line) {
        // Remove comments
        int commentIndex = line.indexOf("//");
        if (commentIndex != -1) {
            line = line.substring(0, commentIndex);
        }
        
        // Remove whitespace
        return line.trim();
    }
    
    private boolean isLabel(String line) {
        return line.startsWith("(") && line.endsWith(")");
    }
    
    private String translateInstruction(String instruction) {
        if (instruction.startsWith("@")) {
            return translateAInstruction(instruction);
        } else {
            return translateCInstruction(instruction);
        }
    }
    
    private String translateAInstruction(String instruction) {
        String symbol = instruction.substring(1); // Remove @
        int address;
        
        if (isNumeric(symbol)) {
            address = Integer.parseInt(symbol);
        } else {
            if (symbolTable.containsKey(symbol)) {
                address = symbolTable.get(symbol);
            } else {
                // New variable
                address = nextVariableAddress++;
                symbolTable.put(symbol, address);
            }
        }
        
        return "0" + String.format("%15s", Integer.toBinaryString(address)).replace(' ', '0');
    }
    
    private String translateCInstruction(String instruction) {
        String dest = "";
        String comp = "";
        String jump = "";
        
        // Parse destination
        if (instruction.contains("=")) {
            String[] parts = instruction.split("=");
            dest = parts[0];
            instruction = parts[1];
        }
        
        // Parse jump
        if (instruction.contains(";")) {
            String[] parts = instruction.split(";");
            comp = parts[0];
            jump = parts[1];
        } else {
            comp = instruction;
        }
        
        // Generate binary code
        String destCode = DEST_CODES.get(dest);
        String compCode = COMP_CODES.get(comp);
        String jumpCode = JUMP_CODES.get(jump);
        
        if (destCode == null || compCode == null || jumpCode == null) {
            throw new IllegalArgumentException("Invalid C-instruction: " + instruction);
        }
        
        return "111" + compCode + destCode + jumpCode;
    }
    
    private boolean isNumeric(String str) {
        try {
            Integer.parseInt(str);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
    
    // Utility method to assemble from file
    public void assembleFile(String inputFile, String outputFile) throws IOException {
        StringBuilder assemblyCode = new StringBuilder();
        
        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
            String line;
            while ((line = reader.readLine()) != null) {
                assemblyCode.append(line).append("\n");
            }
        }
        
        String binaryCode = assemble(assemblyCode.toString());
        
        try (PrintWriter writer = new PrintWriter(new FileWriter(outputFile))) {
            writer.print(binaryCode);
        }
    }
    
    // Main method for testing
    public static void main(String[] args) {
        HackAssembler assembler = new HackAssembler();
        
        if (args.length == 2) {
            // Command line usage: java HackAssembler input.asm output.hack
            try {
                assembler.assembleFile(args[0], args[1]);
                System.out.println("Successfully assembled " + args[0] + " -> " + args[1]);
            } catch (IOException e) {
                System.err.println("Error processing files: " + e.getMessage());
            }
        } else if (args.length == 1) {
            // Single file input: java HackAssembler input.asm (outputs to input.hack)
            try {
                String inputFile = args[0];
                String outputFile = inputFile.replace(".asm", ".hack");
                assembler.assembleFile(inputFile, outputFile);
                System.out.println("Successfully assembled " + inputFile + " -> " + outputFile);
            } catch (IOException e) {
                System.err.println("Error processing file: " + e.getMessage());
            }
        } else {
            // Demo mode - run built-in example
            String testCode = """
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
                0;JMP
                """;
            
            try {
                String binaryCode = assembler.assemble(testCode);
                System.out.println("=== DEMO MODE ===");
                System.out.println("Assembly Code:");
                System.out.println(testCode);
                System.out.println("\nBinary Code:");
                System.out.println(binaryCode);
                System.out.println("\n=== USAGE ===");
                System.out.println("To assemble your own files:");
                System.out.println("java HackAssembler yourfile.asm");
                System.out.println("java HackAssembler input.asm output.hack");
                
            } catch (Exception e) {
                System.err.println("Error: " + e.getMessage());
                e.printStackTrace();
            }
        }
    }
}