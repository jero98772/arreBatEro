// =============================================================================
// lfsr_prng.v — 32-bit Galois LFSR pseudo-random number generator
// Polynomial: x^32 + x^22 + x^2 + x^1 + 1
// Tang Nano 9K — TSU project
// =============================================================================
`timescale 1ns/1ps

module lfsr_prng #(
    parameter SEED = 32'hACE1_0001   // unique seed per instance
)(
    input  wire        clk,
    input  wire        rst_n,
    input  wire        en,           // advance LFSR this cycle
    output wire [31:0] rnd           // random word output
);

    reg [31:0] state;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            state <= SEED;
        else if (en)
            // Galois LFSR: tap bits 31,21,1,0
            state <= {1'b0, state[31:1]} ^
                     (state[0] ? 32'hB400_0000 : 32'h0);
    end

    assign rnd = state;

endmodule
