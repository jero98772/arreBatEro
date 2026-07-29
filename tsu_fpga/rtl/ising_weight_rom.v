// =============================================================================
// ising_weight_rom.v — Weight storage for a small Ising / Boltzmann machine
//
// Stores J (coupling) matrix and h (bias) vector.
// Parameterised for N spins with 2 neighbours each (1D ring).
// Weights are loaded at reset from INIT parameters (BRAM or registers).
//
// For Tang Nano 9K the weights fit in distributed LUT-RAM.
// A larger model should use BSRAM (use gowin_sp primitive).
// =============================================================================
`timescale 1ns/1ps

module ising_weight_rom #(
    parameter N      = 8,           // number of spins
    parameter WBITS  = 8            // weight width (signed)
)(
    input  wire                    clk,
    input  wire [$clog2(N)-1:0]   spin_idx,       // which spin to query
    output reg  signed [WBITS-1:0] h_out,          // bias for spin_idx
    output reg  signed [WBITS-1:0] Jl_out,         // J to left neighbour
    output reg  signed [WBITS-1:0] Jr_out          // J to right neighbour
);

    // ---------------------------------------------------------
    // Bias vector h[i]  (all zeros → unbiased Ising model)
    // ---------------------------------------------------------
    reg signed [WBITS-1:0] h_rom [0:N-1];
    reg signed [WBITS-1:0] J_rom [0:N-1];   // ferromagnetic: J>0 likes same state

    integer i;
    initial begin
        for (i = 0; i < N; i = i+1) begin
            h_rom[i] = 8'sd0;        // no bias
            J_rom[i] = 8'sd20;       // ferromagnetic coupling J=20 (strong)
        end
        // Example: frustrated bond between spin 3 and spin 4
        J_rom[3] = -8'sd20;
    end

    always @(posedge clk) begin
        h_out  <= h_rom[spin_idx];
        Jl_out <= J_rom[(spin_idx == 0) ? N-1 : spin_idx-1];  // left neighbour J
        Jr_out <= J_rom[spin_idx];                               // right neighbour J
    end

endmodule
