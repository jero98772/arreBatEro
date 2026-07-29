// =============================================================================
// energy_calc.v — Ising energy E = -Σ J_ij s_i s_j - Σ h_i s_i
//
// Computed over all spin pairs (ring topology: each spin coupled left+right).
// Uses a pipeline adder tree for speed.  Result valid LATENCY clocks after
// sweep_done input.
//
// s[i] ∈ {0,1} → mapped to σ = 2*s-1 = {-1,+1} for energy calculation.
// =============================================================================
`timescale 1ns/1ps

module energy_calc #(
    parameter N      = 8,
    parameter WBITS  = 8,
    parameter EBITS  = 16          // energy accumulator width
)(
    input  wire                    clk,
    input  wire                    rst_n,
    input  wire                    calc_en,         // start calculation
    input  wire [N-1:0]            spin_vec,        // current spin states
    input  wire signed [WBITS-1:0] J_ring,          // uniform ring coupling
    input  wire signed [WBITS-1:0] J_frustrated,    // one frustrated bond
    output reg  signed [EBITS-1:0] energy,           // Ising energy
    output reg                     energy_valid      // pulses when energy ready
);

    // Map spin bits to ±1 using 2*s-1
    wire signed [1:0] sigma [0:N-1];
    genvar g;
    generate
        for (g = 0; g < N; g = g+1)
            assign sigma[g] = spin_vec[g] ? 2'sd1 : -2'sd1;
    endgenerate

    // Single-cycle adder (small N: combinational)
    reg signed [EBITS-1:0] acc;
    integer k;

    always @(*) begin
        acc = 0;
        for (k = 0; k < N; k = k+1) begin
            // ring bond: spin k ↔ spin (k+1)%N
            if (k == 3)      // frustrated bond
                acc = acc - $signed(J_frustrated) * $signed(sigma[k]) * $signed(sigma[(k+1) % N]);
            else
                acc = acc - $signed(J_ring) * $signed(sigma[k]) * $signed(sigma[(k+1) % N]);
        end
    end

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            energy       <= 0;
            energy_valid <= 1'b0;
        end else begin
            energy_valid <= 1'b0;
            if (calc_en) begin
                energy       <= acc;
                energy_valid <= 1'b1;
            end
        end
    end

endmodule
