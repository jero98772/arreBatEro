// =============================================================================
// pbit_cell.v — Probabilistic Bit (p-bit) cell
//
// Emulates a thermodynamic p-bit:
//   s_i ∈ {-1, +1}  →  stored as 0/1
//
// Update rule (Gibbs):
//   I_i  = Σ_j (J_ij * s_j)  +  h_i        (local field)
//   σ_i  = sigmoid(I_i / T)                  (activation probability)
//   s_i  ~ Bernoulli(σ_i)                    (sample new state)
//
// Sigmoid is approximated with a 5-bit comparator table so it fits in LUTs.
// Temperature T is a global 4-bit parameter (1..15).
// =============================================================================
`timescale 1ns/1ps

module pbit_cell #(
    parameter ID   = 0,             // unique cell index
    parameter N    = 8,             // number of neighbours (degree)
    parameter WBITS = 8             // weight bit-width (signed)
)(
    input  wire                    clk,
    input  wire                    rst_n,
    input  wire                    update_en,       // Gibbs step trigger
    input  wire [31:0]             rnd,             // random word from PRNG
    input  wire [3:0]              temperature,     // global T (1=cold, 15=hot)
    input  wire signed [WBITS-1:0] h_bias,          // local bias h_i
    input  wire signed [WBITS-1:0] J_w [0:N-1],    // coupling weights J_ij
    input  wire                    s_in [0:N-1],    // neighbour states
    output reg                     s_out            // this p-bit state
);

    // -----------------------------------------------------------------
    // 1. Compute local field I = Σ J_ij*s_j + h
    // -----------------------------------------------------------------
    localparam ABITS = WBITS + $clog2(N+1) + 1;  // accumulator width

    integer k;
    reg signed [ABITS-1:0] field;

    always @(*) begin
        field = {{(ABITS-WBITS){h_bias[WBITS-1]}}, h_bias};
        for (k = 0; k < N; k = k+1) begin
            if (s_in[k])
                field = field + {{(ABITS-WBITS){J_w[k][WBITS-1]}}, J_w[k]};
            else
                field = field - {{(ABITS-WBITS){J_w[k][WBITS-1]}}, J_w[k]};
        end
    end

    // -----------------------------------------------------------------
    // 2. Scaled field:  F = I / T   (integer divide, clamp to [-15,+15])
    // -----------------------------------------------------------------
    reg signed [ABITS-1:0] scaled;
    reg signed [4:0]        clamped;   // 5-bit signed: -15..+15

    always @(*) begin
        if (temperature == 4'd0)
            scaled = field;               // T=0 → deterministic
        else
            scaled = $signed(field) / $signed({1'b0, temperature});

        // clamp to [-15, +15]
        if (scaled > 15)
            clamped = 5'sd15;
        else if (scaled < -15)
            clamped = -5'sd15;
        else
            clamped = scaled[4:0];
    end

    // -----------------------------------------------------------------
    // 3. Sigmoid LUT: P(s=1) = 1/(1+exp(-F))  →  8-bit threshold
    //    Precomputed for F = -15..+15  (31 entries)
    // -----------------------------------------------------------------
    reg [7:0] sigma;   // probability threshold in [0,255]

    always @(*) begin
        case (clamped)
            -15: sigma = 8'd1;
            -14: sigma = 8'd2;
            -13: sigma = 8'd3;
            -12: sigma = 8'd5;
            -11: sigma = 8'd8;
            -10: sigma = 8'd12;
             -9: sigma = 8'd18;
             -8: sigma = 8'd27;
             -7: sigma = 8'd40;
             -6: sigma = 8'd58;
             -5: sigma = 8'd83;
             -4: sigma = 8'd114;
             -3: sigma = 8'd144;
             -2: sigma = 8'd172;
             -1: sigma = 8'd197;
              0: sigma = 8'd128;   // 0.5
              1: sigma = 8'd153;
              2: sigma = 8'd181;
              3: sigma = 8'd203;
              4: sigma = 8'd223;
              5: sigma = 8'd234;
              6: sigma = 8'd242;
              7: sigma = 8'd247;
              8: sigma = 8'd251;
              9: sigma = 8'd253;
             10: sigma = 8'd254;
             11: sigma = 8'd255;
             12: sigma = 8'd255;
             13: sigma = 8'd255;
             14: sigma = 8'd255;
             15: sigma = 8'd255;
            default: sigma = 8'd128;
        endcase
    end

    // -----------------------------------------------------------------
    // 4. Bernoulli sample: s_new = (rnd[7:0] < sigma) ? 1 : 0
    // -----------------------------------------------------------------
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            s_out <= 1'b0;
        else if (update_en)
            s_out <= (rnd[7:0] < sigma) ? 1'b1 : 1'b0;
    end

endmodule
