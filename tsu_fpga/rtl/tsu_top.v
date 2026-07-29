// =============================================================================
// tsu_top.v — Thermodynamic Sampling Unit, top-level
//
// Tang Nano 9K  (GW1NR-9, 8448 LUT4, 468Kb BSRAM, 27MHz OSC)
//
// Architecture:
//   ┌────────────────────────────────────────────────────────┐
//   │  8 p-bit cells  (pbit_cell × 8)                        │
//   │  8 LFSRs        (one per cell, unique seeds)           │
//   │  Gibbs scheduler (2-colour: even/odd)                  │
//   │  Ising weight ROM                                       │
//   │  Energy calculator                                      │
//   │  UART TX (115200 baud @ 27 MHz)  → stream spin states  │
//   │  RGB LED  → energy visualisation                        │
//   └────────────────────────────────────────────────────────┘
//
// Pin assignments (Tang Nano 9K defaults):
//   clk_27   → pin 52  (27 MHz crystal)
//   rst_n    → pin 4   (S1 button, active-low)
//   uart_tx  → pin 17  (USB-UART TX)
//   led_r    → pin 10
//   led_g    → pin 11
//   led_b    → pin 13
// =============================================================================
`timescale 1ns/1ps

module tsu_top (
    input  wire clk_27,     // 27 MHz system clock
    input  wire rst_n,      // active-low reset (button S1)
    output wire uart_tx,    // UART TX to host
    output wire led_r,      // RGB LED: red   (active low on Tang Nano)
    output wire led_g,      // RGB LED: green
    output wire led_b       // RGB LED: blue
);

    // =========================================================
    // Parameters
    // =========================================================
    localparam N      = 8;       // number of p-bits
    localparam WBITS  = 8;       // weight width
    localparam EBITS  = 16;      // energy accumulator width
    localparam COLORS = 2;       // graph coloring: even/odd
    localparam SETTLE = 4;       // pipeline settle cycles per Gibbs phase

    // Temperature: 1=very cold (ordered), 15=very hot (disordered)
    // Hard-coded here; could be driven from UART RX commands
    localparam [3:0] TEMPERATURE = 4'd6;

    // =========================================================
    // Clock & reset synchronisation
    // =========================================================
    reg [2:0] rst_sync;
    wire      rst_n_sync = rst_sync[2];

    always @(posedge clk_27 or negedge rst_n) begin
        if (!rst_n) rst_sync <= 3'b000;
        else        rst_sync <= {rst_sync[1:0], 1'b1};
    end

    // =========================================================
    // Gibbs scheduler
    // =========================================================
    wire                    phase_w;     // 0=even group, 1=odd group
    wire                    update_en_w;
    wire                    sweep_done_w;

    gibbs_scheduler #(
        .COLORS (COLORS),
        .SETTLE (SETTLE)
    ) u_sched (
        .clk        (clk_27),
        .rst_n      (rst_n_sync),
        .run        (1'b1),
        .phase      (phase_w),
        .update_en  (update_en_w),
        .sweep_done (sweep_done_w)
    );

    // =========================================================
    // p-bit state vector
    // =========================================================
    wire [N-1:0] s;          // spin states (live)
    wire [N-1:0] update_mask;// which cells update this phase

    // Even/odd graph coloring: cell i updates when phase == (i % 2)
    genvar ci;
    generate
        for (ci = 0; ci < N; ci = ci+1)
            assign update_mask[ci] = (phase_w == ci[0]);
    endgenerate

    // =========================================================
    // PRNGs — one 32-bit LFSR per cell (different seeds)
    // =========================================================
    wire [31:0] rnd [0:N-1];

    generate
        for (ci = 0; ci < N; ci = ci+1) begin : prng_inst
            lfsr_prng #(
                .SEED (32'hACE10001 + ci * 32'h12345678)
            ) u_prng (
                .clk   (clk_27),
                .rst_n (rst_n_sync),
                .en    (1'b1),
                .rnd   (rnd[ci])
            );
        end
    endgenerate

    // =========================================================
    // Coupling weights (static ROM values, uniform Ising ring)
    // =========================================================
    // J_ferromagnetic = +20, J_frustrated (bond 3-4) = -20
    localparam signed [WBITS-1:0] J_POS = 8'sd20;
    localparam signed [WBITS-1:0] J_NEG = -8'sd20;
    localparam signed [WBITS-1:0] H_ZERO = 8'sd0;

    // Build weight arrays per cell
    // Ring: each cell has exactly 2 neighbours (left, right)
    wire signed [WBITS-1:0] J_left  [0:N-1];
    wire signed [WBITS-1:0] J_right [0:N-1];
    wire signed [WBITS-1:0] h_bias  [0:N-1];

    generate
        for (ci = 0; ci < N; ci = ci+1) begin : weight_assign
            // Frustrated bond between spin 3 and spin 4
            if (ci == 3)
                assign J_right[ci] = J_NEG;
            else
                assign J_right[ci] = J_POS;

            assign J_left[ci] = J_right[(ci == 0) ? N-1 : ci-1];
            assign h_bias[ci]  = H_ZERO;
        end
    endgenerate

    // =========================================================
    // p-bit cells
    // =========================================================
    generate
        for (ci = 0; ci < N; ci = ci+1) begin : pbit_inst

            // Flatten 2-element neighbour arrays for pbit_cell
            wire signed [WBITS-1:0] Jw[0:1];
            assign Jw[0] = J_left[ci];
            assign Jw[1] = J_right[ci];

            wire s_nb[0:1];
            assign s_nb[0] = s[(ci == 0) ? N-1 : ci-1];   // left
            assign s_nb[1] = s[(ci+1) % N];                 // right

            pbit_cell #(
                .ID    (ci),
                .N     (2),
                .WBITS (WBITS)
            ) u_pbit (
                .clk         (clk_27),
                .rst_n       (rst_n_sync),
                .update_en   (update_en_w & update_mask[ci]),
                .rnd         (rnd[ci]),
                .temperature (TEMPERATURE),
                .h_bias      (h_bias[ci]),
                .J_w         (Jw),
                .s_in        (s_nb),
                .s_out       (s[ci])
            );
        end
    endgenerate

    // =========================================================
    // Energy calculator
    // =========================================================
    wire signed [EBITS-1:0] energy_w;
    wire                    energy_valid_w;

    energy_calc #(
        .N      (N),
        .WBITS  (WBITS),
        .EBITS  (EBITS)
    ) u_energy (
        .clk           (clk_27),
        .rst_n         (rst_n_sync),
        .calc_en       (sweep_done_w),
        .spin_vec      (s),
        .J_ring        (J_POS),
        .J_frustrated  (J_NEG),
        .energy        (energy_w),
        .energy_valid  (energy_valid_w)
    );

    // =========================================================
    // UART TX  — send one packet per sweep:
    //   [0xAA][spin_byte][E_hi][E_lo][0x55]  (5 bytes)
    // =========================================================
    uart_tx_framer #(
        .CLK_HZ   (27_000_000),
        .BAUD     (115_200),
        .N        (N),
        .EBITS    (EBITS)
    ) u_uart (
        .clk          (clk_27),
        .rst_n        (rst_n_sync),
        .send_en      (energy_valid_w),
        .spin_vec     (s),
        .energy       (energy_w),
        .uart_tx      (uart_tx)
    );

    // =========================================================
    // RGB LED: map energy sign and magnitude to colour
    //   Low energy (ordered)    → blue
    //   High energy (disordered) → red
    //   Zero / neutral          → green
    // =========================================================
    wire signed [EBITS-1:0] E_abs = energy_w[EBITS-1] ? -energy_w : energy_w;

    // Very rough threshold: N*J = 8*20=160, so max ring ~160
    localparam [EBITS-1:0] E_THR_LO = 16'd60;
    localparam [EBITS-1:0] E_THR_HI = 16'd120;

    reg r_r, r_g, r_b;
    always @(posedge clk_27) begin
        if (energy_valid_w) begin
            if (E_abs < E_THR_LO)      begin r_r<=0; r_g<=0; r_b<=1; end  // ordered   → blue
            else if (E_abs < E_THR_HI) begin r_r<=0; r_g<=1; r_b<=0; end  // mid       → green
            else                        begin r_r<=1; r_g<=0; r_b<=0; end  // disordered→ red
        end
    end

    // Tang Nano 9K LEDs are active-LOW
    assign led_r = ~r_r;
    assign led_g = ~r_g;
    assign led_b = ~r_b;

endmodule
