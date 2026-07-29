// =============================================================================
// gibbs_scheduler.v — Block Gibbs sampling controller
//
// Divides N p-bits into COLOR groups (graph-coloring).
// Each group is updated in parallel; groups are cycled in sequence.
// This preserves detailed balance: no p-bit reads itself while being written.
//
// Timeline per full sweep:
//   phase 0: update all bits in color group 0   (clk 0..SETTLE)
//   phase 1: update all bits in color group 1   (clk SETTLE+1..2*SETTLE)
//   ...
//   phase COLOR-1: last group
//   → emit sweep_done pulse
//
// =============================================================================
`timescale 1ns/1ps

module gibbs_scheduler #(
    parameter COLORS  = 2,          // number of graph colour classes
    parameter SETTLE  = 4           // pipeline settle cycles per phase
)(
    input  wire                     clk,
    input  wire                     rst_n,
    input  wire                     run,            // high = keep sweeping
    output reg  [$clog2(COLORS)-1:0] phase,         // current colour group
    output reg                      update_en,      // pulse: update active group
    output reg                      sweep_done      // one pulse per full sweep
);

    localparam PBITS = $clog2(SETTLE+1);
    reg [PBITS-1:0] settle_cnt;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase      <= 0;
            settle_cnt <= 0;
            update_en  <= 0;
            sweep_done <= 0;
        end else begin
            update_en  <= 1'b0;
            sweep_done <= 1'b0;

            if (run) begin
                if (settle_cnt == 0) begin
                    update_en <= 1'b1;          // trigger update for this phase
                end

                if (settle_cnt == SETTLE-1) begin
                    settle_cnt <= 0;
                    if (phase == COLORS-1) begin
                        phase      <= 0;
                        sweep_done <= 1'b1;
                    end else begin
                        phase <= phase + 1;
                    end
                end else begin
                    settle_cnt <= settle_cnt + 1;
                end
            end
        end
    end

endmodule
