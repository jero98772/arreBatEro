// =============================================================================
// uart_tx_framer.v — UART 8N1 transmitter with packet framing
//
// Sends one 5-byte packet per sweep:
//   Byte 0: 0xAA  (start-of-frame)
//   Byte 1: spin_vec[7:0]
//   Byte 2: energy[15:8]
//   Byte 3: energy[7:0]
//   Byte 4: 0x55  (end-of-frame)
//
// If send_en arrives while a packet is in-flight it is silently dropped
// (rate-limiting: Gibbs sweeps << UART baud).
// =============================================================================
`timescale 1ns/1ps

module uart_tx_framer #(
    parameter CLK_HZ = 27_000_000,
    parameter BAUD   = 115_200,
    parameter N      = 8,
    parameter EBITS  = 16
)(
    input  wire              clk,
    input  wire              rst_n,
    input  wire              send_en,
    input  wire [N-1:0]      spin_vec,
    input  wire signed [EBITS-1:0] energy,
    output wire              uart_tx
);

    // Baud divider
    localparam DIV = CLK_HZ / BAUD;   // 234 for 27MHz/115200

    // State machine
    localparam IDLE    = 3'd0;
    localparam LOAD    = 3'd1;
    localparam START   = 3'd2;
    localparam DATA    = 3'd3;
    localparam STOP    = 3'd4;
    localparam NEXT    = 3'd5;

    reg [2:0]  state;
    reg [8:0]  baud_cnt;
    reg [7:0]  shift;
    reg [2:0]  bit_cnt;
    reg [2:0]  byte_idx;
    reg        tx_reg;

    // Packet bytes
    reg [7:0]  pkt [0:4];

    assign uart_tx = tx_reg;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state    <= IDLE;
            tx_reg   <= 1'b1;
            baud_cnt <= 0;
            bit_cnt  <= 0;
            byte_idx <= 0;
        end else begin
            case (state)
                IDLE: begin
                    tx_reg <= 1'b1;
                    if (send_en) begin
                        // Latch packet
                        pkt[0] <= 8'hAA;
                        pkt[1] <= {{(8-N){1'b0}}, spin_vec};
                        pkt[2] <= energy[15:8];
                        pkt[3] <= energy[7:0];
                        pkt[4] <= 8'h55;
                        byte_idx <= 0;
                        state    <= LOAD;
                    end
                end

                LOAD: begin
                    shift    <= pkt[byte_idx];
                    baud_cnt <= 0;
                    bit_cnt  <= 0;
                    state    <= START;
                end

                START: begin
                    tx_reg <= 1'b0;   // start bit
                    if (baud_cnt == DIV-1) begin
                        baud_cnt <= 0;
                        state    <= DATA;
                    end else begin
                        baud_cnt <= baud_cnt + 1;
                    end
                end

                DATA: begin
                    tx_reg <= shift[0];
                    if (baud_cnt == DIV-1) begin
                        baud_cnt <= 0;
                        shift    <= {1'b0, shift[7:1]};  // LSB first
                        if (bit_cnt == 7) begin
                            bit_cnt <= 0;
                            state   <= STOP;
                        end else begin
                            bit_cnt <= bit_cnt + 1;
                        end
                    end else begin
                        baud_cnt <= baud_cnt + 1;
                    end
                end

                STOP: begin
                    tx_reg <= 1'b1;   // stop bit
                    if (baud_cnt == DIV-1) begin
                        baud_cnt <= 0;
                        state    <= NEXT;
                    end else begin
                        baud_cnt <= baud_cnt + 1;
                    end
                end

                NEXT: begin
                    if (byte_idx == 4) begin
                        state <= IDLE;
                    end else begin
                        byte_idx <= byte_idx + 1;
                        state    <= LOAD;
                    end
                end

                default: state <= IDLE;
            endcase
        end
    end

endmodule
