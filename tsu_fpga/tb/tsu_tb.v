// =============================================================================
// tsu_tb.v — Testbench for tsu_top
//
// Run with:
//   iverilog -g2012 -o tsu_sim \
//       tb/tsu_tb.v \
//       rtl/tsu_top.v rtl/pbit_cell.v rtl/lfsr_prng.v \
//       rtl/gibbs_scheduler.v rtl/energy_calc.v rtl/uart_tx_framer.v
//   vvp tsu_sim
// =============================================================================
`timescale 1ns/1ps

module tsu_tb;

    // -------------------------------------------------------------------------
    // DUT signals
    // -------------------------------------------------------------------------
    reg  clk_27;
    reg  rst_n;
    wire uart_tx;
    wire led_r, led_g, led_b;

    // -------------------------------------------------------------------------
    // DUT instantiation
    // -------------------------------------------------------------------------
    tsu_top dut (
        .clk_27  (clk_27),
        .rst_n   (rst_n),
        .uart_tx (uart_tx),
        .led_r   (led_r),
        .led_g   (led_g),
        .led_b   (led_b)
    );

    // -------------------------------------------------------------------------
    // Clock: 27 MHz → 37.04 ns period
    // -------------------------------------------------------------------------
    initial clk_27 = 0;
    always #18.52 clk_27 = ~clk_27;

    // -------------------------------------------------------------------------
    // Reset sequence
    // -------------------------------------------------------------------------
    initial begin
        rst_n = 0;
        #200;
        rst_n = 1;
    end

    // -------------------------------------------------------------------------
    // UART RX decoder (for console readout)
    // -------------------------------------------------------------------------
    localparam BAUD_PERIOD = 8680;   // ns @ 115200 baud

    reg [7:0] rx_byte;
    integer   rx_bit;
    integer   pkt_byte_cnt;
    reg [7:0] pkt_buf [0:4];

    initial begin
        pkt_byte_cnt = 0;
        forever begin
            @(negedge uart_tx);              // start bit
            #(BAUD_PERIOD + BAUD_PERIOD/2);  // skip to middle of bit 0
            for (rx_bit = 0; rx_bit < 8; rx_bit = rx_bit+1) begin
                rx_byte[rx_bit] = uart_tx;
                #BAUD_PERIOD;
            end
            // store and decode packet
            pkt_buf[pkt_byte_cnt] = rx_byte;
            pkt_byte_cnt = pkt_byte_cnt + 1;
            if (pkt_byte_cnt == 5) begin
                pkt_byte_cnt = 0;
                if (pkt_buf[0] == 8'hAA && pkt_buf[4] == 8'h55) begin
                    $display("[UART PKT] spins=%08b  energy=%0d",
                             pkt_buf[1],
                             $signed({pkt_buf[2], pkt_buf[3]}));
                end
            end
        end
    end

    // -------------------------------------------------------------------------
    // Energy and spin state monitor (direct RTL probe)
    // -------------------------------------------------------------------------
    wire [7:0] spins_probe = dut.s;

    integer sweep_cnt;
    initial sweep_cnt = 0;

    always @(posedge clk_27) begin
        if (dut.sweep_done_w) begin
            sweep_cnt = sweep_cnt + 1;
            if (sweep_cnt % 50 == 0)
                $display("[SWEEP %0d] spins=%08b  E=%0d  LED=(%b,%b,%b)",
                         sweep_cnt, spins_probe,
                         $signed(dut.energy_w),
                         ~led_r, ~led_g, ~led_b);
        end
    end

    // -------------------------------------------------------------------------
    // Run for 2000 sweeps then finish
    // -------------------------------------------------------------------------
    initial begin
        $dumpfile("tsu_sim.vcd");
        $dumpvars(0, tsu_tb);

        wait(rst_n);
        repeat(2000) @(posedge dut.sweep_done_w);
        $display("\n=== SIMULATION COMPLETE: 2000 sweeps ===");
        $finish;
    end

    // -------------------------------------------------------------------------
    // Timeout guard
    // -------------------------------------------------------------------------
    initial begin
        #500_000_000;
        $display("TIMEOUT");
        $finish;
    end

endmodule
