# =============================================================================
# synth.tcl — Gowin EDA synthesis script for TSU on Tang Nano 9K
# Run from project root:  gowin_ide -script syn/synth.tcl
# =============================================================================

set_device GW1NR-LV9QN88PC6/I5 -name GW1NR-9C

# RTL sources
add_file rtl/lfsr_prng.v
add_file rtl/pbit_cell.v
add_file rtl/gibbs_scheduler.v
add_file rtl/ising_weight_rom.v
add_file rtl/energy_calc.v
add_file rtl/uart_tx_framer.v
add_file rtl/tsu_top.v

# Constraints
add_file syn/tangnano9k.cst

# Synthesis options
set_option -synthesis_tool gowinsynthesis
set_option -output_base_name tsu_top
set_option -top_module tsu_top
set_option -verilog_std sysv2017
set_option -use_mspi_as_gpio 1
set_option -use_sspi_as_gpio 1
set_option -use_done_as_gpio 1
set_option -rw_check_on_ram 1

# Run flow: synth → place → route → bitstream
run synthesis
run place
run route
run bitstream
