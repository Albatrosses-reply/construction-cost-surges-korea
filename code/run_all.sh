#!/bin/sh
# Runs the full pipeline in paper order. Outputs: code/out (estimates), code/tables (LaTeX tables), code/figures.
set -e; cd "$(dirname "$0")"; mkdir -p out tables figures
for s in d7_ic_lags n0_markov n01_descriptive_figures n0b_markov_table n1_estimate n2_tables n3_figures n5_placebo_new n6_rest_estimate n7_rest_tables n8_rest_figures n10_v2_extras n12_17regions n13_descriptives n17_robust_avg_costlevel n19_cap_timing n20_credit_demand; do
  t0=$(date +%s); Rscript "$s.R" > "out/log_$s.txt" 2>&1 || { echo "FAILED: $s"; tail -20 "out/log_$s.txt"; exit 1; }; echo "$s $(( $(date +%s) - t0 ))s"
done
python3 n11_fig_institutions.py > out/log_n11.txt 2>&1 && echo "n11 done"
python3 finalize_tables.py
