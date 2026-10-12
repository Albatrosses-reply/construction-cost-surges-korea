"""Final layout of two tables as printed in the paper (run after the R scripts).
- Table 1 (tab_t1_new.tex): drop the commodity-price rows, which the paper does not use
- Table 8 (tab_t6_new.tex): drop columns 5-6 (one-lag specification without cost-growth lags) and the inference row
Stops with an error if a table no longer has the expected layout."""
import os, sys
os.chdir(os.path.dirname(os.path.abspath(__file__)))

p1 = "tables/tab_t1_new.tex"
s = open(p1, encoding="utf-8").read()
a = s.find("\\textit{Instruments")
b = s.find("\\textit{Auxiliary dependent variable}")
if a < 0 or b < a:
    sys.exit("tab_t1_new.tex: instrument block not found")
s = s[:a] + s[b:]
open(p1, "w", encoding="utf-8").write(s)

p6 = "tables/tab_t6_new.tex"
out, n_cut = [], 0
for line in open(p6, encoding="utf-8").read().split("\n"):
    t = line.strip()
    if line.startswith("\\begin{tabular}"):
        line = "\\begin{tabular}{@{}l ccccccc@{}}"
    elif "\\multicolumn{9}{c}{Dependent variable" in line:
        line = line.replace("\\multicolumn{9}", "\\multicolumn{7}")
    elif line.startswith("\\cmidrule(l){2-10}"):
        line = "\\cmidrule(l){2-8}"
    elif "Lag structure and inference" in line:
        line = "& \\multicolumn{4}{c}{Lag structure} & \\multicolumn{3}{c}{Regime definition and sample}\\\\"
    elif line.startswith("\\cmidrule(lr){2-7}"):
        line = "\\cmidrule(lr){2-5}\\cmidrule(l){6-8}"
    elif t.startswith("& DK & DK"):
        continue  # every remaining column uses Driscoll-Kraay errors; stated in the table note
    elif "\\multicolumn{10}{@{}l}" in line:
        line = line.replace("\\multicolumn{10}", "\\multicolumn{8}")
    elif "&" in line and t.endswith("\\\\"):
        cells = line.rstrip()[:-2].split("&")
        if len(cells) != 10:
            sys.exit(f"tab_t6_new.tex: unexpected row: {line[:80]}")
        del cells[5:7]
        n_cut += 1
        if cells[1].strip() == "(1)":
            cells = [cells[0]] + [f" ({i}) " for i in range(1, 8)]
        line = "&".join(cells).rstrip() + "\\\\"
    out.append(line)
if n_cut < 15 or any("Original" in l for l in out):
    sys.exit("tab_t6_new.tex: column removal incomplete")
open(p6, "w", encoding="utf-8").write("\n".join(out))
print(f"Table 1: commodity-price rows dropped; Table 8: {n_cut} rows cut to 7 columns")
