# LaTeX for Tables 1, 2, 5 and A.2 (tabular-only files + wrapper with captions and notes)
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(jsonlite)})
pv <- function(b, s) 2 * pnorm(-abs(b / s))
stars <- function(p) ifelse(p < .01, "^{***}", ifelse(p < .05, "^{**}", ifelse(p < .1, "^{*}", "")))
num <- function(x, d = 3) { s <- formatC(x, format = "f", digits = d); z <- formatC(0, format = "f", digits = d); ifelse(s == paste0("-", z), z, s) }
mnum <- function(x, d = 3) paste0("$", num(x, d), "$")
cf <- function(b, se, d = 3) paste0("$", num(b, d), stars(pv(b, se)), "$")
sef <- function(se, d = 3) paste0("$(", num(se, d), ")$")
comma <- function(n) formatC(n, format = "d", big.mark = "{,}")
row <- function(label, cells) paste0(label, " & ", paste(cells, collapse = " & "), "\\\\")
meta <- fromJSON("out/meta.json"); mr <- fromJSON("out/meta_rest.json"); P <- meta$p_main
ym <- function(d) sprintf("%s:%d", substr(d, 1, 4), as.integer(substr(d, 6, 7)))
star_note <- "\\sym{*}\\,$p<0.10$, \\sym{**}\\,$p<0.05$, \\sym{***}\\,$p<0.01$."

# ------------------------------------------------------------------ Table 1
t1 <- read.csv("out/t1_sumstat_new.csv")
L <- c("% structure-ok: no-r2 (summary statistics)", "\\begin{tabular}{@{}l rrrrr@{}}", "\\toprule",
       "Variable & Obs. & Mean & Std.\\ dev. & Min. & Max.\\\\", "\\midrule")
for (b in unique(t1$block)) {
  L <- c(L, sprintf("\\textit{%s} & & & & &\\\\", b))
  for (i in which(t1$block == b)) L <- c(L, with(t1[i, ], row(paste0("\\quad ", label), c(comma(n), mnum(mean), mnum(sd), mnum(min), mnum(max)))))
}
L <- c(L, "\\bottomrule", "\\end{tabular}"); writeLines(L, "tables/tab_t1_new.tex")

# ------------------------------------------------------------------ Table 2
t2 <- read.csv("out/t2_region_struct_new.csv")
lab <- c(seoul = "Seoul", busan = "Busan", daegu = "Daegu", incheon = "Incheon", gwangju = "Gwangju", daejeon = "Daejeon", ulsan = "Ulsan", gyeonggi = "Gyeonggi")
sg <- function(x) paste0("$", ifelse(x >= 0, "+", ""), formatC(x, format = "f", digits = 1), "$")
L <- c("% structure-ok: no-r2 (descriptive table)", "\\begin{tabular}{@{}l ccccc c@{}}", "\\toprule",
       "& \\multicolumn{5}{c}{Sample mean (log)} & Surge change\\\\", "\\cmidrule(lr){2-6}\\cmidrule(l){7-7}",
       "Region & Starts & Permits (monthly) & Unsold & Transactions & Price index & $\\Delta$ Unsold\\\\", "\\midrule")
for (i in seq_len(nrow(t2))) L <- c(L, with(t2[i, ], row(lab[[region]], c(num(starts, 2), num(permits, 2), num(unsold, 2), num(trans, 2), num(price, 2), sg(d_unsold)))))
L <- c(L, "\\bottomrule", "\\end{tabular}"); writeLines(L, "tables/tab_t2_new.tex")

# ------------------------------------------------------------------ Table 5
t5 <- read.csv("out/t5_price_new.csv"); nc <- nrow(t5)
L <- c("% structure: rows = estimates by panel, columns = horizons", sprintf("\\begin{tabular}{@{}l %s@{}}", paste(rep("c", nc), collapse = "")), "\\toprule",
  sprintf("& \\multicolumn{%d}{c}{Dependent variable: $\\ln p_{i,t+h}-\\ln p_{i,t-1}$, average new-sale price}\\\\", nc),
  sprintf("\\cmidrule(l){2-%d}", nc + 1), row("Horizon $h$ (months)", paste0("$", t5$h, "$")), row("", paste0("(", seq_len(nc), ")")), "\\midrule",
  sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel A. Average response}}\\\\[1pt]", nc + 1),
  row("\\quad Construction cost growth $c_t$", cf(t5$avg_b, t5$avg_se)), row("", sef(t5$avg_se)), row("\\quad $R^2$", num(t5$avg_r2)),
  "\\addlinespace[3pt]", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel B. High-cost regime}}\\\\[1pt]", nc + 1),
  row("\\quad Cost growth, ordinary regime ($\\beta_h$)", cf(t5$beta, t5$beta_se)), row("", sef(t5$beta_se)),
  row("\\quad Total effect, high-cost regime ($\\beta_h{+}\\theta_h$)", cf(t5$reg_b, t5$reg_se)), row("", sef(t5$reg_se)), row("\\quad $R^2$", num(t5$reg_r2)),
  "\\midrule", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Specification (both panels)}}\\\\[1pt]", nc + 1),
  row("\\quad Region fixed effects", rep("Yes", nc)),
  row(sprintf("\\quad Lags 1--%d of log new-sale price and cost growth", P), rep("Yes", nc)),
  row(sprintf("\\quad Lags 1--%d of housing-market and macroeconomic controls", P), rep("Yes", nc)),
  row("\\quad Observations", comma(t5$avg_n)), row("\\quad Months", comma(as.integer(t5$avg_n / 8))), "\\bottomrule", "\\end{tabular}")
writeLines(L, "tables/tab_t5_new.tex")

# ------------------------------------------------------------------ Table A.2 (sums of lag coefficients)
a2 <- read.csv("out/tA2_lagsums_new.csv"); HA <- sort(unique(a2$h)); nc2 <- length(HA)
terms <- c(yoy_CCI_t = "Construction cost growth $c_t$", l_start_pri = "Log housing starts", yoy_CCI = "Construction cost growth",
           l_permit_flow = "Housing permits (monthly, log)", l_unsold_pri = "Unsold housing units (log)", l_hh = "Registered households (log)",
           l_apt_trd = "Apartment transaction volume (log)", l_apt_price_idx = "Apartment sales price index (log)",
           rate_k3y = "Three-year Treasury yield", spread_corp3y = "Corporate--Treasury spread", l_ppi = "Producer price index (log)",
           l_fx_usdkrw = "KRW/USD exchange rate (log)")
g <- function(tm, what) { x <- a2[a2$term == tm, ]; x[match(HA, x$h), what] }
L <- c("% structure: rows = regressors (sums of lag coefficients), columns = horizons", sprintf("\\begin{tabular}{@{}l %s@{}}", paste(rep("c", nc2), collapse = "")), "\\toprule",
  sprintf("& \\multicolumn{%d}{c}{\\shortstack{Dependent variable: cumulative change\\\\in log housing starts, $t-1$ to $t+h$}}\\\\", nc2),
  sprintf("\\cmidrule(l){2-%d}", nc2 + 1), row("Regressor", paste0("$h=", HA, "$")), row("", paste0("(", seq_len(nc2), ")")), "\\midrule",
  row(terms[["yoy_CCI_t"]], cf(g("yoy_CCI_t", "b"), g("yoy_CCI_t", "se"))), row("", sef(g("yoy_CCI_t", "se"))), "\\addlinespace[3pt]",
  sprintf("\\multicolumn{%d}{@{}l}{\\textit{Sum of coefficients on lags 1--%d}}\\\\[1pt]", nc2 + 1, P))
for (tm in names(terms)[-1]) L <- c(L, row(paste0("\\quad ", terms[[tm]]), cf(g(tm, "b"), g(tm, "se"))), row("", sef(g(tm, "se"))))
L <- c(L, "\\midrule", row("\\quad Region fixed effects", rep("Yes", nc2)), row("\\quad $R^2$", num(g("yoy_CCI_t", "r2"))),
       row("\\quad Observations", comma(g("yoy_CCI_t", "n"))), "\\bottomrule", "\\end{tabular}")
writeLines(L, "tables/tab_tA2_new.tex")

# ------------------------------------------------------------------ Table A.3 (first stage, h = 0)
fc <- read.csv("out/tA3_firststage_coef.csv"); fs <- read.csv("out/tA3_firststage_stats.csv")
inst <- c(yoy_l_Iron_ore = "Iron-ore price growth", yoy_l_Aluminum = "Aluminum price growth", yoy_l_Copper = "Copper price growth")
cell <- function(col, term) { x <- fc[fc$col == col & fc$term == term, ]; if (nrow(x) == 0) return(c("", "")); c(cf(x$b, x$se), sef(x$se)) }
L <- c("% structure: rows = excluded instruments, columns = first-stage regressions", "\\begin{tabular}{@{}l ccc@{}}", "\\toprule",
  "& \\multicolumn{3}{c}{Dependent variable (first stage)}\\\\", "\\cmidrule(l){2-4}",
  "& $c_t$ & $c_t$ & $c_t\\times D_t$\\\\", "& Average model & Regime model & Regime model\\\\", "& (1) & (2) & (3)\\\\", "\\midrule")
for (tm in names(inst)) { a <- sapply(1:3, function(k) cell(k, tm)); L <- c(L, row(inst[[tm]], a[1, ]), row("", a[2, ])) }
for (tm in names(inst)) { a <- sapply(1:3, function(k) cell(k, paste0(tm, "_D"))); L <- c(L, row(paste0(inst[[tm]], " $\\times D_t$"), a[1, ]), row("", a[2, ])) }
fsv <- function(w, d = 3) sapply(1:3, function(k) fs[fs$col == k, w])
L <- c(L, "\\midrule", row("\\quad First-stage $F$ (Driscoll--Kraay)", num(fsv("F"), 1)), row("\\quad $R^2$", num(fsv("r2"))),
       row("\\quad Observations", comma(fsv("n"))), row("\\quad High-cost regime indicator $D_t$", c("No", "Yes", "Yes")),
       row(sprintf("\\quad Lags 1--%d of starts, cost growth, controls, instruments", P), rep("Yes", 3)),
       row("\\quad Region fixed effects", rep("Yes", 3)), "\\bottomrule", "\\end{tabular}")
writeLines(L, "tables/tab_tA3_new.tex")

# ------------------------------------------------------------------ wrapper
ctrl_note <- sprintf("lags 1--%d of the dependent variable, cost growth, the housing-market controls (apartment transaction volume, monthly housing permits, unsold units, registered households, apartment sales price index) and the macroeconomic controls (three-year Treasury yield, corporate--Treasury spread, producer price index, KRW/USD exchange rate)", P)
wrap <- function(file, caption, label, notes, size = "\\footnotesize", sep = "6pt") c(
  "\\begin{table}[p]\\centering\\begin{threeparttable}", sprintf("\\caption{%s}\\label{%s}", caption, label),
  sprintf("%s\\setlength{\\tabcolsep}{%s}\\renewcommand{\\arraystretch}{0.9}", size, sep), sprintf("\\input{%s}", file),
  "\\begin{tablenotes}[flushleft]\\scriptsize", paste0("\\item \\textit{Notes:} ", notes), "\\end{tablenotes}\\end{threeparttable}\\end{table}", "\\clearpage")
n1 <- paste("The sample comprises eight Korean regions over 2011:1--2026:1 (1,448 region--months); the auxiliary new-sale price is available for",
  "2013:9--2026:1 (1,192 region--months). Construction cost growth and the macroeconomic and instrument variables are common to all regions",
  "and repeated across the panel. Housing starts equal $\\ln(1+\\text{starts})$. Housing permits are monthly flows, $\\ln(1+\\text{permits})$;",
  "the source series is cumulative from January of each year and is differenced within the year.")
n2 <- paste("Columns 1--5 report each region's sample mean of the (log) variable over 2011:1--2026:1; permits are monthly flows.",
  "The final column is the change in log unsold inventory during the high-cost regime (2021:4--2022:9) relative to the preceding 24 months,",
  "in log points $\\times 100$.")
n5 <- paste(sprintf("An observation is a region--month (eight regions; estimation sample %s--2026:1, shortened by $h$ months at the end).", ym(mr$sale_first)),
  "The dependent variable is the cumulative change in the log average sale price of newly supplied housing (HUG) from $t-1$ to $t+h$.",
  sprintf("All specifications include region fixed effects and %s.", ctrl_note),
  "Panel B adds the high-cost-regime indicator $D_t$ and $c_t\\times D_t$. Driscoll--Kraay standard errors (bandwidth $h+1$) in parentheses.", star_note)
nA2 <- paste("Average-response local projection of Table 3, Panel A. The first row is the coefficient on current cost growth.",
  sprintf("The remaining rows report, for each variable, the sum of the coefficients on lags 1--%d with the Driscoll--Kraay standard error of the sum", P),
  "(bandwidth $h+1$). The individual lag coefficients and the joint tests are in \\texttt{tA2\\_full\\_coefficients\\_all\\_lags.csv}.", star_note)
nA3 <- paste("First-stage regressions of the instrumented local projections at $h=0$. Column 1 is the first stage of Table 3, Panel C; columns 2--3 are the",
  "two first stages of Panel D, where the excluded instruments are the three commodity-price growth rates and their interactions with $D_t$.",
  sprintf("All columns include region fixed effects and %s, and lags 1--%d of the instruments.", ctrl_note, P),
  "The $F$ statistic is the Wald test of the excluded instruments with the Driscoll--Kraay covariance. Driscoll--Kraay standard errors in parentheses.", star_note)
doc <- c("\\documentclass[10pt]{article}", "\\usepackage[margin=2.5cm]{geometry}\\usepackage{amsmath,amssymb}\\usepackage{booktabs,threeparttable,caption}",
  "\\providecommand{\\sym}[1]{\\textsuperscript{#1}}\\captionsetup{labelsep=period}", "\\begin{document}",
  wrap("tab_t1_new", "Summary Statistics.", "tab:sumstat", n1, "\\footnotesize", "7pt"),
  "\\setcounter{table}{1}", wrap("tab_t2_new", "Regional Structure and the Surge in Unsold Inventory.", "tab:regionstat", n2),
  "\\setcounter{table}{4}", wrap("tab_t5_new", "The Effect of Construction Cost Growth on New-Sale Prices.", "tab:price", n5, "\\scriptsize", "5pt"),
  "\\renewcommand{\\thetable}{A.\\arabic{table}}\\setcounter{table}{1}",
  wrap("tab_tA2_new", "Coefficient Estimates, Average Response (Lag-Augmented Panel LP).", "tab:fullcoef", nA2, "\\scriptsize", "6pt"),
  wrap("tab_tA3_new", "First-Stage Estimates: Global Commodity Prices and Construction Cost Growth.", "tab:firststage", nA3, "\\footnotesize", "8pt"),
  "\\end{document}")
writeLines(doc, "tables/tables_rest_new.tex")
cat("rest tables written\n")
