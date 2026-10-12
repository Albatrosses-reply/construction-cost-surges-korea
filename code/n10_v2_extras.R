# Further outputs
#   tables/tab_t3_main.tex  Table 3
#   tables/tab_tA4_iv.tex   instrumented local projections (not reported in the paper)
#   tables/tab_tA5_ic.tex   lag-length selection (Table A.3)
#   figures/fig4_placebo_new.png  placebo 18-month regime windows (Figure 5)
#   out/conversion.csv      national starts relative to trailing 12-month permits, by period (Figure 1, Section 7.5)
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(jsonlite); library(ggplot2); library(dplyr)})
pv <- function(b, s) 2 * pnorm(-abs(b / s))
stars <- function(p) ifelse(p < .01, "^{***}", ifelse(p < .05, "^{**}", ifelse(p < .1, "^{*}", "")))
num <- function(x, d = 3) { s <- formatC(x, format = "f", digits = d); z <- formatC(0, format = "f", digits = d); ifelse(s == paste0("-", z), z, s) }
cf <- function(b, se) paste0("$", num(b), stars(pv(b, se)), "$"); sef <- function(se) paste0("$(", num(se), ")$")
comma <- function(n) formatC(n, format = "d", big.mark = "{,}")
row <- function(label, cells) paste0(label, " & ", paste(cells, collapse = " & "), "\\\\")
meta <- fromJSON("out/meta.json"); P <- meta$p_main
main <- read.csv("out/est_main.csv"); iv <- read.csv("out/est_iv.csv"); ar <- read.csv("out/est_iv_ar.csv")
HR <- c(0, 3, 6, 9, 12, 15, 18, 24); m <- main[match(HR, main$h), ]; v <- iv[match(HR, iv$h), ]; a <- ar[match(HR, ar$h), ]; nc <- length(HR)
head3 <- function(dv) c(sprintf("\\begin{tabular}{@{}l %s@{}}", paste(rep("c", nc), collapse = "")), "\\toprule",
  sprintf("& \\multicolumn{%d}{c}{%s}\\\\", nc, dv), sprintf("\\cmidrule(l){2-%d}", nc + 1),
  row("Horizon $h$ (months)", paste0("$", HR, "$")), row("", paste0("(", seq_len(nc), ")")), "\\midrule")
spec <- function(iv = FALSE) c("\\midrule", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Specification (all panels)}}\\\\[1pt]", nc + 1),
  row("\\quad Region fixed effects", rep("Yes", nc)), row(sprintf("\\quad Lags 1--%d of log starts and cost growth", P), rep("Yes", nc)),
  row(sprintf("\\quad Lags 1--%d of housing-market controls", P), rep("Yes", nc)), row(sprintf("\\quad Lags 1--%d of macroeconomic controls", P), rep("Yes", nc)),
  if (iv) row(sprintf("\\quad Lags 1--%d of instruments", P), rep("Yes", nc)) else NULL,
  row("\\quad Observations", comma(m$avg_n)), row("\\quad Months", comma(as.integer(m$n_periods))), "\\bottomrule", "\\end{tabular}")
dv <- "Dependent variable: $\\ln(1+\\mathrm{starts})_{i,t+h}-\\ln(1+\\mathrm{starts})_{i,t-1}$"
t3 <- c("% structure: rows = estimates by panel, columns = horizons; OLS panels of Table 3", head3(dv),
  sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel A. Average response}}\\\\[1pt]", nc + 1),
  row("\\quad Construction cost growth $c_t$", cf(m$avg_b, m$avg_se)), row("", sef(m$avg_se)), row("\\quad $R^2$", num(m$avg_r2)),
  "\\addlinespace[3pt]", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel B. High-cost regime}}\\\\[1pt]", nc + 1),
  row("\\quad Cost growth, ordinary regime ($\\beta_h$)", cf(m$reg_beta, m$reg_beta_se)), row("", sef(m$reg_beta_se)),
  row("\\quad Total effect, high-cost regime ($\\beta_h{+}\\theta_h$)", cf(m$reg_b, m$reg_se)), row("", sef(m$reg_se)), row("\\quad $R^2$", num(m$reg_r2)), spec(FALSE))
writeLines(t3, "tables/tab_t3_main.tex")
arcell <- function(x) paste0("$", num(x, 2), "$")
tA4 <- c("% structure: rows = IV estimates by panel, columns = horizons", head3(dv),
  sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel A. IV local projection: average response}}\\\\[1pt]", nc + 1),
  row("\\quad Construction cost growth $c_t$", cf(v$iva_b, v$iva_se)), row("", sef(v$iva_se)),
  row("\\quad First-stage $F$ (DK)", num(v$iva_F, 1)), row("\\quad AR 95\\% set, lower bound", arcell(a$ar_lo)), row("\\quad AR 95\\% set, upper bound", arcell(a$ar_hi)),
  "\\addlinespace[3pt]", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel B. IV local projection: high-cost regime}}\\\\[1pt]", nc + 1),
  row("\\quad Total effect, high-cost regime ($\\beta_h{+}\\theta_h$)", cf(v$ivr_b, v$ivr_se)), row("", sef(v$ivr_se)),
  row("\\quad First-stage $F$ for $c_t$ (DK)", num(v$ivr_F1, 1)), row("\\quad First-stage $F$ for $c_t\\times D_t$ (DK)", num(v$ivr_F2, 1)),
  "\\addlinespace[3pt]", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Memo: OLS estimates (Table 3)}}\\\\[1pt]", nc + 1),
  row("\\quad Average response", cf(m$avg_b, m$avg_se)), row("\\quad Total effect, high-cost regime", cf(m$reg_b, m$reg_se)),
  row("\\quad $R^2$ (OLS, average response)", num(m$avg_r2)), spec(TRUE))
writeLines(tA4, "tables/tab_tA4_iv.tex")
ic <- read.csv("out/d7_ic_table.csv")
best <- function(k) which.min(ic[[k]])
mark <- function(x, k) ifelse(seq_along(x) == best(k), paste0("\\textbf{", num(x), "}"), num(x))
tA5 <- c("% structure-ok: no-r2 (information criteria table)", "\\begin{tabular}{@{}c ccc@{}}", "\\toprule",
  "Lag length $p$ & AIC & HQ & BIC\\\\", "\\midrule",
  apply(cbind(ic$p, mark(ic$AIC, "AIC"), mark(ic$HQ, "HQ"), mark(ic$BIC, "BIC")), 1, function(r) paste0(paste(r, collapse = " & "), "\\\\")),
  "\\bottomrule", "\\end{tabular}")
writeLines(tA5, "tables/tab_tA5_ic.tex")

# ---- placebo 18-month windows (n5 output): timing of the boom and the bust
pl <- read.csv("out/placebo_windows_new.csv"); pl$start <- as.Date(pl$start)
true <- pl[pl$start == as.Date("2021-04-01"), ]
ov <- (pl$start + 17 * 30.5 >= as.Date("2021-04-01")) & (pl$start <= as.Date("2022-09-01"))
long <- bind_rows(data.frame(panel = "(a) Total effect at h = 11", start = pl$start, est = pl$boom, overlap = ov),
                  data.frame(panel = "(b) Mean total effect at h = 16-19", start = pl$start, est = pl$bust, overlap = ov))
tl <- data.frame(panel = c("(a) Total effect at h = 11", "(b) Mean total effect at h = 16-19"), start = true$start, est = c(true$boom, true$bust))
share <- c(mean(pl$boom[!ov] >= true$boom), mean(pl$bust[!ov] <= true$bust)); npl <- sum(!ov)
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
ovr <- range(pl$start[ov])
g <- ggplot(long, aes(start, est)) +
  annotate("rect", xmin = ovr[1], xmax = ovr[2], ymin = -Inf, ymax = Inf, fill = "#f0efec") +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.45) +
  geom_hline(data = tl, aes(yintercept = est), colour = ORANGE, linewidth = 0.6, linetype = "22") +
  geom_line(data = long[!long$overlap, ], aes(group = interaction(panel, start > ovr[1])), colour = BLUE, linewidth = 0.6) +
  geom_point(data = long[!long$overlap, ], colour = "white", fill = BLUE, shape = 21, size = 1.8, stroke = 0.4) +
  geom_point(data = tl, colour = "white", fill = ORANGE, shape = 23, size = 3.6, stroke = 0.6) +
  geom_text(data = tl, aes(label = "Markov regime\n2021:4-2022:9"), colour = INK, size = 3.1, hjust = 0.5, vjust = -0.55, lineheight = 0.9) +
  facet_wrap(~ panel, nrow = 1, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "First month of the 18-month window", y = "Cumulative response of log housing starts", title = NULL) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 11, hjust = 0, colour = INK), axis.title = element_text(colour = INK2, size = 11),
        axis.text = element_text(colour = INK2, size = 10), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = GRID, linewidth = 0.4), plot.margin = margin(8, 14, 8, 8))
dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
tmp <- tempfile(fileext = ".png"); ggsave(tmp, g, width = 10.5, height = 4.0, dpi = 300, device = dev, bg = "white"); stopifnot(file.copy(tmp, "figures/fig4_placebo_new.png", overwrite = TRUE))

# ---- national private starts relative to trailing 12-month mean of monthly permits
r <- read.csv("../data/monthly_series_wide.csv", check.names = FALSE); r$date <- as.Date(substr(r$Monthly2, 1, 10)); r$m <- as.integer(format(r$date, "%m"))
r$pflow <- ifelse(r$m == 1, r$permit_pri_nat, r$permit_pri_nat - c(NA, head(r$permit_pri_nat, -1)))
r$p12 <- as.numeric(stats::filter(c(NA, head(r$pflow, -1)), rep(1 / 12, 12), sides = 1)); r$conv <- r$start_pri_nat / r$p12
per <- list("2015:1-2018:12" = c("2015-01-01", "2018-12-01"), "2019:1-2020:12" = c("2019-01-01", "2020-12-01"), "2021:1-2021:12" = c("2021-01-01", "2021-12-01"),
            "2022:1-2022:9" = c("2022-01-01", "2022-09-01"), "2022:10-2023:12" = c("2022-10-01", "2023-12-01"), "2024:1-2026:1" = c("2024-01-01", "2026-01-01"))
conv <- data.frame(period = names(per), ratio = sapply(per, function(p) mean(r$conv[r$date >= as.Date(p[1]) & r$date <= as.Date(p[2])], na.rm = TRUE)))
write.csv(conv, "out/conversion.csv", row.names = FALSE)
write_json(list(placebo_share_boom = share[1], placebo_share_bust = share[2], placebo_n = npl, true_boom = true$boom, true_bust = true$bust),
           "out/placebo_summary.json", auto_unbox = TRUE, pretty = TRUE)
print(conv, digits = 3); cat(sprintf("placebo: N=%d, share boom>=true %.3f, share bust<=true %.3f\n", npl, share[1], share[2]))
