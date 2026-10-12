# Seventeen-province panel (Table A.5; no comparable permit series for this panel)
#   lags 1-4 of log starts, cost growth, unsold, price index, households, transactions, macro controls; DK (h+1)
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(dplyr); library(fixest)})
tb <- read.csv("../data/panel_17_provinces.csv", check.names = FALSE)
tb$date <- as.Date(tb$date); names(tb) <- gsub("\\.", "_", gsub(" ", "_", names(tb)))
tb <- tb %>% arrange(region, date) %>% group_by(region) %>% mutate(tid = row_number(), yoy_CCI_regime = yoy_CCI * regime_dummy) %>% ungroup()
V <- c("l_start", "yoy_CCI", "l_unsold", "l_price", "l_hh", "l_trd", "rate_k3y", "spread_corp3y", "l_ppi", "l_fx_usdkrw")
for (v in V) for (l in 1:4) tb[[paste0(v, "_L", l)]] <- ave(tb[[v]], tb$region, FUN = function(x) dplyr::lag(x, l))
cc <- paste(as.vector(outer(V, 1:4, function(a, b) paste0(a, "_L", b))), collapse = " + ")
HR <- c(0, 3, 6, 9, 12, 15, 18, 24)
res <- bind_rows(lapply(HR, function(h) {
  dd <- tb %>% group_by(region) %>% mutate(y = dplyr::lead(l_start, h) - l_start_L1) %>% ungroup()
  ea <- feols(as.formula(paste("y ~ yoy_CCI +", cc, "| region")), dd, panel.id = ~ region + tid, vcov = DK(lag = h + 1), notes = FALSE, warn = FALSE)
  er <- feols(as.formula(paste("y ~ yoy_CCI + regime_dummy + yoy_CCI_regime +", cc, "| region")), dd, panel.id = ~ region + tid, vcov = DK(lag = h + 1), notes = FALSE, warn = FALSE)
  w <- c("yoy_CCI", "yoy_CCI_regime"); V2 <- vcov(er)
  data.frame(h = h, avg_b = unname(coef(ea)["yoy_CCI"]), avg_se = unname(se(ea)["yoy_CCI"]), avg_r2 = unname(r2(ea, "r2")), n = nobs(ea),
             beta = unname(coef(er)["yoy_CCI"]), beta_se = unname(se(er)["yoy_CCI"]), reg_b = sum(coef(er)[w]), reg_se = sqrt(sum(V2[w, w])),
             reg_r2 = unname(r2(er, "r2")), regions = length(unique(dd$region)))
}))
write.csv(res, "out/est_17reg.csv", row.names = FALSE)
pv <- function(b, s) 2 * pnorm(-abs(b / s))
stars <- function(p) ifelse(p < .01, "^{***}", ifelse(p < .05, "^{**}", ifelse(p < .1, "^{*}", "")))
num <- function(x, d = 3) { s <- formatC(x, format = "f", digits = d); z <- formatC(0, format = "f", digits = d); ifelse(s == paste0("-", z), z, s) }
cf <- function(b, se) paste0("$", num(b), stars(pv(b, se)), "$"); sef <- function(se) paste0("$(", num(se), ")$")
comma <- function(n) formatC(n, format = "d", big.mark = "{,}"); row <- function(l, c) paste0(l, " & ", paste(c, collapse = " & "), "\\\\")
nc <- nrow(res)
t <- c("% structure: rows = estimates by panel, columns = horizons; seventeen-province panel",
  sprintf("\\begin{tabular}{@{}l %s@{}}", paste(rep("c", nc), collapse = "")), "\\toprule",
  sprintf("& \\multicolumn{%d}{c}{Dependent variable: $\\ln(1+\\mathrm{starts})_{i,t+h}-\\ln(1+\\mathrm{starts})_{i,t-1}$}\\\\", nc),
  sprintf("\\cmidrule(l){2-%d}", nc + 1), row("Horizon $h$ (months)", paste0("$", res$h, "$")), row("", paste0("(", seq_len(nc), ")")), "\\midrule",
  sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel A. Average response}}\\\\[1pt]", nc + 1),
  row("\\quad Construction cost growth $c_t$", cf(res$avg_b, res$avg_se)), row("", sef(res$avg_se)), row("\\quad $R^2$", num(res$avg_r2)),
  "\\addlinespace[3pt]", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel B. High-cost regime}}\\\\[1pt]", nc + 1),
  row("\\quad Cost growth, ordinary regime ($\\beta_h$)", cf(res$beta, res$beta_se)), row("", sef(res$beta_se)),
  row("\\quad Total effect, high-cost regime ($\\beta_h{+}\\theta_h$)", cf(res$reg_b, res$reg_se)), row("", sef(res$reg_se)), row("\\quad $R^2$", num(res$reg_r2)),
  "\\midrule", row("\\quad Region fixed effects", rep("Yes", nc)), row("\\quad Lags 1--4 of log starts and cost growth", rep("Yes", nc)),
  row("\\quad Lags 1--4 of controls (no permits)", rep("Yes", nc)),
  row("\\quad Regions", res$regions), row("\\quad Observations", comma(res$n)), "\\bottomrule", "\\end{tabular}")
writeLines(t, "tables/tab_tA6_17reg.tex")
st <- function(b, s) { p <- pv(b, s); ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))) }
cat(sprintf("h=%2d avg %6.3f%-3s(%.3f) | beta %6.3f%-3s | total %6.3f%-3s(%.3f) | N %d\n", res$h, res$avg_b, st(res$avg_b, res$avg_se), res$avg_se,
            res$beta, st(res$beta, res$beta_se), res$reg_b, st(res$reg_b, res$reg_se), res$reg_se, res$n), sep = "")
