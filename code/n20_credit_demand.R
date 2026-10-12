# Credit and resale-market conditions after a cost surprise (Table A.4), the starts response with their
# realized paths held fixed, state dependence, and the two-step fall in the conversion of permits into starts
#   tables/tab_t9_episode.tex (A.4): responses of the Treasury yield, corporate spread (national series), resale price index
#                                 and transactions (regional panel); Panel A average, Panel B high-cost-regime total effect
#   tables/tab_t10_paths.tex (7) : starts response with the realized changes t-1..t+h of the yield, spread, resale price
#                                 and transactions added, and on surprises dated up to 2019:12
#   out/est_episode.csv, out/est_paths.csv, out/conversion_steps.json
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(dplyr); library(fixest); library(sandwich); library(lmtest)})
d0 <- read.csv("../data/panel_8_regions.csv", check.names = FALSE)
d0$date <- as.Date(d0$Monthly2); names(d0) <- gsub("\\.", "_", gsub(" ", "_", names(d0)))
raw <- read.csv("../data/monthly_series_wide.csv", check.names = FALSE)
raw$date <- as.Date(substr(raw$Monthly2, 1, 10)); raw$m <- as.integer(format(raw$date, "%m"))
regs <- c("seoul", "busan", "daegu", "incheon", "gwangju", "daejeon", "ulsan", "gyeonggi")
pm <- bind_rows(lapply(regs, function(g) {
  ytd <- raw[[paste0("permit_pri_", g)]]
  data.frame(date = raw$date, region = g, permit_flow = pmax(ifelse(raw$m == 1, ytd, ytd - dplyr::lag(ytd)), 0))
}))
d0 <- d0 %>% left_join(pm, by = c("date", "region")) %>% mutate(l_permit_flow = log(1 + permit_flow)) %>%
  left_join(raw %>% transmute(date, l_cci = log(CCI)), by = "date")
HX <- c("l_apt_trd", "l_permit_flow", "l_unsold_pri", "l_hh", "l_apt_price_idx")
MX <- c("rate_k3y", "spread_corp3y", "l_ppi", "l_fx_usdkrw")
d0 <- d0 %>% arrange(region, date) %>% group_by(region) %>% mutate(tid = row_number()) %>% ungroup()
for (v in c("l_start_pri", "yoy_CCI", HX, MX, "l_cci")) for (l in 1:4) d0[[paste0(v, "_L", l)]] <- ave(d0[[v]], d0$region, FUN = function(x) dplyr::lag(x, l))
lt <- function(vars) paste(as.vector(outer(vars, 1:4, function(a, b) paste0(a, "_L", b))), collapse = " + ")
base_ctrl <- lt(c("l_start_pri", "yoy_CCI", HX, MX)); nat_ctrl <- lt(c("yoy_CCI", "l_cci", MX))
H <- c(0, 3, 6, 9, 12, 15, 18, 24)
nat <- d0 %>% filter(region == "seoul") %>% mutate(region = "nat")
fd <- function(x, h) dplyr::lead(x, h) - dplyr::lag(x, 1)
lp <- function(dat, yv, ctrl, regime, extra = "", tmax = NULL) {
  bind_rows(lapply(H, function(h) {
    dd <- dat %>% group_by(region) %>% mutate(y = fd(.data[[yv]], h), drate = fd(rate_k3y, h), dspr = fd(spread_corp3y, h),
                                              dprice = fd(l_apt_price_idx, h), dtrd = fd(l_apt_trd, h)) %>% ungroup()
    if (!is.null(tmax)) dd <- dd %>% filter(date <= tmax)
    rhs <- paste(if (regime) "yoy_CCI + regime_dummy + yoy_CCI_regime" else "yoy_CCI", extra, "+", ctrl)
    e <- feols(as.formula(paste("y ~", rhs, "| region")), dd, panel.id = ~ region + tid, vcov = DK(lag = h + 1), notes = FALSE, warn = FALSE)
    k <- if (regime) c("yoy_CCI", "yoy_CCI_regime") else "yoy_CCI"
    data.frame(h = h, b = sum(coef(e)[k]), se = sqrt(sum(vcov(e)[k, k])), n = nobs(e), r2 = unname(r2(e, "r2")))
  }))
}
# ---- Table 9: credit and resale-market responses
ep <- list(
  list(lab = "yield", dat = nat, y = "rate_k3y", ctrl = nat_ctrl),
  list(lab = "spread", dat = nat, y = "spread_corp3y", ctrl = nat_ctrl),
  list(lab = "price", dat = d0, y = "l_apt_price_idx", ctrl = base_ctrl),
  list(lab = "trd", dat = d0, y = "l_apt_trd", ctrl = base_ctrl))
r9 <- bind_rows(lapply(ep, function(s) bind_rows(
  lp(s$dat, s$y, s$ctrl, FALSE) %>% mutate(panel = "avg", spec = s$lab),
  lp(s$dat, s$y, s$ctrl, TRUE) %>% mutate(panel = "reg", spec = s$lab))))
write.csv(r9, "out/est_episode.csv", row.names = FALSE)
# ---- Table 10: starts with the realized paths held fixed, and before 2020
pspec <- list(
  list(lab = "baseline", extra = "", tmax = NULL),
  list(lab = "yield", extra = "+ drate", tmax = NULL),
  list(lab = "yield_spread", extra = "+ drate + dspr", tmax = NULL),
  list(lab = "price", extra = "+ dprice", tmax = NULL),
  list(lab = "all", extra = "+ drate + dspr + dprice + dtrd", tmax = NULL),
  list(lab = "pre2020", extra = "", tmax = as.Date("2019-12-01")))
r10 <- bind_rows(lapply(pspec, function(s) bind_rows(
  lp(d0, "l_start_pri", base_ctrl, FALSE, s$extra, s$tmax) %>% mutate(panel = "avg", spec = s$lab),
  if (is.null(s$tmax)) lp(d0, "l_start_pri", base_ctrl, TRUE, s$extra) %>% mutate(panel = "reg", spec = s$lab))))
write.csv(r10, "out/est_paths.csv", row.names = FALSE)

# ---- tables (tabular only)
pv <- function(b, s) 2 * pnorm(-abs(b / s))
stars <- function(p) ifelse(p < .01, "^{***}", ifelse(p < .05, "^{**}", ifelse(p < .1, "^{*}", "")))
num <- function(x, d = 3) { s <- formatC(x, format = "f", digits = d); z <- formatC(0, format = "f", digits = d); ifelse(s == paste0("-", z), z, s) }
comma <- function(n) formatC(n, format = "d", big.mark = "{,}")
row <- function(l, v) paste0(l, " & ", paste(v, collapse = " & "), "\\\\")
cellf <- function(res, pn, s, h, what) { x <- res[res$panel == pn & res$spec == s & res$h == h, ]
  if (!nrow(x)) return("")
  if (what == "b") paste0("$", num(x$b), stars(pv(x$b, x$se)), "$") else paste0("$(", num(x$se), ")$") }
gx <- function(res, sp, pn, h, col) sapply(sp, function(s) { x <- res[res$panel == pn & res$spec == s & res$h == h, col]; if (length(x)) x else NA })
panels <- function(res, sp, ns) {
  t <- sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel A. Average response, $\\beta_h$}}\\\\[1pt]", ns + 1)
  for (h in H) t <- c(t, row(sprintf("\\quad $h=%d$", h), sapply(sp, function(s) cellf(res, "avg", s, h, "b"))), row("", sapply(sp, function(s) cellf(res, "avg", s, h, "s"))))
  t <- c(t, "\\addlinespace[3pt]", sprintf("\\multicolumn{%d}{@{}l}{\\textit{Panel B. Total effect in the high-cost regime, $\\beta_h+\\theta_h$}}\\\\[1pt]", ns + 1))
  for (h in H) t <- c(t, row(sprintf("\\quad $h=%d$", h), sapply(sp, function(s) cellf(res, "reg", s, h, "b"))), row("", sapply(sp, function(s) cellf(res, "reg", s, h, "s"))))
  t
}
sp9 <- c("yield", "spread", "price", "trd")
t9 <- c("% structure: rows = horizons by panel, columns = dependent variables; each cell is a separate LP; generated by n20_credit_demand.R",
  "\\begin{tabular}{@{}l cccc@{}}", "\\toprule",
  "& \\multicolumn{2}{c}{National series} & \\multicolumn{2}{c}{Eight regions}\\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(l){4-5}",
  row("", c("Treasury", "Corporate", "Resale", "Apartment")),
  row("", c("yield", "spread", "prices", "transactions")),
  row("", paste0("(", 1:4, ")")), "\\midrule", panels(r9, sp9, 4), "\\midrule",
  row("\\quad Region fixed effects", c("No", "No", "Yes", "Yes")),
  row("\\quad Controls, lags 1--4", c("National", "National", "Regional", "Regional")),
  row("\\quad Observations ($h=0$)", comma(gx(r9, sp9, "avg", 0, "n"))),
  row("\\quad Observations ($h=24$)", comma(gx(r9, sp9, "avg", 24, "n"))),
  row("\\quad $R^2$, Panel A ($h=0$)", num(gx(r9, sp9, "avg", 0, "r2"))),
  row("\\quad $R^2$, Panel A ($h=24$)", num(gx(r9, sp9, "avg", 24, "r2"))),
  "\\bottomrule", "\\end{tabular}")
writeLines(t9, "tables/tab_t9_episode.tex")
sp10 <- c("baseline", "yield", "yield_spread", "price", "all", "pre2020")
t10 <- c("% structure: rows = horizons by panel, columns = conditioning sets and samples; Panel B column 6 is undefined (no high-cost months before 2020); generated by n20_credit_demand.R",
  "\\begin{tabular}{@{}l cccccc@{}}", "\\toprule",
  "& \\multicolumn{6}{c}{Cumulative change in log starts; OLS local projections}\\\\", "\\cmidrule(l){2-7}",
  "& & \\multicolumn{4}{c}{Realized changes from $t-1$ to $t+h$ added} & Surprises\\\\", "\\cmidrule(lr){3-6}",
  row("", c("Baseline", "Yield", "Yield and", "Resale", "All four", "to 2019:12")),
  row("", c("", "", "spread", "price", "", "")),
  row("", paste0("(", 1:6, ")")), "\\midrule", panels(r10, sp10, 6), "\\midrule",
  row("\\quad Region FE; lags 1--4 of all variables", rep("Yes", 6)),
  row("\\quad Observations ($h=0$)", comma(gx(r10, sp10, "avg", 0, "n"))),
  row("\\quad Observations ($h=24$)", comma(gx(r10, sp10, "avg", 24, "n"))),
  row("\\quad $R^2$, Panel A ($h=0$)", num(gx(r10, sp10, "avg", 0, "r2"))),
  row("\\quad $R^2$, Panel A ($h=24$)", num(gx(r10, sp10, "avg", 24, "r2"))),
  "\\bottomrule", "\\end{tabular}")
writeLines(t10, "tables/tab_t10_paths.tex")

# ---- two-step fall in the conversion of permits into starts (national ratio of starts to the trailing twelve-month mean of permits)
pf <- pmax(ifelse(raw$m == 1, raw$permit_pri_nat, raw$permit_pri_nat - dplyr::lag(raw$permit_pri_nat)), 0)
cr <- data.frame(date = raw$date, s = raw$start_pri_nat, p = pf) %>% arrange(date) %>%
  mutate(pm12 = as.numeric(stats::filter(dplyr::lag(p), rep(1 / 12, 12), sides = 1)), ratio = s / pm12) %>%
  filter(!is.na(ratio), date >= as.Date("2012-01-01"), date <= as.Date("2026-01-01"))
brk <- as.Date(c("2012-01-01", "2015-01-01", "2019-01-01", "2021-01-01", "2022-01-01", "2022-10-01", "2024-01-01", "2030-01-01"))
lab <- c("2012-14", "2015-18", "2019-20", "2021", "2022:1-9", "2022:10-2023", "2024-")
cr$per <- cut(cr$date, brk, right = FALSE, labels = lab)
nw <- function(ref, term) { x <- cr; x$per <- relevel(x$per, ref = ref); m <- lm(ratio ~ per, x); ct <- coeftest(m, vcov = NeweyWest(m, lag = 12, prewhite = FALSE))
  list(b = unname(ct[paste0("per", term), 1]), se = unname(ct[paste0("per", term), 2]), p = unname(ct[paste0("per", term), 4])) }
steps <- list(means = as.list(tapply(cr$ratio, cr$per, mean)), n = as.list(table(cr$per)),
  step1_2021_to_2022q1_3 = nw("2021", "2022:1-9"), step2_to_freeze = nw("2022:1-9", "2022:10-2023"),
  total_2021_to_freeze = nw("2021", "2022:10-2023"), sample = format(range(cr$date)), nw_lag = 12)
jsonlite::write_json(steps, "out/conversion_steps.json", auto_unbox = TRUE, pretty = TRUE, digits = 6)
st <- function(b, s) { p <- pv(b, s); ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))) }
for (r in list(r9, r10)) print(r %>% mutate(lab = sprintf("%.3f%s", b, st(b, se))) %>% select(panel, spec, h, lab) %>% tidyr::pivot_wider(names_from = spec, values_from = lab), n = 40)
str(steps[c("step1_2021_to_2022q1_3", "step2_to_freeze")])
