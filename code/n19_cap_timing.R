# Table 6: timing of capped presales around the March 1 and September 15 revisions of the basic construction cost
#   private APT sale-housing announcements (Applyhome, 2020:2-2026:7); window of 45 days before and after each date
#   cells: date x capped x post; PPML with date-by-capped fixed effects and a post indicator; clusters = dates
#   placebo dates: June 1 and December 1
#   revision dates are the effective dates of the notices (law.go.kr history; data/basic_construction_cost_revisions.csv): the scheduled
#   revisions are on March 1 and September 15 except 13 September 2024; extraordinary revisions on 19 July 2021, 15 July 2022, 10 February 2023
#   outputs: tables/tab_t8_cap_timing.tex (tabular only), out/cap_timing.csv
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(dplyr); library(fixest)})
AH <- "../data/applyhome_announcements.csv"
ah <- read.csv(AH, colClasses = "character") %>% distinct(HOUSE_MANAGE_NO, .keep_all = TRUE) %>%
  filter(HOUSE_SECD_NM == "APT", RENT_SECD_NM == "분양주택", HOUSE_DTL_SECD_NM == "민영") %>%
  mutate(date = as.Date(RCRIT_PBLANC_DE), cap = as.integer(PARCPRC_ULS_AT == "Y"), units = suppressWarnings(as.numeric(TOT_SUPLY_HSHLDCO))) %>%
  filter(!is.na(date))
REV <- "../data/basic_construction_cost_revisions.csv"
rv <- read.csv(REV, colClasses = "character") %>% mutate(d = as.Date(effective_date))
real <- sort(rv$d[rv$type == "scheduled" & rv$d >= as.Date("2020-03-01") & rv$d <= as.Date("2026-03-01")])
xo <- sort(rv$d[rv$type == "extraordinary" & rv$d <= max(ah$date)])
stopifnot(length(real) == 13, length(xo) == 3)
fake <- as.Date(c(sprintf("%d-06-01", 2020:2025), sprintf("%d-12-01", 2020:2025)))
cells <- function(dates, W = 45) {
  bind_rows(lapply(dates, function(R) {
    w <- ah %>% mutate(rel = as.numeric(date - R)) %>% filter(rel >= -W, rel <= W - 1) %>% mutate(post = as.integer(rel >= 0))
    g <- expand.grid(cap = 0:1, post = 0:1)
    g %>% left_join(w %>% group_by(cap, post) %>% summarise(n = n(), u = sum(units, na.rm = TRUE), .groups = "drop"), by = c("cap", "post")) %>%
      mutate(n = coalesce(n, 0L), u = coalesce(u, 0), R = as.character(R))
  }))
}
cr <- cells(real); cf <- cells(fake)
est <- function(dat, y) {
  e <- fepois(as.formula(paste(y, "~ post + post:cap | R^cap")), dat, vcov = ~R, notes = FALSE, warn = FALSE)
  k <- "post:cap"
  data.frame(b = unname(coef(e)[k]), se = unname(se(e)[k]), bp = unname(coef(e)["post"]), sep = unname(se(e)["post"]),
             n = nobs(e), clusters = length(unique(dat$R)), pr2 = unname(fitstat(e, "pr2")[[1]]), events = sum(dat[[y]]))
}
res <- bind_rows(est(cr, "n") %>% mutate(col = 1, dates = "revision", outcome = "announcements"),
                 est(cf, "n") %>% mutate(col = 2, dates = "placebo", outcome = "announcements"),
                 est(cr, "u") %>% mutate(col = 3, dates = "revision", outcome = "units"),
                 est(cf, "u") %>% mutate(col = 4, dates = "placebo", outcome = "units"))
ratio <- function(dat, y) dat %>% group_by(cap) %>% summarise(pre = sum(.data[[y]][post == 0]), post = sum(.data[[y]][post == 1])) %>% mutate(r = post / pre)
rr <- ratio(cr, "n"); rf <- ratio(cf, "n")
inc <- cr %>% filter(cap == 1) %>% group_by(R) %>% summarise(d = n[post == 1] - n[post == 0])
res$ratio_capped <- c(rr$r[rr$cap == 1], rf$r[rf$cap == 1], NA, NA); res$ratio_uncapped <- c(rr$r[rr$cap == 0], rf$r[rf$cap == 0], NA, NA)
res$n_rev_increase <- c(sum(inc$d > 0), NA, NA, NA); res$n_rev_decrease <- c(sum(inc$d < 0), NA, NA, NA)
write.csv(res, "out/cap_timing.csv", row.names = FALSE)
win <- function(R, W) ah %>% mutate(rel = as.numeric(date - R)) %>% filter(rel >= -W, rel <= W - 1) %>%
  summarise(cap_pre = sum(cap == 1 & rel < 0), cap_post = sum(cap == 1 & rel >= 0), unc_pre = sum(cap == 0 & rel < 0), unc_post = sum(cap == 0 & rel >= 0))
# extraordinary revisions of the basic construction cost on 15 July 2021 and 15 July 2022; July 15 of 2020 and 2023-2025 had none
# July windows: centred on the extraordinary revision in 2021 (19 July) and 2022 (15 July), on 15 July in the other years
yrs <- 2020:2025
jc <- sapply(yrs, function(y) { x <- xo[format(xo, "%Y") == y & format(xo, "%m") == "07"]; if (length(x)) as.character(x) else sprintf("%d-07-15", y) })
jl <- bind_rows(lapply(seq_along(yrs), function(i) win(as.Date(jc[i]), 30) %>% mutate(year = yrs[i], center = jc[i], rev = as.Date(jc[i]) %in% xo)))
js <- jl %>% group_by(rev) %>% summarise(across(c(cap_pre, cap_post, unc_pre, unc_post), sum))
pd <- bind_rows(lapply(sort(real), function(R) win(R, 45) %>% mutate(R = as.character(R))))
# triple difference on the July windows: post x capped x revision year, year-by-cap fixed effects, robust standard errors
jd <- bind_rows(lapply(seq_len(nrow(jl)), function(i) data.frame(year = jl$year[i], rev = as.integer(jl$rev[i]), cap = c(1, 1, 0, 0), post = c(0, 1, 0, 1),
  n = c(jl$cap_pre[i], jl$cap_post[i], jl$unc_pre[i], jl$unc_post[i]))))
ej <- fepois(n ~ post + post:cap + post:rev + post:cap:rev | year^cap, jd, vcov = "hetero", notes = FALSE, warn = FALSE)
cnt <- list(capped_pre = rr$pre[rr$cap == 1], capped_post = rr$post[rr$cap == 1], uncapped_pre = rr$pre[rr$cap == 0], uncapped_post = rr$post[rr$cap == 0],
  placebo_capped_pre = rf$pre[rf$cap == 1], placebo_capped_post = rf$post[rf$cap == 1], placebo_uncapped_pre = rf$pre[rf$cap == 0], placebo_uncapped_post = rf$post[rf$cap == 0],
  n_dates = length(real), n_increase = sum(pd$cap_post > pd$cap_pre), n_decrease = sum(pd$cap_post < pd$cap_pre), n_tie = sum(pd$cap_post == pd$cap_pre),
  irr = exp(res$b), excl_2023m3 = { x <- est(cr %>% filter(R != "2023-03-01"), "n"); list(b = x$b, se = x$se, irr = exp(x$b), dates = 12) },
  real_dates = as.character(real), extraordinary_dates = as.character(xo), july_centers = jc, july_ddd_b = unname(coef(ej)["post:cap:rev"]), july_ddd_se = unname(se(ej)["post:cap:rev"]), july_ddd_p = unname(pvalue(ej)["post:cap:rev"]), july_rev = as.list(js[js$rev, -1]), july_norev = as.list(js[!js$rev, -1]), per_date = pd, july_by_year = jl,
  first_date = as.character(min(ah$date)), last_date = as.character(max(ah$date)))
jsonlite::write_json(cnt, "out/cap_timing_counts.json", auto_unbox = TRUE, pretty = TRUE, digits = 6)
pv <- function(b, s) 2 * pnorm(-abs(b / s))
stars <- function(p) ifelse(p < .01, "^{***}", ifelse(p < .05, "^{**}", ifelse(p < .1, "^{*}", "")))
num <- function(x, d = 3) formatC(x, format = "f", digits = d)
row <- function(l, v) paste0(l, " & ", paste(v, collapse = " & "), "\\\\")
t <- c("% structure: rows = coefficients and fit statistics, columns = date sets and outcomes; generated by n19_cap_timing.R",
  "\\begin{tabular}{@{}l cccc@{}}", "\\toprule",
  "& \\multicolumn{4}{c}{PPML; count in a 45-day half-window}\\\\", "\\cmidrule(l){2-5}",
  "& \\multicolumn{2}{c}{Announcements} & \\multicolumn{2}{c}{Units offered}\\\\", "\\cmidrule(lr){2-3}\\cmidrule(l){4-5}",
  row("", c("Revision dates", "Placebo dates", "Revision dates", "Placebo dates")),
  row("", paste0("(", 1:4, ")")), "\\midrule",
  row("Post $\\times$ capped", paste0("$", num(res$b), stars(pv(res$b, res$se)), "$")),
  row("", paste0("$(", num(res$se), ")$")),
  row("\\quad Implied ratio, $\\exp(\\hat b)$", num(exp(res$b), 2)),
  "\\addlinespace[3pt]",
  row("Post", paste0("$", num(res$bp), stars(pv(res$bp, res$sep)), "$")),
  row("", paste0("$(", num(res$sep), ")$")),
  "\\midrule",
  row("\\quad Date $\\times$ capped fixed effects", rep("Yes", 4)),
  row("\\quad Dates (clusters)", res$clusters),
  row("\\quad Observations (cells)", res$n),
  row("\\quad Announcements or units in windows", formatC(res$events, format = "d", big.mark = "{,}")),
  row("\\quad Pseudo $R^2$", num(res$pr2)),
  "\\bottomrule", "\\end{tabular}")
writeLines(t, "tables/tab_t8_cap_timing.tex")
print(res, row.names = FALSE); print(rr); print(rf)
