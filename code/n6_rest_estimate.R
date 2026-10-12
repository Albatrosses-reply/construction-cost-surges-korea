# Estimates for Tables 1, 2, 5 and A.2
#   Table 1 (summary statistics)   Table 2 (regional structure)
#   Table 5 (new-sale price LP)   Table A.2 (coefficient sums of lags)   first stage of the instrumented projections (not reported)
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(dplyr); library(fixest); library(jsonlite)})
src <- readLines("n1_estimate.R"); stop_at <- grep("^# -+ spec builders", src)
eval(parse(text = src[1:(stop_at - 1)]))                     # same data construction as n1 (d0, HX, MX, Z, ZD, lag_terms, dep, dk, pv)
ctrl <- function(p) lag_terms(c("l_start_pri", "yoy_CCI", HX, MX), p)

# ---------------------------------------------------------------- Table 1: summary statistics (full panel 2011:1-2026:1)
sale <- read.csv("../data/panel_8_regions_newsale_price.csv", check.names = FALSE); sale$date <- as.Date(sale$Monthly2)
names(sale) <- gsub("\\.", "_", gsub(" ", "_", names(sale)))
vars1 <- list(
  c("Dependent variable", "Housing starts (log)", "l_start_pri"),
  c("Main regressor", "Construction cost growth (\\%, YoY)", "yoy_CCI"),
  c("Main regressor", "High-cost regime (dummy)", "regime_dummy"),
  c("Housing-market controls (log)", "Housing permits (monthly)", "l_permit_flow"),
  c("Housing-market controls (log)", "Unsold housing units", "l_unsold_pri"),
  c("Housing-market controls (log)", "Registered households", "l_hh"),
  c("Housing-market controls (log)", "Apartment transaction volume", "l_apt_trd"),
  c("Housing-market controls (log)", "Apartment sales price index", "l_apt_price_idx"),
  c("Macroeconomic controls", "Three-year Treasury yield (\\%)", "rate_k3y"),
  c("Macroeconomic controls", "Corporate bond spread (\\%)", "spread_corp3y"),
  c("Macroeconomic controls", "Producer price index (log)", "l_ppi"),
  c("Macroeconomic controls", "KRW/USD exchange rate (log)", "l_fx_usdkrw"),
  c("Instruments (\\%, YoY)", "Iron-ore price growth", "yoy_l_Iron_ore"),
  c("Instruments (\\%, YoY)", "Aluminum price growth", "yoy_l_Aluminum"),
  c("Instruments (\\%, YoY)", "Copper price growth", "yoy_l_Copper"))
t1 <- bind_rows(lapply(vars1, function(v) { x <- d0[[v[3]]]; x <- x[!is.na(x)]
  data.frame(block = v[1], label = v[2], var = v[3], n = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x)) }))
xs <- sale$l_average_sale_price[!is.na(sale$l_average_sale_price)]
t1 <- bind_rows(t1, data.frame(block = "Auxiliary dependent variable", label = "Average new-sale price (log)", var = "l_average_sale_price",
                               n = length(xs), mean = mean(xs), sd = sd(xs), min = min(xs), max = max(xs)))
old_permit <- d0$l_permit_pri; t1_old_permit <- c(mean = mean(old_permit), sd = sd(old_permit), min = min(old_permit), max = max(old_permit))

# ---------------------------------------------------------------- Table 2: regional structure + surge change in unsold inventory
regs <- c("seoul", "busan", "daegu", "incheon", "gwangju", "daejeon", "ulsan", "gyeonggi")
dts <- sort(unique(d0$date)); r0 <- min(d0$date[d0$regime_dummy == 1]); i0 <- which(dts == r0); pre <- dts[(i0 - 24):(i0 - 1)]
t2 <- bind_rows(lapply(regs, function(g) { s <- d0[d0$region == g, ]
  data.frame(region = g, starts = mean(s$l_start_pri), permits = mean(s$l_permit_flow), permits_ytd = mean(s$l_permit_pri),
             unsold = mean(s$l_unsold_pri), trans = mean(s$l_apt_trd), price = mean(s$l_apt_price_idx),
             d_unsold = 100 * (mean(s$l_unsold_pri[s$regime_dummy == 1]) - mean(s$l_unsold_pri[s$date %in% pre]))) }))

# ---------------------------------------------------------------- Table 5: new-sale price LP (lags 1-4 of price, cost growth, controls)
sale <- sale %>% left_join(d0 %>% dplyr::select(date, region, l_permit_flow), by = c("date", "region")) %>% arrange(region, date)
sale <- sale %>% group_by(region) %>% mutate(tid = row_number()) %>% ungroup()
for (v in c("l_average_sale_price", "yoy_CCI", HX, MX)) for (l in 1:P_MAIN) sale[[paste0(v, "_L", l)]] <- ave(sale[[v]], sale$region, FUN = function(x) dplyr::lag(x, l))
cp <- lag_terms(c("l_average_sale_price", "yoy_CCI", HX, MX), P_MAIN)
HP <- c(0, 5, 10, 15, 18, 24)
t5 <- bind_rows(lapply(HP, function(h) {
  dd <- sale %>% group_by(region) %>% mutate(y = dplyr::lead(l_average_sale_price, h) - l_average_sale_price_L1) %>% ungroup()
  ea <- feols(as.formula(paste("y ~ yoy_CCI +", paste(cp, collapse = " + "), "| region")), dd, panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
  er <- feols(as.formula(paste("y ~ yoy_CCI + regime_dummy + yoy_CCI_regime +", paste(cp, collapse = " + "), "| region")), dd, panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
  w <- c("yoy_CCI", "yoy_CCI_regime"); V <- vcov(er)
  data.frame(h = h, avg_b = unname(coef(ea)["yoy_CCI"]), avg_se = unname(se(ea)["yoy_CCI"]), avg_r2 = unname(r2(ea, "r2")), avg_n = nobs(ea),
             beta = unname(coef(er)["yoy_CCI"]), beta_se = unname(se(er)["yoy_CCI"]),
             reg_b = sum(coef(er)[w]), reg_se = sqrt(sum(V[w, w])), reg_r2 = unname(r2(er, "r2")), reg_n = nobs(er))
}))
sale_first <- min(sale$date[!is.na(sale$l_average_sale_price_L4) & !is.na(sale$l_permit_flow_L4)])

# ---------------------------------------------------------------- Table A.2: average-response LP, coefficient on c_t and sums of lags 1-4
HA <- c(0, 6, 12, 18, 24); lagvars2 <- c("l_start_pri", "yoy_CCI", HX, MX)
full_coef <- list()
tA2 <- bind_rows(lapply(HA, function(h) {
  dd <- dep(d0, h)
  e <- feols(as.formula(paste("y ~ yoy_CCI +", paste(ctrl(P_MAIN), collapse = " + "), "| region")), dd, panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
  b <- coef(e); V <- vcov(e)
  full_coef[[as.character(h)]] <<- data.frame(h = h, term = names(b), b = unname(b), se = unname(sqrt(diag(V))))
  sums <- bind_rows(lapply(lagvars2, function(v) { tt <- paste0(v, "_L", 1:P_MAIN)
    W <- wald(e, keep = paste0("^(", paste(tt, collapse = "|"), ")$"), print = FALSE)
    data.frame(h = h, term = v, b = sum(b[tt]), se = sqrt(sum(V[tt, tt])), joint_p = W$p) }))
  bind_rows(data.frame(h = h, term = "yoy_CCI_t", b = unname(b["yoy_CCI"]), se = unname(sqrt(V["yoy_CCI", "yoy_CCI"])), joint_p = NA), sums) %>%
    mutate(r2 = unname(r2(e, "r2")), n = nobs(e))
}))

# ---------------------------------------------------------------- Table A.3: first stage at h = 0 (Driscoll-Kraay)
dd0 <- dep(d0, 0); dd0 <- dd0[!is.na(dd0$y), ]
ca <- c(ctrl(P_MAIN), lag_terms(Z, P_MAIN))
fs_one <- function(lhs, inst, extra = NULL) {
  e <- feols(as.formula(paste(lhs, "~", paste(c(inst, extra, ca), collapse = " + "), "| region")), dd0, panel.id = ~ region + tid, vcov = dk(0), notes = FALSE, warn = FALSE)
  W <- wald(e, keep = paste0("^(", paste(inst, collapse = "|"), ")$"), print = FALSE)
  list(coef = data.frame(lhs = lhs, term = inst, b = unname(coef(e)[inst]), se = unname(se(e)[inst])),
       stats = data.frame(lhs = lhs, F = W$stat, F_p = W$p, r2 = unname(r2(e, "r2")), n = nobs(e)))
}
f1 <- fs_one("yoy_CCI", Z); f2 <- fs_one("yoy_CCI", c(Z, ZD), "regime_dummy"); f3 <- fs_one("yoy_CCI_regime", c(Z, ZD), "regime_dummy")
tA3 <- list(coef = bind_rows(f1$coef %>% mutate(col = 1), f2$coef %>% mutate(col = 2), f3$coef %>% mutate(col = 3)),
            stats = bind_rows(f1$stats %>% mutate(col = 1), f2$stats %>% mutate(col = 2), f3$stats %>% mutate(col = 3)))

# ---------------------------------------------------------------- save
write.csv(t1, "out/t1_sumstat_new.csv", row.names = FALSE); write.csv(t2, "out/t2_region_struct_new.csv", row.names = FALSE)
write.csv(t5, "out/t5_price_new.csv", row.names = FALSE); write.csv(tA2, "out/tA2_lagsums_new.csv", row.names = FALSE)
write.csv(bind_rows(full_coef), "out/tA2_full_coefficients_all_lags.csv", row.names = FALSE)
write.csv(tA3$coef, "out/tA3_firststage_coef.csv", row.names = FALSE); write.csv(tA3$stats, "out/tA3_firststage_stats.csv", row.names = FALSE)
write_json(list(sale_first = as.character(sale_first), old_permit_stats = t1_old_permit), "out/meta_rest.json", auto_unbox = TRUE, pretty = TRUE)
cat("done | sale-price estimation sample starts", as.character(sale_first), "\n")
print(t5, digits = 3); print(t2, digits = 3); print(tA3$stats, digits = 3)
