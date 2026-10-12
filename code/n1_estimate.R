# Main specification: Tables 3, 4, 8 and Figure 4
#   lag-augmented panel LP: p = AIC-selected 3 + 1 = 4 lags of starts, cost growth and all controls
#   Driscoll-Kraay standard errors (bandwidth h + 1), region fixed effects, monthly permit flow
# Outputs go to out/ ; tables and figures are built from these files by n2/n3.
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(dplyr); library(fixest); library(MASS); library(jsonlite)})
set.seed(20261004)
P_MAIN <- 4

# ---------------------------------------------------------------- data
d0 <- read.csv("../data/panel_8_regions.csv", check.names = FALSE)
d0$date <- as.Date(d0$Monthly2)
names(d0) <- gsub("\\.", "_", gsub(" ", "_", names(d0)))
raw <- read.csv("../data/monthly_series_wide.csv", check.names = FALSE)
raw$date <- as.Date(substr(raw$Monthly2, 1, 10)); raw$m <- as.integer(format(raw$date, "%m"))
regs <- c("seoul", "busan", "daegu", "incheon", "gwangju", "daejeon", "ulsan", "gyeonggi")
# permits in the source file are year-to-date cumulative; convert to monthly flow
pm <- bind_rows(lapply(regs, function(g) {
  ytd <- raw[[paste0("permit_pri_", g)]]
  data.frame(date = raw$date, region = g, permit_flow = pmax(ifelse(raw$m == 1, ytd, ytd - dplyr::lag(ytd)), 0))
}))
d0 <- d0 %>% left_join(pm, by = c("date", "region")) %>% mutate(l_permit_flow = log(1 + permit_flow))
mk <- read.csv("out/markov_probs.csv"); mk$date <- as.Date(mk$Monthly2)
stopifnot(all(mk$D50 == mk$regime_dummy_paper))
d0 <- d0 %>% left_join(mk[, c("date", "Dtop10")], by = "date") %>% mutate(c_top10 = yoy_CCI * Dtop10)
Z  <- c("yoy_l_Aluminum", "yoy_l_Iron_ore", "yoy_l_Copper")
for (z in Z) d0[[paste0(z, "_D")]] <- d0[[z]] * d0$regime_dummy
ZD <- paste0(Z, "_D")
HX <- c("l_apt_trd", "l_permit_flow", "l_unsold_pri", "l_hh", "l_apt_price_idx")
MX <- c("rate_k3y", "spread_corp3y", "l_ppi", "l_fx_usdkrw")
for (r in regs[-1]) d0[[paste0("c_x_", r)]] <- d0$yoy_CCI * (d0$region == r)
RX <- paste0("c_x_", regs[-1])
d0 <- d0 %>% arrange(region, date) %>% group_by(region) %>% mutate(tid = row_number()) %>% ungroup()
lagvars <- c("l_start_pri", "yoy_CCI", HX, MX, Z, ZD, "regime_dummy", "yoy_CCI_regime", "Dtop10", "c_top10", "l_permit_pri")
for (v in lagvars) for (l in 1:P_MAIN) d0[[paste0(v, "_L", l)]] <- ave(d0[[v]], d0$region, FUN = function(x) dplyr::lag(x, l))
H <- 0:24
lag_terms <- function(vars, p) as.vector(outer(vars, seq_len(p), function(a, b) paste0(a, "_L", b)))
dep <- function(dat, h) dat %>% group_by(region) %>% mutate(y = dplyr::lead(l_start_pri, h) - l_start_pri_L1) %>% ungroup()
dk  <- function(h) DK(lag = h + 1)
pv  <- function(b, s) 2 * pnorm(-abs(b / s))

# ---------------------------------------------------------------- spec builders
ctrl <- function(p, regime = NULL, extra = NULL) {
  v <- lag_terms(c("l_start_pri", "yoy_CCI", HX, MX), p)
  if (!is.null(regime)) v <- c(v, lag_terms(regime, p))
  c(v, extra)
}
fit_ols <- function(dat, h, p, model = c("avg", "reg", "region"), regime = c("regime_dummy", "yoy_CCI_regime"), regime_lags = FALSE) {
  model <- match.arg(model)
  dd <- dep(dat, h)
  rhs <- switch(model,
    avg    = c("yoy_CCI", ctrl(p)),
    reg    = c("yoy_CCI", regime, ctrl(p, if (regime_lags) regime else NULL)),
    region = c("yoy_CCI", RX, ctrl(p)))
  feols(as.formula(paste("y ~", paste(rhs, collapse = " + "), "| region")), dd,
        panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
}
total <- function(e, terms) { b <- coef(e); V <- vcov(e); c(b = sum(b[terms]), se = sqrt(sum(V[terms, terms]))) }

# ---------------------------------------------------------------- 1) OLS-LP, new baseline, all horizons
main <- bind_rows(lapply(H, function(h) {
  ea <- fit_ols(d0, h, P_MAIN, "avg"); er <- fit_ols(d0, h, P_MAIN, "reg")
  tr <- total(er, c("yoy_CCI", "yoy_CCI_regime"))
  data.frame(h = h,
             avg_b = unname(coef(ea)["yoy_CCI"]), avg_se = unname(se(ea)["yoy_CCI"]), avg_n = nobs(ea), avg_r2 = unname(r2(ea, "r2")),
             reg_b = unname(tr["b"]), reg_se = unname(tr["se"]), reg_n = nobs(er), reg_r2 = unname(r2(er, "r2")),
             reg_beta = unname(coef(er)["yoy_CCI"]), reg_beta_se = unname(se(er)["yoy_CCI"]), reg_theta = unname(coef(er)["yoy_CCI_regime"]),
             n_periods = nobs(ea) / length(regs))
}))
main$avg_p <- pv(main$avg_b, main$avg_se); main$reg_p <- pv(main$reg_b, main$reg_se)

# ---------------------------------------------------------------- 2) IV-LP, new baseline (lags of instruments in both stages)
iv_rows <- lapply(H, function(h) {
  dd <- dep(d0, h)
  ca <- c(ctrl(P_MAIN), lag_terms(Z, P_MAIN))
  ea <- feols(as.formula(paste("y ~", paste(ca, collapse = " + "), "| region | yoy_CCI ~", paste(Z, collapse = " + "))),
              dd, panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
  fs <- feols(as.formula(paste("yoy_CCI ~", paste(c(Z, ca), collapse = " + "), "| region")), dd[!is.na(dd$y), ],
              panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
  Fa <- wald(fs, keep = paste0("^(", paste(Z, collapse = "|"), ")$"), print = FALSE)$stat
  cr <- c(ctrl(P_MAIN), lag_terms(Z, P_MAIN))
  er <- feols(as.formula(paste("y ~ regime_dummy +", paste(cr, collapse = " + "), "| region | yoy_CCI + yoy_CCI_regime ~",
                               paste(c(Z, ZD), collapse = " + "))), dd, panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
  Fr <- sapply(c("yoy_CCI", "yoy_CCI_regime"), function(en) {
    f1 <- feols(as.formula(paste(en, "~ regime_dummy +", paste(c(Z, ZD, cr), collapse = " + "), "| region")), dd[!is.na(dd$y), ],
                panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
    wald(f1, keep = paste0("^(", paste(c(Z, ZD), collapse = "|"), ")$"), print = FALSE)$stat })
  tr <- total(er, c("fit_yoy_CCI", "fit_yoy_CCI_regime"))
  data.frame(h = h, iva_b = unname(coef(ea)["fit_yoy_CCI"]), iva_se = unname(se(ea)["fit_yoy_CCI"]), iva_n = nobs(ea), iva_F = Fa,
             ivr_b = unname(tr["b"]), ivr_se = unname(tr["se"]), ivr_n = nobs(er), ivr_F1 = unname(Fr[1]), ivr_F2 = unname(Fr[2]))
})
iv <- bind_rows(iv_rows); iv$iva_p <- pv(iv$iva_b, iv$iva_se); iv$ivr_p <- pv(iv$ivr_b, iv$ivr_se)

# Anderson-Rubin 95% set for the average IV-LP (weak-instrument robust), reported horizons only
HR <- c(0, 3, 6, 9, 12, 15, 18, 24); grid <- sort(unique(c(seq(-10, -1.6, by = 0.1), seq(-1.5, 1.5, by = 0.01), seq(1.6, 10, by = 0.1))))
ar <- bind_rows(lapply(HR, function(h) {
  dd <- dep(d0, h); dd <- dd[!is.na(dd$y), ]
  ca <- c(Z, ctrl(P_MAIN), lag_terms(Z, P_MAIN))
  acc <- sapply(grid, function(b0) {
    dd$ytil <- dd$y - b0 * dd$yoy_CCI
    e <- feols(as.formula(paste("ytil ~", paste(ca, collapse = " + "), "| region")), dd, panel.id = ~ region + tid,
               vcov = dk(h), notes = FALSE, warn = FALSE)
    w <- wald(e, keep = paste0("^(", paste(Z, collapse = "|"), ")$"), print = FALSE)
    w$p >= 0.05 })
  inside <- grid[acc]
  lo <- if (length(inside)) min(inside) else NA; hi <- if (length(inside)) max(inside) else NA
  gaps <- length(inside) > 0 && any(diff(which(acc)) > 1)
  data.frame(h = h, ar_lo = lo, ar_hi = hi, ar_lo_open = isTRUE(acc[1]), ar_hi_open = isTRUE(acc[length(acc)]), ar_disjoint = gaps, ar_empty = !any(acc))
}))

# ---------------------------------------------------------------- 3) sup-t bands (stacked LP, joint DK covariance across horizons)
stack_model <- function(p, model) {
  st <- bind_rows(lapply(H, function(h) dep(d0, h) %>% mutate(hh = h))) %>% filter(!is.na(y)) %>%
    mutate(hf = factor(hh), unit = paste(region, hh))
  rhs <- if (model == "avg") c("yoy_CCI", ctrl(p)) else c("yoy_CCI", "regime_dummy", "yoy_CCI_regime", ctrl(p))
  e <- feols(as.formula(paste("y ~", paste(paste0("i(hf, ", rhs, ")"), collapse = " + "), "| unit")), st,
             panel.id = ~ unit + tid, vcov = DK(lag = 25), notes = FALSE, warn = FALSE)
  b <- coef(e); V <- vcov(e); n1 <- paste0("hf::", H, ":yoy_CCI")
  if (model == "avg") { est <- b[n1]; VV <- V[n1, n1] } else {
    n2 <- paste0("hf::", H, ":yoy_CCI_regime"); A <- cbind(diag(length(H)), diag(length(H)))
    est <- b[n1] + b[n2]; VV <- A %*% V[c(n1, n2), c(n1, n2)] %*% t(A) }
  se <- sqrt(diag(VV)); crit <- unname(quantile(apply(abs(mvrnorm(50000, rep(0, length(H)), cov2cor(VV))), 1, max), 0.95))
  wald_p <- function(hs) { i <- match(hs, H); w <- as.numeric(t(est[i]) %*% solve(VV[i, i]) %*% est[i]); 1 - pchisq(w, length(i)) }
  list(band = data.frame(model = model, h = H, b = unname(est), se_joint = se, crit = crit, sup_lo = unname(est) - crit * se, sup_hi = unname(est) + crit * se),
       tests = data.frame(model = model, crit = crit, p_h0_12 = wald_p(0:12), p_h18_24 = wald_p(18:24), mean_h18_24 = mean(est[H %in% 18:24])))
}
sa <- stack_model(P_MAIN, "avg"); sr <- stack_model(P_MAIN, "reg")
bands <- bind_rows(sa$band, sr$band); joint <- bind_rows(sa$tests, sr$tests)

# ---------------------------------------------------------------- 4) Table 4: regional responses (new baseline + region x cost growth)
HG <- c(0, 6, 12, 18, 24)
reg_tab <- bind_rows(lapply(HG, function(h) {
  e <- fit_ols(d0, h, P_MAIN, "region")
  bind_rows(lapply(regs, function(r) { t <- if (r == "seoul") "yoy_CCI" else c("yoy_CCI", paste0("c_x_", r)); tt <- total(e, t)
    data.frame(region = r, h = h, b = unname(tt["b"]), se = unname(tt["se"]), n = nobs(e), r2 = unname(r2(e, "r2"))) }))
}))
reg_tab$p <- pv(reg_tab$b, reg_tab$se); reg_tab$p_holm <- p.adjust(reg_tab$p, "holm")

# ---------------------------------------------------------------- 5) Table 6: robustness of the high-cost-regime total effect
HB <- c(0, 5, 10, 15, 18, 24)
orig_fit <- function(dat, h, model = "reg", vc = "cluster") {
  dd <- dep(dat, h)
  cc <- c("l_start_pri_L1", paste0(c("l_apt_trd", "l_permit_pri", "l_unsold_pri", "l_hh", "l_apt_price_idx", MX), "_L1"))
  rhs <- if (model == "reg") c("yoy_CCI", "regime_dummy", "yoy_CCI_regime", cc) else c("yoy_CCI", cc)
  fml <- as.formula(paste("y ~", paste(rhs, collapse = " + "), "| region"))
  if (vc == "cluster") feols(fml, dd, vcov = ~ date, notes = FALSE, warn = FALSE) else
    feols(fml, dd, panel.id = ~ region + tid, vcov = dk(h), notes = FALSE, warn = FALSE)
}
swap_regime <- function(dat) {
  dat$regime_dummy <- dat$Dtop10; dat$yoy_CCI_regime <- dat$c_top10
  for (l in 1:P_MAIN) { dat[[paste0("regime_dummy_L", l)]] <- dat[[paste0("Dtop10_L", l)]]; dat[[paste0("yoy_CCI_regime_L", l)]] <- dat[[paste0("c_top10_L", l)]] }
  dat }
rob_cols <- list(
  c01_base_p4  = function(h) fit_ols(d0, h, 4, "reg"),
  c02_p2       = function(h) fit_ols(d0, h, 2, "reg"),
  c03_p3       = function(h) fit_ols(d0, h, 3, "reg"),
  c04_orig     = function(h) orig_fit(d0, h, "reg", "cluster"),
  c05_orig_dk  = function(h) orig_fit(d0, h, "reg", "DK"),
  c06_top10    = function(h) fit_ols(swap_regime(d0), h, 4, "reg"),
  c07_ex_gg    = function(h) fit_ols(d0 %>% filter(region != "gyeonggi"), h, 4, "reg"),
  c08_ex_dg    = function(h) fit_ols(d0 %>% filter(region != "daegu"), h, 4, "reg"),
  c09_reglags  = function(h) fit_ols(d0, h, 4, "reg", regime_lags = TRUE))
rob <- bind_rows(lapply(names(rob_cols), function(cn) bind_rows(lapply(HB, function(h) {
  e <- rob_cols[[cn]](h); tt <- total(e, c("yoy_CCI", "yoy_CCI_regime"))
  data.frame(col = cn, h = h, b = unname(tt["b"]), se = unname(tt["se"]), n = nobs(e), r2 = unname(r2(e, "r2"))) }))))
rob$p <- pv(rob$b, rob$se)

# ---------------------------------------------------------------- 6) original specification paths (for comparison figures)
orig <- bind_rows(lapply(H, function(h) {
  ea <- orig_fit(d0, h, "avg"); er <- orig_fit(d0, h, "reg"); tr <- total(er, c("yoy_CCI", "yoy_CCI_regime"))
  data.frame(h = h, avg_b = unname(coef(ea)["yoy_CCI"]), avg_se = unname(se(ea)["yoy_CCI"]), reg_b = unname(tr["b"]), reg_se = unname(tr["se"]))
}))

# ---------------------------------------------------------------- 7) size of a cost-growth innovation (one obs per month)
nat <- d0 %>% filter(region == "seoul")
inn <- feols(as.formula(paste("yoy_CCI ~", paste(lag_terms(c("yoy_CCI", MX), P_MAIN), collapse = " + "))), nat, notes = FALSE)
sd_innov <- sd(resid(inn))

# ---------------------------------------------------------------- save
write.csv(main, "out/est_main.csv", row.names = FALSE); write.csv(iv, "out/est_iv.csv", row.names = FALSE)
write.csv(ar, "out/est_iv_ar.csv", row.names = FALSE); write.csv(bands, "out/bands_supt.csv", row.names = FALSE)
write.csv(joint, "out/joint_tests.csv", row.names = FALSE); write.csv(reg_tab, "out/est_region.csv", row.names = FALSE)
write.csv(rob, "out/est_robust.csv", row.names = FALSE); write.csv(orig, "out/est_orig.csv", row.names = FALSE)
write_json(list(p_main = P_MAIN, sd_innovation_pp = sd_innov, n_regions = length(regs),
                sample = c(as.character(min(d0$date[!is.na(d0$l_start_pri_L4)])), as.character(max(d0$date)))),
           "out/meta.json", auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("done. SD of cost-growth innovation = %.3f pp | sample from %s\n", sd_innov, min(d0$date[!is.na(d0$l_start_pri_L4)])))
print(joint)
