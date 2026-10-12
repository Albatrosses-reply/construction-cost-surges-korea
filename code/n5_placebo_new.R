# Placebo 18-month regime windows (Figure 5; 4 lags, monthly permits); point estimates only
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(dplyr); library(fixest)})
src <- readLines("n1_estimate.R"); stop_at <- grep("^# -+ spec builders", src)
eval(parse(text = src[1:(stop_at - 1)]))                     # data construction identical to n1
dep2 <- function(h) d0 %>% group_by(region) %>% mutate(y = dplyr::lead(l_start_pri, h) - l_start_pri_L1) %>% ungroup()
HH <- c(11, 16, 17, 18, 19); dats <- lapply(HH, dep2); names(dats) <- HH
ctl <- paste(lag_terms(c("l_start_pri", "yoy_CCI", HX, MX), 4), collapse = " + ")
months <- sort(unique(d0$date))
est <- function(s) {
  win <- months[months >= s][1:18]
  sapply(as.character(HH), function(hc) { dd <- dats[[hc]]; dd$D <- as.integer(dd$date %in% win); dd$cD <- dd$yoy_CCI * dd$D
    e <- feols(as.formula(paste("y ~ yoy_CCI + D + cD +", ctl, "| region")), dd, notes = FALSE, warn = FALSE)
    unname(coef(e)["yoy_CCI"] + coef(e)["cD"]) })
}
starts <- months[months >= as.Date("2011-05-01") & months <= as.Date("2023-01-01")]   # all 18 window months observed at h = 19 (window end + 19 <= 2026-01)
r <- t(sapply(starts, est)); r <- data.frame(start = starts, r); names(r) <- c("start", paste0("h", HH))
r$boom <- r$h11; r$bust <- rowMeans(r[, c("h16", "h17", "h18", "h19")])
true <- r[r$start == as.Date("2021-04-01"), ]
pl <- r[(r$start + 17 * 30.5 < as.Date("2021-04-01")) | (r$start > as.Date("2022-09-01")), ]
cat(sprintf("true window: boom h11 = %.3f, bust mean(h16-19) = %.3f | placebo windows N = %d\n", true$boom, true$bust, nrow(pl)))
cat(sprintf("share placebo boom >= true: %.3f | share placebo bust <= true: %.3f | both: %.3f\n",
            mean(pl$boom >= true$boom), mean(pl$bust <= true$bust), mean(pl$boom >= true$boom & pl$bust <= true$bust)))
write.csv(r, "out/placebo_windows_new.csv", row.names = FALSE)
