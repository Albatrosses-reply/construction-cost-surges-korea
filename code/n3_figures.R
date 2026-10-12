# Figure 4 (one PNG per panel), the instrumented comparison and a comparison with a one-lag specification (not reported)
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(tidyr)})
main <- read.csv("out/est_main.csv"); iv <- read.csv("out/est_iv.csv"); bands <- read.csv("out/bands_supt.csv"); orig <- read.csv("out/est_orig.csv")

# palette: two categorical colours
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; BAND95 <- "#cde2fb"; BAND90 <- "#9ec5f4"; SUPT <- "#1c5cab"
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"; GREY <- "#8a8986"; GREYBAND <- "#e7e6e2"
dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
# save to a temp file and copy (avoids problems with non-ASCII paths)
save_png <- function(file, plot, ...) { tmp <- tempfile(fileext = ".png"); ggsave(tmp, plot, device = dev, bg = "white", ...); stopifnot(file.copy(tmp, file, overwrite = TRUE)) }

theme_irf <- function() theme_minimal(base_size = 12) + theme(
  plot.title = element_text(face = "bold", size = 13, colour = INK, hjust = 0, margin = margin(b = 6)),
  axis.title = element_text(colour = INK2, size = 11), axis.text = element_text(colour = INK2, size = 10),
  panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = GRID, linewidth = 0.4),
  legend.position = "none", plot.margin = margin(10, 14, 8, 8))
xh <- scale_x_continuous(breaks = seq(0, 24, 6), expand = expansion(mult = c(0.02, 0.04)))
ylab <- "Cumulative response of log housing starts"

irf_panel <- function(b, se, band, title) {
  d <- data.frame(h = 0:24, b = b, lo95 = b - 1.96 * se, hi95 = b + 1.96 * se, lo90 = b - 1.645 * se, hi90 = b + 1.645 * se,
                  slo = band$sup_lo, shi = band$sup_hi)
  ggplot(d, aes(h)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = 0.45) +
    geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = BAND95) +
    geom_ribbon(aes(ymin = lo90, ymax = hi90), fill = BAND90) +
    geom_line(aes(y = slo), colour = SUPT, linewidth = 0.55, linetype = "22") +
    geom_line(aes(y = shi), colour = SUPT, linewidth = 0.55, linetype = "22") +
    geom_line(aes(y = b), colour = BLUE, linewidth = 1) +
    geom_point(aes(y = b), colour = BLUE, fill = BLUE, size = 1.9, shape = 21, stroke = 0.5) +
    xh + labs(title = title, x = "Horizon (months)", y = ylab) + theme_irf()
}
ba <- bands[bands$model == "avg", ]; br <- bands[bands$model == "reg", ]
g3a <- irf_panel(main$avg_b, main$avg_se, ba, "(a) Average response")
g3b <- irf_panel(main$reg_b, main$reg_se, br, "(b) Total effect in the high-cost regime")
save_png("figures/fig3a_avg_irf_new.png", g3a, width = 5.4, height = 3.6, dpi = 300)
save_png("figures/fig3b_regime_irf_new.png", g3b, width = 5.4, height = 3.6, dpi = 300)

# Figure 4: OLS-LP vs IV-LP (new specification), 90% band around OLS
ivols_panel <- function(b_ols, se_ols, b_iv, title) {
  d <- data.frame(h = 0:24, ols = b_ols, iv = b_iv, lo = b_ols - 1.645 * se_ols, hi = b_ols + 1.645 * se_ols)
  lab <- data.frame(h = 24, y = c(d$ols[25], d$iv[25]), txt = c("OLS-LP", "IV-LP"), col = c(BLUE, ORANGE))
  ggplot(d, aes(h)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = 0.45) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = BAND95) +
    geom_line(aes(y = iv), colour = ORANGE, linewidth = 0.9, linetype = "52") +
    geom_point(aes(y = iv), colour = "white", fill = ORANGE, shape = 24, size = 2.2, stroke = 0.6) +
    geom_line(aes(y = ols), colour = BLUE, linewidth = 1) +
    geom_point(aes(y = ols), colour = "white", fill = BLUE, shape = 21, size = 2.2, stroke = 0.6) +
    geom_text(data = lab, aes(x = h + 0.6, y = y, label = txt), colour = INK, hjust = 0, size = 3.4) +
    scale_x_continuous(breaks = seq(0, 24, 6), limits = c(0, 28.5), expand = expansion(mult = c(0.02, 0))) +
    labs(title = title, x = "Horizon (months)", y = "Cumulative response of log starts") + theme_irf()
}
g4a <- ivols_panel(main$avg_b, main$avg_se, iv$iva_b, "(a) Average response")
g4b <- ivols_panel(main$reg_b, main$reg_se, iv$ivr_b, "(b) Total effect in the high-cost regime")
save_png("figures/fig4a_ivols_new.png", g4a, width = 4.6, height = 3.8, dpi = 300)
save_png("figures/fig4b_ivols_new.png", g4b, width = 4.6, height = 3.8, dpi = 300)

# Comparison with a one-lag specification (95% pointwise bands, not reported)
cmp <- bind_rows(
  data.frame(panel = "(a) Average response", spec = "Original (1 lag, month-clustered)", h = 0:24, b = orig$avg_b, se = orig$avg_se),
  data.frame(panel = "(a) Average response", spec = "New (4 lags, Driscoll-Kraay)", h = 0:24, b = main$avg_b, se = main$avg_se),
  data.frame(panel = "(b) Total effect in the high-cost regime", spec = "Original (1 lag, month-clustered)", h = 0:24, b = orig$reg_b, se = orig$reg_se),
  data.frame(panel = "(b) Total effect in the high-cost regime", spec = "New (4 lags, Driscoll-Kraay)", h = 0:24, b = main$reg_b, se = main$reg_se)) %>%
  mutate(lo = b - 1.96 * se, hi = b + 1.96 * se, spec = factor(spec, levels = c("Original (1 lag, month-clustered)", "New (4 lags, Driscoll-Kraay)")))
endlab <- cmp %>% filter(h == 24) %>% mutate(txt = ifelse(grepl("^New", spec), "New", "Original"))
gc <- ggplot(cmp, aes(h, b, colour = spec, fill = spec)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.45) +
  geom_ribbon(aes(ymin = lo, ymax = hi), colour = NA, alpha = 0.55) +
  geom_line(linewidth = 1) + geom_point(size = 1.6) +
  geom_text(data = endlab, aes(x = 24.7, label = txt), hjust = 0, size = 3.4, colour = INK, show.legend = FALSE) +
  facet_wrap(~ panel, nrow = 1) +
  scale_colour_manual(values = c(GREY, BLUE)) + scale_fill_manual(values = c(GREYBAND, BAND95)) +
  scale_x_continuous(breaks = seq(0, 24, 6), limits = c(0, 28), expand = expansion(mult = c(0.02, 0))) +
  labs(x = "Horizon (months)", y = ylab, colour = NULL, fill = NULL,
       title = "Original vs. lag-augmented specification",
       subtitle = "Point estimates with 95% pointwise bands; cost growth in percentage points (YoY)") +
  theme_irf() + theme(legend.position = "top", legend.justification = "left", strip.text = element_text(face = "bold", size = 11, hjust = 0, colour = INK),
                      plot.subtitle = element_text(colour = INK2, size = 10, margin = margin(b = 6)))
save_png("figures/cmp_fig3_old_vs_new.png", gc, width = 10.5, height = 4.4, dpi = 200)

writeLines(c(
  "Figure 3 caption (new): Dynamic response of housing starts to construction cost growth. Panel (a): average response. Panel (b): total effect in the high-cost regime. Lag-augmented local projections with four lags of all variables. Shaded areas are 90% and 95% pointwise confidence bands from Driscoll-Kraay standard errors; dashed lines are 95% sup-t uniform bands (Montiel Olea and Plagborg-Moller, 2019) from the joint covariance across horizons.",
  "",
  "Figure 4 caption (new): Ordinary least squares versus instrumented local projections. Panel (a): average response. Panel (b): total effect in the high-cost regime. Solid lines with circles are OLS-LP; dashed lines with triangles are IV-LP, instrumenting construction-cost growth with the global price growth of iron ore, aluminum, and copper. Both use four lags of all variables (and of the instruments for IV-LP). Shaded areas are 90% pointwise bands (Driscoll-Kraay) around the OLS path. Driscoll-Kraay first-stage F statistics are reported in Table 3."),
  "figures/captions_new.txt")
cat("figures written\n")
