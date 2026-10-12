# Figures 2 and 3: year-on-year construction cost growth with the high-cost regime, and housing starts in four regions
# (trailing twelve-month moving average indexed to its 2019 mean). Needs out/markov_probs.csv from n0_markov.R.
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressMessages({library(dplyr); library(ggplot2)})
dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
save_png <- function(file, plot, ...) { tmp <- tempfile(fileext = ".png"); ggsave(tmp, plot, device = dev, bg = "white", dpi = 300, ...); stopifnot(file.copy(tmp, file, overwrite = TRUE)) }
dir.create("figures", showWarnings = FALSE)

mk <- read.csv("out/markov_probs.csv"); mk$date <- as.Date(substr(mk$Monthly2, 1, 10))
reg <- range(mk$date[mk$D50 == 1]); reg_end <- seq(reg[2], by = "month", length.out = 2)[2]
band <- annotate("rect", xmin = reg[1], xmax = reg_end, ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.8)

d <- read.csv("../data/panel_8_regions.csv", check.names = FALSE); d$date <- as.Date(substr(d$Monthly2, 1, 10))
cg <- d %>% group_by(date) %>% summarise(c = first(yoy_CCI), .groups = "drop")
p2 <- ggplot(cg, aes(date, c)) + band +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", colour = "grey35") +
  geom_line(linewidth = 0.8) + labs(x = NULL, y = "Construction cost growth (%, year-on-year)") +
  theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank())
save_png("figures/fig_cost_growth.png", p2, width = 6.6, height = 3.4)

lab <- c(daegu = "Daegu", seoul = "Seoul", incheon = "Incheon", gyeonggi = "Gyeonggi")
ev <- d %>% filter(region %in% names(lab)) %>% arrange(region, date) %>% group_by(region) %>%
  mutate(lvl = exp(`l.start_pri`), ma = as.numeric(stats::filter(lvl, rep(1 / 12, 12), sides = 1)),
         idx = 100 * ma / mean(ma[format(date, "%Y") == "2019"])) %>%
  ungroup() %>% filter(date >= as.Date("2019-01-01"), !is.na(idx)) %>% mutate(region = factor(lab[region], levels = lab))
p3 <- ggplot(ev, aes(date, idx, colour = region, linetype = region)) + band +
  geom_hline(yintercept = 100, colour = "grey50", linewidth = 0.4) + geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(Daegu = "black", Seoul = "grey45", Incheon = "black", Gyeonggi = "grey45"), name = NULL) +
  scale_linetype_manual(values = c(Daegu = "solid", Seoul = "solid", Incheon = "22", Gyeonggi = "42"), name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Housing starts, 12-month moving average (2019 = 100)") +
  theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = c(0.13, 0.2),
                                   legend.background = element_rect(colour = "grey70"))
save_png("figures/fig_event.png", p3, width = 6.0, height = 4.1)
cat("figures 2-3 written; last month", format(max(ev$date)), "\n")
print(ev %>% group_by(region) %>% summarise(last = round(last(idx)), min = round(min(idx))))
