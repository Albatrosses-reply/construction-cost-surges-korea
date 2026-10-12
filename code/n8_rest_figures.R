# Figures 6-7: regional maps (h = 6, 12, 18, 24 and h = 18 with capital-region inset), and a
# scatter of the h = 24 response against the surge change in unsold inventory (not reported).
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
suppressMessages({library(sf); library(ggplot2); library(dplyr); library(scales); library(ggrepel); library(patchwork)})
dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
save_png <- function(file, plot, ...) { tmp <- tempfile(fileext = ".png"); ggsave(tmp, plot, device = dev, bg = "white", ...); stopifnot(file.copy(tmp, file, overwrite = TRUE)) }
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"; BLUE <- "#2a78d6"; BAND95 <- "#cde2fb"
NEG <- "#B2182B"; POS <- "#2166AC"; MID <- "#f0efec"; OUTSIDE <- "#d4d2cc"

# province boundaries: Statistics Korea 2018, via the public southkorea-maps repository
gj <- tempfile(fileext = ".geojson"); download.file("https://raw.githubusercontent.com/southkorea/southkorea-maps/master/kostat/2018/json/skorea-provinces-2018-geo.json", gj, quiet = TRUE, mode = "wb")  # Statistics Korea 2018 province boundaries
kor <- st_read(gj, quiet = TRUE) |> st_make_valid() |> st_transform(5179)
map_eng <- c(Seoul = "seoul", Busan = "busan", Daegu = "daegu", Incheon = "incheon", Gwangju = "gwangju", Daejeon = "daejeon", Ulsan = "ulsan", "Gyeonggi-do" = "gyeonggi")
kor$slug <- unname(map_eng[kor$name_eng]); kor$disp <- sub("-do$", "", kor$name_eng)

reg <- read.csv("out/est_region.csv")
st_s <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
get_h <- function(h) reg[reg$h == h, ] |> transmute(slug = region, est = b, lab_sig = paste0(st_s(p), ifelse(p_holm < 0.10, "†", "")))
pct <- function(v) sprintf("%+.1f%%", v * 100)
LIM <- 0.40
fillscale <- scale_fill_gradient2(low = NEG, mid = MID, high = POS, midpoint = 0, limits = c(-LIM, LIM), oob = squish, na.value = OUTSIDE,
  name = "Cumulative response of log housing starts to a 1 pp cost-growth innovation (in %)",
  labels = function(x) sprintf("%+.0f%%", x * 100), breaks = seq(-0.4, 0.4, 0.1),
  guide = guide_colourbar(title.position = "top", title.hjust = 0.5))
make_layer <- function(h) { d <- left_join(kor, get_h(h), by = "slug"); d$lab <- ifelse(is.na(d$slug), NA, paste0(d$disp, "\n", pct(d$est), d$lab_sig)); d }
cent <- function(d) { xy <- st_coordinates(st_centroid(st_geometry(d))); cbind(as.data.frame(d)[, c("disp", "slug", "est", "lab")], x = xy[, 1], y = xy[, 2]) }
theme_map <- function() theme_void(base_size = 11) + theme(
  plot.title = element_text(face = "bold", size = 13, hjust = 0, colour = INK), plot.subtitle = element_text(size = 10, colour = INK2, hjust = 0),
  legend.position = "bottom", legend.title = element_text(size = 9.5, colour = INK2), legend.text = element_text(colour = INK2),
  legend.key.width = unit(1.7, "cm"), legend.key.height = unit(0.3, "cm"))

# ---------------- Figure 6: h = 18 with capital-region inset
H <- 18; d <- make_layer(H); lab <- cent(d) |> filter(!is.na(slug))
hero <- ggplot(d) +
  geom_sf(aes(fill = est), colour = "white", linewidth = 0.25) +
  geom_sf(data = filter(d, !is.na(slug)), fill = NA, colour = "grey20", linewidth = 0.45) + fillscale +
  geom_text_repel(data = lab, aes(x, y, label = lab), size = 3.0, lineheight = 0.9, fontface = "bold", seed = 1, min.segment.length = 0,
                  segment.size = 0.3, box.padding = 0.5, point.padding = 0.3, colour = "grey10", bg.color = "white", bg.r = 0.12) +
  coord_sf(datum = NA) +
  labs(title = "Regional response of housing starts to construction cost growth",
       subtitle = paste0("Cumulative effect ", H, " months after a cost-growth innovation (eight-region sample; grey = not in sample)")) + theme_map()
cap <- d |> filter(slug %in% c("seoul", "incheon", "gyeonggi")); bb <- st_bbox(cap)
padx <- (bb["xmax"] - bb["xmin"]) * 0.08; pady <- (bb["ymax"] - bb["ymin"]) * 0.08
labc <- cent(d) |> filter(slug %in% c("seoul", "incheon", "gyeonggi"))
inset <- ggplot(d) +
  geom_sf(aes(fill = est), colour = "white", linewidth = 0.3) +
  geom_sf(data = filter(d, !is.na(slug)), fill = NA, colour = "grey20", linewidth = 0.5) + fillscale +
  geom_text_repel(data = labc, aes(x, y, label = lab), size = 3.1, fontface = "bold", lineheight = 0.9, seed = 2, min.segment.length = 0,
                  segment.size = 0.3, box.padding = 0.6, colour = "grey10", bg.color = "white", bg.r = 0.12) +
  coord_sf(xlim = c(bb["xmin"] - padx, bb["xmax"] + padx), ylim = c(bb["ymin"] - pady, bb["ymax"] + pady), datum = NA) +
  labs(title = "Capital region") + theme_void(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.6), plot.background = element_rect(fill = "white", colour = NA))
save_png("figures/fig6_map_h18_new.png", hero + inset_element(inset, left = 0.60, bottom = 0.58, right = 1.02, top = 1.02), width = 8.2, height = 8.6, dpi = 200)

# ---------------- Figure 5: small multiples h = 6, 12, 18, 24
panels <- lapply(c(6, 12, 18, 24), function(h) { d <- make_layer(h)
  ggplot(d) + geom_sf(aes(fill = est), colour = "white", linewidth = 0.2) +
    geom_sf(data = filter(d, !is.na(slug)), fill = NA, colour = "grey25", linewidth = 0.35) + fillscale + coord_sf(datum = NA) +
    labs(title = paste0("h = ", h, " months")) + theme_void(base_size = 10) +
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5, colour = INK), legend.position = "none") })
leg <- cowplot::get_legend(panels[[1]] + fillscale + theme(legend.position = "bottom", legend.title = element_text(size = 9.5, colour = INK2),
                                                           legend.key.width = unit(1.6, "cm"), legend.key.height = unit(0.3, "cm")))
sm <- ((panels[[1]] | panels[[2]] | panels[[3]] | panels[[4]]) / cowplot::ggdraw(leg)) + plot_layout(heights = c(1, 0.12)) +
  plot_annotation(title = "Dynamic evolution of the regional housing-start response",
                  subtitle = "Cumulative effect of a 1 pp cost-growth innovation, by horizon",
                  theme = theme(plot.title = element_text(face = "bold", size = 13, colour = INK), plot.subtitle = element_text(size = 10, colour = INK2)))
save_png("figures/fig5_map_evolution_new.png", sm, width = 11, height = 4.6, dpi = 200)

# ---------------- Figure 7: h = 24 response vs. surge change in unsold inventory
t2 <- read.csv("out/t2_region_struct_new.csv")
lab7 <- c(seoul = "Seoul", busan = "Busan", daegu = "Daegu", incheon = "Incheon", gwangju = "Gwangju", daejeon = "Daejeon", ulsan = "Ulsan", gyeonggi = "Gyeonggi")
mech <- reg[reg$h == 24, c("region", "b")] |> left_join(t2[, c("region", "d_unsold")], by = "region") |> mutate(name = lab7[region])
rho <- cor(mech$b, mech$d_unsold); rt <- cor.test(mech$b, mech$d_unsold)
write.csv(mech, "out/fig7_mech_new.csv", row.names = FALSE)
g7 <- ggplot(mech, aes(d_unsold, b)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.45) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#1c5cab", fill = BAND95, linewidth = 0.8, alpha = 0.7) +
  geom_point(shape = 21, fill = BLUE, colour = "white", size = 3.2, stroke = 0.7) +
  geom_text_repel(aes(label = name), size = 3.3, colour = INK, seed = 1, box.padding = 0.45, min.segment.length = 0, segment.colour = INK2) +
  annotate("text", x = -Inf, y = -Inf, hjust = -0.05, vjust = -1.2, size = 3.4, colour = INK2,
           label = sprintf("Correlation = %.2f (p = %.2f, 8 regions)", rho, rt$p.value)) +
  labs(x = expression("Rise in unsold inventory during the 2021-22 surge (100" %*% Delta * "log)"),
       y = "Regional cumulative start response at h = 24") +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(colour = INK2, size = 11), axis.text = element_text(colour = INK2, size = 10),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = GRID, linewidth = 0.4),
        plot.margin = margin(10, 14, 8, 8))
save_png("figures/fig7_mech_new.png", g7, width = 5.4, height = 3.9, dpi = 300)

writeLines(c(
  "Figure 5 caption (new): Dynamic evolution of the regional housing-start response at horizons of six, twelve, eighteen, and twenty-four months. Fill is the region-specific cumulative response of log housing starts to a one-percentage-point cost-growth innovation from the lag-augmented local projection (Table 4). Map lines delineate study areas and do not necessarily depict accepted national boundaries.",
  "",
  "Figure 6 caption (new): Regional response of housing starts to construction cost growth, eighteen months after a cost-growth innovation. Fill is the region-specific cumulative response (Table 4); grey regions are not in the sample. ***, **, *: significance at 1, 5, 10%; a dagger marks significance at 10% after a Holm correction across the 40 region-horizon tests. Inset enlarges the capital region. Map lines delineate study areas and do not necessarily depict accepted national boundaries.",
  "",
  sprintf("Figure 7 caption (new): Regional start response and the accumulation of unsold inventory. Each point is one region. The vertical axis is the region-specific cumulative response of housing starts at the twenty-four-month horizon (Table 4); the horizontal axis is the change in log unsold inventory during the high-cost regime (2021:4-2022:9) relative to the preceding 24 months (Table 2). The line is an ordinary least squares fit with its 95%% confidence band; the correlation is %.2f.", rho)),
  "figures/captions_rest_new.txt")
cat(sprintf("figures 5-7 written | corr = %.3f (p = %.3f)\n", rho, rt$p.value))
