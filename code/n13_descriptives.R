# National descriptives cited in the text: annual private starts and permits, construction cost index changes
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
r <- read.csv("../data/monthly_series_wide.csv", check.names = FALSE); r$date <- as.Date(substr(r$Monthly2, 1, 10))
r$yr <- as.integer(format(r$date, "%Y")); r$m <- as.integer(format(r$date, "%m"))
r$pflow <- ifelse(r$m == 1, r$permit_pri_nat, r$permit_pri_nat - c(NA, head(r$permit_pri_nat, -1)))
y <- aggregate(cbind(starts = start_pri_nat, permits = pflow) ~ yr, r[r$yr >= 2011 & r$yr <= 2025, ], sum)
cci_y <- aggregate(CCI ~ yr, r, mean)
y <- merge(y, cci_y, by = "yr")
write.csv(y, "out/national_annual.csv", row.names = FALSE)
cc <- function(a, b) 100 * (r$CCI[r$date == as.Date(b)] / r$CCI[r$date == as.Date(a)] - 1)
out <- data.frame(measure = c("CCI Dec2019->Dec2022", "CCI Jan2020->Jan2023", "CCI Dec2020->Dec2023", "CCI 2020avg->2023avg", "starts 2021->2023 pct", "starts 2021->2022 pct"),
                  value = c(cc("2019-12-01", "2022-12-01"), cc("2020-01-01", "2023-01-01"), cc("2020-12-01", "2023-12-01"),
                            100 * (cci_y$CCI[cci_y$yr == 2023] / cci_y$CCI[cci_y$yr == 2020] - 1),
                            100 * (y$starts[y$yr == 2023] / y$starts[y$yr == 2021] - 1), 100 * (y$starts[y$yr == 2022] / y$starts[y$yr == 2021] - 1)))
write.csv(out, "out/national_changes.csv", row.names = FALSE)
print(y); print(out, digits = 4)
