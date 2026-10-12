# 2-state mean-switching Markov model on national YoY construction-cost growth (common variance)
# Hamilton filter + Kim smoother; dates the high-cost regime (Table A.1) and writes the smoothed probabilities
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
d <- read.csv("../data/panel_8_regions.csv", check.names = FALSE)
m <- unique(d[, c("Monthly2", "yoy_CCI", "regime_dummy")]); m <- m[order(m$Monthly2), ]
y <- m$yoy_CCI; Tn <- length(y)
nll <- function(par, full = FALSE) {
  mu <- par[1:2]; s <- exp(par[3]); p11 <- plogis(par[4]); p22 <- plogis(par[5])
  P <- matrix(c(p11, 1 - p11, 1 - p22, p22), 2, byrow = TRUE)
  xi <- c(1 - p22, 1 - p11) / (2 - p11 - p22)                     # ergodic start
  filt <- pred <- matrix(0, Tn, 2); ll <- 0
  for (t in 1:Tn) {
    pr <- if (t == 1) xi else as.vector(t(P) %*% filt[t - 1, ])
    pred[t, ] <- pr; dens <- dnorm(y[t], mu, s); jt <- pr * dens; lt <- sum(jt)
    ll <- ll + log(lt); filt[t, ] <- jt / lt
  }
  if (!full) return(-ll)
  sm <- filt; for (t in (Tn - 1):1) sm[t, ] <- filt[t, ] * as.vector(P %*% (sm[t + 1, ] / pred[t + 1, ]))
  list(ll = ll, mu = mu, sigma = s, p11 = p11, p22 = p22, smooth = sm)
}
best <- NULL
for (st in list(c(2, 10, 0, 4, 2), c(3, 12, 0.5, 5, 3), c(1, 8, 0, 3, 1))) {
  o <- optim(st, nll, method = "BFGS", control = list(maxit = 2000))
  if (is.null(best) || o$value < best$value) best <- o
}
r <- nll(best$par, full = TRUE); hi <- which.max(r$mu)
ph <- r$smooth[, hi]
out <- data.frame(Monthly2 = m$Monthly2, yoy_CCI = y, p_high = ph, D50 = as.integer(ph > 0.5), D60 = as.integer(ph > 0.6),
                  Dtop10 = as.integer(y >= quantile(y, 0.9)), regime_dummy_paper = m$regime_dummy)
write.csv(out, "out/markov_probs.csv", row.names = FALSE)
cat(sprintf("means: %.3f / %.3f | sigma %.3f | p(L->L) %.3f | p(H->H) %.3f | loglik %.2f\n", min(r$mu), max(r$mu), r$sigma,
            ifelse(hi == 2, r$p11, r$p22), ifelse(hi == 2, r$p22, r$p11), r$ll))
rng <- function(v) { w <- out$Monthly2[v == 1]; sprintf("%s ~ %s (%d months)", substr(min(w), 1, 7), substr(max(w), 1, 7), length(w)) }
cat("p>0.5:", rng(out$D50), "| matches paper dummy:", all(out$D50 == out$regime_dummy_paper), "\n")
cat("p>0.6:", rng(out$D60), "| top-10%:", rng(out$Dtop10), "\n")
