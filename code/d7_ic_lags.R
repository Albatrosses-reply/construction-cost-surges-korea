suppressPackageStartupMessages({library(dplyr); library(fixest)})
args <- commandArgs(trailingOnly = FALSE); f <- sub("--file=", "", args[grep("--file=", args)]); if (length(f)) setwd(dirname(normalizePath(f)))
d0 <- read.csv("../data/panel_8_regions.csv", check.names=FALSE); d0$date <- as.Date(d0$Monthly2)
names(d0) <- gsub("\\.", "_", gsub(" ", "_", names(d0)))
# ---- 1) lag selection on the national system (one obs per month): VAR in cost growth, mean log starts, rates, spread, ppi, fx, mean log transactions
nat <- d0 %>% group_by(date) %>% summarise(c=first(yoy_CCI), s=mean(l_start_pri), tr=mean(l_apt_trd), r=first(rate_k3y), sp=first(spread_corp3y), p=first(l_ppi), fx=first(l_fx_usdkrw)) %>% arrange(date)
Y <- as.matrix(nat[,c("c","s","tr","r","sp","p","fx")]); K <- ncol(Y); Tn <- nrow(Y); pmax <- 13
ic <- t(sapply(1:pmax, function(p) {
  Yt <- Y[(pmax+1):Tn,]; X <- cbind(1, do.call(cbind, lapply(1:p, function(l) Y[(pmax+1-l):(Tn-l),])))
  E <- Yt - X %*% solve(crossprod(X), crossprod(X, Yt)); S <- crossprod(E)/nrow(Yt); n <- nrow(Yt)
  c(p=p, AIC=log(det(S)) + 2*p*K^2/n, BIC=log(det(S)) + log(n)*p*K^2/n, HQ=log(det(S)) + 2*log(log(n))*p*K^2/n) }))
print(round(ic,3)); cat(sprintf("selected p: AIC=%d  HQ=%d  BIC=%d\n", which.min(ic[,"AIC"]), which.min(ic[,"HQ"]), which.min(ic[,"BIC"])))
# ---- 2) LP with p lags of y, cost growth, all controls (lag augmentation = selected p + 1), DK SE; 8 regions
macro_x <- c("rate_k3y","spread_corp3y","l_ppi","l_fx_usdkrw"); housing_x <- c("l_apt_trd","l_permit_pri","l_unsold_pri","l_hh","l_apt_price_idx")
d0 <- d0 %>% arrange(region,date) %>% group_by(region) %>% mutate(tid=row_number()) %>% ungroup()
for (v in c("l_start_pri","yoy_CCI",macro_x,housing_x,"regime_dummy","yoy_CCI_regime")) for (l in 1:13) d0[[paste0(v,"_L",l)]] <- ave(d0[[v]], d0$region, FUN=function(x) dplyr::lag(x,l))
lp <- function(p) bind_rows(lapply(c(0,3,5,6,9,10,12,15,18,24), function(h) {
  dd <- d0 %>% group_by(region) %>% mutate(y=dplyr::lead(l_start_pri,h)-l_start_pri_L1) %>% ungroup()
  cc <- paste(as.vector(outer(c("l_start_pri","yoy_CCI",macro_x,housing_x),1:p,function(a,b) paste0(a,"_L",b))), collapse="+")
  rc <- paste(c(paste0("regime_dummy_L",1:p), paste0("yoy_CCI_regime_L",1:p)), collapse="+")
  ea <- feols(as.formula(paste("y ~ yoy_CCI +", cc, "| region")), dd, panel.id=~region+tid, vcov=DK(lag=h+1), notes=FALSE, warn=FALSE)
  er <- feols(as.formula(paste("y ~ yoy_CCI + regime_dummy + yoy_CCI_regime +", cc, "+", rc, "| region")), dd, panel.id=~region+tid, vcov=DK(lag=h+1), notes=FALSE, warn=FALSE)
  w <- c("yoy_CCI","yoy_CCI_regime"); data.frame(p=p, h=h, avg=coef(ea)["yoy_CCI"], avg_se=se(ea)["yoy_CCI"], reg=sum(coef(er)[w]), reg_se=sqrt(sum(vcov(er)[w,w]))) }))
st <- function(t) ifelse(abs(t)>=2.576,"***",ifelse(abs(t)>=1.96,"**",ifelse(abs(t)>=1.645,"*","")))
sel <- sort(unique(c(which.min(ic[,"BIC"])+1, which.min(ic[,"HQ"])+1, which.min(ic[,"AIC"])+1, 2, 3, 4)))
res <- bind_rows(lapply(sel, lp))
for (k in split(res, res$p)) { cat(sprintf("\n== p=%d lags of everything (incl. cost growth), DK SE\n", k$p[1]))
  cat(sprintf("  h=%2d avg %6.3f%-3s(%.3f)  regime %6.3f%-3s(%.3f)\n", k$h, k$avg, st(k$avg/k$avg_se), k$avg_se, k$reg, st(k$reg/k$reg_se), k$reg_se), sep="") }
write.csv(res, "out/d7_ic_lags.csv", row.names=FALSE); write.csv(ic, "out/d7_ic_table.csv", row.names=FALSE)
