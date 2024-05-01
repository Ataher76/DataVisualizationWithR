library(readxl)
library(openxlsx)
library(Matrix)
library(TropFishR)
setwd
set.seed(1)
data <- read_excel("EL.xlsx")
dates <- as.Date(paste0("14-", 1:12,"-2018"), format = "%d-%m-%y")
lfq <- list(dates=dates, midLengths=data$LengthClass, catch=as.matrix(data[, 2:ncol(data)]))
class(lfq) <- "lfq"
lfq_bin2 <- lfqModify(lfq)
ma <- 7
lfq_bin2_res <- lfqRestructure(lfq_bin2, MA=7, addl.sqrt = F)
opar <- par(mfrow=c(2,1), mar=c(2,5,2,3), oma= c(2,0,0,0))
plot(lfq_bin2_res, Fname="catch", date.axis="modern")
plot(lfq_bin2_res, Fname="rcounts", date.axis="modern")
par(opar)
Lmax= 28
linf_guess <- Lmax/0.95
linf_guess
low_par <- list(Linf=0.8*linf_guess, k=0.01, t_anchor=0, c=0, ts=0)
up_par <- list(Linf=1.2*linf_guess, k=1, t_amchor=1, c=1, ts=1)
res_GA <- ELEFAN_GA(lfq_bin2, MA=ma, seasonalised = F,
                    maxiter = 20,
                    addl.sqrt = F,
                    low_par = low_par,
                    up_par = up_par,
                    monitor = F)
res_GA
res_GA$par
res_GA$Rn_max

plot(lfq_bin2_res, Fname="rcounts", date.axis="modern", ylim=c(2,30))
lt <- lfqFitCurves(lfq_bin2, par = res_GA$par, draw = T, col=3, lty = 1, lwd=2)
lfq_bin2 <- lfqModify(lfq_bin2, par=res_GA$par)
Ms <- M_empirical(Linf = lfq_bin2$par$Linf, K_l =lfq_bin2$par$K, method = "Then_growth")
Ms
lfq_bin2$par$M <- as.numeric(Ms)
plus_group <- lfq_bin2$midLengths[max(which(lfq_bin2$midLengths<lfq_bin2$par$Linf))]
lfq_catch_vec <- lfqModify(lfq_bin2, vectorise_catch = T, plus_group = plus_group)
plot(catchCurve(lfq_catch_vec))
res_cc <- catchCurve(lfq_catch_vec, reg_int = c(5,12), calc_ogive = T)
res_cc
lfq_catch_vec$par$Z <- res_cc$Z
lfq_catch_vec$par$FM <- as.numeric(lfq_catch_vec$par$Z - lfq_catch_vec$par$M)
lfq_catch_vec$par$E <- lfq_catch_vec$par$FM/lfq_catch_vec$par$Z
lfq_catch_vec$par$a <- 0.015
lfq_catch_vec$par$b <- 3
selectivity_list <- list(selecType="trawl_ogive", L50=res_cc$L50, L75=res_cc$L75)
TB1 <- predict_mod(lfq_catch_vec, type = "ThompBell",
                   FM_change = seq(0, 4, 0.05),
                   stock_size_1 = 1,
                   curr.E = lfq_catch_vec$par$E,
                   s_list = selectivity_list,
                   plot = F, hide.progressbar = T)


TB2 <- predict_mod(lfq_catch_vec, type = "ThompBell",
                   FM_change = seq(0, 4, 0.05),
                   Lc_change = seq(5,30,0.1),
                   stock_size_1 = 1,
                   curr.E = lfq_catch_vec$par$E,
                   curr.Lc = res_cc$L50,
                   s_list = selectivity_list,
                   plot = F, hide.progressbar = T)
par(mfrow=c(1,1), mar=c(4,5,2,4.5), oma=c(1,0,0,0))
plot(TB1, mark=T)
mtext(("a"), side = 3, at=0.1, line = 0.6)
plot(TB2, type="Isopleth", xaxis1="FM", mark=T, contour=8)
mtext(("b"), side = 3, at= -0.1, line = 0.8)
TB1$df_Es
TB1$currents






