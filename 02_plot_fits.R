library(spant)
library(cowplot)

fits_a <- readRDS(file.path("fitting_results",
                            "fit_res_snr_10_pdist_norm.rds"))

fits_b <- readRDS(file.path("fitting_results",
                            "fit_res_snr_30_pdist_norm.rds"))

fits_c <- readRDS(file.path("fitting_results",
                            "fit_res_snr_100_pdist_norm.rds"))

fit_n <- 100

a <- function() fits_a[[1]] |> plot(n = fit_n, restore_def_par = FALSE, main = "ABfit, SNR = 10")
b <- function() fits_a[[2]] |> plot(n = fit_n, restore_def_par = FALSE, main = "ABfit-reg, SNR = 10")
c <- function() fits_a[[3]] |> plot(n = fit_n, restore_def_par = FALSE, main = "LCModel, SNR = 10")

d <- function() fits_b[[1]] |> plot(n = fit_n, restore_def_par = FALSE, main = "ABfit, SNR = 30")
e <- function() fits_b[[2]] |> plot(n = fit_n, restore_def_par = FALSE, main = "ABfit-reg, SNR = 30")
f <- function() fits_b[[3]] |> plot(n = fit_n, restore_def_par = FALSE, main = "LCModel, SNR = 30")

g <- function() fits_c[[1]] |> plot(n = fit_n, restore_def_par = FALSE, main = "ABfit, SNR = 100")
h <- function() fits_c[[2]] |> plot(n = fit_n, restore_def_par = FALSE, main = "ABfit-reg, SNR = 100")
i <- function() fits_c[[3]] |> plot(n = fit_n, restore_def_par = FALSE, main = "LCModel, SNR = 100")

pdf(file.path("figures", "fits_plot.pdf"), width = 10, height = 10)
plot_grid(a, b, c, d, e, f, g, h, i, ncol = 3)
dev.off()
