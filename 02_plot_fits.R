library(spant)
library(cowplot)

fits_a <- readRDS(file.path("fitting_results",
                            "fit_res_snr_10_pdist_norm.rds"))

fits_b <- readRDS(file.path("fitting_results",
                            "fit_res_snr_30_pdist_norm.rds"))

fits_c <- readRDS(file.path("fitting_results",
                            "fit_res_snr_100_pdist_norm.rds"))

fit_n <- 100

plot_a <- function() fits_a[[1]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "ABfit, SNR = 10")
plot_b <- function() fits_a[[2]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "ABfit-reg, SNR = 10")
plot_c <- function() fits_a[[3]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "LCModel, SNR = 10")
plot_d <- function() fits_b[[1]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "ABfit, SNR = 30")
plot_e <- function() fits_b[[2]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "ABfit-reg, SNR = 30")
plot_f <- function() fits_b[[3]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "LCModel, SNR = 30")
plot_g <- function() fits_c[[1]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "ABfit, SNR = 100")
plot_h <- function() fits_c[[2]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "ABfit-reg, SNR = 100")
plot_i <- function() fits_c[[3]] |> plot(n = fit_n, restore_def_par = FALSE,
                                    main = "LCModel, SNR = 100")

pdf(file.path("figures", "fits_plot.pdf"), width = 10, height = 10)
plot_grid(plot_a, plot_b, plot_c, plot_d, plot_e, plot_f, plot_g, plot_h,
          plot_i, ncol = 3)
dev.off()
