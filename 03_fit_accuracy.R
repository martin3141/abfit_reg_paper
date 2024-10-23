library(ggplot2)
library(ggsignif)
library(dplyr)
library(cowplot)

theme_set(theme_classic())

res_dir <- "fitting_results"

# create an output dir for the figures
dir.create("figures", showWarnings = FALSE)

true_amps <- read.csv(file.path("synth_data", "true_amps.csv"))

SNRS <- c("10", "30", "60", "100")

fit_amps <- vector("list", length(SNRS))

fit_errors_plot <- vector("list", length(SNRS))

for (n in 1:length(SNRS)) {
  
  fname <- paste0("fit_amps_snr_", SNRS[n], "_pdist_norm.csv")
  
  fit_amps <- read.csv(file.path(res_dir, fname))
  
  # confirm the basis ordering hasn't been confused
  if (!identical(colnames(true_amps),
                 colnames(fit_amps)[1:length(true_amps)])) {
    stop("fit and true names do not match")
  }
  
  N_spec    <- max(fit_amps$num)
  N_methods <- length(unique(fit_amps$method))
  N_basis   <- ncol(true_amps)
  
  rm_inds <- grep("^Lip|^MM|^X.CrCH2", colnames(true_amps))
  fit_errors <- fit_amps[, 1:N_basis] - true_amps[rep(1, N_spec * N_methods), ]
  fit_errors_cut <- data.frame(error = rowSums(fit_errors[, -rm_inds] ^ 2))
  fit_errors_cut['method'] <- fit_amps['method']
  fit_errors_cut['num']    <- fit_amps['num']
  fit_errors_plot[[n]]     <- fit_errors_cut
}

max_y <- fit_errors_plot[[1]] |> filter(method == "ABfit") |> select(error) |> 
  unlist() |>  as.numeric() |> (\(x) quantile(x)[4] + 1.5 * IQR(x))() |> 
  as.numeric()

# signif_map <- function(p) sprintf("p = %.2g", p)
signif_map <- c("*"=0.05, "**"=0.005, "***"=0.0005)
comps <- list(c("ABfit-reg", "LCModel"), c("ABfit", "ABfit-reg"))

plot_a <- ggplot(data = fit_errors_plot[[1]], aes(x = method, y = error)) +
  geom_boxplot(width = 0.15, outliers = FALSE) +
  geom_signif(comparisons = comps, test = "t.test",
              map_signif_level = signif_map,
              y_position = max_y, tip_length = 0.01, textsize = 3.5,
              vjust = -0.2) +
  xlab(NULL) + ylab(error~(mM^2)) + ylim(c(0, max_y * 1.28))

max_y <- fit_errors_plot[[2]] |> filter(method == "ABfit") |> select(error) |> 
  unlist() |>  as.numeric() |> (\(x) quantile(x)[4] + 1.5 * IQR(x))() |> 
  as.numeric()

plot_b <- ggplot(data = fit_errors_plot[[2]], aes(x = method, y = error)) +
  geom_boxplot(width = 0.15, outliers = FALSE) +
  geom_signif(comparisons = comps, test = "t.test",
              map_signif_level = signif_map,
              y_position = max_y, tip_length = 0.015, textsize = 3.5,
              vjust = -0.2) +
  xlab(NULL) + ylab(error~(mM^2)) + ylim(c(0, max_y * 1.25))

max_y <- fit_errors_plot[[3]] |> filter(method == "ABfit") |> select(error) |> 
  unlist() |>  as.numeric() |> (\(x) quantile(x)[4] + 1.5 * IQR(x))() |> 
  as.numeric()

plot_c <- ggplot(data = fit_errors_plot[[3]], aes(x = method, y = error)) +
  geom_boxplot(width = 0.15, outliers = FALSE) +
  geom_signif(comparisons = comps, test = "t.test",
              map_signif_level = signif_map,
              y_position = max_y, tip_length = 0.008, textsize = 3.5,
              vjust = -0.2) +
  xlab(NULL) + ylab(error~(mM^2)) + ylim(c(0, max_y * 1.35))

max_y <- fit_errors_plot[[4]] |> filter(method == "ABfit") |> select(error) |> 
  unlist() |>  as.numeric() |> (\(x) quantile(x)[4] + 1.5 * IQR(x))() |> 
  as.numeric()

plot_d <- ggplot(data = fit_errors_plot[[4]], aes(x = method, y = error)) +
  geom_boxplot(width = 0.15, outliers = FALSE) +
  geom_signif(comparisons = comps, test = "t.test",
              map_signif_level = signif_map,
              y_position = max_y, tip_length = 0.006, textsize = 3.5,
              vjust = -0.2) +
  xlab(NULL) + ylab(error~(mM^2)) + ylim(c(0, max_y * 1.45))

labs <- paste0(c("SNR = "), c(10, 30, 60, 100))
plot_grid(plot_a, plot_b, plot_c, plot_d, labels = labs, label_size = 11, 
          scale = 0.95, label_x = 0.38, align = "v")

ggsave(file.path("figures", "fit_res_norm.pdf"), width = 6, height = 5)


fit_errors_plot[[1]] |> filter(method == "LCModel")   |> select(error) |> colMeans()
fit_errors_plot[[1]] |> filter(method == "ABfit-reg") |> select(error) |> colMeans()

