library(ggplot2)
library(ggsignif)
library(dplyr)
library(cowplot)

res_dir <- "fitting_results_lb_init_0p55"
res_dir <- "fitting_results"

# create an output dir for the figures
dir.create("figures", showWarnings = FALSE)

true_amps <- read.csv(file.path("synth_data", "true_amps.csv"))

SNRS <- c("10", "30", "60", "100")
# SNRS <- c("10", "30", "60")

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
  
  theme_set(theme_classic())
  
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

colSums((fit_errors ^ 2)[1:1000,])
colSums((fit_errors ^ 2)[1001:2000,])
colSums((fit_errors ^ 2)[2001:3000,])


abfit_reg <- fit_amps |> filter(method == "ABfit-reg") |> select(-c(method, num))
abfit     <- fit_amps |> filter(method == "ABfit") |> select(-c(method, num))
lcmodel   <- fit_amps |> filter(method == "LCModel") |> select(-c(method, num))

x <- (lcmodel$GABA + abfit$GABA) / 2
y <- lcmodel$GABA - abfit$GABA

plot(x, y)

x <- (lcmodel$GABA + abfit_reg$GABA) / 2
y <- lcmodel$GABA - abfit_reg$GABA

plot(x, y)

x <- (lcmodel$Glu + abfit$Glu) / 2
y <- lcmodel$Glu - abfit$Glu

plot(x, y)

x <- (lcmodel$Glu + abfit_reg$Glu) / 2
y <- lcmodel$Glu - abfit_reg$Glu

plot(x, y)

mean(lcmodel$Glu - true_amps$Glu)
mean(abfit_reg$Glu - true_amps$Glu)

mean(lcmodel$GABA - true_amps$GABA)
mean(abfit_reg$GABA - true_amps$GABA)

mean(lcmodel$NAA - true_amps$NAA)
mean(abfit_reg$NAA - true_amps$NAA)

sum((lcmodel$NAA - true_amps$NAA)^2)
sum((abfit_reg$NAA - true_amps$NAA)^2)

sum((lcmodel$NAAG - true_amps$NAAG)^2)
sum((abfit_reg$NAAG - true_amps$NAAG)^2)

(lcmodel$NAA + lcmodel$NAAG - true_amps$NAA - true_amps$NAAG) |> plot()
(abfit_reg$NAA + abfit_reg$NAAG - true_amps$NAA - true_amps$NAAG) |> plot()

(lcmodel$GABA - true_amps$GABA) |> plot()
(abfit_reg$GABA - true_amps$GABA) |> plot()

(lcmodel$NAAG - true_amps$NAAG) |> plot()
(abfit_reg$NAA - true_amps$NAA) |> plot()

break

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

ggsave(file.path("figures", "fit_res_norm.pdf"), scale = 0.8)