library(ggplot2)
library(ggsignif)
library(dplyr)

true_amps <- read.csv(file.path("synth_data", "true_amps.csv"))
fit_amps  <- read.csv(file.path("fitting_results_128",
                                "fit_amps_snr_100_pdist_norm.csv"))

# check the basis ordering hasn't been confused
if (!identical(colnames(true_amps), colnames(fit_amps)[1:length(true_amps)])) {
  stop("fit and true names do not match")
}

# make sure the ordering is correct
# true_amps <- true_amps[,colnames(fit_amps)[1:29]]

N_spec    <- max(fit_amps$num)
N_methods <- length(unique(fit_amps$method))
N_basis   <- ncol(true_amps)

rm_inds <- grep("^Lip|^MM|^X.CrCH2", colnames(true_amps))
fit_errors <- fit_amps[, 1:N_basis] - true_amps[rep(1, N_spec * N_methods), ]
fit_errors_cut <- data.frame(error = rowSums(fit_errors[, -rm_inds] ^ 2))
fit_errors_cut['method'] <- fit_amps['method']
fit_errors_cut['num']    <- fit_amps['num']

ggplot(data = filter(fit_errors_cut, method != "ABfit"), aes(x = method, y = error)) +
  geom_point(size = 2, alpha = 0.2, position = position_jitter(seed = 1, height = 0.0, width = 0.05)) +
  geom_boxplot(width = 0.05, outlier.shape = NA, position = position_nudge(x = 0.12)) +
  geom_signif(comparisons = list(c("ABfit-reg", "LCModel")), test = "t.test",
              map_signif_level = function(p) sprintf("p = %.2g", p)) +
  theme_classic() + xlab(NULL) + ylab("error")

# ggplot(data = fit_errors_cut, aes(x = method, y = error)) +
#   geom_boxplot(width = 0.10) +
#   geom_signif(comparisons = list(c("ABfit-reg", "LCModel"), c("ABfit", "ABfit-reg")), test = "t.test",
#               map_signif_level = function(p) sprintf("p = %.2g", p)) +
#   theme_classic() + xlab(NULL) + ylab("error") +
#   stat_summary(fun = mean, geom = 'point', shape = 1, size = 2, col = "red") +
#   stat_summary(fun = mean, geom = 'line', group = 1, col = "red") 


filter(fit_errors_cut, method == "ABfit-reg") |> select(error) |> colMeans()
filter(fit_errors_cut, method == "LCModel")   |> select(error) |> colMeans()

filter(fit_errors_cut, method == "ABfit-reg") |> select(error) |> unlist() |> which.max()

# fits <- readRDS(file.path("fitting_results", "fit_res_snr_100_pdist_norm.rds"))
# fits[[2]] |> plot(n = 471, plot_sigs = "Asp")
# fits[[3]] |> plot(n = 471, plot_sigs = "Asp")

#fits[[2]] |> plot(n = 862, plot_sigs = "Asp")
#fits[[3]] |> plot(n = 862, plot_sigs = "Asp")

# ggplot(data = fit_errors_cut, aes(x = method, y = error)) +
#   geom_point(size = 2, alpha = 0.2, position = position_jitter(seed = 1, height = 0.0, width = 0.05)) +
#   geom_boxplot(width = 0.05, outlier.shape = NA, position = position_nudge(x = 0.12)) +
#   geom_signif(comparisons = list(c("ABfit-reg", "LCModel"), c("ABfit-reg", "ABfit"), 
#                                  c("ABfit", "LCModel")), test = "t.test",
#               map_signif_level = function(p) sprintf("p = %.2g", p), y_position = c(15, 17, 19)) +
#   theme_classic() + xlab(NULL) + ylab("error")

# fit_errors_tcr <- fit_errors[, -rm_inds]
# fit_errors_tcr['tCr'] <- fit_errors['Cr'] + fit_errors['PCr']
# fit_errors_tcr['Cr']  <- NULL
# fit_errors_tcr['PCr'] <- NULL
# fit_errors_tcr <- data.frame(error = rowSums(fit_errors_tcr ^ 2))
# fit_errors_tcr['method'] <- fit_amps['method']
# fit_errors_tcr['num']    <- fit_amps['num']

# ggplot(data = fit_errors_tcr, aes(x = method, y = error)) +
#   geom_point(size = 2, alpha = 0.2, position = position_jitter(seed = 1, height = 0.0, width = 0.05)) +
#   geom_boxplot(width = 0.05, outlier.shape = NA, position = position_nudge(x = 0.12)) +
#   geom_signif(comparisons = list(c("ABfit-reg", "LCModel"), c("ABfit-reg", "ABfit"), 
#                                  c("ABfit", "LCModel")), test = "t.test",
#               map_signif_level = function(p) sprintf("p = %.2g", p), y_position = c(15, 17, 19)) +
#   theme_classic() + xlab(NULL) + ylab("error")

