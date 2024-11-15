library(ggplot2)
library(cowplot)
library(dplyr)

theme_set(theme_classic())
theme_update(plot.title = element_text(hjust = 0.5))

res_dir <- "fitting_results"

# create an output dir for the figures
dir.create("figures", showWarnings = FALSE)

true_amps <- read.csv(file.path("synth_data", "true_amps.csv"))

SNR <- c("60")

fname <- paste0("fit_amps_snr_", SNR, "_pdist_norm.csv")

fit_amps <- read.csv(file.path(res_dir, fname))

# confirm the basis ordering hasn't been confused
if (!identical(colnames(true_amps),
               colnames(fit_amps)[1:length(true_amps)])) {
  stop("fit and true names do not match")
}

abfit_reg <- fit_amps |> filter(method == "ABfit-reg") |> select(-c(method, num))
abfit     <- fit_amps |> filter(method == "ABfit")     |> select(-c(method, num))
lcmodel   <- fit_amps |> filter(method == "LCModel")   |> select(-c(method, num))

abfit_reg$tNAA <- abfit_reg$NAA + abfit_reg$NAAG
abfit$tNAA     <- abfit$NAA + abfit$NAAG
lcmodel$tNAA   <- lcmodel$NAA + lcmodel$NAAG

ba_plot <- function(a, b, title = "title", ylim = NULL) {
  ba_df <- data.frame(means = (a + b) / 2, differences = a - b)
  bias  <- mean(ba_df$differences)
  sd    <- sd(ba_df$differences)
  max_x <- max(ba_df$means)
  min_x <- min(ba_df$means)
  xlab_pos <- max_x + (max_x - min_x) * 0.17
  # Limits of agreement
  upper_loa <- bias + 1.96 * sd
  lower_loa <- bias - 1.96 * sd
  ggplot(ba_df) + geom_point(aes(x = means, y = differences), alpha = 0.3) + ggtitle(title) +
    xlab("Mean (mM)") + ylab("Difference (mM)") + 
    geom_segment(y = 0, yend = 0, x = -Inf, xend = max_x) +
    geom_segment(y = upper_loa, yend = upper_loa, x = -Inf, xend = max_x, lty = "dashed") +
    geom_segment(y = bias, yend = bias, x = -Inf, xend = max_x, lty = "dashed") +
    geom_segment(y = lower_loa, yend = lower_loa, x = -Inf, xend = max_x, lty = "dashed") +
    ylim(ylim) + 
    annotate("text", x = xlab_pos, y = upper_loa, label = round(upper_loa, 2), hjust = 1) +
    annotate("text", x = xlab_pos, y = bias, label = round(bias, 2), hjust = 1) +
    annotate("text", x = xlab_pos, y = lower_loa, label = round(lower_loa, 2), hjust = 1)
}

ba_title <- "GABA estimates of ABfit vs LCModel"
a <- ba_plot(abfit$GABA, lcmodel$GABA, ba_title, c(-1.6, 1.6))

ba_title <- "GABA estimates of ABfit-reg vs LCModel"
b <- ba_plot(abfit_reg$GABA, lcmodel$GABA, ba_title, c(-1.6, 1.6))

ba_title <- "Glu estimates of ABfit vs LCModel"
c <- ba_plot(abfit$Glu, lcmodel$Glu, ba_title, c(-1.6, 1.6))

ba_title <- "Glu estimates of ABfit-reg vs LCModel"
d <- ba_plot(abfit_reg$Glu, lcmodel$Glu, ba_title, c(-1.6, 1.6))

ba_title <- "tNAA estimates of ABfit vs LCModel"
e <- ba_plot(abfit$tNAA, lcmodel$tNAA, ba_title, c(-1.6, 1.6))

ba_title <- "tNAA estimates of ABfit-reg vs LCModel"
f <- ba_plot(abfit_reg$tNAA, lcmodel$tNAA, ba_title, c(-1.6, 1.6))

plot_grid(a, b, c, d, e, f, align = "v", nrow = 3)
ggsave(file.path("figures", "ba_plots.pdf"), width = 8, height = 10)
