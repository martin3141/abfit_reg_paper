library(spant)
library(logr)

# results dir
fit_res_dir <- "fitting_results_05"

# create an output dir
dir.create(fit_res_dir, showWarnings = FALSE)

# do fitting in parallel?
parallel <- TRUE
n_cores  <- 24

# parallel processing config.
if (parallel) {
  cl <- parallel::makeCluster(n_cores)
} else {
  cl <- NULL 
}

# time string for log files
time_str <- format(Sys.time(), "%d_%H_%M_%S")
log_open(paste0(time_str, ".log"), compact = TRUE, show_notes = FALSE)

# simulation "runs" to cycle though
# para_df <- data.frame(spec_snr  = rep(c(10, 30, 60, 100), 2),
#                       prob_dist = rep(c("norm", "unif"), each = 4))

para_df <- data.frame(spec_snr  = rep(c(10, 30, 60, 100)),
                      prob_dist = rep(c("norm"), each = 4))

# para_df <- data.frame(spec_snr = 10, prob_dist = "norm")

# read basis
basis <- read_basis(file.path("synth_data", "brain_basis.basis"),
                    sort_basis = FALSE)

# read water reference data
wref  <- read_mrs(file.path("synth_data", "wref.nii.gz"))

for (n in 1:nrow(para_df)) {
  log_print(paste("simulation run :", n))
  
  # spectral SNR
  spec_snr <- para_df$spec_snr[n]

  # prob. distribution of shifts and T2s
  prob_dist <- para_df$prob_dist[n]
  
  input_f <- paste0("metab_snr_", spec_snr, "_pdist_", prob_dist, ".nii.gz")
  
  metab <- read_mrs(file.path("synth_data", input_f))
  N     <- Ndyns(metab)
  
  # abfit
  log_print("running abfit")
  abfit_options <- abfit_opts(lb_init = "lcm_compat", maxiters = 128,
                              adaptive_bl_comps_pppm = TRUE)
  
  abfit_res     <- fit_mrs(metab, basis, method = "abfit", opts = abfit_options,
                           parallel = parallel, cl = cl)
  
  abfit_res     <- abfit_res |> scale_amp_molar(wref)
  
  # abfit-reg
  log_print("running abfit-reg")
  abfit_reg_options <- abfit_opts(lb_reg = "lcm_compat", lb_init = "lcm_compat",
                                  freq_reg = 0.004, max_basis_shift = Inf,
                                  max_basis_damping = Inf,
                                  max_shift_fine = 0.05, maxiters = 128,
                                  adaptive_bl_comps_pppm = TRUE,
                                  max_asym = Inf, asym_reg = 0.1)
  
  abfit_reg_res <- fit_mrs(metab, basis, method = "abfit",
                           opts = abfit_reg_options, parallel = parallel,
                           cl = cl)
  
  abfit_reg_res <- abfit_reg_res |> scale_amp_molar(wref)
  
  # lcmodel
  lcm_options   <- c("NSIMUL=0", "NRATIO=0")
  
  log_print("running LCModel")
  lcm_res       <- fit_mrs(metab, basis, method = "lcmodel", opts = lcm_options,
                           w_ref = get_dyns(wref, 1),
                           parallel = parallel, cl = cl)
  
  # extract all fit amplitudes
  abfit_amps     <- fit_amps(abfit_res,     append_common_1h_comb = FALSE)
  abfit_reg_amps <- fit_amps(abfit_reg_res, append_common_1h_comb = FALSE)
  lcm_amps       <- fit_amps(lcm_res,       append_common_1h_comb = FALSE)
  
  all_fit_amps <- rbind(abfit_amps, abfit_reg_amps, lcm_amps)
  all_fit_amps['method'] <- rep(c("ABfit", "ABfit-reg", "LCModel"), each = N)
  all_fit_amps['num'] <- rep(1:N, 3)
  
  out_f <- paste0("fit_amps_snr_", spec_snr, "_pdist_", prob_dist, ".csv")
  write.csv(all_fit_amps, file.path(fit_res_dir, out_f),
            row.names = FALSE)
  
  out_f <- paste0("fit_res_snr_", spec_snr, "_pdist_", prob_dist, ".rds")
  fit_results <- list(abfit_res, abfit_reg_res, lcm_res)
  saveRDS(fit_results, file.path(fit_res_dir, out_f))
}

log_close()
