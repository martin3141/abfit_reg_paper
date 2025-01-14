# install any necessary packages
if (!require("pacman")) install.packages("pacman",repos = "http://cran.us.r-project.org")
pacman::p_load(spant, logr, cowplot, ggplot2, ggsignif, dplyr, cowplot, 
               parallel, doParallel,gridGraphics)

set_lcm_cmd('/Users/pfuchs/Library/CloudStorage/OneDrive-UniversiteitAntwerpen/Reviews/MRM-24-25374/abfit_reg_paper/')

# create an output dir
dir.create("synth_data", showWarnings = FALSE)

# number of spectra to simulate per run
N <- 1000

# simulation "runs" to cycle though
para_df <- data.frame(spec_snr  = rep(c(10, 30, 60, 100), 2),
                      prob_dist = rep(c("norm", "unif"), each = 4))

# set acquisition parameters
acq_paras <- def_acq_paras()

# simulate a typical healthy brain spectrum
brain_sim <- sim_brain_1h(full_output = TRUE, acq_paras = acq_paras)
amps      <- brain_sim$amps
basis     <- brain_sim$basis

# write amps to file
write.csv(data.frame(t(amps)), file.path("synth_data", "true_amps.csv"),
          row.names = FALSE)

# write basis to file
write_basis(basis, file.path("synth_data", "brain_basis.basis"))

# re-read basis from file to ensure consistent rounding errors from conversion
# between acsii and floats, ensure ordering isn't changed
basis <- read_basis(file.path("synth_data", "brain_basis.basis"),
                    sort_basis = FALSE)

# generate a dataframe of metabolite values
amps_df <- data.frame(t(amps))
amps_df <- amps_df[rep(1, N),]

# generate and export water reference data as NIfTI MRS
wref <- sim_resonances(4.65, lw = 4, amp = 0.7 * 35880, acq_paras = acq_paras)
write_mrs(wref, file.path("synth_data", "wref.nii.gz"), force = TRUE)

for (n in 1:nrow(para_df)) {
  
  # spectral SNR
  spec_snr <- para_df$spec_snr[n]

  # prob. distribution of shifts and T2s
  prob_dist <- para_df$prob_dist[n]
  
  # basis linebroading expectation
  lb_ex <- 0.780
  
  # basis linebroading standard deviation in Hz
  lb_sd <- 0.156
  
  # basis frequency shift standard deviation in ppm
  freq_sd <- 0.004
  
  # set random number generator seed
  set.seed(100)
  
  # initialise structure to store simulated spectra
  mrs_data_list <- vector("list", N)
  
  neg_lbs <- 0
  
  for (n in 1:N) {
    if (prob_dist == "norm") {
      lbs    <- rnorm(length(basis$names), lb_ex, lb_sd) 
      shifts <- rnorm(length(basis$names), 0, freq_sd)
    } else if (prob_dist == "unif") {
      # 95% within 1.96 standard deviations
      lbs    <- runif(length(basis$names), lb_ex - lb_sd * 1.96,
                      lb_ex + lb_sd * 1.96) 
      shifts <- runif(length(basis$names), -1.96 * freq_sd, 1.96 * freq_sd)
    } else {
      stop("prob_dist not recognised")
    }
    neg_lbs <- neg_lbs + sum(lbs < 0.1)
    lbs[lbs < 0] <- 0 # condition was never met for the published results
    mrs_sim <- basis2mrs_data(basis, sum_elements = TRUE, amps = amps,
                              shifts = shifts, lbs = lbs)
    mrs_sim <- mrs_sim |> lb(4) |> add_noise_spec_snr(spec_snr)
    mrs_data_list[[n]] <- mrs_sim
  }
  
  # print(neg_lbs) # keep track of any negative values
  
  metab <- append_dyns(mrs_data_list)
  
  # export as NIfTI MRS
  out_f <- paste0("metab_snr_", spec_snr, "_pdist_", prob_dist, ".nii.gz")
  write_mrs(metab, file.path("synth_data", out_f), force = TRUE)
}
