## Instructions for use

To reproduce all synthetic data and paper results the .R scripts should be run in order, eg:

Rscript 00_sim_basis_and_mrs_data.R

Rscript 01_run_fitting.R

...

R must be installed <https://www.r-project.org/> and version 4.4.1 has been
tested, however other recent versions should also work.

All required R packages (including spant) will be automatically installed when
running the first script : 00_sim_basis_and_mrs_data.R.

The LCModel binary must also be installed in the user's home directory
(eg ~/lcmodel). Run:

```
library(spant)
check_lcm() 
```

in R to confirm LCModel is correctly installed. The `set_lcm_cmd()` function can
be used if LCModel is installed elsewhere. Binaries for LCModel are available
from : <https://github.com/schorschinho/LCModel>

01_run_fitting.R is configured to be run on a computer cluster with 24 cores and
the accompanying slurm job file is included : job_run_fitting.sh. The `parallel`
and `n_cores` variables in the R script may need to edited if running on a PC.
This script will take *significantly* longer than the others (approx 60 mins on 
a modern cluster running in parallel across 24 cores).

## Expected output

### 00_sim_basis_and_mrs_data.R

All synthetic MRS data, basis set and true amplitudes will be saved in the
"synth_data" directory.

### 01_run_fitting.R

All MRS analyses will be run and results saved in the "fitting_results"
directory.

### 02_plot_fits.R

"fits_plot.pdf" will be saved in the "figures" directory.

### 03_fit_accuracy_norm.R

"fit_res_norm.pdf" will be saved in the "figures" directory.

### 04_fit_accuracy_unif.R

"fit_res_unif.pdf" will be saved in the "figures" directory.

### 05_fit_agreement.R

"ba_plots.pdf" will be saved in the "figures" directory.
