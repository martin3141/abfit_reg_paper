#!/bin/bash

#SBATCH --mail-type END
#SBATCH --ntasks 24
#SBATCH --nodes 1
#SBATCH --cpus-per-task 1
#SBATCH --time 05:00:00
#SBATCH --account wilsonmp-mrs-analysis
#SBATCH --qos bbdefault

# mail-type options : NONE, BEGIN, END, FAIL
# qos options       : bbshort, bbdefault, castles

set -e

module purge; module load bluebear

module load bear-apps/2023a
module load R/4.4.1-gfbf-2023a
module load R-bundle-CRAN/2024.06-foss-2023a

Rscript 1_run_fitting.R
