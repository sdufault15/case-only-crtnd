# Analysis of Counts for Cluster Randomized Trials: Negative Controls and Test-Negative Designs

## Objective of this Repository
This repository holds all of the code necessary to recreate the analysis performed in the paper by the same name. The code has not been optimized, but should accurately return the results described in the paper. 

## Organization of this Repository
This repository is organized in the following fashion:
* **analysis** contains the R scripts that were run either locally or on the cluster to generate the results used in the reports/docs
   + **2019-06-25_multinomial-evaluations_hcsb.R** contains the code necessary to simulate the data and analyze the data for all simulations reported.
   + **2019-08-07_estimating-hcsb.R** estimates bias/power/coverage in estimating the differential health-care--seeking behavior relative risk. These estimates were reported in the table in the manuscript.
   + **2019-11-12_multinomial-evaluations_hcsb.R** runs the same code as previously but with RR = 0.8 and 0.2
   + **2019-11-19_running-count-models-on-existing-data.R** using the simulated data that had previously been generated, this function ran the Poisson GEE and mixed effects models. 
* **lib** contains user-written functions
   + Simulating Data:
      + **sample_size_function.R** builds sample for each period and intervention effect requested, relies on generate_sample_function.R
      + **generate_sample_function.R** builds sample for each sample size and health-care--seeking behavior effect requested, relies on multinomial_sample_function_hcsb.R
      + **multinomial_sample_function_hcsb.R** applies health-care--seeking behavior and intervention effects and then draws the proper sample size according to a multinomial distribution across the clusters
   + Analyzing Data:
      + **performance-evaluation-function.R** takes in the simulated data and applies the desired estimator (simple TND, Count Ratio, Poison and logistic ME and GEE) to each of the simulated datasets. Returns the estimated intervention effect, its p-value or significance below 0.05, and the standard error (on the log scale). Relies on the four helper functions below: 
      + **2019-03-08_agg-OR-function.R** applies the simple TND estimator with inference as described in (Jewell 2019) 
      + **2019-03-08_me-gee-function.R** applies the logistic mixed effects and GEE models (case counts and negative controls)
      + **2019-03-08_test-positive-function.R** applies the test-positive estimator defined in this manuscript
      + **2019-11-19_me-gee-counts.R** applies Poisson mixed effects and GEE models (case counts only)
* **docs** contains any finalized pdf reports produced with the code from lib
   + **2019-12-03_tables-figures-revision.pdf** contains versions of the figures and supporting tables included in the manuscript
   + **case-only-health-care-seeking-sims.pdf** describes how the differential health-care--seeking behavior was applied.
   + **test-positive-only.pdf** contains the work behind estimation of means and variances in the test-positive only setting.
* **reports** contains the .Rmd files used to generate the reports contained in docs
* **sandbox** contains code that is in progress or code that is used to generate the reports/docs
