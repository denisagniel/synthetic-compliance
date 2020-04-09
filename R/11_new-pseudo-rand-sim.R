remotes::install_github('denisagniel/synthate')  
tmpdir <- '/n/data1/hms/dbmi/zaklab/dma12/synthetic-causal-estimation/tmp-compliance/'

library(tidyverse)
library(clustermq)
library(synthate)
library(glue)
library(here)

tmpdir <- here('results/tmp/')
fs::dir_create(tmpdir)
#'
#' Set up simulations settings.
#' 
sim_params <- expand.grid(compliance_p = seq(0.5, 0.9, length = 5),
                          p = 8,
                          delta = c(0, 1),
                          n = c(200, 500, 1000),
                          run = 1:3)

xs <- paste('x', 1:8, collapse = ' + ', sep = '')
sfm <- as.formula(glue('s ~ {xs} + z'))
psfm <- as.formula(glue('s ~ {xs}'))
yfm <- as.formula(glue('y ~ shat + {xs}'))
fm <- as.formula(glue('y ~ s + {xs}'))
fnl <- list(
  iv_fn,
  tsls_fn = function(ds) tsls_fn(ds = ds,
                                 sfm = sfm,
                                 yfm = yfm),
  atregr_fn = function(ds) atregr_fn(ds = ds,
                                     fm = fm),
  ppregr_fn= function(ds) ppregr_fn(ds = ds,
                                    fm = fm),
  ivs_fn,
  ats_fn = function(ds) ats_fn(ds = ds, fm = fm),
  pps_fn = function(ds) pps_fn(ds = ds, fm = fm),
  ipw_fn = function(ds) ipw_fn(ds = ds, p = 8),
  ipw_regr_fn = function(ds) ipw_regr_fn(ds = ds, p = 8),
  ipws_fn = function(ds) ipws_fn(ds = ds, p = 8),
  ipwrs_fn = function(ds) ipwrs_fn(ds = ds, p = 8)
)

# tst <- synthate:::psr_sim_fn(n = 500, p = 8, delta = 0, compliance_p = 0.6, run = 221, psfm = psfm, fn_list = fnl, tmpdir = tmpdir)

options(
  clustermq.defaults = list(ptn="short",
                            log_file="Rout/log%a.log",
                            time_amt = "12:00:00"
  )
)
sim_res <- Q_rows(sim_params, synthate:::psr_sim_fn, 
                  const = list(fn_list = fnl,
                               tmpdir = tmpdir),
                  fail_on_error = FALSE,
                  n_jobs = 3)
saveRDS(sim_res, here('results/pseudo-rand-sim.rds'))
