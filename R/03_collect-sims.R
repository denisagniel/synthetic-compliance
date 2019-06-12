library(tidyverse)
library(here)
library(glue)

sim_params <- expand.grid(compliance_p = seq(0.5, 0.9, length = 3),
                          alpha_n = 0,
                          alpha_c = seq(0, 0.5, length = 6),
                          lambda_n = 0:1,
                          lambda_c = 0:1,
                          gamma_c = 0:1,
                          gamma_n = 0,
                          n = c(200, 500, 1000),
                          run = 1:1000)

myfn <- function(n, alpha_c, alpha_n, lambda_c, lambda_n, gamma_c, gamma_n, compliance_p, run) {
  tst <- try(readRDS(glue('{tmpdir}compliance-res-n{n}-alpha_c{alpha_c}-alpha_n{alpha_n}-lambda_c{lambda_c}-lambda_n{lambda_n}-gamma_c{gamma_c}-gamma_n{gamma_n}-compliance_p{compliance_p}-sim{run}.rds')), silent = TRUE)
  if(class(tst) == 'try-error') return(NULL)
  tst
}


tmpdir <- '/n/data1/hms/dbmi/zaklab/dma12/synthetic-causal-estimation/tmp-compliance/'


res_l <- 
  sim_params %>%
  pmap(myfn) %>%
  bind_rows
saveRDS(res_l, 
        here('results/all-compliance-main-sim-results.rds'))

res_sum <- res_l %>%
  group_by(compliance_p,
           alpha_c,
           lambda_n,
           lambda_c,
           gamma_c,
           gamma_n,
           n,
           theta_0,
           shrunk,
           sample,
           synthetic) %>%
  summarise(
    bias = mean(estimate - gamma_c, na.rm = TRUE),
    est_var = mean(var, na.rm = TRUE),
    emp_var = var(estimate, na.rm = TRUE),
    mse = mean((estimate - gamma_c)^2, na.rm = TRUE),
    ci_coverage = mean(estimate - 1.96*sqrt(var) < gamma_c &
                         estimate + 1.96*sqrt(var) > gamma_c),
    nsim = length(estimate),
    n_NA = sum(is.na(estimate))
  ) %>%
  ungroup

saveRDS(res_sum, 
        here('results/compliance-main-sim-summary.rds'))
