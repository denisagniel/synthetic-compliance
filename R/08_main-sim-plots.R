#' ---
#' title: "Compliance main sim results"
#' output: github_document
#' ---
#' 
library(knitr)
opts_chunk$set(warning = FALSE, message = FALSE, cache = FALSE, fig.width = 7, fig.height = 7)

#'
library(tidyverse)
library(glue)
library(here)

sim_res <- readRDS(here(
  'results/compliance-main-sim-summary.rds'
)) %>%
  filter(theta_0 != 'ivs_est')

res <- sim_res %>%
  filter(
         # alpha_c == 0.5,
         gamma_n == 0,
         gamma_c == 0,
         lambda_n == 1,
         lambda_c == 1,
         # compliance_p == 0.7,
         # n == 1000,
         # !shrunk,
         !(synthetic & theta_0 %in% 
             c('atregr_est', 'ppregr_est',
               'ats_est', 'ipw_est',
               'pps_est', 'ipw_regr_est')))
res <- res %>%
  mutate(g = paste0(theta_0, shrunk, sample, synthetic, shrunk),
         synth = case_when(
           sample == 'CV' & synthetic & !shrunk ~ 'synthetic-CV',
           sample == 'full' & synthetic & !shrunk ~ 'synthetic-full',
           sample == 'CV' & synthetic  & shrunk ~ 'synthetic-CV-shrunk',
           sample == 'full' & synthetic & shrunk ~ 'synthetic-full-shrunk',
           TRUE ~ 'non-synthetic'
         ))
#-----------------------------------------
## gamma_c = 0
## lambda_n = 1
## lambda_c = 1
## compliance_effect = 2
## n = 200
## compliance_p = 0.6
#-----------------------------------------------
## comparing synthetic vs regular
#----------------------------------------------
this_res <- res %>%
  filter(synth %in% c('non-synthetic', 'synthetic-full'),
         n == 200, compliance_p == 0.5) %>%
  mutate(estimator =
           case_when(
             synth == 'non-synthetic' ~ case_when(
               theta_0 == 'atregr_est' ~ 'AT',
               theta_0 == 'iv_est' ~ 'IV',
               theta_0 == 'ppregr_est' ~ 'PP',
               theta_0 == 'tsls_est' ~ 'TSLS',
               theta_0 == 'ipw_est' ~ ''
             ),
             theta_0 == 'iv_est' ~ 'Synthetic IV',
             theta_0 == 'tsls_est' ~ 'Synthetic TSLS'
           ))

mse_pl <- ggplot(this_res,
                 aes(x = alpha_c, y = mse, group = g, color = theta_0, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'MSE') +
  ggtitle(expression('MSE as a function of NCE violation'))

tsls_res <- res %>%
  filter(theta_0 == 'tsls_est',
         synthetic,
         sample == 'full')
