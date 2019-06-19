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
               'pps_est', 'ipw_regr_est')),
         !theta_0 %in% c('ats_est',
                         'ipw_est',
                         'pps_est'))
res <- res %>%
  mutate(g = paste0(theta_0, shrunk, sample, synthetic, shrunk),
         synth = case_when(
           sample == 'CV' & synthetic & !shrunk ~ 'Synthetic-CV',
           sample == 'full' & synthetic & !shrunk ~ 'Synthetic-full',
           sample == 'CV' & synthetic  & shrunk ~ 'Synthetic-CV-shrunk',
           sample == 'full' & synthetic & shrunk ~ 'Synthetic-full-shrunk',
           TRUE ~ 'Non-synthetic'
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
  filter(synth %in% c('Non-synthetic', 'Synthetic-full')) %>%
  mutate(estimator =
           case_when(
             synth == 'non-synthetic' ~ case_when(
               theta_0 == 'atregr_est' ~ 'AT',
               theta_0 == 'iv_est' ~ 'IV',
               theta_0 == 'ppregr_est' ~ 'PP',
               theta_0 == 'tsls_est' ~ 'Two-stage LS',
               theta_0 == 'ipw_regr_est' ~ 'PS-model-assisted'
             ),
             theta_0 == 'iv_est' ~ 'Synthetic IV',
             theta_0 == 'tsls_est' ~ 'Synthetic TSLS'
           ))

mse_pl <- ggplot(this_res,
                 aes(x = alpha_c, y = mse, group = g, linetype = theta_0, color = synth)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = 16) +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'MSE') +
  ggtitle(expression('MSE as a function of NCE violation'),
          subtitle = 'Simulation 1, by sample size and compliance proportion') +
  scale_color_grey('', end = 0.75) +
  scale_linetype_discrete(expression(hat(theta)),
                          labels = c('As-treated', 
                                     'PS-model-assisted',
                                     'IV', 'Per-protocol',
                                     'Two-stage LS')) +
  facet_grid(compliance_p ~ n, scales = 'free')

mse_pl
ggsave(here('figures/main-sim-mse-plot.png'),
       height = 16, width = 16)
