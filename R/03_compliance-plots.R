#-------------------------------
## compliance plots
#------------------------------

library(tidyverse)
library(patchwork)
library(glue)
res_sum <- readRDS('~/projects/ma/results/compliance/results_2018_09_18_summary.rds')
figdir <- '~/projects/ma/results/compliance/figures/'
dir.create(figdir)

res <- res_sum %>%
  filter(alpha_n == 0,
         # alpha_c == 0.5,
         gamma_n == 0,
         gamma_c == 0,
         lambda_n == 1,
         lambda_c == 1,
         compliance_effect == 2,
         # compliance_p == 0.7,
         # n == 1000,
         # !shrunk,
         !(synthetic & theta_0 %in% c('atregr_est', 'ppregr_est')))
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
         n == 200, compliance_p == 0.6) %>%
  mutate(estimator =
           case_when(
             synth == 'non-synthetic' ~ case_when(
               theta_0 == 'atregr_est' ~ 'AT',
               theta_0 == 'iv_est' ~ 'IV',
               theta_0 == 'ppregr_est' ~ 'PP',
               theta_0 == 'tsls_est' ~ 'TSLS'
             ),
             theta_0 == 'iv_est' ~ 'Synthetic IV',
             theta_0 == 'tsls_est' ~ 'Synthetic TSLS'
           ))

mse_pl <- ggplot(this_res,
       aes(x = alpha_c, y = mse, group = estimator, color = estimator, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'MSE') +
  ggtitle(expression('MSE as a function of magnitude of NCEC/NCET violation'))

bias_pl <- ggplot(this_res,
       aes(x = alpha_c, y = bias, group = estimator, color = estimator, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'Bias') +
  ggtitle(expression('Bias as a function of magnitude of NCEC/NCET violation'))

var_pl <- ggplot(this_res,
       aes(x = alpha_c, y = var, group = estimator, color = estimator, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'Variance') +
  ggtitle(expression('Variance as a function of magnitude of NCEC/NCET violation'))

final_pl <- mse_pl + bias_pl + var_pl + plot_layout(ncol = 1)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.png'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.pdf'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.tiff'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.eps'), height = 8, width = 8, plot = final_pl)

#-----------------------------------------------------------
## comparing CV vs regular synthetic
#--------------------------------------------------------
this_res <- res %>%
  filter(#synth %in% c('non-synthetic', 'synthetic-full', 'synthetic-CV'),
         !(synthetic & theta_0 == 'iv_est'),
         n == 200, compliance_p == 0.6,
         theta_0 == 'tsls_est') %>%
  mutate(estimator =
           case_when(
             synth == 'non-synthetic' ~ case_when(
               theta_0 == 'atregr_est' ~ 'AT',
               theta_0 == 'iv_est' ~ 'IV',
               theta_0 == 'ppregr_est' ~ 'PP',
               theta_0 == 'tsls_est' ~ 'TSLS'
             ),
             synth == 'synthetic-CV' ~ 'Sample-split Synthetic TSLS',
             synth == 'synthetic-full' ~ 'Synthetic TSLS',
             synth == 'synthetic-CV-shrunk' ~ 'Sample-split Modified Synthetic TSLS',
             synth == 'synthetic-full-shrunk' ~ 'Synthetic Modified TSLS'
           ))

mse_pl <- ggplot(this_res,
                 aes(x = alpha_c, y = mse, group = estimator, color = estimator, linetype = estimator)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'MSE') +
  ggtitle(expression('MSE as a function of magnitude of NCEC/NCET violation'))

bias_pl <- ggplot(this_res,
                  aes(x = alpha_c, y = bias, group = estimator, color = estimator, linetype = estimator)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'Bias') +
  ggtitle(expression('Bias as a function of magnitude of NCEC/NCET violation'))

var_pl <- ggplot(this_res,
                 aes(x = alpha_c, y = var, group = estimator, color = estimator, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'Variance') +
  ggtitle(expression('Variance as a function of magnitude of NCEC/NCET violation'))
final_pl <- mse_pl + bias_pl + var_pl + plot_layout(ncol = 1)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06_with_cv.png'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06_with_cv.pdf'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06_with_cv.tiff'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06_with_cv.eps'), height = 8, width = 8, plot = final_pl)


#-----------------------------------------------------
## null compliance effect
#-----------------------------------------------------
res <- res_sum %>%
  filter(alpha_n == 0,
         # alpha_c == 0.5,
         gamma_n == 0,
         gamma_c == 0,
         lambda_n == 1,
         lambda_c == 0,
         compliance_effect == 2,
         # compliance_p == 0.7,
         # n == 1000,
         # !shrunk,
         !(synthetic & theta_0 %in% c('atregr_est', 'ppregr_est')))
res <- res %>%
  mutate(g = paste0(theta_0, shrunk, sample, synthetic, shrunk),
         synth = case_when(
           sample == 'CV' & synthetic & !shrunk ~ 'synthetic-CV',
           sample == 'full' & synthetic & !shrunk ~ 'synthetic-full',
           sample == 'CV' & synthetic  & shrunk ~ 'synthetic-CV-shrunk',
           sample == 'full' & synthetic & shrunk ~ 'synthetic-full-shrunk',
           TRUE ~ 'non-synthetic'
         ))

this_res <- res %>%
  filter(synth %in% c('non-synthetic', 'synthetic-full'),
         n == 500, compliance_p == 0.6) %>%
  mutate(estimator =
           case_when(
             synth == 'non-synthetic' ~ case_when(
               theta_0 == 'atregr_est' ~ 'AT',
               theta_0 == 'iv_est' ~ 'IV',
               theta_0 == 'ppregr_est' ~ 'PP',
               theta_0 == 'tsls_est' ~ 'TSLS'
             ),
             theta_0 == 'iv_est' ~ 'Synthetic IV',
             theta_0 == 'tsls_est' ~ 'Synthetic TSLS'
           ))

mse_pl <- ggplot(this_res,
                 aes(x = alpha_c, y = mse, group = estimator, color = estimator, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'MSE') +
  ggtitle(expression('MSE as a function of magnitude of NCEC/NCET violation'))
mse_pl
bias_pl <- ggplot(this_res,
                  aes(x = alpha_c, y = bias, group = estimator, color = estimator, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'Bias') +
  ggtitle(expression('Bias as a function of magnitude of NCEC/NCET violation'))

var_pl <- ggplot(this_res,
                 aes(x = alpha_c, y = var, group = estimator, color = estimator, linetype = synth)) +
  geom_line() +
  theme_bw() +
  # scale_y_log10() +
  labs(x = expression(alpha[c]), y = 'Variance') +
  ggtitle(expression('Variance as a function of magnitude of NCEC/NCET violation'))

final_pl <- mse_pl + bias_pl + var_pl + plot_layout(ncol = 1)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.png'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.pdf'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.tiff'), height = 8, width = 8, plot = final_pl)
ggsave(glue('{figdir}mse_bias_var_for_n200_cp06.eps'), height = 8, width = 8, plot = final_pl)