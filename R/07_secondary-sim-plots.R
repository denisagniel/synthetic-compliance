#' ---
#' title: "Compliance plots"
#' output: github_document
#' ---
#' 
library(knitr)
opts_chunk$set(warning = FALSE, message = FALSE, cache = FALSE, fig.width = 7, fig.height = 7)

#'
library(tidyverse)
library(here)
library(glue)
library(patchwork)

secondary_res <- readRDS(here(
  'results/secondary-compliance-sim-results.rds'
)) %>%
  keep(is.data.frame) %>%
  bind_rows %>%
  filter(theta_0 != 'ivs_est')

sec_sum <- secondary_res %>%
  group_by(theta_0, shrunk, sample, synthetic, n, theta) %>%
  summarise(
    bias = mean(estimate - 1, na.rm = TRUE),
    est_var = mean(var, na.rm = TRUE),
    var = var(estimate, na.rm = TRUE),
    mse = mean((estimate - 1)^2, na.rm = TRUE),
    n_NA = sum(is.na(estimate))
  ) %>%
  ungroup


cv_compare <- sec_sum %>%
  filter((synthetic & theta_0 %in% c('iv_est', 'tsls_est') & !shrunk) | 
           !synthetic & theta_0 %in% c('atregr_est',
                                       'ipw_regr_est',
                                       'iv_est',
                                       'ppregr_est',
                                       'tsls_est')) %>%
  mutate(name = 
           case_when(synthetic ~ glue('synthetic-{theta_0}-{sample}'),
                     TRUE ~ theta_0),
         lt = if_else(synthetic, glue('Synthetic-{sample}'), 'Non-synthetic'))
bias_pl <- ggplot(cv_compare,
       aes(x = theta, y = bias, group = name, color = lt, linetype = theta_0)) +
  geom_point() +
  geom_line() + 
  theme_bw(base_size = 16) +
  ggtitle('Bias as a function of NCE violation',
          subtitle = 'Simulation 2') +
  labs(x = 'Value of omitted parameter', y = 'Bias') +
  scale_linetype_discrete(expression(hat(theta)),
                          labels = c('As-treated', 
                                     'PS-model-assisted',
                                     'IV', 'Per-protocol',
                                     'Two-stage LS')) +
  facet_wrap(~ n, scales = 'free') +
  scale_color_grey('', start = 0.3)
mse_pl <- bias_pl + aes(y = mse) +
  ggtitle('MSE as a function of NCE violation')
var_pl <- bias_pl + aes(y = var) +
  ggtitle('Variance as a function of NCE violation')

full_pl <- mse_pl + bias_pl + var_pl + plot_layout(ncol = 1)
full_pl
ggsave(here('figures/secondary-sim-mse-plot.png'),
       height = 16, width = 16)
