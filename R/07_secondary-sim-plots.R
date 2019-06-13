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
           !synthetic,
          n == 1000) %>%
  mutate(name = 
           case_when(synthetic ~ glue('synthetic-{theta_0}-{sample}'),
                     TRUE ~ theta_0),
         lt = if_else(synthetic, glue('Synthetic-{sample}'), 'Non-synthetic'))
ggplot(cv_compare,
       aes(x = theta, y = bias, group = name, color = theta_0, linetype = lt)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('Bias as a function of NCE violation',
          subtitle = 'Simulation 2') +
  labs(x = 'Value of omitted parameter', y = 'Bias') +
  scale_linetype_discrete('') +
  scale_color_discrete(expression(hat(theta)),
    labels = c('As-treated', 'AT-stratified', 'PS-weighted', 
               'PS-model-assisted',
               'IV', 'Per-protocol', 'PP-stratified', 
               'Two-stage LS')
  )
ggplot(cv_compare,
       aes(x = theta, y = mse, group = name, color = theta_0, linetype = lt)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('MSE as a function of NCE violation',
          subtitle = 'Simulation 2') +
  labs(x = 'Value of omitted parameter', y = 'MSE')+
  scale_linetype_discrete('') +
  scale_color_discrete(expression(hat(theta)),
                       labels = c('As-treated', 'AT-stratified', 'PS-weighted', 
                                  'PS-model-assisted',
                                  'IV', 'Per-protocol', 'PP-stratified', 
                                  'Two-stage LS')
  ) +
  scale_y_log10()
ggplot(cv_compare,
       aes(x = theta, y = var, group = name, color = theta_0, linetype = lt)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('Variance as a function of NCE violation',
          subtitle = 'Simulation 2') +
  labs(x = 'Value of omitted parameter', y = 'Variance')+
  scale_linetype_discrete('') +
  scale_color_discrete(expression(hat(theta)),
                       labels = c('As-treated', 'AT-stratified', 'PS-weighted', 
                                  'PS-model-assisted',
                                  'IV', 'Per-protocol', 'PP-stratified', 
                                  'Two-stage LS')
  )
