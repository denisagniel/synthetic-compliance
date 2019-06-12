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

base_estimators <- sec_sum %>%
  filter(!synthetic,
         n == 1000)

ggplot(base_estimators,
       aes(x = theta, y = bias, group = theta_0, color = theta_0)) +
  facet_wrap(~ n, scales = 'free') +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('Bias of candidate')
ggplot(base_estimators,
       aes(x = theta, y = mse, group = theta_0, color = theta_0)) +
  facet_wrap(~ n, scales = 'free') +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('MSE of candidate estimators')

synthetic_compare <- sec_sum %>%
  filter((synthetic & theta_0 %in% c('iv_est', 'tsls_est') & sample == 'full' & !shrunk) | 
           !synthetic, n == 1000) %>%
  mutate(name = 
           case_when(synthetic ~ glue('synthetic-{theta_0}'),
                     TRUE ~ theta_0))
ggplot(synthetic_compare,
       aes(x = theta, y = bias, group = name, color = theta_0, linetype = synthetic)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('Bias of estimators')
ggplot(synthetic_compare,
       aes(x = theta, y = mse, group = name, color = theta_0, linetype = synthetic)) +
  facet_wrap(~ n, scales = 'free') +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('MSE of candidate estimators')
ggplot(synthetic_compare,
       aes(x = theta, y = var, group = name, color = theta_0, linetype = synthetic)) +
  facet_wrap(~ n, scales = 'free') +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('Variance of candidate estimators')

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
  facet_wrap(~ n, scales = 'free') +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('Bias of candidate')
ggplot(cv_compare,
       aes(x = theta, y = mse, group = name, color = theta_0, linetype = lt)) +
  facet_wrap(~ n, scales = 'free') +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('MSE of candidate estimators')
ggplot(cv_compare,
       aes(x = theta, y = var, group = name, color = theta_0, linetype = lt)) +
  facet_wrap(~ n, scales = 'free') +
  geom_point() +
  geom_line() + 
  theme_bw() +
  ggtitle('Variance of candidate estimators')
