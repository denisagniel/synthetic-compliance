library(dplyr)
library(cci)


date <- '2018_09_26'
resdir <- paste0('/n/data1/hms/dbmi/zaklab/dma12/cci/results/compliance_', date, '/')
res_l <- list()
for (i in 1:1000) {
  res <- Dmisc::myTry(readRDS(paste0(resdir, 'res_', i, '.rds')))
  if(!Dmisc::isErr(res)) res_l[[i]] <- res
}
full_res <- bind_rows(res_l)
saveRDS(full_res, paste0(resdir, 'results_', date, '_full.rds'))

res_sum <- full_res %>%
  group_by(theta_0, shrunk, sample, synthetic, n, alpha_n, alpha_c, gamma_n, gamma_c, lambda_c, lambda_n, compliance_p, compliance_effect) %>%
  summarise(mse = mean((estimate - gamma_c)^2),
            bias = mean(estimate - gamma_c),
            var = var(estimate),
            nsim = n())

saveRDS(res_sum, paste0(resdir, 'results_', date, '_summary.rds'))

res_sum %>%
  filter(n == 200,
         gamma_n == 0,
         lambda_n == 1,
         lambda_c == 1,
         alpha_n == 1,
         alpha_c == 1,
         gamma_c == 1,
         compliance_p == 0.5,
         compliance_effect == 0.5) %>%
  arrange(mse) %>%
  select(theta_0, shrunk, sample, synthetic, mse, bias, var, nsim) %>%
  data.frame

full_res %>%
  group_by(estimator, sample) %>%
  summarise(mse = mean((est - 0.5)^2),
            bias = mean(est - 0.5),
            var = var(est)) %>%
  arrange(mse) %>%
  data.frame %>%
  filter(sample %in% c('full', 'CV'))

full_res %>%
  group_by(estimator, sample) %>%
  summarise(mse = mean((est - 0.5)^2),
            bias = mean(est - 0.5),
            var = var(est)) %>%
  ungroup %>%
  filter(sample %in% c('full', 'CV')) %>%
  mutate(mse_pct = mse/mse[estimator == 'iv_est'],
         bias_pct = abs(bias)/abs(bias)[estimator == 'iv_est'],
         var_pct = var/var[estimator == 'iv_est']) %>%
  arrange(mse) %>%
  data.frame


date <- '2018_06_06'
resdir <- paste0('/n/data1/hms/dbmi/zaklab/dma12/cci/results/compliance_', date, '/')
res_l <- list()
for (i in 1:3) {
  res <- Dmisc::myTry(readRDS(paste0(resdir, 'res_', i, '.rds')))
  if(!Dmisc::isErr(res)) res_l[[i]] <- res
}
full_res <- bind_rows(res_l)


full_res %>%
  group_by(theta_0, sample, shrunk, sample, synthetic, n, alpha_c, compliance_p, compliance_effect) %>%
  summarise(mse = mean((estimate - 0.5)^2),
            bias = mean(estimate - 0.5),
            var = var(estimate),
            n_sim = n()) %>%
  # filter(alpha_c == 0.5, compliance_p == 0.5, compliance_effect == 0.5, n == 500, !shrunk) %>%
  ungroup %>%
  mutate(mse_pct = mse/mse[theta_0 == 'ivw_fn' & sample == 'full' & !synthetic],
         bias_pct = abs(bias)/abs(bias)[theta_0 == 'ivw_fn' & sample == 'full' & !synthetic],
         var_pct = var/var[theta_0 == 'ivw_fn' & sample == 'full' & !synthetic]) %>%
  arrange(mse) %>%
  data.frame

#-----------------------
## 9/18

date <- '2018_09_18'
resdir <- paste0('/n/data1/hms/dbmi/zaklab/dma12/cci/results/compliance_', date, '/')
res_l <- list()
for (i in 1:1000) {
  res <- Dmisc::myTry(readRDS(paste0(resdir, 'res_', i, '.rds')))
  if(!Dmisc::isErr(res)) res_l[[i]] <- res
}
full_res <- bind_rows(res_l, .id = 'run')
saveRDS(full_res, stringr::str_c(resdir, 'full_res_', date, '.rds'))

res_sum <- full_res %>%
  group_by(theta_0, shrunk, sample, synthetic, n, alpha_n, alpha_c, gamma_n, gamma_c, lambda_c, lambda_n, compliance_p, compliance_effect) %>%
  summarise(mse = mean((estimate - gamma_c)^2),
            bias = mean(estimate - gamma_c),
            var = var(estimate),
            nsim = n())

saveRDS(res_sum, paste0(resdir, 'results_', date, '_summary.rds'))


res_sum %>%
  ungroup %>%
  filter(n == 200, lambda_n == 0, lambda_c == 0, gamma_c == 0, compliance_p == 0.5, compliance_effect == 2, alpha_c == 0.5) %>%
  select(theta_0, shrunk, sample, synthetic, mse, bias, var, nsim) %>%
  arrange(mse)

