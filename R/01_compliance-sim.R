#### WIP
## need to add clustermq functionality.
remotes::install_github('denisagniel/synthate')  
tmpdir <- '/n/data1/hms/dbmi/zaklab/dma12/synthetic-causal-estimation/tmp-compliance/'
fs::dir_create(tmpdir)

library(purrr)
library(tidyr)
library(dplyr)
library(clustermq)
library(synthate)
library(glue)
#'
#' Set up simulations settings.
#' 
sim_params <- expand.grid(compliance_p = seq(0.5, 0.9, length = 5),
                          compliance_effect = c(0, 0.5, 2),
                          alpha_n = 0,
                          alpha_c = seq(0, 0.5, length = 6),
                          lambda_n = 0:1,
                          lambda_c = 0:1,
                          gamma_c = 0:1,
                          gamma_n = 0,
                          n = c(1000, 500, 200))

fnl <- list(
  iv_fn,
  # at_fn,
  # pp_fn,
  tsls_fn,
  # regr_fn,
  atregr_fn,
  ppregr_fn,
  ivs_fn,
  ats_fn,
  pps_fn,
  # ivw_fn,
  ipw_fn,
  ipw_regr_fn
)
bootfn <- function(x, i) {
  # browser()
  new_data <- x[i,]

  ps_model <- glm(s ~ x, family = binomial, data = new_data %>% filter(z == 1))
  new_data <- new_data %>%
    mutate(pr_score = predict(ps_model, newdata = new_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5))


  out <- estimate_ates(new_data,  fnl)
  as.matrix(out)
}

params <- expand.grid(compliance_p = seq(0.5, 0.9, length = 5),
                      compliance_effect = c(0, 0.5, 2),
                      alpha_n = 0,
                      alpha_c = seq(0, 0.5, length = 6),
                      lambda_n = 0:1,
                      lambda_c = 0:1,
                      gamma_c = 0:1,
                      gamma_n = 0,
                      n = c(1000, 500, 200))

sim_fn <- function(n,
                   alpha_c,
                   alpha_n,
                   lambda_c,
                   lambda_n,
                   gamma_c,
                   gamma_n,
                   compliance_p,
                   compliance_effect) {
  
  sim_data <- generate_data_sj(n, compliance_p = compliance_p, compliance_effect = compliance_effect, alpha_c = alpha_c, alpha_n = alpha_n, lambda_c = lambda_c, lambda_n = lambda_n, gamma_c = gamma_c, gamma_n = gamma_n)
  train_data <- sim_data %>% sample_frac(0.5)
  test_data <- sim_data %>% anti_join(train_data)
  
  #------------------------------
  ## first, all analysis on full data
  #-----------------------------
  ps_model <- glm(s ~ x, family = binomial, data = sim_data %>% filter(z == 1))
  
  sim_data <- sim_data %>%
    mutate(pr_score = predict(ps_model, newdata = sim_data, type = 'response'))
  full_caces <- estimate_ates(sim_data, fnl)
  b_theta <- boot::boot(sim_data, bootfn, R = 200)$t
  boot_theta_s <- combine_estimators(ests = full_caces, boot_ests = b_theta)
  full_synth_ests <- boot_theta_s$ate_res %>%
    group_by(theta_0, shrunk, synthetic) %>%
    transmute(estimate = ate,
              sample = 'full')
  
  #----------------------------
  ## now use train data to fit model and combine on test data
  #---------------------------
  ps_model_1 <- glm(s ~ x, family = binomial, data = train_data %>% filter(z == 1))
  train_data <- train_data %>%
    mutate(pr_score = predict(ps_model_1, newdata = train_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5))
  test_data <- test_data %>%
    mutate(pr_score = predict(ps_model_1, newdata = test_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5))
  train_caces_1 <- estimate_ates(train_data, fnl)
  test_caces_1 <- estimate_ates(test_data, fnl)
  
  # b_theta_1 <- boot::boot(train_data, bootfn, R = 200)$t
  boot_theta_s_1 <- combine_estimators(ests = train_caces_1, boot_ests = b_theta)
  
  
  # bhat_1 <- boot_theta_s_1$b_res %>% filter(!shrunk) %>% select(b) %>% unlist
  # holdout_est_1 <- unlist(test_caces_1) %*% bhat_1
  
  test_caces_ds <- test_caces_1 %>%
    gather(est, cace)
  holdout_ests_1 <- test_caces_ds %>%
    inner_join(boot_theta_s_1$b_res) %>%
    group_by(theta_0, shrunk) %>%
    summarise(estimate = sum(cace*b))%>%
    mutate(sample = 'CV',
           synthetic = TRUE)
  
  #-------------------------------
  ## flip the script
  #-----------------------------
  ps_model_2 <- glm(s ~ x, family = binomial, data = test_data %>% filter(z == 1))
  train_data <- train_data %>%
    mutate(pr_score = predict(ps_model_2, newdata = train_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5))
  test_data <- test_data %>%
    mutate(pr_score = predict(ps_model_2, newdata = test_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5))
  train_caces_2 <- estimate_ates(train_data, fnl)
  test_caces_2 <- estimate_ates(test_data, fnl)
  
  # b_theta_2 <- boot::boot(test_data, bootfn, R = 200)$t
  boot_theta_s_2 <- combine_estimators(ests = test_caces_2, boot_ests = b_theta)
  
  # bhat_2 <- boot_theta_s_2$b_res %>% filter(!shrunk) %>% select(b) %>% unlist
  # holdout_est_2 <- unlist(train_caces_2) %*% bhat_2
  train_caces_ds <- train_caces_2 %>%
    gather(est, cace)
  holdout_ests_2 <- train_caces_ds %>%
    inner_join(boot_theta_s_2$b_res) %>%
    group_by(theta_0, shrunk) %>%
    summarise(estimate = sum(cace*b)) %>%
    mutate(sample = 'CV',
           synthetic = TRUE)
  
  holdout_ests <- holdout_ests_1 %>%
    full_join(holdout_ests_2) %>%
    group_by(theta_0, shrunk, sample, synthetic) %>%
    summarise(estimate = mean(estimate))
  
  long_caces  <- full_caces %>%
    gather(theta_0, estimate) %>%
    mutate(shrunk = FALSE,
           sample = 'full',
           synthetic = FALSE) %>%
    full_join(full_synth_ests) %>%
    full_join(holdout_ests)
  
  
  # ests_out <- data.frame(
  #   estimator = c(
  #     names(full_caces),
  #     names(train_caces_1),
  #     names(test_caces_2),
  #     rep('synthetic',4)
  #   ),
  #   est = c(unlist(full_caces),
  #           unlist(train_caces_1),
  #           unlist(test_caces_2),
  #           boot_theta_s$ate_res$ate[1],
  #           boot_theta_s_1$ate_res$ate[1],
  #           boot_theta_s_2$ate_res$ate[1],
  #           (boot_theta_s_1$ate_res$ate[1]+
  #              boot_theta_s_2$ate_res$ate[1])/2
  #   ),
  #   sample = c(rep('full', ncol(full_caces)),
  #              rep('train', ncol(train_caces_1)),
  #              rep('test', ncol(test_caces_2)),
  #              'full', 'train->test', 'test->train', 'CV'
  #   )
  # )
  # })
  
  res_l[[i]] <- long_caces %>%
    mutate(n = n,
           alpha_n = alpha_n,
           alpha_c = alpha_c,
           gamma_c = gamma_c,
           gamma_n = gamma_n,
           lambda_c = lambda_c,
           lambda_n = lambda_n,
           compliance_p = compliance_p,
           compliance_effect = compliance_effect)
}

# n <- 1000
res_l <- list()
# for (i in 1:20) {
for (i in 1:nrow(params)) {
  # system.time({
# i <- sample(1:nrow(params), 1)
# params[i,]
  
  print(res_l[[i]])
  res <- bind_rows(res_l)
  saveRDS(res, file = paste0(resdir, 'res_', pn, '.rds'))
  print(i)
}

# cbind(caces %>% unlist, test_caces %>% unlist, bhat %>% round(3), bhat_test %>% round(3), varest = diag(boot_theta_s$C) %>% round(3))



