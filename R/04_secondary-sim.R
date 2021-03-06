
remotes::install_github('denisagniel/synthate')  
tmpdir <- '/n/data1/hms/dbmi/zaklab/dma12/synthetic-causal-estimation/tmp-compliance-dl/'
fs::dir_create(tmpdir)


library(tidyverse)
library(clustermq)
library(synthate)
library(glue)
library(here)
#'
#' Set up simulations settings.
#'
sim_params <- expand.grid(n = c(200, 500, 1000),
                          theta = seq(-2, 2, length = 11),
                          run = 1:1000)
xs <- paste('x')
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

sim_fn <- function(n,
                   theta,
                   fn_list, 
                   run,
                   tmpdir) {
  # browser()
  library(purrr)
  library(tidyr)
  library(dplyr)
  library(clustermq)
  library(synthate)
  library(glue)
  xs <- paste('x')
  sfm <- as.formula(glue('s ~ {xs} + z'))
  psfm <- as.formula(glue('s ~ {xs}'))
  yfm <- as.formula(glue('y ~ shat + {xs}'))
  fm <- as.formula(glue('y ~ s + {xs}'))
  
  bootfn <- function(x, i) {
    # browser()
    new_data <- x[i,]
    
    ps_model <- glm(s ~ x, family = binomial, data = new_data %>% filter(z == 1))
    new_data <- new_data %>%
      mutate(pr_score = predict(ps_model, newdata = new_data, type = 'response'),
             ps_grp = Hmisc::cut2(pr_score, g = 5),
             pi_c = mean(pr_score)) %>%
      group_by(ps_grp) %>%
      mutate(pi_cj = mean(pr_score)) %>%
      ungroup
    
    
    out <- estimate_ates(new_data,  fn_list)
    # if (any(is.nan(unlist(out))) | 
    #     any(is.na(unlist(out)))) browser()
    as.matrix(out)
  }
  
  set.seed(run)
  print(glue('Simulation {run} for n = {n}, theta = {theta}.'))
  sim_data <- 
    generate_data_dl(n, 
                     theta = theta)
  train_data <- sim_data %>% sample_frac(0.5)
  test_data <- sim_data %>% anti_join(train_data)
  
  #------------------------------
  ## first, all analysis on full data
  #-----------------------------
  ps_model <- glm(s ~ x, family = binomial, 
                  data = sim_data %>% filter(z == 1))
  
  sim_data <- sim_data %>%
    mutate(pr_score = predict(ps_model, newdata = sim_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5),
           pi_c = mean(pr_score)) %>%
    group_by(ps_grp) %>%
    mutate(pi_cj = mean(pr_score)) %>%
    ungroup
  full_caces <- estimate_ates(sim_data, fn_list)
  b_theta <- boot::boot(sim_data, bootfn, R = 200)$t
  boot_theta_s <- combine_estimators(ests = full_caces, boot_ests = b_theta)
  full_synth_ests <- boot_theta_s$ate_res %>%
    group_by(theta_0, shrunk, synthetic) %>%
    transmute(estimate = ate,
              var = var,
              sample = 'full')
  
  #----------------------------
  ## now use train data to fit model and combine on test data
  #---------------------------
  ps_model_1 <- glm(s ~ x, family = binomial, data = train_data %>% filter(z == 1))
  train_data <- train_data %>%
    mutate(pr_score = predict(ps_model_1, newdata = train_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5),
           pi_c = mean(pr_score)) %>%
    group_by(ps_grp) %>%
    mutate(pi_cj = mean(pr_score)) %>%
    ungroup
  test_data <- test_data %>%
    mutate(pr_score = predict(ps_model_1, newdata = test_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5),
           pi_c = mean(pr_score)) %>%
    group_by(ps_grp) %>%
    mutate(pi_cj = mean(pr_score)) %>%
    ungroup
  train_caces_1 <- estimate_ates(train_data, fn_list)
  test_caces_1 <- estimate_ates(test_data, fn_list)
  
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
           ps_grp = Hmisc::cut2(pr_score, g = 5),
           pi_c = mean(pr_score)) %>%
    group_by(ps_grp) %>%
    mutate(pi_cj = mean(pr_score)) %>%
    ungroup
  test_data <- test_data %>%
    mutate(pr_score = predict(ps_model_2, newdata = test_data, type = 'response'),
           ps_grp = Hmisc::cut2(pr_score, g = 5),
           pi_c = mean(pr_score)) %>%
    group_by(ps_grp) %>%
    mutate(pi_cj = mean(pr_score)) %>%
    ungroup
  train_caces_2 <- estimate_ates(train_data, fn_list)
  test_caces_2 <- estimate_ates(test_data, fn_list)
  
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
    mutate(var = diag(cov(b_theta)),
           shrunk = FALSE,
           sample = 'full',
           synthetic = FALSE) %>%
    full_join(full_synth_ests) %>%
    full_join(holdout_ests)
  
  out <- long_caces %>%
    mutate(n = n,
           theta = theta)
  saveRDS(out, 
          glue('{tmpdir}compliance-res-n{n}-theta{theta}-sim{run}.rds'))
  out
}
# 
# tst <- sample_n(sim_params, 1)
# with(tst, sim_fn(n = n,
#                  theta = theta,
#                  fn_list = fnl,
#                  run = run,
#                  tmpdir = tmpdir))



options(
  clustermq.defaults = list(ptn="short",
                            log_file="Rout/dl_log%a.log",
                            time_amt = "12:00:00"
  )
)
sim_res <- Q_rows(sim_params, sim_fn, 
                  const = list(fn_list = fnl,
                               tmpdir = tmpdir),
                  export = list(sfm = sfm,
                                fm = fm,
                                psfm = psfm,
                                yfm = yfm),
                  fail_on_error = FALSE,
                  n_jobs = 100)
saveRDS(sim_res, here('results/secondary-compliance-sim-results.rds'))
