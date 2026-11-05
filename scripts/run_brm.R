# run bayesian model from shell

library(brms)             # for Bayesian model
library(tidyverse)

# load data
d2 <- read_csv('data/exp_2_data_long.csv', col_types = 'fffiiiffdiifi')

# remove everything that is not our desired count outcome
d2 <- d2 |> 
  filter(str_detect(response_data_type, 'responses'))

# parse response objects into broad types: lever/entries, then aggregate counts
agg_d2 <- d2 |> 
  mutate(
    response_type = factor(str_extract(response_object, '[a-z]+(?=-)'))
  ) |> 
  group_by(rat_id, trial_id, response_type) |> 
  summarize(
    across(c(rat:day), unique),
    count = sum(response)
  ) |> 
  ungroup()

# apply codings where applicable and create centered variables within rats
agg_d2 <- agg_d2 |> 
  mutate(
    c_drug = as.numeric(scale(drug))
  ) |> 
  group_by(rat_id) |> 
  mutate(
    c_trial = as.numeric(scale(trial_id))
  ) |> 
  ungroup()

# contrast coding for cats
contrasts(agg_d2$response_type) <- contr.sum(n_distinct(agg_d2$response_type))
contrasts(agg_d2$sex) <- contr.sum(n_distinct(agg_d2$sex))

# fit model
message('Data cleaned, now compiling model...')

b <- brm(
  # model setup
  formula = bf(
    count ~ response_type * sex * c_trial * c_drug + (1 | cohort) + # this is the count model
      (c_trial * response_type | rat_id),                 
    hu ~ response_type * sex * c_trial * c_drug + (1 | cohort) +    # this is the hurdle model
      (c_trial * response_type | rat_id)                     
  ), 
  data = agg_d2, 
  family = hurdle_negbinomial(),
  
  # priors
  prior = c(
    prior(normal(0, 3), class = 'b'),                # for count/neg-binom model FEs
    prior(normal(0, 3), class = 'b', dpar = 'hu'),   # for hurdle model
    prior(exponential(1), class = 'sd'),
    prior(exponential(1), class = 'sd', dpar = 'hu') # for hurdle model
  ),
  
  # control pars for HMC
  chains = 4, cores = 4,
  backend = 'cmdstanr',
  threads = threading(2),
  iter = 2000, warmup = 500,
  refresh = 1,
  normalize = FALSE,
  control = list(adapt_delta = .99), 
  seed = 11052026,
  file = 'fits/b2_full_final.rds'
)
