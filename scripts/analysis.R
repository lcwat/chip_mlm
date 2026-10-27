# goal/sign tracking Poisson GLMM

# Lucas Watson

# load libraries ----------------------------------------------------------

library(tidyverse)
library(glmmTMB)          # for negative binomial model
library(DHARMa)           # model diagnositics
library(brms)             # for Bayesian model

# load data ---------------------------------------------------------------

# model remains the same across exp, slightly different data structure requires
# loading in clean data slightly differently
d <- read_csv('data/exp_1_data_long.csv', col_types = 'fffiiiffdifi')
d <- read_csv('data/exp_2_data_long.csv', col_types = 'fffiiiffdiifi')

glimpse(d)

# clean up into format we need, need to aggregate responses across LR for both
# levers and head entries

# remove everything that is not our desired count outcome
d <- d |> 
  filter(str_detect(response_data_type, 'responses'))

# parse response objects into broad types: lever/entries, then aggregate counts
agg_d <- d |> 
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
agg_d <- agg_d |> 
  mutate(
    c_drug = as.numeric(scale(drug))
  ) |> 
  group_by(rat_id) |> 
  mutate(
    c_trial = as.numeric(scale(trial_id))
  ) |> 
  ungroup()

# contrast coding for cats
contrasts(agg_d$response_type) <- contr.sum(n_distinct(agg_d$response_type))
contrasts(agg_d$sex) <- contr.sum(n_distinct(agg_d$sex))

# exp 1 only! 
# clean up cohort names to remove unnecessary spaces and concatenate with _
agg_d <- agg_d |> 
  mutate(
    cohort = factor(str_remove(str_replace(str_to_lower(agg_d$cohort), ' ', '_'), ' '))
  )

# take a look
summary(agg_d)

# model -------------------------------------------------------------------

# look at the distribution of responses to see if there is visual zero inflation
agg_d |> 
  mutate(
    zero_count = factor(if_else(count == 0, 'zero', 'not-zero'))
  ) |> 
  ggplot(aes(x = count, fill = zero_count)) + 
  geom_bar() + 
  scale_x_continuous(breaks = seq(0, 40)) +
  scale_fill_manual(guide = 'none', values = c('black', 'firebrick')) + 
  theme_bw()

# lots of zero inflation, will almost certainly be overdispersed, opting for 
# negative binomial likelihood distribution to account for zero-inflation

# build the model, start with a standard poisson to verify overdispersion and 
# zero-inflation is driving that
base <- glmmTMB(
  formula = count ~ response_type * sex * c_drug * c_trial + (c_trial * response_type | rat_id), 
  data = agg_d, 
  family = poisson
)

# get residuals
resid <- simulateResiduals(base)

plot(resid)

# check overdispersion and zero-inflation
testDispersion(resid)
testZeroInflation(resid)

# suggests we need a hurdle negative binomial model to account for both overdispersion
# and zero-inflation
m_tmb <- glmmTMB(
  formula = count ~ response_type * sex * c_drug * c_trial + (c_trial * response_type | rat_id) + (1 | cohort), 
  ziformula = ~ response_type * sex * c_drug * c_trial, 
  data = agg_d, 
  family = truncated_nbinom2
)

# check residuals
resid <- simulateResiduals(m_tmb)

plot(resid)

# check overdispersion and zero-inflation
testDispersion(resid)
testZeroInflation(resid)

summary(m_tmb)

# write to file
write_rds(m_tmb, 'fits/tmb2_final.rds')

# bayesian model ----------------------------------------------------------

# model will need weakly informed priors for FEs, weakly regularizing for REs
b_final <- brm(
  # model setup
  formula = bf(
    count ~ response_type * sex * c_trial * c_drug + (1 | cohort) + 
      (c_trial * response_type | rat_id),                 # this is the count model
    hu ~ response_type * sex * c_trial * c_drug    # this is the hurdle model
  ), 
  data = agg_d, 
  family = hurdle_negbinomial(),
  
  # priors
  prior = c(
    prior(normal(0, 3), class = 'b'),              # for count/neg-binom model FEs
    prior(normal(0, 3), class = 'b', dpar = 'hu'), # for hurdle model
    prior(exponential(1), class = 'sd')            # for count model RE variances
  ),
  
  # control pars for HMC
  chains = 4, cores = 4,
  backend = 'cmdstanr',
  iter = 3000, warmup = 1000, 
  control = list(adapt_delta = .95), 
  seed = 10262026,
  init = 0, 
  file = 'fits/b1_final.rds'
)

