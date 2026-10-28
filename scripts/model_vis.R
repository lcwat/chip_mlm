# model follow up, visualizing chains and probing significant effects

# Lucas Watson

# load libraries ----------------------------------------------------------

library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(marginaleffects)

source('util/classic_bn_theme.r')

# load fits ---------------------------------------------------------------

# load in models
b1_model <- read_rds('fits/b1_final.rds')
b2_model <- read_rds('fits/b2_final.rds')

# load in datasets
b1_data <- read_csv('data/model_1_data.csv')
b2_data <- read_csv('data/model_2_data.csv')

# attach data to models
set_modeldata(b1_model, b1_data)
set_modeldata(b2_model, b2_data)

# chain diagnostics -------------------------------------------------------

# visualize chain sampling behavior
draws1 <- tidy_draws(b1_model)
draws2 <- tidy_draws(b2_model)

plot(b1_model)
plot(b2_model)

# chains look good across both models, only questionable one is the variance of 
# the cohort intercept which doesn't look appreciably different from the prior

# parameter summary -------------------------------------------------------

# get a numerical summary of the model parameters
summary_table1 <- draws1 |> 
  summarise_draws() |> 
  mutate(
    across(mean:rhat, \(x) round(x, 2))
  ) |> 
  knitr::kable(
    format = 'markdown', 
    col.names = c('Parameter', 'Mean', 'Median', 'SD', 'MAD', 'Lower C.I.', 'Upper C.I.', 'Rhat', 'Bulk ESS', 'Tail ESS')
  )

summary_table2 <- draws2 |> 
  summarise_draws() |> 
  mutate(
    across(mean:rhat, \(x) round(x, 2))
  ) |> 
  knitr::kable(
    format = 'markdown', 
    col.names = c('Parameter', 'Mean', 'Median', 'SD', 'MAD', 'Lower C.I.', 'Upper C.I.', 'Rhat', 'Bulk ESS', 'Tail ESS')
  )

# kable tables for markdown report
write_lines(summary_table1, 'tables/b1_full_table.md')
write_lines(summary_table2, 'tables/b2_full_table.md')

# plot out halfeyes for easy visual
b_halfeyes <- draws2 |> 
  select(starts_with('b_')) |> 
  pivot_longer(everything(), names_to = 'par', values_to = 'draw') |> 
  mutate(
    par = factor(par),
    mu_hu = if_else(str_detect(par, '_hu_'), 'hurdle', 'count')
  ) |> 
  # plot
  ggplot(aes(x = draw, y = par)) + 
  stat_halfeye(fill = 'grey', size = .05) + 
  geom_vline(xintercept = 0, linetype = 3) + 
  classic_bn_theme() + 
  facet_wrap(~mu_hu, scales = 'free_y')

# save
ggsave(plot = b_halfeyes, filename = 'fig_output/b2_halfeyes.png', device = 'png', width = 10, height = 5, units = 'in')

# variances
sd_halfeyes <- draws2 |> 
  select(starts_with('sd_')) |> 
  pivot_longer(everything(), names_to = 'par', values_to = 'draw') |> 
  mutate(
    par = factor(par)
  ) |> 
  # plot
  ggplot(aes(x = draw, y = par)) + 
  stat_halfeye(fill = 'grey', size = .1) + 
  geom_vline(xintercept = 0, linetype = 3) + 
  classic_bn_theme()

ggsave(plot = sd_halfeyes, filename = 'fig_output/sd2_halfeyes.png', device = 'png', width = 10, height = 6, units = 'in')

# parameter visualization -------------------------------------------------

# visualize interactions of interest

# clean up model data
plot_b1_data <- b1_data |> 
  mutate(
    response_type = factor(if_else(response_type == 'entries', 'Head entry', 'Lever press'))
  )
plot_b2_data <- b2_data |> 
  mutate(
    sex = factor(if_else(sex == 'M', 'Male', 'Female')),
    response_type = factor(if_else(response_type == 'entries', 'Head entry', 'Lever press'))
  )

# response type, sex, trial
preds <- b2_model |> 
  predictions(
    newdata = datagrid(
      sex = unique,
      response_type = unique, 
      c_trial = seq(min(b1_data$c_trial), max(b1_data$c_trial), length.out = 10)
    ), 
    re_formula = NA # exclude uncertainty from REs?
  ) |> 
  posterior_draws()

preds |> 
  mutate(
    sex = factor(if_else(sex == 'M', 'Male', 'Female')), # uncomment if model 1
    response_type = factor(if_else(response_type == 'entries', 'Head entry', 'Lever press'))
  ) |> 
  ggplot(aes(x = c_trial, y = draw)) + 
  geom_point(
    data = plot_b2_data, 
    aes(x = c_trial, y = count), 
    alpha = .1, 
    shape = 1
  ) +
  stat_lineribbon(point_interval = 'median_hdi', alpha = .7) + 
  scale_fill_manual('Density interval', values = c('grey95', 'grey80', 'grey40')) + 
  scale_x_continuous(
    'Trial', 
    breaks = seq(min(plot_b2_data$c_trial), max(plot_b2_data$c_trial), length.out = 10), 
    labels = round(seq(0, 96, length.out = 10), -1)
  ) + 
  labs(
    x = 'Trial', 
    y = '# Responses'
  ) + 
  classic_bn_theme() +
  facet_grid(cols = vars(response_type), rows = vars(sex))

ggsave('fig_output/b2_resp_trial_sex_ixn_w_data.png', device = 'png', width = 8, height = 6, units = 'in')

# model 1
# trial x drug

# response type x drug

# trial x response type

# model 2
# trial x sex x response type

# trial x response type


# need to also figure out how we could potentially visualize the hurdle effects, 
# given that there are a lot of significant, sizeable par values. for interpretation, 
# it is like the probability of making it over the hurdle to make a response. could
# maybe find a way to visualize this as P(response) varied by predictors.
