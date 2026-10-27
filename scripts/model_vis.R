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
b2_model <- read_rds('fits/b2_final.rds')
b1_model <- read_rds('fits/b1_final.rds')

# load in datasets
b2_data <- read_csv('data/exp_2_data_long.csv')

# chain diagnostics -------------------------------------------------------

# visualize chain sampling behavior
draws <- tidy_draws(b2_model)

plot(b2_model)

# chains look good but I used the wrong RE structure :)

# parameter summary -------------------------------------------------------

# can at least look at how to extract important stuff for checking model
summary_table <- draws |> 
  summarise_draws()

# plot out halfeyes for easy visual
b_halfeyes <- draws |> 
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
sd_halfeyes <- draws |> 
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
preds <- b2_model |> 
  predictions(
    newdata = datagrid(
      response_type = unique, 
      c_trial = seq(-2, 2, length.out = 10)
    )
  ) |> 
  posterior_draws()

preds |> 
  ggplot(aes(x = c_trial, y = draw)) + 
  stat_lineribbon(point_interval = 'median_hdi') + 
  scale_fill_manual('Posterior density', values = c('grey95', 'grey80', 'grey40')) + 
  scale_x_continuous(
    'Trial', 
    breaks = seq(-2, 2, length.out = 6), 
    labels = round(seq(1, 96, length.out = 6), -1)
  ) + 
  labs(
    x = 'Trial', 
    y = '# Responses'
  ) + 
  classic_bn_theme() +
  facet_wrap(~response_type)

# four-way interaction: sex, response-type, drug, and trial
preds <- b2_model |> 
  predictions(
    newdata = datagrid(
      sex = unique, 
      response_type = unique,
      c_drug = unique, 
      c_trial = seq(-2, 2, length.out = 10)
    )
  ) |> 
  posterior_draws()

preds |> 
  group_by(sex, response_type, c_drug, c_trial) |> 
  median_hdi(draw) |> 
  mutate(
    sex = factor(if_else(sex == 'M', 'Male', 'Female')),
    response_type = factor(if_else(response_type == 'entries', 'Head entry', 'Lever press'))
  ) |> 
  ggplot(aes(x = c_trial, y = draw, linetype = factor(c_drug))) + 
  geom_line() + 
  # geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = factor(c_drug)), alpha = .2, color = NA) +
  scale_x_continuous(
    'Trial', 
    breaks = seq(-2, 2, length.out = 6), 
    labels = round(seq(1, 96, length.out = 6), -1)
  ) + 
  scale_linetype_manual('Treatment', labels = c('Saline', '3 mg/L', '6 mg/L'), values = c(1, 2, 3)) +
  labs(
    x = 'Trial', 
    y = '# Responses'
  ) + 
  classic_bn_theme() +
  facet_grid(rows = vars(sex), cols = vars(response_type))
