# initial descriptive visualizations to verify data look the same as Chip
# mapped out

# Lucas Watson

# load libraries ----------------------------------------------------------

library(tidyverse)
library(showtext)



# load data ---------------------------------------------------------------

# load clean, long df
d <- read_csv('data/exp_2_data_long.csv', col_types = 'fffiiiffdfifi')

# summary
summary(d)

# numerical summary

# all unique response types in this dataset
d |> 
  count(response_object, response_data_type)

# response count and presence/absence for lever/head entries are of main interest

# demographics for cohorts
d |> 
  group_by(cohort) |> 
  summarize(
    prop_male = sum(sex == 'M') / n(), 
    prop_female = sum(sex == 'F') / n(), 
    n = length(unique(rat_id))
  )

# visualize ---------------------------------------------------------------

# plot mean lever responses as a function of trials
d_resp_mean <- d |> 
  filter(response_data_type == 'responses-count') |>
  group_by(rat_id, day, response_object) |> 
  mutate(
    cum_resp = cumsum(response)
  ) |> 
  ungroup() |> 
  group_by(trial, response_object) |> 
  summarize(
    mean_resp = mean(cum_resp, na.rm = T), 
    median_resp = median(cum_resp, na.rm = T)
  )

# plot individual cumsum traces under mean/median line for all rats
d |> 
  filter(response_data_type == 'responses-count') |> 
  group_by(rat_id, day, response_object) |> 
  mutate(
    cum_resp = cumsum(response)
  ) |> 
  ggplot(aes(x = trial, y = log(cum_resp))) + 
  geom_path(aes(color = rat_id), alpha = .2, linewidth = .5) + 
  geom_line(
    data = d_resp_mean,
    aes(y = log(mean_resp)), 
    color = 'black', 
    linewidth = 1.2
  ) + 
  scale_color_grey(guide = 'none') + 
  classic_bn_theme() +
  facet_wrap(~response_object)

# looks about like what chip was plotting out with his code just without the individual
# traces, as expected, quite a bit of variation in individuals with regard to 
# sign tracking vs goal tracking

# effect of drug dose on cumulative responses
d |> 
  filter(response_data_type == 'responses-count') |> 
  group_by(drug, day, response_object) |> 
  mutate(
    cum_resp = cumsum(response)
  ) |> 
  ggplot(aes(x = trial, y = cum_resp)) + 
  geom_path(aes(linetype = as.factor(drug))) + 
  scale_linetype('Drug dose', labels = c('Saline', '3 mg/L', '6 mg/L')) + 
  classic_bn_theme() +
  facet_grid(response_object ~ day)

# not super different, but perceptible overall difference as more sign-tracking for
# the higher dose group compared to saline controls

# sex
d |> 
  filter(response_data_type == 'responses-count') |> 
  group_by(sex, response_object) |> 
  mutate(
    cum_resp = cumsum(response)
  ) |> 
  ggplot(aes(x = trial_id, y = cum_resp)) + 
  geom_line(aes(color = as.factor(sex))) + 
  scale_color_viridis_d('Sex', option = 'magma', begin = .2, end = .8) + 
  theme_bw() + 
  facet_wrap(~response_object)

# pretty big differences for males sign tracking more than females, especially
# pronounced on the right lever for some reason

# cohort differences
d |> 
  filter(response_data_type == 'responses-count') |> 
  group_by(cohort, response_object) |> 
  mutate(
    cum_resp = cumsum(response)
  ) |> 
  ggplot(aes(x = trial_id, y = cum_resp)) + 
  geom_line(aes(linetype = as.factor(cohort))) +
  facet_wrap(~response_object)

# definitely a difference here, swirl had far more sign tracking, definitely 
# important to account for this in the model

