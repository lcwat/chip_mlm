# cleaning and reshaping trial data for MLM

# Lucas Watson

# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)

# load data ---------------------------------------------------------------

raw_exp2 <- read_xlsx('data/exp_2_raw_data.xlsx')
raw_exp1 <- read_xlsx('data/exp_1_raw_data.xlsx')

# clean -------------------------------------------------------------------

# grab info from colnames, exp2 is sex, exp1 is dose
sex <- str_extract(colnames(raw_exp2), '(?=...)[A-Z]')

raw_exp2 <- raw_exp2 |> 
  rbind(sex)

dose <- str_extract(colnames(raw_exp1), '(?=...)\\d+')

raw_exp1 <- raw_exp1 |> 
  rbind(dose)

# set up clean colnames
colnames(raw_exp2) <- str_c("v", seq(1, ncol(raw_exp2), 1))

colnames(raw_exp1) <- str_c("v", seq(1, ncol(raw_exp1), 1))

# divvy up rat details from trial data
rat_details2 <- raw_exp2[c(1:5, nrow(raw_exp2)),]

rat_details1 <- raw_exp1[c(1:3, nrow(raw_exp1)),]

# clean up
rat_details2 <- rat_details2 |> 
  select(where(~ !all(is.na(.)))) # remove all NA cols

rat_details1 <- rat_details1 |> 
  select(where(~ !all(is.na(.)))) # remove all NA cols

# rename cols
rat_details2$v3 <- c('drug', 'drop_rat', 'cohort', 'rat', 'box', 'sex')

rat_details2 <- rat_details2 |> 
  filter(v3 != 'drop_rat')

rat_details1$v3 <- c('sex', 'cohort', 'rat', 'drug')

# transpose
t_rat_details2 <- as_tibble(t(rat_details2[-1]), .name_repair = 'unique')
colnames(t_rat_details2) <- rat_details2$v3

t_rat_details1 <- as_tibble(t(rat_details1[-1]), .name_repair = 'unique')
colnames(t_rat_details1) <- rat_details1$v3

# edit drug dose to make it purely numerical
rat_key2 <- t_rat_details2 |> 
  mutate(
    drug = if_else(drug == 'S', 0, as.numeric(drug))
  )

rat_key1 <- t_rat_details1 |> 
  mutate(
    drug = as.numeric(drug)
  )

# assign unique id for each row of the key for matching
rat_key2["row_id"] = 1:nrow(rat_key2)
rat_key1["row_id"] = 1:nrow(rat_key1)

# slice out trial data
trial_data2 <- raw_exp2[-c(1:5, nrow(raw_exp2)),]
trial_data1 <- raw_exp1[-c(1:3, nrow(raw_exp1)),]

# remove blank rows
trial_data2 <- trial_data2 |> 
  filter(!if_all(everything(), is.na))

trial_data1 <- trial_data1 |> 
  filter(!if_all(everything(), is.na))

# extract number from day
trial_data2 <- trial_data2 |> 
  mutate(
    v1 = str_extract(v1, '\\d')
  )

trial_data1 <- trial_data1 |> 
  mutate(
    v1 = str_extract(v1, '\\d')
  )

# create trial id
trial_data_w_ids2 <- trial_data2 |> 
  group_by(v1, v2, v3) |> 
  mutate(
    trial = row_number()
  ) |> 
  relocate(trial) |> 
  ungroup()

trial_data_w_ids1 <- trial_data1 |> 
  group_by(v1, v2, v3) |> 
  mutate(
    trial = row_number()
  ) |> 
  relocate(trial) |> 
  ungroup()

# rename useful cols
trial_data_w_ids2 <- trial_data_w_ids2 |> 
  rename(
    day = v1, response_data_type = v2, response_object = v3
  )

trial_data_w_ids1 <- trial_data_w_ids1 |> 
  rename(
    day = v1, response_data_type = v2, response_object = v3
  )

# recode numbers
trial_data_w_ids2 <- trial_data_w_ids2 |> 
  mutate(
    across(c(trial, day, starts_with('v')), as.numeric)
  )

trial_data_w_ids1 <- trial_data_w_ids1 |> 
  mutate(
    across(c(trial, day, starts_with('v')), as.numeric)
  )

# create row_id row
new_names2 <- c(colnames(trial_data_w_ids2)[1:4], str_c('v', seq(1, ncol(trial_data_w_ids2)-4)))
new_names1 <- c(colnames(trial_data_w_ids1)[1:4], str_c('v', seq(1, ncol(trial_data_w_ids1)-4)))

colnames(trial_data_w_ids2) <- new_names2
colnames(trial_data_w_ids1) <- new_names1

# pivot
long_tdwid2 <- trial_data_w_ids2 |> 
  pivot_longer(
    starts_with('v'), names_to = 'row_id', values_to = 'response'
  ) |> 
  mutate(
    row_id = as.numeric(str_extract(row_id, '\\d+'))
  )

long_tdwid1 <- trial_data_w_ids1 |> 
  pivot_longer(
    starts_with('v'), names_to = 'row_id', values_to = 'response'
  ) |> 
  mutate(
    row_id = as.numeric(str_extract(row_id, '\\d+'))
  )

# merge with rat key to replicate rat details for each day, trial, type combo
final_data2 <- long_tdwid2 |> 
  left_join(rat_key2, join_by(row_id))

final_data1 <- long_tdwid1 |> 
  left_join(rat_key1, join_by(row_id))

# reorder cols for clarity
final_data2 <- final_data2 |> 
  relocate(
    rat, cohort, sex, drug, day, trial, response_data_type, response_object, response, box, row_id
  )

final_data1 <- final_data1 |> 
  relocate(
    rat, cohort, sex, drug, day, trial, response_data_type, response_object, response, row_id
  )

# assign unique rat and trial ids for modeling
final_data2 <- final_data2 |> 
  group_by(rat, cohort) |>
  mutate(
    rat_id = cur_group_id()
  ) |> 
  ungroup()

final_data1 <- final_data1 |> 
  group_by(rat, cohort) |>
  mutate(
    rat_id = cur_group_id()
  ) |> 
  ungroup()

# trial ids, unique, 1 through final trial on final day
final_data2 <- final_data2 |> 
  mutate(
    trial_id = (day - 1) * 12 + trial
  )

final_data1 <- final_data1 |> 
  mutate(
    trial_id = (day - 1) * 12 + trial
  )

# apply proper variable coding
final_data2 <- final_data2 |> 
  mutate(
    across(c(rat:sex, response_data_type, response_object, rat_id), as.factor)
  )

final_data1 <- final_data1 |> 
  mutate(
    across(c(rat:sex, response_data_type, response_object, rat_id), as.factor)
  )

glimpse(final_data1)

# write clean df to disk
write_csv(final_data2, 'data/exp_2_data_long.csv')
write_csv(final_data1, 'data/exp_1_data_long.csv')
