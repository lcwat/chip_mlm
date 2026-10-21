# cleaning and reshaping trial data for MLM

# Lucas Watson

# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)

# load data ---------------------------------------------------------------

raw_exp2 <- read_xlsx('data/exp_2_raw_data.xlsx')

# clean -------------------------------------------------------------------

# grab sex from colnames
sex <- str_extract(colnames(raw_exp2), '(?=...)[A-Z]')

raw_exp2 <- raw_exp2 |> 
  rbind(sex)

# set up clean colnames
colnames(raw_exp2) <- str_c("v", seq(1, ncol(raw_exp2), 1))

# divvy up rat details from trial data
rat_details <- raw_exp2[c(1:5, nrow(raw_exp2)),]

# clean up
rat_details <- rat_details |> 
  select(where(~ !all(is.na(.)))) # remove all NA cols

# rename cols
rat_details$v3 <- c('drug', 'drop_rat', 'cohort', 'rat', 'box', 'sex')

rat_details <- rat_details |> 
  filter(v3 != 'drop_rat')

# transpose
t_rat_details <- as_tibble(t(rat_details[-1]), .name_repair = 'unique')
colnames(t_rat_details) <- rat_details$v3

# edit drug dose to make it purely numerical
rat_key <- t_rat_details |> 
  mutate(
    drug = if_else(drug == 'S', 0, as.numeric(drug))
  )

# assign unique id for each row of the key for matching
rat_key["row_id"] = 1:nrow(rat_key)

# slice out trial data
trial_data <- raw_exp2[-c(1:5, nrow(raw_exp2)),]

# remove blank rows
trial_data <- trial_data |> 
  filter(!if_all(everything(), is.na))

# extract number from day
trial_data <- trial_data |> 
  mutate(
    v1 = str_extract(v1, '\\d')
  )

# create trial id
trial_data_w_ids <- trial_data |> 
  group_by(v1, v2, v3) |> 
  mutate(
    trial_id = row_number()
  ) |> 
  relocate(trial_id) |> 
  ungroup()


# rename useful cols
trial_data_w_ids <- trial_data_w_ids |> 
  rename(
    day = v1, response_data_type = v2, response_object = v3
  )

# recode numbers
trial_data_w_ids <- trial_data_w_ids |> 
  mutate(
    across(c(trial_id, day, starts_with('v')), as.numeric)
  )

# create row_id row
new_names <- c(colnames(trial_data_w_ids)[1:4], str_c('v', seq(1, ncol(trial_data_w_ids)-4)))

colnames(trial_data_w_ids) <- new_names

# pivot
long_tdwid <- trial_data_w_ids |> 
  pivot_longer(
    starts_with('v'), names_to = 'row_id', values_to = 'response'
  ) |> 
  mutate(
    row_id = as.numeric(str_extract(row_id, '\\d+'))
  )

# merge with rat key to replicate rat details for each day, trial, type combo
final_data <- long_tdwid |> 
  left_join(rat_key, join_by(row_id))

# reorder cols for clarity
final_data <- final_data |> 
  relocate(
    rat, cohort, sex, drug, day, trial_id, response_data_type, response_object, response, box, row_id
  )

# write clean df to disk
write_csv(final_data, 'data/exp_2_data_long.csv')
