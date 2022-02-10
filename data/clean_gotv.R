library(haven)
library(dplyr)
library(digest)

# Have to copy the preprocessing done in https://journals.sagepub.com/doi/full/10.1177/1532673X1668655
# Unfortunately their data is a .dta and they preprocess in stata, so we need to
# turn this into an RDS
gotv <- read_dta("data/PublicReplicationData.dta")

nrow(gotv)
gotv %>%
  mutate(salience = case_when( # Add salience scores
    state == "AR" ~  3 ,
    state == "AK" ~  4 ,
    state == "AZ" ~  1 ,
    state == "CO" ~  4 ,
    state == "FL" ~  2 ,
    state == "GA" ~  4 ,
    state == "IA" ~  3 ,
    state == "KS" ~  4 ,
    state == "KY" ~  1 ,
    state == "LA" ~  2 ,
    state == "ME" ~  3 ,
    state == "MI" ~  3 ,
    state == "NC" ~  2 ,
    state == "NH" ~  3 ,
    state == "SD" ~  2 ,
    state == "TX" ~  2 ,
    state == "WI" ~  2
  )) %>% # Add high salience indicator
  mutate(high_salience = ifelse(salience > 2, 1, 0)) -> gotv


# In order to match the analysis in https://albertfang.com/research/self_paper.pdf
# We do:
# 1) Remove validated vote history (2,887)
# 2) exclude from the analysis sample any subject who belonged to a household
#    with more than one person (n=416,436 from the treatment group, n=25,679
#    from the control group)
# 3) We also found that the randomization procedure was mishandled
#    among subjects 23 years old and younger in North Carolina,
duplicated_id <- gotv$hhid[which(duplicated(gotv$hhid))]
gotv %>%
  filter(!(hhid %in% duplicated_id)) %>%
  filter(!(state == "NC" & i_age <= 23)) %>%
  filter(!(state == "NC" & age_miss == 1)) %>%
  filter(!(flag_hhid_mult_hhid == 1 | flag_hhid_mult_z == 1 | flag_drop_hhid == 1)) %>%
  dplyr::select(-flag_hhid_mult_hhid,-flag_hhid_mult_z,
                -flag_drop_hhid,-id,-hhid,-hi_salience) -> train_data


# Check for duplicated columns
any(duplicated(lapply(train_data, digest)))

print(nrow(train_data))
print(ncol(train_data))

saveRDS(object = train_data, file = "data/gotv.RDS")
