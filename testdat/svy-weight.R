# weight survey using population data

library(tidyverse)
library(sastats)

outfile_svy <- "data/interim/svy-weight.rds" # updated svy data frame
outfile_wt <- "data/interim/svy-weight-object.rds" # list returned by sastats::rake_weight()
    
svy <- readRDS("data/interim/svy-demo.rds")
pop_data <- readRDS("data/interim/pop_data.rds")
flags <- readRDS("data/interim/svy-flag.rds")

# Add Flags ---------------------------------------------------------------

# Respondents passing the flag threshold will be excluded from analysis (and weighting)
svy$person <- svy$person %>%
    left_join(flags$flag_totals, by = "Vrid") %>%
    mutate(flag = ifelse(is.na(flag), 0, flag))
count(svy$person, flag)

svy_wt <- filter(svy$person, flag < 4)
count(svy_wt, flag)

# Get Pop Targets ---------------------------------------------------------

# using pop_data survey data for target population
pop_data <- pop_data %>%
    filter(in_co_pop) %>% # to match CO target pop
    select(Vrid, sex, age_weight, income_weight, race_weight, stwt)

# get population distribution targets
wt_vars <- setdiff(names(pop_data), c("Vrid", "stwt"))
pop <- sapply(wt_vars, function(x) weights::wpct(pop_data[[x]], pop_data$stwt))
pop

# Weight ------------------------------------------------------------------

# check: distributions of weighting variables
# - more female, older, less hispanic
sapply(names(pop), function(x) weights::wpct(svy_wt[[x]]))

# run weighting
rake_output <- rake_weight(svy_wt, pop, "Vrid")
svy_wt <- select(rake_output$svy, Vrid, weight)
svy$person <- left_join(svy$person, svy_wt, by = "Vrid")

# check - should show TRUE
x <- sapply(names(pop), function(x) weights::wpct(svy$person[[x]], svy$person$weight))
all.equal(x, pop)

# Save --------------------------------------------------------------------

saveRDS(svy, outfile_svy)
saveRDS(rake_output, outfile_wt)

# summarize
glimpse(svy$person)
summary(rake_output$wts)
