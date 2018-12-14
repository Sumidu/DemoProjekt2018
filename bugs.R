

raw <- raw %>%  rename(date_created = `date_created X3`)
raw <- raw %>%  rename(date_modified = `date_modified X4`)



raw <- add_survey_response_duration(raw)