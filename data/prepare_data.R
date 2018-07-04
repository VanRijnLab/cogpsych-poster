#!/usr/bin/env Rscript
#
#
# This script does the following:
# 
# - Read in one or more raw data files exported from RuggedLearning;
# - Merge them into a single data frame;
# - Filter out participants who did not consent to their data being analysed;
# - Filter out duplicate records;
# - Anonymise participants;
# - Mark the learning session boundaries;
# - Estimate the model parameters (alpha, estimated activation, estimated RT).
# - Combine learning data and exam data in a single data file

library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(readr)
library(lubridate)
library(stringr)
library(forcats)
library(xml2)
library(jsonlite)
library(parallel)


# Set up multi-core processing
no_cores <- max(1, detectCores() - 1)
cl <- makeCluster(no_cores, type = "FORK")

# Import all .xlsx files in the raw/ directory
path_raw_data <- "data/raw/"
xlsx_list <- list.files(path_raw_data, pattern = ".xlsx", recursive = TRUE, full.names = TRUE)
message(paste(length(xlsx_list), "data files found."))

# Read into a single data frame
dat <- map_df(xlsx_list, read_excel)
message(paste(nrow(dat), "observations in total."))

# Clean up columns
dat <- dat %>%
  mutate(course_id = `Course ID`,
         course = `Course`,
         chapter_id = `Chapter ID`,
         chapter = `Chapter`,
         fact_id = `Fact ID`,
         fact = `Fact`,
         user = `User`,
         sequence_number = `Sequence Number`,
         time = `Time`,
         alternatives = `Number of Alternatives`,
         data = `Data`) %>%
  select(course_id, course, user, chapter_id, chapter, sequence_number, fact_id, fact, time, alternatives, data)


# Clean up data
whitelist <- read_csv("data/whitelist.csv",
                      col_types = list(col_character(), col_character(), col_character()),
                      col_names = c("time", "student_id", "student_email"),
                      skip = 1) %>%
  mutate(student_id = paste0("s", str_extract(student_id, "\\d+"))) %>%
  pull(student_id) %>%
  unique()

dat <- filter(dat, course_id %in% c("_287157_1", "_287240_1"), # Only keep trials from CogPsych courses (NL and EN)
              user %in% whitelist)  %>% # Only keep trials from consenting users
  droplevels() # Remove unused factor levels


# Unpack JSON data stored in data column
json_data <- map_df(dat$data, function(x) {
  d <- fromJSON(x)
  if(is.null(d[[2]])) { # Replace NULL RT with NA so that it can be parsed as integer
    d[[2]] <- NA
  }
  d[-3] # Drop fact ID (it's already a column in the data frame)
})

# Add columns extracted from JSON to the data frame
d <- bind_cols(dat, json_data) %>%
  select(-data) %>%
  rename(start_time = presentationStartTime,
         rt = reactionTime)

rm(dat, json_data)


# Filter out duplicate observations (only keep the first)
d <- d %>%
  group_by(user, start_time) %>%
  slice(1) %>%
  ungroup()

message(paste(nrow(d), "observations from consenting participants in CogPsych courses."))


# Estimate session boundaries
# The beginning and end of a session are not marked in the data, so we must estimate when one session ends and a new one begins.

# We declare the start of a new session whenever the difference in start_time between two consecutive trials is at least 1 minute higher than the RT on the first trial.
session_lag_boundary <- 1 * 60 * 1000

d <- d %>%
  group_by(user) %>%
  arrange(start_time) %>%
  filter(start_time - (lag(start_time) + lag(rt)) >= session_lag_boundary | user != lag(user) | chapter_id != lag(chapter_id)) %>%
  mutate(session = 1:n() + 1) %>%
  right_join(d, by = c("course_id", "course", "user", "chapter_id", "chapter", "sequence_number", "fact_id", "fact", "time", "alternatives", "start_time", "rt", "correct")) %>%
  fill(session) %>%
  ungroup() %>%
  mutate(session = if_else(is.na(session), 1, session))

session_stats <- d %>%
  distinct(user, session)
message(paste("Data contains", nrow(session_stats), "learning sessions from", n_distinct(session_stats$user), "users."))


# Keep track of repetition of each fact within a session
d <- d %>%
  group_by(user, session, fact_id) %>%
  arrange(start_time) %>%
  mutate(repetition = 1:n()) %>%
  ungroup()


# Prepare data for Javascript app that estimates model parameters
d_formatted <- d %>%
  mutate(rt = if_else(is.na(rt), "null", as.character(rt))) %>% # Change NA to "null" so that JS recognises it correctly
  mutate(sequenceNumber = sequence_number,
         modelName = NA,
         factId = fact_id,
         fact = fact,
         presentationStartTime = start_time,
         reactionTime = rt,
         correct = as.integer(correct),
         chapterId = chapter_id,
         numberOfAlternatives = alternatives,
         time = time,
         user = user,
         courseId = course_id,
         data = paste0("{\"presentationStartTime\":", presentationStartTime, ",\"reactionTime\":", reactionTime, ",\"factId\":\"", factId, "\",\"correct\":", correct, "}")) %>%
  select(user, sequenceNumber, modelName, fact, factId, data, chapterId, numberOfAlternatives, time, user, courseId, course, session, repetition) %>%
  collect() %>%
  arrange(user, factId, sequenceNumber)

# Split by user only (since alpha estimates are carried over between sessions)
d_formatted_split <- split(d_formatted, d_formatted$user)

d_by_user <- d_formatted_split %>%
  parLapply(cl, ., function(x) {
    x %>%
      toJSON() %>%
      gsub("\\[|\\]|", "", .) %>% # Remove square brackets
      gsub("\\},", "}\n", .) # Replace commas with newlines
  })


# Send formatted JSON to javascript app (has to be serial since we communicate via a single text file) and get output back
outputdat <- list()
for(i in seq_along(d_by_user)) {
  write(d_by_user[[i]], file = "data/calculateAlpha/input.txt")
  
  system("cd data/calculateAlpha && nodejs app.js")
  
  # Keep a backup of the output file
  system(paste0("cp data/calculateAlpha/output.txt data/calculateAlpha/output_temp/output_", i, ".txt"))
  
  outputdat[[i]] <- read_csv(file = "data/calculateAlpha/output.txt",
                             col_types = cols(
                               chapterId = col_character(),
                               user = col_character(),
                               factId = col_character(),
                               time = col_datetime(),
                               sequenceNumber = col_integer(),
                               modelName = col_character(),
                               presentationStartTime = col_double(),
                               reactionTime = col_integer(),
                               correct = col_integer(),
                               numberOfAlternatives = col_integer(),
                               activation = col_double(),
                               estimatedAlpha = col_double(),
                               estimatedResponseTime = col_double()
                               )
                             )
}

outputdat <- bind_rows(outputdat) %>%
  mutate(chapterId = as.factor(chapterId),
         user = as.factor(user),
         factId = as.factor(factId)) %>%
  select(-modelName)

d_merged <- d_formatted %>%
  mutate(chapterId = as.factor(chapterId),
         user = as.factor(user),
         factId = as.factor(factId)) %>%
  left_join(., outputdat, by = c("user", "sequenceNumber", "factId", "chapterId", "numberOfAlternatives", "time"))



d <- d_merged %>%
  select(course_id = courseId,
         course,
         user,
         session,
         seq_num = sequenceNumber,
         chapter_id = chapterId,
         time,
         fact_id = factId,
         fact,
         num_alternatives = numberOfAlternatives,
         repetition,
         start_time = presentationStartTime,
         rt = reactionTime,
         correct,
         activation,
         estimated_alpha = estimatedAlpha,
         estimated_rt = estimatedResponseTime) %>%
  arrange(course_id, user, session, start_time)


save(d, file = "data/CogPsych_trial_data_full.Rdata")
  