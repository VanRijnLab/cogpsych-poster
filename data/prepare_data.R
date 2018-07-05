#!/usr/bin/env Rscript
#
#
# Usage: Rscript prepare_data.R
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
# - Combine learning data, exam data, and final course grades in a single data file (cogpsych_data_anon.Rdata)
#

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
  mutate(Course.ID = `Course ID`,
         Chapter.ID = `Chapter ID`,
         factId = `Fact ID`,
         Sequence.Number = `Sequence Number`,
         Number.Of.Alternatives = `Number of Alternatives`) %>%
  select(Course.ID, Course, User, Chapter.ID, Chapter, Sequence.Number, factId, Fact, Time, Number.Of.Alternatives, Data)


# Clean up data

# Only include students who gave consent
whitelist <- read_csv("data/whitelist.csv",
                      col_types = list(col_character(), col_character(), col_character()),
                      col_names = c("Time", "User", "Email"),
                      skip = 1) %>%
  mutate(User = paste0("s", str_extract(User, "\\d+"))) %>%
  select(User) %>%
  unique() %>%
  mutate(Anon.ID = fct_anon(as.factor(User), prefix = "anon-"))

write_csv(whitelist, "data/student_numbers_to_anon_id_conversion.csv")


dat <- filter(dat, Course.ID %in% c("_287157_1", "_287240_1")) %>% # Only keep trials from CogPsych courses (NL and EN)
  inner_join(whitelist, by = "User") %>% # Only keep trials from consenting users
  select(-User) %>% # Keep only anonymised ID in the data
  rename(User = Anon.ID) %>%
  droplevels() # Remove unused factor levels


# Unpack JSON data stored in data column
json_data <- map_df(dat$Data, function(x) {
  d <- fromJSON(x)
  if(is.null(d[[2]])) { # Replace NULL RT with NA so that it can be parsed as integer
    d[[2]] <- NA
  }
  d[-3] # Drop fact ID (it's already a column in the data frame)
})

# Add columns extracted from JSON to the data frame
d <- bind_cols(dat, json_data) %>%
  select(-Data)

rm(dat, json_data)


# Filter out duplicate observations (only keep the first)
d <- d %>%
  group_by(User, presentationStartTime) %>%
  slice(1) %>%
  ungroup()

message(paste(nrow(d), "observations from consenting participants in CogPsych courses."))


# Estimate session boundaries
# The beginning and end of a session are not marked in the data, so we must estimate when one session ends and a new one begins.

# We declare the start of a new session whenever the difference in presentationStartTime between two consecutive trials is at least 1 minute higher than the RT on the first trial.
# In cases where RT is missing (because the user erased their whole answer) we use the user's median RT in its place.
# In cases where the lag between the response and the start of the next trial is small, but the previous RT is very large
# (e.g. when only responding to a prompt after a long period of inactivity and then continuing) we also draw a session boundary
session_lag_boundary <- 1 * 60 * 1000
session_rt_boundary <- 5 * 60 * 1000

d <- d %>%
  group_by(User) %>%
  arrange(presentationStartTime) %>%
  filter(presentationStartTime - (lag(presentationStartTime) + if_else(is.na(lag(reactionTime)), as.integer(median(reactionTime, na.rm = TRUE)), lag(reactionTime))) >= session_lag_boundary |
           if_else(is.na(lag(reactionTime)),  as.integer(median(reactionTime, na.rm = TRUE)), lag(reactionTime)) >= session_rt_boundary |
           User != lag(User) |
           Chapter.ID != lag(Chapter.ID)) %>%
  mutate(session = 1:n() + 1) %>%
  right_join(d, by = c("Course.ID", "Course", "User", "Chapter.ID", "Chapter", "Sequence.Number", "factId", "Fact", "Time", "Number.Of.Alternatives", "presentationStartTime", "reactionTime", "correct")) %>%
  fill(session) %>%
  ungroup() %>%
  mutate(session = if_else(is.na(session), 1, session))

session_stats <- d %>%
  distinct(User, session)
message(paste("Data contains", nrow(session_stats), "learning sessions from", n_distinct(session_stats$User), "users."))


# Keep track of repetition of each fact within a session
d <- d %>%
  group_by(User, session, factId) %>%
  arrange(presentationStartTime) %>%
  mutate(repetition = 1:n()) %>%
  ungroup()


# Prepare data for Javascript app that estimates model parameters
d_formatted <- d %>%
  mutate(reactionTime = if_else(is.na(reactionTime), "null", as.character(reactionTime))) %>% # Change NA to "null" so that JS recognises it correctly
  mutate(user = User,
         sequenceNumber = Sequence.Number,
         modelName = NA,
         fact = Fact,
         correct = as.integer(correct),
         chapterId = Chapter.ID,
         numberOfAlternatives = Number.Of.Alternatives,
         time = Time,
         courseId = Course.ID,
         course = Course,
         data = paste0("{\"presentationStartTime\":", presentationStartTime, ",\"reactionTime\":", reactionTime, ",\"factId\":\"", factId, "\",\"correct\":", correct, "}")) %>%
  select(user, sequenceNumber, modelName, fact, factId, data, chapterId, numberOfAlternatives, time, courseId, course, session, repetition) %>%
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
  # rename(User = user) %>%
  select(-modelName)

d_merged <- d_formatted %>%
  mutate(chapterId = as.factor(chapterId),
         user = as.factor(user),
         factId = as.factor(factId)) %>%
  left_join(., outputdat, by = c("user", "sequenceNumber", "factId", "chapterId", "numberOfAlternatives", "time"))



d <- d_merged %>%
  select(User = user,
         Course.ID = courseId,
         Course = course,
         session,
         Sequence.Number = sequenceNumber,
         Chapter.ID = chapterId,
         Time = time,
         factId,
         Fact = fact,
         Number.Of.Alternatives = numberOfAlternatives,
         repetition,
         presentationStartTime,
         reactionTime,
         correct,
         activation,
         estimatedAlpha,
         estimatedResponseTime) %>%
  arrange(Course.ID, User, session, presentationStartTime)


# Exam data

# Some exam questions were part of the practice questions, so keep just those

dict <- aggregate(Fact ~ factId + Chapter.ID, d, unique)

key_phrases <- read.table(header = TRUE,
text = "
q_num key_phrase
Q1  'looks within'
Q3   'geons'
Q5   'exceed some limit'
Q8   'cycled'
Q11  'misremembers'
Q14  'event are influenced'
Q16  'studying concepts'
Q19  'constituent expressing'
Q22  'by evaluating'
Q25  'ability to deal'
")

key <- NULL

for(i in 1:nrow(key_phrases)) {
  row <- key_phrases[i,]
  key <- rbind(key,
               data.frame(exam = row$q_num,
                          factId = subset(dict, grepl(row$key_phrase, Fact))$factId))
}

exam <- read_delim("data/exam first attempt grade center.csv", delim = ";") %>%
  inner_join(whitelist, by = c("Username" = "User")) %>% # Keep only grades from students who gave consent
  select(-Username, -`Last Name`, -`First Name`, -`Student ID`, -`Last Access`, -`Availability`) %>%
  rename(Username = Anon.ID)

students <- unique(exam$Username)


exam.item <- NULL

# Go through all students that took the exam and extract information question-by-question:
for(student in students) {
  for(Q in as.character(unique(key$exam))) {
    # Did this student answer this question correctly on the exam?
    # We'll only count it as correct if they got 2 out of 2 points for that item on the exam!
    correct <- exam[which(exam$Username == student), which(grepl(paste0(Q, "\\b"), colnames(exam)))] == 2
    
    # Get all responses for this student that relate to this exam question:
    IDs <- key$factId[key$exam == Q]
    
    # NOTE: Only do this for the people that gave informed consent!
    filter(d, User == student)
    this.Q <- subset(d, User == student & factId %in% IDs)
    # this.Q <- subset(all.data, User == student & factId %in% IDs)
    
    if(nrow(this.Q) > 0) {
      # There is data for this item-student pair
      out <- c(student, Q, correct, nrow(this.Q), mean(this.Q$correct), 
               # if item was repeated at least three times, include the final alpha value 
               # (otherwise set alpha to NA)
               ifelse(nrow(this.Q) >= 3, tail(this.Q$estimatedAlpha, 1), NA)) 
    } else {
      # This student did not study this item
      out <- c(student, Q, correct, NA, NA, NA)
    }
    
    exam.item <- rbind(exam.item, out)
  }
}

exam.item <- as.data.frame(exam.item, stringsAsFactors = FALSE)
colnames(exam.item) <- c("Username", "exam.Q", "correct.exam", "reps.study", "PC.study", "final.alpha")
row.names(exam.item) <- NULL

# Enforce variable types:
exam.item$correct.exam <- as.logical(exam.item$correct.exam)
exam.item$reps.study <- as.numeric(exam.item$reps.study)
exam.item$PC.study <- as.numeric(exam.item$PC.study)
exam.item$final.alpha <- as.numeric(exam.item$final.alpha)

exam.item$studied <- factor( ifelse(is.na(exam.item$reps.study), "Not Studied", "Studied") )

  

# Final grades

grades <- read.table("data/CogPsy-grades-1718.txt", header = TRUE) %>%
  inner_join(whitelist, by = "User") %>% # Only keep trials from consenting users
  mutate(User = as.character(Anon.ID)) %>%
  select(User, Grade) # Keep only anonymised ID in the data


# Save to disk
data <- d
save(data, exam.item, grades, file = "data/cogpsych_data_anon.Rdata")

