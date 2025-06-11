source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))

###############################
#
#   Load raw files
#
###############################

load(file = "./02 RData/raw.RData")

# Define Likert scale labels
likert.4.labels <- c(
  "Very insecure" = 1,
  "Insecure" = 2,
  "Secure" = 3,
  "Very secure" = 4
)

# Function to compute AUCg (Area Under the Curve - Ground)
compute.AUCg <- function(time, measure) {
  # Remove time/measure pairs where measure is NA
  valid <- !is.na(measure) & !is.na(time)
  time <- time[valid]
  measure <- measure[valid]
  
  if (length(time) < 2) return(NA)# Need at least 2 points to compute AUC
  
  AUCg <- 0
  for (i in 1:(length(time) - 1)) {     
    delta.time <- as.numeric(difftime(time[i + 1], time[i], units = "hours"))  # Convert difftime to numeric (in hours)
    avg.measure <- (measure[i + 1] + measure[i]) / 2  # Trapezoidal rule for averaging the measures
    AUCg <- AUCg + avg.measure * delta.time
  }
  return(AUCg)
}

# Function to compute AUCi (Area Under the Curve - Increase)
compute.AUCi <- function(time, measure) {
  # Remove NA pairs, but ensure first value is available
  if (is.na(measure[1]) | is.na(time[1])) return(NA)
  
  valid <- !is.na(measure) & !is.na(time)
  time <- time[valid]
  measure <- measure[valid]
  
  if (length(time) < 2) return(NA) # Need at least 2 points to compute AUC
  
  AUCg <- compute.AUCg(time, measure)  # Get the AUCg
  total.time <- as.numeric(difftime(time[length(time)], time[1], units = "hours"))  # Total time interval in hours
  AUCi <- AUCg - measure[1] * total.time  # Subtract the baseline area
  return(AUCi)
}


###############################
#
#   clean and prepare study 1 data frame
#
###############################

#Survey data frame
study1.final.df <- study1.raw.df %>% 
  mutate(condition = case_when(
    treatment == "AR" ~ "Digital",
    treatment == "paper" ~ "Traditional",
    TRUE ~ "unknown"
  )) %>% 
  mutate(resource = case_when(
    instruction == "with" ~ "gain",
    instruction == "without" ~ "loss",
    TRUE ~ "unknown"
  )) %>%
  select(-instruction, -treatment) 

###############################
#
#   clean and prepare study 2 data frame
#
###############################

#Survey data frame
study2.df <- study2.raw.df %>% 
  janitor::clean_names() %>% 
  rename_with(~ gsub("_", ".", .x)) %>%
  mutate(participant.id = str_pad(str_sub(as.character(participant.id), -3), width = 3, pad = "0")) %>% 
  mutate(
    height = height / 100,                     # Convert height from cm to m
    bmi = weight / (height^2)                   # Calculate BMI
  ) %>%
  select(-height, -weight) %>% 
  mutate(bmi.category = case_when(
  bmi < 18.5 ~ "underweight",
  bmi >= 18.5 & bmi < 25 ~ "normal.weight",
  bmi >= 25 & bmi < 30 ~ "overweight",
  bmi >= 30 & bmi < 35 ~ "obesity.class.i",
  bmi >= 35 & bmi < 40 ~ "obesity.class.ii",
  bmi >= 40 ~ "obesity.class.iii",
  TRUE ~ "unknown"
  )) %>% 
  mutate(age.range = case_when(
    age < 21.5 ~ "19-21",
    age >= 21.5 & age < 24.5 ~ "22-24",
    age >= 24.5 & age < 27.5 ~ "25-27",
    age >= 27.5 & age < 30.5 ~ "28-30",
    age >= 30.5 & age < 33.5 ~ "31-33",
    age >= 33.5 ~ "34-36",
    TRUE ~ "unknown"
  )) %>% 
  mutate(learning.disorders = case_when(
    learning.disorders != "None" ~ 1,
    learning.disorders == "None" ~ 0
  )) %>% 
  mutate(across(
    c(c1, c2, c3, c4, c5, c6, c7, c8, c9), as.numeric)) %>% # Ensure C1 to C9 are numeric
  mutate(across(
    c(s1, s2, s3, s4, s5, s6, s7, s8, s9), as.numeric)) %>% 
  mutate(across(
    c(t1, t2, t3, t4, t5, t6, t7, t8, t9), as_hms)) %>% 
  mutate(across(
    c(
      gender,
      learning.disorders,   # after cleaning names, LearningDisorders → learningdisorders
      en.native,
      experience.nreal,
      experience.gpt,
      condition
    ),
    ~ as.factor(.x)
  ))

Hmisc::label(study2.df$gender) <- "0 if Male, 1 if Female, 2 if Diverse"
Hmisc::label(study2.df$en.native) <- "1 if native speaker, 0 if not"
Hmisc::label(study2.df$learning.disorders) <- "0 if none, 1 if present"

## Not run:
# Hmisc::describe(study2.df) # Displays a summary of study2.df including labels
## End(Not run)

#Experiment data frame
study2.long.df <- study2.df %>%
  pivot_longer(
    cols = c(
      c1, c2, c3, c4, c5, c6, c7, c8, c9,
      s1, s2, s3, s4, s5, s6, s7, s8, s9,
      t1, t2, t3, t4, t5, t6, t7, t8, t9
    ),
    names_to = c(".value", "timepoint"),     # .value magic!
    names_pattern = "([cst])(\\d+)",          # RegEx: capture C/S/T and the number
    names_transform = list(timepoint = as.integer)
  ) %>%
  select(participant.id, timepoint, cortisol = c, perceived = s, time = t) %>% 
  mutate(
    cortisol.winsorized = { # Winsorize and log-transform Cortisol values
      quantiles <- quantile(cortisol, probs = c(0.05, 0.95), na.rm = TRUE)
      pmin(pmax(cortisol, quantiles[1]), quantiles[2])
    },
    cortisol.log = log(cortisol.winsorized) #Log-transform Cortisol values
  ) %>%
  mutate(
    resource = case_when(
      timepoint %in% 1:3 ~ "lack",
      timepoint %in% 4:6 ~ "gain",
      timepoint %in% 7:9 ~ "loss"
    )
  )
    
# Join the static variables with the long format data
#Runit only once
study2.long.df <- left_join(study2.long.df,
    study2.df %>%
      select(-c(c1, c2, c3, c4, c5, c6, c7, c8, c9,
                s1, s2, s3, s4, s5, s6, s7, s8, s9,
                t1, t2, t3, t4, t5, t6, t7, t8, t9)),
    by = c("participant.id")
  )

# Compute AUCg and AUCi for each participant for Cortisol_log and Perceived using actual time values
# For Cortisol_log
AUCg.results.cortisol <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCg.cortisol = compute.AUCg(time, cortisol.log), .groups = "drop")

AUCi.results.cortisol <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCi.cortisol = compute.AUCi(time, cortisol.log), .groups = "drop")

# For Perceived stress
AUCg.results.perceived <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCg.perceived = compute.AUCg(time, perceived), .groups = "drop")

AUCi.results.perceived <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCi.perceived = compute.AUCi(time, perceived), .groups = "drop")


# Transform data to wide format
study2.wide.df <- study2.long.df %>%
  select(participant.id, timepoint, perceived, cortisol.log, condition, resource, gender, age, bmi, learning.disorders, en.native, experience.gpt) %>%
  pivot_wider(
    names_from = timepoint, 
    values_from = c(cortisol.log, perceived),
    names_sep = "_T"
  )

# Merge AUCi results into wide-format data
study2.final.df <- AUCi.results.cortisol %>%
  left_join(AUCi.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.cortisol, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(study2.wide.df, by = c("participant.id", "resource")) %>%
 # group_by(participant.id) %>%
#  filter(!any(is.na(AUCi.cortisol) | is.na(AUCi.perceived))) %>%            #Remove participants for whom there is NA
#  ungroup() %>%
  select(-matches("^\\.groups")) %>% 
  mutate(resource = factor(resource, levels = c("lack", "gain", "loss"))) %>%  # Set correct resource order
  arrange(participant.id, resource)

remove(AUCg.results.cortisol, AUCg.results.perceived, AUCi.results.cortisol, AUCi.results.perceived, study2.long.df, study2.wide.df) # clean workspace

###############################
#
#   clean and prepare study 3 data frame
#
###############################

#Survey data frame
study3.df <- study3.raw.df %>% 
  janitor::clean_names() %>% 
  rename_with(~ gsub("_", ".", .x)) %>%
  mutate(participant.id = str_pad(str_sub(as.character(participant.id), -3), width = 3, pad = "0")) %>% 
  mutate(
    height = height / 100,                     # Convert height from cm to m
    bmi = weight / (height^2)                   # Calculate BMI
  ) %>%
  select(-height,-weight,-n) %>%
  mutate(bmi.category = case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 & bmi < 25 ~ "normal.weight",
    bmi >= 25 & bmi < 30 ~ "overweight",
    bmi >= 30 & bmi < 35 ~ "obesity.class.i",
    bmi >= 35 & bmi < 40 ~ "obesity.class.ii",
    bmi >= 40 ~ "obesity.class.iii",
    TRUE ~ "unknown"
  )) %>% 
  mutate(age.range = case_when(
    age < 21.5 ~ "19-21",
    age >= 21.5 & age < 24.5 ~ "22-24",
    age >= 24.5 & age < 27.5 ~ "25-27",
    age >= 27.5 & age < 30.5 ~ "28-30",
    age >= 30.5 & age < 33.5 ~ "31-33",
    age >= 33.5 ~ "34-36",
    TRUE ~ "unknown"
  )) %>% 
  mutate(w.corr.lenses = ifelse(w.corr.lenses == "No, I don't wear corrective lenses", "No", "Yes")) %>%
  mutate(across(
    c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13), as.numeric)) %>% # Ensure C1 to C9 are numeric
  mutate(across(
    c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13), as.numeric)) %>% 
  mutate(across(
    c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13), as_hms)) %>% 
  rename(experience.hololens = expertise) %>% 
  mutate(across(
    c(
      gender,
      en.native,
      experience.hololens,
      condition
    ),
    ~ as.factor(.x)
  ))

Hmisc::label(study3.df$en.native) <- "1 if native speaker, 0 if not"

## Not run:
# Hmisc::describe(study2.df) # Displays a summary of study2.df including labels
## End(Not run)

#Experiment data frame

study3.long.df <- study3.df %>%
  pivot_longer(
    cols = c(
      c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
      s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
      t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13
    ),
    names_to = c(".value", "timepoint"),     # .value magic!
    names_pattern = "([cst])(\\d+)",          # RegEx: capture C/S/T and the number
    names_transform = list(timepoint = as.integer)
  ) %>%
  select(participant.id, timepoint, cortisol = c, perceived = s, time = t) %>% 
  mutate(
    cortisol.winsorized = { # Winsorize and log-transform Cortisol values
      quantiles <- quantile(cortisol, probs = c(0.05, 0.95), na.rm = TRUE)
      pmin(pmax(cortisol, quantiles[1]), quantiles[2])
    },
    cortisol.log = log(cortisol.winsorized) #Log-transform Cortisol values
  ) %>%
  mutate(
    resource = case_when(
      timepoint == 1 ~ "baseline",
      timepoint >= 2 & timepoint <= 7 ~ "gain",
      timepoint >= 8 & timepoint <= 13 ~ "loss"
    )
  )

# Join the static variables with the long format data
#Runit only once
study3.long.df <- left_join(study3.long.df,
                            study3.df %>%
                              select(-c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
                                        s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
                                        t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)),
                            by = c("participant.id")
)

# Compute AUCg and AUCi for each participant for Cortisol_log and Perceived using actual time values

# For Cortisol_log
AUCg.results.cortisol <- study3.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCg.cortisol = compute.AUCg(time, cortisol.log), .groups = "drop")

AUCi.results.cortisol <- study3.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCi.cortisol = compute.AUCi(time, cortisol.log), .groups = "drop")

# For Perceived stress
AUCg.results.perceived <- study3.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCg.perceived = compute.AUCg(time, perceived), .groups = "drop")

AUCi.results.perceived <- study3.long.df %>%
  filter(resource != "baseline") %>%
  group_by(participant.id, resource) %>%
  reframe(AUCi.perceived = compute.AUCi(time, perceived), .groups = "drop")


# Transform data to wide format
study3.wide.df <- study3.long.df %>%
  select(participant.id, timepoint, perceived, cortisol.log, condition, resource, gender, age, bmi, en.native, w.corr.lenses, experience.hololens) %>%
  pivot_wider(
    names_from = timepoint, 
    values_from = c(cortisol.log, perceived),
    names_sep = "_T"
  )

# Merge AUCi results into wide-format data
study3.final.df <- AUCi.results.cortisol %>%
  left_join(AUCi.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.cortisol, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(study3.wide.df, by = c("participant.id", "resource")) %>% 
  mutate(
    AUCi.cortisol = if_else(resource == "baseline", cortisol.log_T1, AUCi.cortisol),
    AUCg.cortisol = if_else(resource == "baseline", cortisol.log_T1, AUCg.cortisol),
    AUCi.perceived = if_else(resource == "baseline", perceived_T1, AUCi.perceived),
    AUCg.perceived = if_else(resource == "baseline", perceived_T1, AUCg.perceived)
  ) %>%
 # group_by(participant.id) %>%
#   filter(!any(is.na(AUCi.cortisol) | is.na(AUCi.perceived))) %>%            #Remove participants for whom there is NA
#  ungroup() %>%
  select(-matches("^\\.groups")) %>% 
  mutate(resource = factor(resource, levels = c("baseline", "gain", "loss"))) %>%  # Set correct resource order
  arrange(participant.id, resource)

remove(AUCg.results.cortisol, AUCg.results.perceived, AUCi.results.cortisol, AUCi.results.perceived) # clean workspace


###############################
#
#   clean and prepare study 4 data frame
#
###############################

#Survey data frame
study4.df <- study4.raw.df %>% 
  janitor::clean_names() %>% 
  rename_with(~ gsub("_", ".", .x))%>%
  mutate(participant.id = str_pad(str_sub(as.character(participant.id), -3), width = 3, pad = "0")) %>%
  mutate(
    height = height / 100,                     # Convert height from cm to m
    bmi = weight / (height^2)                   # Calculate BMI
  ) %>%
  select(-height,-weight,-n) %>%
  mutate(bmi.category = case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 & bmi < 25 ~ "normal.weight",
    bmi >= 25 & bmi < 30 ~ "overweight",
    bmi >= 30 & bmi < 35 ~ "obesity.class.i",
    bmi >= 35 & bmi < 40 ~ "obesity.class.ii",
    bmi >= 40 ~ "obesity.class.iii",
    TRUE ~ "unknown"
  )) %>% 
  mutate(age.range = case_when(
    age < 21.5 ~ "19-21",
    age >= 21.5 & age < 24.5 ~ "22-24",
    age >= 24.5 & age < 27.5 ~ "25-27",
    age >= 27.5 & age < 30.5 ~ "28-30",
    age >= 30.5 & age < 33.5 ~ "31-33",
    age >= 33.5 ~ "34-36",
    TRUE ~ "unknown"
  )) %>% 
  mutate(w.corr.lenses = ifelse(w.corr.lenses == "No, I don't wear corrective lenses", "No", "Yes")) %>%
  mutate(across(
    c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13), as.numeric)) %>% # Ensure C1 to C9 are numeric
  mutate(across(
    c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13), as.numeric)) %>% 
  mutate(across(
    c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13), as_hms)) %>% 
  rename(experience.hololens = expertise) %>% 
  mutate(across(
    c(
      gender,
      en.native,
      experience.hololens
    ),
    ~ as.factor(.x)
  ))

Hmisc::label(study4.df$en.native) <- "1 if native speaker, 0 if not"

## Not run:
# Hmisc::describe(study2.df) # Displays a summary of study2.df including labels
## End(Not run)

#Experiment data frame

study4.long.df <- study4.df %>%
  pivot_longer(
    cols = c(
      c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
      s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
      t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13
    ),
    names_to = c(".value", "timepoint"),     # .value magic!
    names_pattern = "([cst])(\\d+)",          # RegEx: capture C/S/T and the number
    names_transform = list(timepoint = as.integer)
  ) %>%
  select(participant.id, timepoint, cortisol = c, perceived = s, time = t) %>% 
  mutate(
    cortisol.winsorized = { # Winsorize and log-transform Cortisol values
      quantiles <- quantile(cortisol, probs = c(0.05, 0.95), na.rm = TRUE)
      pmin(pmax(cortisol, quantiles[1]), quantiles[2])
    },
    cortisol.log = log(cortisol.winsorized) #Log-transform Cortisol values
  ) %>%
  mutate(
    resource = case_when(
      timepoint == 1 ~ "baseline",
      timepoint >= 2 & timepoint <= 7 ~ "digital",
      timepoint >= 8 & timepoint <= 13 ~ "traditional"
    )
  )

# Join the static variables with the long format data
#Runit only once
study4.long.df <- left_join(study4.long.df,
                            study4.df %>%
                              select(-c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
                                        s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
                                        t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)),
                            by = c("participant.id")
)

# Compute AUCg and AUCi for each participant for Cortisol_log and Perceived using actual time values

# For Cortisol_log
AUCg.results.cortisol <- study4.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCg.cortisol = compute.AUCg(time, cortisol.log), .groups = "drop")

AUCi.results.cortisol <- study4.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCi.cortisol = compute.AUCi(time, cortisol.log), .groups = "drop")

# For Perceived stress
AUCg.results.perceived <- study4.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCg.perceived = compute.AUCg(time, perceived), .groups = "drop")

AUCi.results.perceived <- study4.long.df %>%
  group_by(participant.id, resource) %>%
  reframe(AUCi.perceived = compute.AUCi(time, perceived), .groups = "drop")


# Transform data to wide format
study4.wide.df <- study4.long.df %>%
  select(participant.id, timepoint, perceived, cortisol.log, resource, gender, age, bmi, en.native, w.corr.lenses, experience.hololens) %>%
  pivot_wider(
    names_from = timepoint, 
    values_from = c(cortisol.log, perceived),
    names_sep = "_T"
  )

# Merge AUCi results into wide-format data
study4.final.df <- AUCi.results.cortisol %>%
  left_join(AUCi.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.cortisol, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(study4.wide.df, by = c("participant.id", "resource"))%>% 
  mutate(
    AUCi.cortisol = if_else(resource == "baseline", cortisol.log_T1, AUCi.cortisol),
    AUCg.cortisol = if_else(resource == "baseline", cortisol.log_T1, AUCg.cortisol),
    AUCi.perceived = if_else(resource == "baseline", perceived_T1, AUCi.perceived),
    AUCg.perceived = if_else(resource == "baseline", perceived_T1, AUCg.perceived)
  ) %>%
  group_by(participant.id) %>%
  filter(!any(is.na(AUCi.cortisol) | is.na(AUCi.perceived))) %>%            #Remove participants for whom there is NA
  ungroup() %>%
  select(-matches("^\\.groups")) %>% 
  mutate(resource = factor(resource, levels = c("baseline", "digital", "traditional"))) %>%  # Set correct resource order
  arrange(participant.id, resource)


remove(AUCg.results.cortisol, AUCg.results.perceived, AUCi.results.cortisol, AUCi.results.perceived) # clean workspace

##############################
#
#   save data
#
###############################

save(list = c("study1.final.df","study2.final.df","study3.final.df","study4.final.df"), file = "./02 RData/analysis.RData")
