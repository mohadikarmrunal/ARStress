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
compute_AUCg <- function(Time, Measure) {
  AUCg <- 0
  for (i in 1:(length(Time) - 1)) {
    # Convert difftime to numeric (in hours)
    delta_time <- as.numeric(difftime(Time[i + 1], Time[i], units = "hours"))  
    avg_measure <- (Measure[i + 1] + Measure[i]) / 2  # Trapezoidal rule for averaging the measures
    AUCg <- AUCg + avg_measure * delta_time
  }
  return(AUCg)
}

# Function to compute AUCi (Area Under the Curve - Increase)
compute_AUCi <- function(Time, Measure) {
  AUCg <- compute_AUCg(Time, Measure)  # Get the AUCg
  total_time <- as.numeric(difftime(Time[length(Time)], Time[1], units = "hours"))  # Total time interval in hours
  AUCi <- AUCg - Measure[1] * total_time  # Subtract the baseline area
  return(AUCi)
}

###############################
#
#   clean and prepare study 1 data frame
#
###############################

#Survey data frame
study1.df <- study1.raw.df %>% 
  janitor::clean_names() %>% 
  rename_with(~ gsub("_", ".", .x)) %>%
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

Hmisc::label(study1.df$gender) <- "0 if Male, 1 if Female, 2 if Diverse"
Hmisc::label(study1.df$en.native) <- "1 if native speaker, 0 if not"
Hmisc::label(study1.df$learning.disorders) <- "0 if none, 1 if present"

## Not run:
# Hmisc::describe(study1.df) # Displays a summary of study1.df including labels
## End(Not run)

#Experiment data frame

study1.long.df <- study1.df %>%
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
study1.long.df <- left_join(study1.long.df,
    study1.df %>%
      select(-c(c1, c2, c3, c4, c5, c6, c7, c8, c9,
                s1, s2, s3, s4, s5, s6, s7, s8, s9,
                t1, t2, t3, t4, t5, t6, t7, t8, t9)),
    by = c("participant.id")
  )

# Compute AUCg and AUCi for each participant for Cortisol_log and Perceived using actual time values

# For Cortisol_log
AUCg.results.cortisol <- study1.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCg.cortisol = compute_AUCg(time, cortisol.log), .groups = "drop")

AUCi.results.cortisol <- study1.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCi.cortisol = compute_AUCi(time, cortisol.log), .groups = "drop")

# For Perceived stress
AUCg.results.perceived <- study1.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCg.perceived = compute_AUCg(time, perceived), .groups = "drop")

AUCi.results.perceived <- study1.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCi.perceived = compute_AUCi(time, perceived), .groups = "drop")


# Transform data to wide format
study1.wide.df <- study1.long.df %>%
  select(participant.id, timepoint, perceived, cortisol.log, condition, resource, gender, age, bmi, learning.disorders, en.native, experience.gpt) %>%
  pivot_wider(
    names_from = timepoint, 
    values_from = c(cortisol.log, perceived),
    names_sep = "_T"
  )

# Merge AUCi results into wide-format data
study1.final.df <- AUCi.results.cortisol %>%
  left_join(AUCi.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.cortisol, by = c("participant.id", "resource")) %>%
  left_join(AUCg.results.perceived, by = c("participant.id", "resource")) %>%
  left_join(study1.wide.df, by = c("participant.id", "resource"))

remove(AUCg.results.cortisol, AUCg.results.perceived, AUCi.results.cortisol, AUCi.results.perceived) # clean workspace

###############################
#
#   clean and prepare study 2 data frame
#
###############################

#Survey data frame
study2.df <- study2.raw.df %>% 
  janitor::clean_names() %>% 
  rename_with(~ gsub("_", ".", .x)) %>%
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

Hmisc::label(study2.df$en.native) <- "1 if native speaker, 0 if not"

## Not run:
# Hmisc::describe(study1.df) # Displays a summary of study1.df including labels
## End(Not run)

#Experiment data frame

study2.long.df <- study2.df %>%
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
study2.long.df <- left_join(study2.long.df,
                            study2.df %>%
                              select(-c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13,
                                        s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
                                        t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)),
                            by = c("participant.id")
)

# Compute AUCg and AUCi for each participant for Cortisol_log and Perceived using actual time values

# For Cortisol_log
AUCg.results.cortisol <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCg.cortisol = compute_AUCg(time, cortisol.log), .groups = "drop")

AUCi.results.cortisol <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCi.cortisol = compute_AUCi(time, cortisol.log), .groups = "drop")

# For Perceived stress
AUCg.results.perceived <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCg.perceived = compute_AUCg(time, perceived), .groups = "drop")

AUCi.results.perceived <- study2.long.df %>%
  group_by(participant.id, resource) %>%
  summarise(AUCi.perceived = compute_AUCi(time, perceived), .groups = "drop")


# Transform data to wide format
study2.wide.df <- study2.long.df %>%
  select(participant.id, timepoint, perceived, cortisol.log, condition, resource, gender, age, bmi, en.native, w.corr.lenses, experience.hololens) %>%
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
  left_join(study2.wide.df, by = c("participant.id", "resource"))

remove(AUCg.results.cortisol, AUCg.results.perceived, AUCi.results.cortisol, AUCi.results.perceived) # clean workspace



##############################
#
#   save data
#
###############################

save(list = c("study1.final.df","study2.final.df"), file = "./02 RData/analysis.RData")

