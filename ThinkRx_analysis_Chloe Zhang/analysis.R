x <- 2 + 2
print(x)

#Questions 

#What is the research question or hypothesis of this study? (What exactly are the authors testing or comparing?) 
#Hypothesis: There is no significant difference in cognitive skill improvements between one-on-one delivery and mixed delivery of ThinkRx cognitive training.

#What is the ThinkRx program? (What kinds of cognitive skills does it target? What materials or methods are used in the one-on-one version? What is the digital component “Brainskills” and how is it related?) 
#It is for children that targets multiple cognitive skills - including working memory, long-term memory, processing speed, visual processing, auditory processing, logic and reasoning, and attention.

#What are the dependent variables (outcomes) measured? (Which subtests from Woodcock Johnson III, and which constructs do they aim to measure?) 
#Associative memory, visual processing, auditory processing, logic and reasoning, processing speed, working memory and attentional capacity, long-term memory, and general intellectual ability.

#What were the main results? (Which outcomes did not differ between groups? Where was a significant difference found? What was the direction of that difference?)
#Except for long-term memory, there were no significant differences between two delivery methods. 

###################Reproduce ThinkRx paper analysis######################
dir.create("data", showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
getwd()
list.files()
library(dataverse)
library(tidyverse)
library(readr)
library(broom)
library(car)
library(effectsize)
library(clubSanwich)
library(sanwich)
# Download ThinkRx dataset
## load required packages
library(dataverse)
library(readr)
library (purrr)
## Point to the Harvard Dataverse server
Sys.setenv ("DATAVERSE_SERVER" = "dataverse.harvard.edu")
## Create a variable for my data folder
data_folder <- "data"
## Dataset DOI from Harvard Dataverse
doi <- "10.7910/DVN/BF0EEQ"
## Get dataset metadata
ds <- dataverse::get_dataset(doi)
## See what files are available in this dataset
purrr::map_chr(ds$files, ~ .x$label)
#Download ThinkRx dataset
library(dataverse)
library(readr)
Sys.setenv ("DATAVERSE_SERVER" = "dataverse.harvard.edu")
doi <- "10.7910/DVN/BF0EEQ"
files <- dataverse::dataset_files(doi)
files
#################Manually Download CSV folder in the data file#######
library(readr)
library(dplyr)
#load the CSV from my data folder
diagnosis <- read_csv("data/diagnosis.csv")
names(diagnosis)
head(diagnosis)
participants <- read_csv("data/participants.csv")
names(participants)
head(participants)
groups <- read_csv("data/groups.csv")
names(groups)
head(groups)
test_results <- read_csv("data/test_results.csv")
names(test_results)
head(test_results)
#Find shared column names#
library(dplyr)
## View all column names
list(
  diagnosis = names(diagnosis),
  groups = names(groups),
  participants = names(participants),
  test_results = names(test_results)
)
## see which column names appear more than once across datasets
all_cols <- c(names(diagnosis), names(groups), names(participants), names(test_results))
sort(table(all_cols), decreasing = TRUE)
## check pairwise overlaps
intersect(names(diagnosis),    names(groups))
intersect(names(diagnosis),    names(participants))
intersect(names(diagnosis),    names(test_results))
intersect(names(groups),       names(participants))
intersect(names(groups),       names(test_results))
intersect(names(participants), names(test_results))
#split a single combined column into two in "groups" CSV file
library (readr)
library(dplyr)
library(tidyr)
tmp <- read_csv("data/groups.csv", col_names = FALSE, show_col_types = FALSE)
head(tmp)
##split the single column on the comma into two columns
groups <- tmp %>%
  tidyr::separate(X1, into =c("participant_id", "group"),
                  sep =",", convert = TRUE, remove = TRUE)
## If the first row is actually the header text, drop it:
if(is.character(groups$participant_id) && groups$participant_id[1] == "participant_id") {
  groups <- groups[-1, ]}
groups <- groups %>% mutate(participant_id = as.double(participant_id), group = as.character(group))
names(groups)
head(groups)
unique(groups$group)
#label the groups
groups <- groups %>%
  mutate(group = dplyr::recode(group, "1" = "one_on_one", "2" = "mixed", .default = NA_character_))%>%
  mutate(group = factor(group, levels = c("one_on_one", "mixed")))
head(groups)
table(groups$group)
#check the actual type and values
str(groups)
unique(groups$group)
library(readr)
library(dplyr)
library(tidyr)

# Read the raw file as before
tmp <- read_csv("data/groups.csv", col_names = FALSE, show_col_types = FALSE)

# Split into two columns and auto-convert types
groups <- tmp %>%
  tidyr::separate(X1, into = c("participant_id", "group"),
                  sep = ",", convert = TRUE, remove = TRUE) %>%
  # Drop the header row that became NA after convert=TRUE
  filter(!is.na(participant_id)) %>%
  # Relabel using numeric comparison (robust)
  mutate(
    group = dplyr::case_when(
      group == 1 ~ "one_on_one",
      group == 2 ~ "mixed",
      TRUE       ~ NA_character_
    ),
    group = factor(group, levels = c("one_on_one","mixed"))
  )

# Sanity checks
str(groups)
head(groups)
table(groups$group, useNA = "ifany")
#reomve the NA level that came from header
groups <- groups %>%
  filter(participant_id != "participant_id") %>%
  mutate(participant_id = as.double(participant_id), group = droplevels(group))
#sanity checks
str(groups)
head(groups)
table(groups$group, useNA = "ifany")
#participants without group
participants %>% anti_join(groups, by = "participant_id")
#groups without participants
groups %>% anti_join(participants, by = "participant_id")

library(dplyr)

master <- participants %>%
  left_join(groups, by = "participant_id")%>%
  left_join(diagnosis, by = "participant_id")%>%
  left_join(test_results, by = "participant_id")
#Keep only rows that have a group label
gain_vars <- c("associative_memory_gain","visual_processing_gain","auditory_processing_gain",
               "logic_reasoning_gain","processing_speed_gain","working_memory_gain",
               "long_term_memory_gain","iq_gain")

analysis_df <- master %>%
  filter(!is.na(group)) %>%
  filter(if_all(all_of(gain_vars), ~!is.na(.)))

dim(analysis_df)
summary(analysis_df$group)

#Run the MANOVA
Y <- as.matrix(analysis_df[, gain_vars])
fit_manova <- manova(Y ~ group, data = analysis_df)
summary(fit_manova, test = "Wilks")

#Univariate follow-ups
uni <- summary.aov(fit_manova)
uni_p <- sapply(uni, function(x) coef(summary(x))["group", "Pr(>F)"])
p_adj <- p.adjust(uni_p, method = "bonferroni")

data.frame(
  measure = gain_vars,
  p_raw = as.numeric(uni_p),
  p_bonf = as.numeric(p_adj)
)|> arrange(p_bonf)

#Run separate ANOVAS
library(broom)
library(dplyr)

uni_tbl <- lapply(gain_vars, function(v) {
  fit <- aov(reformulate("group", v), data = analysis_df)
  broom::tidy(fit) |>
    dplyr::filter(term == "group") |>
    dplyr::mutate(measure = v) |>
    dplyr::select(measure, statistic, df, p.value)
}) |> dplyr::bind_rows()

uni_tbl <- uni_tbl |>
  mutate(p_bonf = p.adjust(p.value, method = "bonferroni")) |>
  arrange(p_bonf)

uni_tbl

#Reproduce Table 3
library(dplyr)
library(tidyr)
library(broom)
library(effectsize)
library(stringr)
library(gt)   

gain_vars <- c(
  "associative_memory_gain",
  "visual_processing_gain",
  "auditory_processing_gain",
  "logic_reasoning_gain",
  "processing_speed_gain",
  "working_memory_gain",
  "long_term_memory_gain",
  "iq_gain"
)

# Pretty labels to match the paper
pretty <- c(
  associative_memory_gain = "Associative memory",
  visual_processing_gain  = "Visual processing",
  auditory_processing_gain= "Auditory processing",
  logic_reasoning_gain    = "Logic and reasoning",
  processing_speed_gain   = "Processing speed",
  working_memory_gain     = "Working memory",
  long_term_memory_gain   = "Long-term memory",
  iq_gain                 = "IQ score"
)

# Pre-outcome ANOVA + effect size
one_outcome <- function(var){
  # group-wise change mean/sd
  grp <- analysis_df %>%
    select(group, change = all_of(var)) %>%
    group_by(group) %>%
    summarise(mean = mean(change, na.rm = TRUE),
              sd   = sd(change, na.rm = TRUE),
              n    = n(),
              .groups = "drop") %>%
    mutate(group = as.character(group)) %>%
    pivot_wider(names_from = group,
                values_from = c(mean, sd, n))
  
  # Univariate ANOVA (unadjusted p as in Table 3)
  fit <- aov(reformulate("group", var), data = analysis_df)
  aov_row <- tidy(fit) %>% filter(term == "group")
  
  # Cohen's d (one_on_one vs mixed)
  d_val <- effectsize::cohens_d(reformulate("group", var), data = analysis_df,
                                pooled_sd = TRUE)$Cohens_d
  
  tibble(
    Variable = pretty[[var]],
    # per-group change columns (mean (SD))
    `One-on-one Change Mean (SD)` =
      sprintf("%.2f (%.2f)", grp$mean_one_on_one, grp$sd_one_on_one),
    `Mixed Change Mean (SD)` =
      sprintf("%.2f (%.2f)", grp$mean_mixed, grp$sd_mixed),
    # difference M1 - M2 (change means)
    `M1–M2` = grp$mean_one_on_one - grp$mean_mixed,
    F = aov_row$statistic,
    p = aov_row$p.value,
    d = d_val
  )
}

#Build the table for all outcomes
tab <- lapply(gain_vars, one_outcome) %>% bind_rows()

# Order rows like the paper
tab <- tab %>% mutate(Variable = factor(Variable, levels = pretty[gain_vars])) %>% arrange(Variable)

# Nicely formatted table
tab_print <- tab %>%
  mutate(
    `M1–M2` = sprintf("%.2f", `M1–M2`),
    F = sprintf("%.2f", F),
    p = ifelse(p < .001, "<.001", sprintf("%.3f", p)),
    d = sprintf("%.2f", d)
  ) %>%
  select(Variable,
         `One-on-one Change Mean (SD)`,
         `Mixed Change Mean (SD)`,
         `M1–M2`, F, p, d)

# Save a CSV you can share
readr::write_csv(tab_print, "output/table3_like_changes_only.csv")

# Show a styled table in the Viewer (optional)
gt_tbl <- tab_print %>%
  gt() %>%
  tab_header(title = "Results of significance testing between groups (Change scores)") %>%
  cols_label(
    Variable = "Variable",
    `One-on-one Change Mean (SD)` = "One-on-one Change\nMean (SD)",
    `Mixed Change Mean (SD)` = "Mixed Change\nMean (SD)",
    `M1–M2` = html("Difference<br>(M1–M2)")
  )

gt_tbl


