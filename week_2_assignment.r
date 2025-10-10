# Questions 
# What is the research question or hypothesis of this study? (What exactly are the authors testing or comparing?) 
      # "Are there statistically significant differences in cognitive growth between those who experience cognitive training entirely through a clinician-delivered model compared to those who experience 	cognitive training in a mixed-delivery model (online and clinician-delivered)?"
# What is the ThinkRx program? (What kinds of cognitive skills does it target? What materials or methods are used in the one-on-one version? What is the digital component “Brainskills” and how is it related?) 
      # ThinkRx is a "clinician-delivered training program for children that targets multiple cognitive skills—including working memory, long-term memory, processing speed, visual processing, auditory 	processing, logic and reasoning, and attention".
# What are the dependent variables (outcomes) measured? (Which subtests from Woodcock Johnson III, and which constructs do they aim to measure?) 
      # Associative memory				                Visual auditory learning		      Participant learns a rebus and then recalls and recites the association between the pictures and the words.
      # Visual processing				                  Spatial relations			            Participant visually matches individual puzzle pieces to a completed shape.
      # Auditory processing				                Sound blending				            Participant hears a series of phonemes and then blends them to form a word.
      # Logic and reasoning			  	              Concept formation			            Participant applies inductive rules to a set of shapes and indicates the rule that differentiates them.
      # Processing speed			      	            Visual matching				            In 3 min, participant identifies and circles pairs of matching numbers in each row.
      # Working memory and attentional capacity		Numbers reversed			            Participant hears a list of numbers and repeats them in reverse order.
      # Long-term memory				                  Visual auditory learning—delayed	Participant recalls verbal-visual associations learned earlier by reading rebus passages.
      # General intellectual ability (GIA)		    Composite score for g			         GIA is a weighted composite of tests 1–7 on the WJ III.
# What were the main results? (Which outcomes did not differ between groups? Where was a significant difference found? What was the direction of that difference?) 
      # "Results showed no significant differences between groups on tests of working memory, logic and reasoning, auditory processing, visual processing, processing speed, or overall IQ score. Results were significantly different on the test of long-term memory."

# set wd
setwd("C:/Users/Will/Desktop/week_2_assignment")

# load packages
library(tidyverse)
library(readr)
library(janitor)
library(effsize)

# merge datasets and make the treatment and disability entries easier to understand (as opposed to numerical values)
test_results <- read_csv("test_results(in).csv")
participants <- read_csv("participants(in).csv")
groups <- read_csv("groups(in).csv")
diagnosis <- read_csv("diagnosis(in).csv")
View(diagnosis)
df <- test_results %>%
  # full_join() joins the datasets by the participant_id column
  full_join(participants, by = "participant_id") %>%
  full_join(groups, by = "participant_id") %>%
  full_join(diagnosis, by = "participant_id") %>%
  # grepl() checks if the string contains the specified pattern; the ~ symbol is used to assign the value to the variable if it matches in a new column on df
  mutate(
    treatment = case_when(
      grepl("1", group) ~ "One-on-one delivery", 
      grepl("2", group) ~ "Mixed delivery",
      TRUE ~ NA_character_
    ),
    disability = case_when(
      grepl("0", no_disability) ~ "No disability",
      grepl("1", no_disability) ~ "Disability",
      TRUE ~ NA_character_
    )
  )
View(df)
# dependent variables
dependent_vars <- c(
  "associative_memory_gain",
  "visual_processing_gain",
  "auditory_processing_gain",
  "logic_reasoning_gain",
  "processing_speed_gain",
  "working_memory_gain",
  "long_term_memory_gain",
  "iq_gain"
)

# anova function for comparing dependent variables between treatment groups
run_anova_summary <- function(df, dependent_vars, independent_var = "treatment") {
  results <- map_dfr(dependent_vars, function(variable) { # map_dfr performs iteration of function over each element of dependent_vars
                                          # function(variable) is a function defined within subsequent curly brackets that 
                                          # is applied to each element of dependent_vars
                                          # without this results/second function, you would not calculate statistics for every dependent_vars
    # anova function 
    formula <- as.formula(paste(variable, "~", independent_var)) # pastes together dependent and independent variables into the anova function
    model <- aov(formula, data = df) # anova function
    summary_model <- summary(model)[[1]]  # the summary function produces model as a list (a collection of data that can contain many types of data like df, 
                                          # matrices, vectors, etc.), which for model, contains only the ANOVA table.
                                          # [[1]] extracts the first element of this list, which is the ANOVA table (a data frame).
                                          # you won't be able to extract the F and p values from summary_model directly, since it is just a list of elements.
                                          # example output:
                                          # > summary(model)[[1]]
                                          # [[1]]
                                          #             Df Sum Sq Mean Sq F value Pr(>F)
                                          # treatment    1   2000   2000    25.0  0.001
                                          # Residuals    8    640     80
    F_value <- summary_model[["F value"]][1] # this does the same thing as above, except it only takes the desired F value without the NA value that would show up in Residuals row (element 1 ([1]) of list F value ([["F value"]]))
    p_value <- summary_model[["Pr(>F)"]][1] # this does the same thing as above, except it only takes the desired p value without the NA value that would show up in Residuals row (element 1 ([1]) of list Pr(>F) ([["Pr(>F)"]]))
    
    # group by treatment and calculate means and SDs
    group_stats <- df %>%
      group_by(!!sym(independent_var)) %>%  # groups the data by treatment
                                            # the sym() function makes the string into a symbol (which dplyr uses for column names)
                                            # !! is used to remove the quotation marks around the string (tidy-eval package)    
      summarise(
        mean = mean(.data[[variable]], na.rm = TRUE), # in tidyverse, functions like mutate()/summarise()/etc. use a masked version of your df variables
                                                      # i.e., instead of having to df$treatment, you can just write treatment.
                                                      # if your variable name is stored as a string, you can use .data[[variable]] (unique pronoun to tidyverse).
                                                      # otherwise, if you were to do something such as mean(treatment), it would try to find the mean of the string "treatment".
                                                      # na.rm = TRUE removes any missing values (noted as NA) from the calculation
        sd = sd(.data[[variable]], na.rm = TRUE),
        .groups = "drop" # removes grouping by treatment after summarizing, which prevents the grouping from altering results downstream 
      )
    
    # extract specific statistics
    mean_one_on_one <- group_stats %>%
      filter(!!sym(independent_var) == "One-on-one delivery") %>%
      pull(mean)
    
    mean_mixed <- group_stats %>%
      filter(!!sym(independent_var) == "Mixed delivery") %>%
      pull(mean)
    
    sd_one_on_one <- group_stats %>%
      filter(!!sym(independent_var) == "One-on-one delivery") %>%
      pull(sd)
    
    sd_mixed <- group_stats %>%
      filter(!!sym(independent_var) == "Mixed delivery") %>%
      pull(sd)
    
    # compute mean difference (M1-M2 on the table provided)
    mean_diff <- mean_one_on_one - mean_mixed
    
    # compute Cohen's d
    group1 <- df %>% filter(!!sym(independent_var) == "One-on-one delivery") %>% pull(!!sym(variable))
    group2 <- df %>% filter(!!sym(independent_var) == "Mixed delivery") %>% pull(!!sym(variable))
    d_value <- cohen.d(group1, group2, na.rm = TRUE)$estimate
    
    # Combine all into a single row tibble
    tibble(
      Variable = variable,
      'One-on-one delivery'      = round(mean_one_on_one, 2),
      'One-on-one delivery SD'   = round(sd_one_on_one, 2),
      'Mixed delivery'           = round(mean_mixed, 2),
      'Mixed delivery SD'        = round(sd_mixed, 2),
      'Mean difference (M1-M2)'  = round(mean_diff, 2),
      'F value'                  = round(F_value, 2),
      'p value'                  = round(p_value, 2),
      'Cohen\'s d'               = round(d_value, 2)
    )
  })
  
  return(results)
}

# run anova function
anova_summary <- run_anova_summary(df, dependent_vars)

# remove "_gain" from variable names and replace underscores with spaces
anova_summary <- anova_summary %>%
  mutate(
    Variable = str_replace_all(Variable, "_gain", "") |> str_replace_all("_", " ") |> str_to_title()
  )

View(anova_summary)
