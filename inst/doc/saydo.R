## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4)
options(pillar.width = 90)

## ----setup--------------------------------------------------------------------
library(saydo)

## ----data---------------------------------------------------------------------
users <- mock_mixed_data(n_users = 150, seed = 2026)
users

## ----codebook-----------------------------------------------------------------
codebook <- c(
  UI_Confusion        = "The user reports being lost, confused, or unable to find something in the interface",
  Positive_Experience = "The user expresses satisfaction, praise, or enjoyment of the product",
  Feature_Request     = "The user asks for a feature or capability that does not currently exist"
)

## ----llm, eval = FALSE--------------------------------------------------------
# coded <- llm_auto_code(
#   users, user_id, transcript_feedback, codebook,
#   chat = ellmer::chat_openai(model = "gpt-4.1-mini")
# )

## ----code---------------------------------------------------------------------
coded <- llm_auto_code(users, user_id, transcript_feedback, codebook,
                       coder = mock_coder())
coded[, c("user_id", names(codebook))]

## ----human--------------------------------------------------------------------
set.seed(1)
sample_rows <- sample(nrow(users), 50)
flip <- function(x, rate = 0.08) xor(x, runif(length(x)) < rate)

human <- data.frame(
  user_id = users$user_id[sample_rows],
  UI_Confusion = flip(users$profile[sample_rows] == "frustrated"),
  Positive_Experience = flip(users$profile[sample_rows] == "efficient"),
  Feature_Request = flip(grepl("wish|please add|would be great",
                               users$transcript_feedback[sample_rows]))
)

code_agreement(coded, human)

## ----cross-tab----------------------------------------------------------------
results <- cross_tab_themes(coded, session_duration_sec)
results

## ----errors-------------------------------------------------------------------
cross_tab_themes(coded, errors_encountered)[, c("theme", "difference", "p_adjusted", "significant")]

## ----plot, fig.alt = "Dot plot of the mean difference in session duration per theme, with confidence intervals."----
plot_theme_impact(results)

## ----segments-----------------------------------------------------------------
segments <- data.frame(
  participant = c("P1", "P1", "P2", "P2", "P3", "P4", "P4", "P5", "P6"),
  segment_id  = 1:9,
  text = c("It was fine at first.", "Then I got completely lost.",
           "Everything was clean and fast.", "I love the export.",
           "So confusing, I could not find the settings.",
           "Normal day.", "Nothing to report.",
           "I got lost twice.", "It works."),
  stringsAsFactors = FALSE
)
logs <- data.frame(participant = paste0("P", 1:6),
                   minutes_active = c(42, 12, 39, 15, 45, 14))

coded_segments <- llm_auto_code(segments, participant, text,
                                c("UI_Confusion", "Positive_Experience"),
                                coder = mock_coder())

cross_tab_themes(coded_segments, minutes_active, metrics = logs)

