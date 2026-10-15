# read scores and missed questions to see which questions were very hard

library(tidyverse)

exam9am <- read_csv("/home/brian/Documents/exams/Fall2026 Exam Performance/9am_consolidated_scores.csv")
exam11am <- read_csv("/home/brian/Documents/exams/Fall2026 Exam Performance/11am_consolidated_scores.csv")
#exam1pm <- read_csv("/home/brian/Documents/exams/Fall2026 Exam Performance/1pm_consolidated_scores.csv")

# add section field and rbind
exam9am$section <- "h2"
exam11am$section <- "h4"
#exam1pm$section <- "h6"

exam <- rbind(exam9am,exam11am)

missed <- exam |>
  select(student_id, incorrect_questions) |>
  separate_longer_delim(incorrect_questions, delim = ";") |>
  filter(!is.na(incorrect_questions), incorrect_questions != "") |>
  mutate(question = as.integer(incorrect_questions)) |>
  count(question, name = "n_incorrect") |>
  mutate(
    n_students = nrow(exam),
    pct_incorrect = round(100 * n_incorrect / n_students, 1)
  ) |>
  arrange(desc(pct_incorrect))

print(missed, n = 100)
