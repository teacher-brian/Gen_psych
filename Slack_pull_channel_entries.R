library(tidyverse)

# --------------------------------------------------
# Paste Slack text here
# If full dates are not present in the paste,
# fallback_date will be used.
# --------------------------------------------------
text <- "Jacob Newell  [8:44 PM]
https://docs.google.com/document/d/1wk4JCjNnHQDW9CU13tccXDXDj3xnYLuaKnPXJA_IJvE/edit?usp=sharing
Marcus Williams  [5:53 AM]
joined #assignment-1.Meenu Bansal  [3:14 PM]
https://docs.google.com/document/d/13gLKI6PKQQLObNasV_SA-m0oJR9uhgKzyUX-o2Bud_w/edit?usp=sharing
Adelyn Rostomily  [7:31 PM]
joined #assignment-1. Also,  joined.Ewan Shaw  [6:27 PM]
https://docs.google.com/document/d/1WpgVQcDYqmu3YJ_7e3X9g06xVYxQ7yimNEYoturivN4/edit?usp=sharing
Hannah Leahy  [12:50 PM]
joined #assignment-1.Hannah Leahy  [1:48 PM]
https://docs.google.com/document/d/14TTG_il_2E-z7dbdpbYPsSbBXs-wZXJMi3Cff4Z_8Ss/edit?usp=drivesdk
"

# --------------------------------------------------
# Use this when the pasted Slack text does NOT include
# actual dates like "April 17th"
# --------------------------------------------------
fallback_date <- as.Date("2026-04-17")

# --------------------------------------------------
# Patterns
# --------------------------------------------------
google_doc_pattern <- "https://docs\\.google\\.com/document/d/[^[:space:]]+"
time_pattern <- "\\[(\\d{1,2}:\\d{2}\\s*[AP]M)\\]"
name_time_pattern <- "^(.*?)\\s*\\[(\\d{1,2}:\\d{2}\\s*[AP]M)\\]$"

# A light attempt to catch date headers if they appear in pasted Slack text
# Example lines this may catch:
# "April 17th"
# "Apr 17"
# "Friday, April 17th"
date_header_pattern <- paste0(
  "(?i)^(?:",
  "(?:monday|tuesday|wednesday|thursday|friday|saturday|sunday),?\\s+)?",
  "(jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|",
  "aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)\\s+",
  "\\d{1,2}(?:st|nd|rd|th)?(?:,\\s*\\d{4})?$"
)

# --------------------------------------------------
# Split into lines
# --------------------------------------------------
lines_df <- tibble(raw_line = str_split(text, "\n")[[1]]) %>%
  mutate(raw_line = str_replace_all(raw_line, "\u00A0", " ")) %>%
  mutate(line = str_squish(raw_line)) %>%
  filter(line != "")

# --------------------------------------------------
# Parse date headers if they exist
# --------------------------------------------------
parse_slack_date <- function(x, fallback_year = lubridate::year(fallback_date)) {
  cleaned <- x %>%
    str_to_lower() %>%
    str_replace_all("^(monday|tuesday|wednesday|thursday|friday|saturday|sunday),?\\s+", "") %>%
    str_replace_all("(st|nd|rd|th)", "") %>%
    str_squish()

  # If year missing, add fallback year
  if (!str_detect(cleaned, "\\b\\d{4}\\b")) {
    cleaned <- paste(cleaned, fallback_year)
  }

  parsed <- suppressWarnings(lubridate::mdy(cleaned))
  parsed
}

lines_df <- lines_df %>%
  mutate(
    is_date_header = str_detect(line, regex(date_header_pattern)),
    parsed_date = if_else(is_date_header, as.character(map_chr(line, ~ as.character(parse_slack_date(.x)))), NA_character_)
  ) %>%
  mutate(parsed_date = as.Date(parsed_date)) %>%
  fill(parsed_date, .direction = "down") %>%
  mutate(parsed_date = coalesce(parsed_date, fallback_date))

# --------------------------------------------------
# Extract names, times, and links
# --------------------------------------------------
parsed <- lines_df %>%
  mutate(
    extracted_name = str_match(line, name_time_pattern)[, 2],
    extracted_time = str_match(line, name_time_pattern)[, 3],
    link = str_extract(line, google_doc_pattern)
  ) %>%
  mutate(
    extracted_name = na_if(extracted_name, ""),
    # Ignore obvious non-message noise as "names"
    extracted_name = if_else(
      !is.na(extracted_name) &
        str_detect(str_to_lower(extracted_name), "^(joined|left|added|removed|set the channel topic|set the channel purpose)"),
      NA_character_,
      extracted_name
    )
  ) %>%
  fill(extracted_name, extracted_time, .direction = "down") %>%
  filter(!is.na(link)) %>%
  mutate(
    name = extracted_name,
    date = parsed_date,
    time = extracted_time
  ) %>%
  filter(!is.na(name)) %>%
  filter(!str_detect(str_to_lower(name), "^joined\\b")) %>%
  select(name, date, time, link) %>%
  distinct()

print(parsed)

write_csv(parsed, "slack_google_doc_links.csv")