
# conjugate_igbo_vowel_harmony.R
# Uses vowel harmony rules:
# past       = stem + "r" + V
# pres_perf  = stem + "go"
# past_perf  = stem + "bugo"
# where V is the final vowel of the stem.

# install.packages(c("readxl","dplyr","stringr","tidyr","writexl"))

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

dataset <- "igbo_verb_conjugator_vowel_harmony.xlsx"

lexicon <- read_excel("C:\\Users\\obflo\\OneDrive\\Desktop\\Ling 349\\conjugation\\igbo_verb_conjugator_vowel_harmony.xlsx", sheet = "lexicon", col_types = "text")
persons <- read_excel("C:\\Users\\obflo\\OneDrive\\Desktop\\Ling 349\\conjugation\\igbo_verb_conjugator_vowel_harmony.xlsx", sheet = "persons", col_types = "text")
tenses <- read_excel("C:\\Users\\obflo\\OneDrive\\Desktop\\Ling 349\\conjugation\\igbo_verb_conjugator_vowel_harmony.xlsx", sheet = "tenses", col_types = "text")
irreg <- read_excel("C:\\Users\\obflo\\OneDrive\\Desktop\\Ling 349\\conjugation\\igbo_verb_conjugator_vowel_harmony.xlsx", sheet = "irregulars", col_types = "text")

lexicon <- lexicon %>% mutate(
  infinitive = coalesce(infinitive, ""),
  stem = coalesce(stem, "")
)

persons <- persons %>% mutate(
  person = coalesce(person, ""),
  pronoun = coalesce(pronoun, "")
)

tenses <- tenses %>% mutate(
  tense = coalesce(tense, "")
)

# Helper: get final vowel
get_final_vowel <- function(stem) {
  ifelse(is.na(stem) | stem == "", "", str_sub(stem, -1))
}

make_past <- function(stem) {
  V <- get_final_vowel(stem)
  paste0(stem, "r", V)
}

make_pres_perf <- function(stem) {
  V <- get_final_vowel(stem)
  paste0(stem,  "go")
}

make_past_perf <- function(stem) {
  V <- get_final_vowel(stem)
  paste0(stem,  "bugo")
}

make_present <- function(pronoun, stem) {
  str_squish(paste(pronoun, stem))
}

make_pres_cont <- function(pronoun, stem) {
  str_squish(paste(pronoun, "na", stem))
}

make_future <- function(pronoun, stem) {
  str_squish(paste(pronoun, "ga", stem))
}

# Expand verb × person × tense
grid <- lexicon %>%
  crossing(persons) %>%
  crossing(tenses)

# Generate forms
grid <- grid %>%
  mutate(
    generated = case_when(
      tense == "past" ~ make_past(stem),
      tense == "pres_perf" ~ make_pres_perf(stem),
      tense == "past_perf" ~ make_past_perf(stem),
      tense == "present" ~ make_present(pronoun, stem),
      tense == "pres_cont" ~ make_pres_cont(pronoun, stem),
      tense == "future" ~ make_future(pronoun, stem),
      TRUE ~ ""
    )
  )

# Apply irregular overrides if present
if (nrow(irreg) > 0) {
  grid <- grid %>%
    left_join(irreg %>%
                rename(irreg_full = full_form),
              by = c("infinitive", "tense", "person")) %>%
    mutate(final_form = ifelse(!is.na(irreg_full) & irreg_full != "",
                               irreg_full,
                               generated))
} else {
  grid <- grid %>% mutate(final_form = generated)
}

out <- grid %>%
  select(infinitive, stem, tense, person, pronoun, final_form) %>%
  arrange(infinitive, tense, person)

write_xlsx(list(conjugated = out),
           path = "conjugated_igbo_vowel_harmony.xlsx")
write.csv(out, "conjugated_igbo_vowel_harmony.csv", row.names = FALSE)

cat("Done. Created conjugated_igbo_vowel_harmony.xlsx and .csv\n")
