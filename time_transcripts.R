# time stamps from transcripts

library(officer)
library(xml2)
library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(tidyr)


# Ścieżka do katalogu z plikami DOCX (relatywna do katalogu projektu RStudio)
folder_path <- "transkrypcje08052025"

# Lista plików .docx
docx_files <- list.files(folder_path, pattern = "\\.docx$", full.names = TRUE)

# Funkcja wyodrębniająca tekst z jednego pliku
extract_paragraphs <- function(file_path) {
  doc <- read_docx(file_path)
  doc_xml <- docx_summary(doc)
  
  # Wyciągamy tylko paragrafy (type == "paragraph")
  paragraphs <- doc_xml %>%
    filter(content_type == "paragraph") %>%
    pull(text)
  
  tibble(
    filename = basename(file_path),
    paragraph = paragraphs
  )
}

# Zastosuj funkcję do wszystkich plików i połącz w jedną ramkę danych
all_data <- map_df(docx_files, extract_paragraphs)




# Wzorzec timestampu
timestamp_pattern <- "\\b\\d{2}:\\d{2}:\\d{2}\\b"

# Funkcja przetwarzająca paragrafy z jednego pliku
process_paragraphs <- function(file_paragraphs, filename) {
  results <- list()
  current_timestamp <- NA
  current_speaker_line <- NA
  current_text <- c()
  
  for (line in file_paragraphs) {
    if (str_detect(line, timestamp_pattern)) {
      # Zapisz poprzednią wypowiedź, jeśli istnieje
      if (!is.na(current_timestamp)) {
        results <- append(results, list(tibble(
          filename = filename,
          timestamp = current_timestamp,
          speaker_line = current_speaker_line,
          utterance = paste(current_text, collapse = " ")
        )))
      }
      # Rozpocznij nowy blok
      current_timestamp <- str_extract(line, timestamp_pattern)
      current_speaker_line <- str_trim(str_remove(line, ".*?(\\d{2}:\\d{2}:\\d{2})\\s*")) # tekst po timestamp
      current_text <- c()
    } else {
      # Dodaj linijkę do aktualnej wypowiedzi
      current_text <- c(current_text, line)
    }
  }
  
  # Dodaj ostatnią wypowiedź, jeśli istnieje
  if (!is.na(current_timestamp)) {
    results <- append(results, list(tibble(
      filename = filename,
      timestamp = current_timestamp,
      speaker_line = current_speaker_line,
      utterance = paste(current_text, collapse = " ")
    )))
  }
  
  bind_rows(results)
}

# Przetwarzamy wszystkie pliki z all_data
processed_data <- all_data %>%
  group_by(filename) %>%
  summarise(result = list(process_paragraphs(paragraph, first(filename)))) %>%
  unnest(result, names_sep = "_")

# Podgląd
print(head(processed_data, 5))

saveRDS(processed_data, "processed_data")



