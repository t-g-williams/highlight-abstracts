###### HIGHLIGHT KEYWORDS IN TITLES/ABSTRACTS ######
###### MAKE AN HTML FILE ######
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tibble)
library(htmltools)
library(rlang)

main <- function() {
  # 1) Named list of keyword groups
  # Each group corresponds to a clause of your search string
  # (i.e., each group is separated by AND in your search)
  # and will be highlighted in a different color
  keywords <- list(
    model   = c("model*"),
    agrifood= c("*agri*", "agro*", "food", "farm*", "land", "*forest*"),
    market  = c("general equilibrium","partial equilibrium","CGE","GTAP",
                "economy[- ]?wide","market","integrated assessment"),
    env     = c("pollinat*","seed dispers*","biological","control","biocontrol*",
                "natural","enem*","predator*","herbivor*","pest*",
                "pathogen*","disease*","microb*","mycorrhiz*",
                "fungi","fungus*","earthworm*","soil fauna",
                "soil biodiversity","biodivers*","species",
                "functional diversity","taxonomic diversity",
                "fauna","flora","vegetation*",
                "ecosystem service*","nature's contribution*",
                "biotic*","organism*","habitat*",
                "bee","bees","insect*","arthropod*","invertebrate*","bird*","bat","bats","rodent*")
  )
  
  # 2) further query details
  color_mapping_hex <- list(
    "model"="#0000FF", 
    "agrifood"="#006400", 
    "market"="#ADD8E6", 
    "env"="#FFA500")
  
  ## settings
  # inputs
  w_dir <- 'C:/Users/User/Documents/highlight_abstracts/'
  csv_in <- file.path(w_dir, 'scopus_raw.csv') # input CSV file from Scopus export
  max_N <- NA # set to a number to speed up for testing. set to NA to run for all
  shuffle <- T # set to TRUE to shuffle the order of the papers when making the file
  # outputs
  dir_out <- file.path(w_dir, 'outputs')
  fn_out <- 'scopus_bolded'
  

  ## bold keywords
  make_bolded_html(csv_in, max_N, shuffle, keywords, color_mapping_hex, w_dir, dir_out, fn_out)
}


make_bolded_html <- function(csv_in, max_N, shuffle, keywords, color_mapping_hex, w_dir, dir_out, fn_out) {
  # Read data
  df <- read.csv(csv_in, stringsAsFactors = FALSE) %>% 
    as_tibble() %>%
    mutate(id = row_number())
  
  # shuffle (optional)
  if (shuffle) {
    set.seed(1)
    df <- df[sample(nrow(df)), ]
  }
  # filter (optional)
  if (!is.na(max_N)) {
    df <- df[1:max_N, ]
  }
  
  # format the individual elements
  df <- df %>%
    mutate(
      title_bolded = bold_keywords(Title, keywords, color_mapping_hex),
      abstract_bolded = bold_keywords(Abstract, keywords, color_mapping_hex),
      keywords_bolded = bold_keywords(Author.Keywords, keywords, color_mapping_hex),
      index_keywords_bolded = bold_keywords(Index.Keywords, keywords, color_mapping_hex),
      link_hyperlink = clean_scopus_link(Link),
      doi_hyperlink = make_doi_link(DOI)
    )
  
  # remove the copyright info at the end of the abstracts
  # (remove all text following the copyright symbol)
  df$abstract_bolded <- gsub("©.*", "", df$abstract_bolded)
  
  
  # Export to CSV (Markdown or HTML style)
  fs::dir_create(dir_out)
  write.csv(df, file.path(dir_out, paste0(fn_out, ".csv")), row.names = FALSE)
  
  
  ## NEW
  # Fast HTML (no pandoc dependency)
  items <- pmap_chr(
    list(df$id, df$title_bolded, df$Authors, df$Source.title, df$Year, 
         df$keywords_bolded, df$index_keywords_bolded, 
         df$link_hyperlink, df$doi_hyperlink, df$abstract_bolded),
    function(id, title, auth, journal, year, ak, ik, linka, doia, abs) {
      paste0(
        "<h3>[", id, "]</h3>",
        "<div class='meta'>", auth %||% "", "</div>",
        "<div class='meta'><i>", htmlEscape(journal %||% ""), " (", htmlEscape(as.character(year %||% "")), ")</i></div>",
        "<div>", title, "</div>",
        "<div class='meta'>Author keywords: ", ak, "</div>",
        "<div class='meta'>Index keywords: ", ik, "</div>",
        "<div class='meta'>", paste(c(linka, doia)[c(linka, doia)!=""], collapse=" | "), "</div>",
        "<p>", abs, "</p>"
      )
    }
  )
  
  css <- build_css(color_mapping_hex)
  html <- paste0("<!doctype html><meta charset='utf-8'>", css, "<h1>Highlighted abstracts</h1>", paste(items, collapse = "\n"))
  out_html <- file.path(dir_out, paste0(fn_out, ".html"))
  writeLines(html, out_html)
  
}


bold_keywords <- function(text, keyword_list, color_map) {
  
  # Loop over each color group in 'keyword_list'
  for (color_group in names(keyword_list)) {
    # The hex color for this group
    current_color <- color_map[[color_group]]
    
    # The keywords for this group
    these_keywords <- keyword_list[[color_group]]
    
    for (w in these_keywords) {
      # 1) Convert "*" to a regex that matches zero or more letters or digits
      w_regex <- gsub("\\*", "[A-Za-z0-9]*", w)
      
      # 2) Word-boundary pattern so we highlight the entire word
      pattern <- paste0("\\b", w_regex, "\\b")
      
      # 3) Use HTML <b> + inline style for color and bold
      replacement <- paste0("<b style=\"color:", current_color, ";\">\\0</b>")
      
      # 4) Fix NA characters in the text
      text[is.na(text)] <- ""
      
      # 5) Replace
      text <- str_replace_all(
        text,
        regex(pattern, ignore_case = TRUE),
        replacement
      )
    }
  }
  return(text)
}


clean_scopus_link <- function(link_text) {
  return(ifelse(is.na(link_text) | link_text == "", 
         "",
         paste0("<a href=\"", link_text, "\" target=\"_blank\">Scopus link</a>")))
}


make_doi_link <- function(doi_raw) {
  return(ifelse(
    is.na(doi_raw) | doi_raw == "", 
    "",
    paste0("<a href=\"https://doi.org/", doi_raw, "\" target=\"_blank\">DOI link</a>")))
}


build_css <- function(color_map) {
  paste0(
    "<style>",
    paste0(
      sprintf(".k-%s { font-weight: 700; color: %s; }",
              names(color_map), unname(color_map)),
      collapse = " "
    ),
    "body{font-family:'Noto Serif','DejaVu Serif',Georgia,serif;",
    "line-height:1.4;padding:1.25rem;max-width:900px;margin:auto}",
    "h3{margin-top:1.5rem;border-top:1px solid #eee;padding-top:1rem}",
    ".meta{color:#555;font-size:0.95rem;margin:.25rem 0}",
    "</style>"
  )
}


main()
