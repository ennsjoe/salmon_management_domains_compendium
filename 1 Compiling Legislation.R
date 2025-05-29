#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 1 - COMPILING LEGISLATION------------------------------------------------
# This script uses HTML files of legislation specific to Pacific salmon (Type A) to map 
# keywords to build further queries in CanLii (https://www.canlii.org/)
# Outputs: HTML Files saved to 2 folders (Type A Legislation, Type B Legislation)
# Note: Downloading HTML files is a manual process, but could be automated in the
# future using APIs
#///////////////////////////////////////////////////////////////////////////////

## **BEFORE STARTING - Download Type A Legislation HTML files
# Query CanLii website for (salmon OR sockeye OR chinook OR coho OR chum) - Type A Keywords
# Download html files to folder called Type A Legislation in working directory

# SETUP ------------------------------------------------------------------------
## i) Set Working Directory ----------------------------------------------------
library(here)

# Get the root directory of the project
here()

## ii) Load Libraries ----------------------------------------------------------
library(data.table)
library(udpipe)
library(xml2)
library(rvest)
library(stringi)  # For text cleaning

## iii) Load Udpipe model for word frequency analysis---------------------------
udpipe_model_path <- here("english-ewt-ud-2.5-191206.udpipe")

## iv) Create the salmon_keywords data table------------------------------------
salmon_keywords <- data.table(
  Keyword = c("salmon", "chinook", "sockeye", "coho", "chum"),
  Specificity = 1,
  Frequency = NA
)

## v) Read in Management Domain Keywords and IUCN Threats-----------------------

# Define file path using the `here()` function
file_path_mgmt <- here("Management Domain Threats and Keywords.csv")

# Check if file exists before reading
if (!file.exists(file_path_mgmt)) stop("Error: Management Domain Threats and Keywords file not found.")

# Read CSV into a data table while preserving multi-word phrases in double quotes
mgmt_d_keywords <- tryCatch({
  fread(file_path_mgmt, quote = "\"")
}, error = function(e) {
  stop("Error reading CSV file: ", e$message)
})

# Verify expected columns exist
required_columns <- c("Management Domain", "L1", "L2", "Keyword", "Specificity")
if (!all(required_columns %in% colnames(mgmt_d_keywords))) {
  stop("Error: Expected columns missing from the dataset.")
}

# 1.1 - Parse HTML Files into paragraphs ---------------------------------------
## 1.1.1 - Read in HTML files---------------------------------------------------
 
# Define the folder dynamically using `here()`
html_dir <- here("Type A Legislation")

# Ensure the directory exists
if (!dir.exists(html_dir)) stop("Error: Directory does not exist. Check the path.")

# Read all HTML files (handling special characters)
html_files <- list.files(path = html_dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)

# Normalize paths to handle special characters
html_files <- normalizePath(html_files, winslash = "/", mustWork = FALSE)

# Debugging print: Confirm files found
cat("Total HTML files detected:", length(html_files), "\n")

# Check if no files are found
if (length(html_files) == 0) stop("No HTML files found in the specified directory.")

## 1.1.2 - Parse paragraph nodes and assign section numbers--------------------

# Initialize Paragraphs_DT
Paragraphs_DT <- data.table(
  `Legislation Name` = character(),
  `Section Number` = character(),
  `XPath` = character(),
  `Paragraph` = character()  # Updated column name
)

# Function to clean text and remove special characters
clean_text <- function(text) {
  text <- stri_trans_general(text, "Latin-ASCII")  # Convert special characters to ASCII equivalents
  text <- gsub("[^[:print:]]", "", text)  # Remove any remaining non-printable characters
  return(trimws(text))
}

# Function to extract section numbers inside `<p>` tags
extract_inline_section <- function(node) {
  section_label <- node %>% html_nodes("span.secnum span.secnumholder b, a.sectionLabel span.sectionLabel") %>% html_text(trim = TRUE)
  if (length(section_label) > 0) return(clean_text(section_label)) else return(NA)
}

# Function to extract the legislation name
extract_legislation_name <- function(html_file) {
  legislation_name <- html_file %>% html_nodes("h1.HeadTitle, div#title h2") %>% html_text(trim = TRUE)
  legislation_name <- ifelse(length(legislation_name) > 0, clean_text(legislation_name[1]), "Unknown Legislation")
  return(legislation_name)
}

# Function to compute XPath depth
get_xpath_depth <- function(xpath) {
  return(length(strsplit(xpath, "/")[[1]]))
}

# Process each HTML file
for (file in html_files) {
  html_file <- read_html(file)
  
  # Extract legislation name
  legislation_name <- extract_legislation_name(html_file)
  
  # Extract all paragraph nodes
  all_paragraphs <- html_file %>% html_nodes("p")
  
  last_section_number <- NA
  last_section_xpath <- NA
  last_xpath_depth <- NA
  
  # Step 1: Capture Section Numbers and Ensure Correct Assignment
  for (node in all_paragraphs) {
    current_xpath <- xml_path(node)
    paragraph_class <- xml_attr(node, "class")  # Check paragraph class
    
    inline_section_number <- extract_inline_section(node)
    
    # If the paragraph is a "division", treat it separately and reset section tracking
    if (!is.na(paragraph_class) && grepl("division", paragraph_class, ignore.case = TRUE)) {
      last_section_number <- NA   # Reset section number
      last_section_xpath <- NA    # Reset XPath tracking
    } else if (!is.na(inline_section_number)) {
      last_section_number <- inline_section_number
      last_section_xpath <- current_xpath
    } 
    
    # Assign previous section number only if the paragraph is not a division
    paragraph_text <- clean_text(xml_text(node, trim = TRUE))
    
    if (nzchar(paragraph_text)) {
      Paragraphs_DT <- rbind(Paragraphs_DT, data.table(
        `Legislation Name` = legislation_name,
        `Section Number` = last_section_number,  # Avoid propagating NA section numbers
        `XPath` = current_xpath,
        `Paragraph` = paragraph_text  # Updated column name
      ), fill = TRUE)
    }
    
    last_xpath_depth <- current_xpath_depth
  }
}

# Step 2: Remove Rows with NA Section Numbers
Paragraphs_DT <- Paragraphs_DT[!is.na(`Section Number`)]

# Step 3: Remove Rows Containing "repeal", "repealed", or "revoked"
filter_words <- c("repeal", "repealed", "revoked")
Paragraphs_DT <- Paragraphs_DT[!grepl(paste(filter_words, collapse = "|"), `Paragraph`, ignore.case = TRUE)]

## 1.1.3 - Group paragraphs by section number-----------------------------------

# Group Paragraphs by Section Number and Legislation Name
Grouped_DT <- Paragraphs_DT[, .(
  `Paragraph` = paste(`Paragraph`, collapse = " ")  # Updated column name
), by = .(`Legislation Name`, `Section Number`)]

# 1.2 - Filter paragraphs by Salmon Keywords (salmon, chinook, coho, sockeye, chum)----

# Ensure keywords are properly formatted as stand-alone words, case insensitive
salmon_keywords[, Keyword := paste0("\\b", tolower(Keyword), "\\b")]

# Create a pattern to match any of the keywords
keyword_pattern <- paste(salmon_keywords$Keyword, collapse = "|")

# Convert Paragraph column to lowercase for case-insensitive matching
Type_A_PE <- Grouped_DT[grepl(keyword_pattern, tolower(Paragraph), ignore.case = FALSE)]

# 1.3 - Extract salmon-adjacent unigrams----------------------------------------

# Define the path to your UDPipe model using the `here()` function
udpipe_model_path <- here("english-ewt-ud-2.5-191206.udpipe")
ud_model <- udpipe_load_model(udpipe_model_path)

# Perform annotation on the Paragraph column of Type_A_PE
annotation <- udpipe_annotate(ud_model, x = tolower(Type_A_PE$Paragraph))  # Convert text to lowercase before processing
annotation <- as.data.table(annotation)

# Filter for unigrams that are only nouns
filtered_unigrams <- annotation[upos == "NOUN", .(Keyword = tolower(lemma))]  # Ensure lemmas are lowercase

# Exclude unigrams containing punctuation or single-letter unigrams
filtered_unigrams <- filtered_unigrams[!grepl("[[:punct:]]", Keyword) & nchar(Keyword) > 1]

# Compute frequency count and exclude unigrams with a frequency of 1
unigram_freq <- filtered_unigrams[, .(Frequency = .N), by = Keyword]
unigram_freq <- unigram_freq[Frequency > 1]

# Create the final datatable
Type_A_unigrams <- unigram_freq

# Calculate the 75th percentile threshold for unigram frequency
freq_threshold <- quantile(Type_A_unigrams$Frequency, probs = 0.75)

# Filter Type_A_unigrams to only keep those in the highest quartile
Type_A_unigrams_highest_quartile <- Type_A_unigrams[Frequency >= freq_threshold]

# Remove unigrams that are present in the Keyword column of mgmt_d_keywords
Type_A_unigrams_highest_quartile <- Type_A_unigrams_highest_quartile[!Keyword %in% mgmt_d_keywords$Keyword]

# Convert salmon_keywords into a single regex pattern
salmon_pattern <- paste(salmon_keywords$Keyword, collapse = "|")

# Convert mgmt_d_keywords into a separate regex pattern
mgmt_pattern <- paste(mgmt_d_keywords$Keyword, collapse = "|")

# Remove unigrams that match either pattern
Type_A_unigrams_highest_quartile <- Type_A_unigrams_highest_quartile[
  !grepl(salmon_pattern, Keyword, ignore.case = TRUE) & !grepl(mgmt_pattern, Keyword, ignore.case = TRUE)
]

# 1.4 - Create a CanLii Keyword Query List--------------------------------------

# Merge Type_A_unigrams_highest_quartile with the Keyword column from mgmt_d_keywords
CanLii_query_keywords <- merge(Type_A_unigrams_highest_quartile, mgmt_d_keywords[, .(Keyword)], by = "Keyword", all.x = TRUE)

# Define output file path (CSV saved to working directory)
output_file <- file.path(getwd(), "CanLii_query_keywords.csv")

# Export the datatable to CSV
fwrite(CanLii_query_keywords, file = output_file, sep = ",", quote = TRUE)

# Print confirmation message
print(paste("CSV file saved to:", output_file))

# 1.5 - Save R Objects----------------------------------------------------------

# Define output file path using Here
output_file <- here("mgmt_d_1.RData")

# Save the R object
save(salmon_keywords, mgmt_d_keywords, Type_A_unigrams_highest_quartile, file = output_file)


