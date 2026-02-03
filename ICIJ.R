setwd("C:/Users/wiedmann4/Documents/Aid and corruption")
library(tidyverse)
library(dplyr)
#library(plyr)
library(countrycode)
library(countries)
library(tidyr)
library(stringr)
library(readr)
library(data.table)

########################
# Upload the ICIJ data #
########################

intermediaries <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-intermediaries.csv") 
intermediaries <- select(intermediaries, -c(status, note, valid_until, sourceID))

entities <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-entities.csv")
entities <- select(entities, -c(status, sourceID, valid_until, note, service_provider, dorm_date))

addresses <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-addresses.csv")
addresses <- select(addresses, -c(sourceID, valid_until, note))

officers <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-officers.csv")
officers <- select(officers, -c(sourceID, valid_until, note))

others <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-others.csv")
others <- select(others, -c(sourceID, valid_until, note))

relationships <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/relationships.csv") 
relationships <- select(relationships, -c(status, sourceID))

DubaiDataAO <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiDataOA.csv")

library(stringr)

relationships[, .(node_id_start, node_id_end, rel_type)]

person_entity <- relationships %>%
  filter(rel_type %in% c("officer_of", "intermediary_of", "connected_to", "same_comapny_as", "same_intermediary_as", "similar_company_as", "probably_same_officer_as", "same_as", "same_id_as")) %>%
  left_join(officers, by = c("node_id_start" = "node_id")) %>%
  left_join(entities, by = c("node_id_end" = "node_id"))

person_entity <- subset(person_entity, select = -c(sourceID.y, sourceID.x, valid_until.y, valid_until.x, service_provider, country_code.x, country_code.y))

oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
  "Costa Rica", "Czech Republic", "Denmark", "Estonia", "Finland",
  "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
  "Israel", "Italy", "Japan", "South Korea", "Latvia", "Lithuania",
  "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain",
  "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States",
  "Saint Kitts and Nevis", "Hong Kong","British Virgin Islands","Jersey",
  "United Arab Emirates","Singapore","Guernsey", "Cayman Islands",
  "Liechtenstein","Fiji"  
)

# Filter out rows where countries.x matches any OECD country
person_entity <- person_entity[!person_entity$countries.x %in% oecd_countries, ]

# Filter out rows where countries.x matches any OECD country
person_entity <- person_entity[!person_entity$countries.y %in% oecd_countries, ]

pe_intermediary <- relationships %>%
  filter(rel_type %in% c("officer_of", "intermediary_of", "connected_to", "same_comapny_as", "same_intermediary_as", "similar_company_as", "probably_same_officer_as", "same_as", "same_id_as")) %>%
  left_join(person_entity, by = c("node_id_start")) %>%
  left_join(intermediaries, by = c("node_id_end2" = "node_id"))

#######################################################
# Check whether these names appear in the DubaiDataAO #
#######################################################

intermediaries <- intermediaries %>%
  mutate(name = ifelse(
    str_detect(name, ","),  # Check if name contains a comma
    paste(
      str_trim(str_extract(name, "(?<=,).*")),  # Extract and trim part after comma
      str_trim(str_extract(name, ".*(?=,)"))    # Extract and trim part before comma
    ),
    name  # Keep original name if no comma
  ))

# Create a column to mark matches
intermediaries <- intermediaries %>%
  mutate(
    in_LeadersName = name %in% DubaiDataAO$LeadersName,
    in_SandName_clean = name %in% DubaiDataAO$SandName_clean,
    in_either = in_LeadersName | in_SandName_clean
  )

# Summary of matches
cat("Total names in intermediaries:", nrow(intermediaries), "\n")
cat("Names found in LeadersName:", sum(intermediaries$in_LeadersName), "\n")
cat("Names found in SandName_clean:", sum(intermediaries$in_SandName_clean), "\n")
cat("Names found in either column:", sum(intermediaries$in_either), "\n\n")

# View names that match
cat("Names that match:\n")
intermediaries %>%
  filter(in_either) %>%
  select(name, in_LeadersName, in_SandName_clean) %>%
  print()

# Optional: View names that don't match
cat("\nNames that don't match:\n")
intermediaries %>%
  filter(!in_either) %>%
  select(name) %>%
  head(10) %>%
  print()


######################################################
# Check if intermediaries appear in the leaders list #
######################################################

leaders <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/leaders.csv")


library(stringdist)
# Function to find best fuzzy match for each leader name
find_best_match <- function(leader_name, inter_names, threshold = 0.85) {
  # Calculate string distance (using Jaro-Winkler similarity)
  similarities <- stringdist::stringsim(leader_name, inter_names, method = "jw")
  
  # Find best match
  best_match_idx <- which.max(similarities)
  best_similarity <- similarities[best_match_idx]
  
  if (best_similarity >= threshold) {
    return(data.frame(
      leader_name = leader_name,
      intermediary_name = inter_names[best_match_idx],
      similarity = best_similarity
    ))
  }
  return(NULL)
}

# Find matches for all leader names
cat("Searching for fuzzy matches...\n\n")

matches <- lapply(leaders$LeadersName, find_best_match, 
                  inter_names = intermediaries$name, threshold = 0.85)
matches_df <- do.call(rbind, matches[!sapply(matches, is.null)])

# Display results
cat("Total leader names checked:", nrow(leaders), "\n")
cat("Total intermediary names:", nrow(intermediaries), "\n")
cat("Fuzzy matches found (similarity >= 0.85):", 
    ifelse(is.null(matches_df), 0, nrow(matches_df)), "\n\n")

if (!is.null(matches_df) && nrow(matches_df) > 0) {
  cat("Matches found:\n")
  print(matches_df %>% arrange(desc(similarity)))
  
  # Save to new dataframe for further analysis
  leader_intermediary_matches <- matches_df
  
  cat("\nMatches saved to 'leader_intermediary_matches'\n")
} else {
  cat("No fuzzy matches found with similarity >= 0.85\n")
  cat("Try lowering the threshold (e.g., 0.75 or 0.80) for more matches\n")
}

# remove doubles

matches_df <- unique(matches_df)

write.csv(matches_df, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/ICIJmatches.csv")


library(stringdist)
# Function to find best fuzzy match for each leader name
find_best_match <- function(leader_name, off_names, threshold = 0.85) {
  # Calculate string distance (using Jaro-Winkler similarity)
  similarities <- stringdist::stringsim(leader_name, off_names, method = "jw")
  
  # Find best match
  best_match_idx <- which.max(similarities)
  best_similarity <- similarities[best_match_idx]
  
  if (best_similarity >= threshold) {
    return(data.frame(
      leader_name = leader_name,
      officer_name = off_names[best_match_idx],
      similarity = best_similarity
    ))
  }
  return(NULL)
}

# Find matches for all leader names
cat("Searching for fuzzy matches...\n\n")

matches <- lapply(leaders$LeadersName, find_best_match, 
                  off_names = officers$name, threshold = 0.85)
matches_df <- do.call(rbind, matches[!sapply(matches, is.null)])

# Display results
cat("Total leader names checked:", nrow(leaders), "\n")
cat("Total officers names:", nrow(officers), "\n")
cat("Fuzzy matches found (similarity >= 0.85):", 
    ifelse(is.null(matches_df), 0, nrow(matches_df)), "\n\n")

if (!is.null(matches_df) && nrow(matches_df) > 0) {
  cat("Matches found:\n")
  print(matches_df %>% arrange(desc(similarity)))
  
  # Save to new dataframe for further analysis
  leader_officers_matches <- matches_df
  
  cat("\nMatches saved to 'leader_intermediary_matches'\n")
} else {
  cat("No fuzzy matches found with similarity >= 0.85\n")
  cat("Try lowering the threshold (e.g., 0.75 or 0.80) for more matches\n")
}

# remove doubles

matches_df <- unique(matches_df)
#write.csv(matches_df, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/ICIJmatches_officers.csv")
officer_matches <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/ICIJmatches_officers.csv")
