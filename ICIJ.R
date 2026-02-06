setwd("C:/Users/wiedmann4/Documents/Aid and corruption")
library(tidyverse)
library(dplyr)
#library(plyr)
library(countrycode)
library(countries)
library(tidyr)
library(stringr)
library(stringdist)
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

 # DubaiDataAO <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiDataOA.csv")
 # DubaiData <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiData.csv")

officers_names <- read.csv("C:/Users/wiedmann4/Documents/Aid and corruption/Out/ICIJmatches_officers_short.csv")
names_to_keep <- officers_names$leader_name
officers_filtered <- officers %>%
  filter(name %in% names_to_keep)


# Find all officers sharing the same last name
normalize_name <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[-_]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

target_lastname <- "allawi"
max_distance <- 1   # light fuzziness only

family_nodes <- officers %>%
  mutate(
    name_norm = normalize_name(name),
    last_token = word(name_norm, -1)
  ) %>%
  filter(
    stringdist::stringdist(last_token, target_lastname, method = "lv") <= max_distance
  ) %>%
  select(node_id, name)

family_nodes

start_nodes <- family_nodes$node_id

trace_network_with_family <- function(start_nodes,
                                      relationships,
                                      nodes_all,
                                      max_depth = 6) {
  
  visited <- tibble(node_id = start_nodes$node_id)
  frontier <- start_nodes %>%
    mutate(depth = 0)
  
  results <- tibble()
  
  # Record family links explicitly
  if (nrow(start_nodes) > 1) {
    results <- bind_rows(
      results,
      tibble(
        from_node = start_nodes$node_id[1],
        to_node = start_nodes$node_id[-1],
        depth = 0,
        rel_type = "same_last_name"
      ) %>%
        left_join(nodes_all, by = c("to_node" = "node_id"))
    )
  }
  
  while (nrow(frontier) > 0) {
    
    current <- frontier[1, ]
    frontier <- frontier[-1, ]
    
    if (current$depth >= max_depth) next
    
    links <- relationships %>%
      filter(
        node_id_start == current$node_id |
          node_id_end == current$node_id
      ) %>%
      mutate(
        next_node = if_else(
          node_id_start == current$node_id,
          node_id_end,
          node_id_start
        )
      )
    
    for (i in seq_len(nrow(links))) {
      
      nid <- links$next_node[i]
      
      if (nid %in% visited$node_id) next
      
      visited <- bind_rows(visited, tibble(node_id = nid))
      frontier <- bind_rows(
        frontier,
        tibble(node_id = nid, depth = current$depth + 1)
      )
      
      node_info <- nodes_all %>%
        filter(node_id == nid)
      
      results <- bind_rows(
        results,
        tibble(
          from_node = current$node_id,
          to_node = nid,
          depth = current$depth + 1,
          rel_type = links$rel_type[i]
        ) %>%
          left_join(node_info, by = c("to_node" = "node_id"))
      )
    }
  }
  
  results
}

ali_family_network <- trace_network_with_family(
  start_nodes = family_nodes,
  relationships = relationships,
  nodes_all = nodes_all,
  max_depth = 3
)

ali_family_network %>%
  arrange(depth) %>%
  select(depth, name, type, rel_type)

ali_family_network <- ali_family_network %>%
  filter(
    type %in% c("officer", "intermediary"),
    !is.na(name),
    name != ""
  )

Allawi_SC <- select(ali_family_network, "name")
write.csv(Allawi_SC, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/Allawi_SC.csv", row.names = F)
















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
  "Liechtenstein","Fiji","ok","Oman","Kuwait","Monaco"
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



