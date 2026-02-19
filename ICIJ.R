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

#######################
# Test run Ali Allawi #
#######################

# Ensure all node_id columns are character type
officers <- officers %>% mutate(node_id = as.character(node_id))
intermediaries <- intermediaries %>% mutate(node_id = as.character(node_id))
entities <- entities %>% mutate(node_id = as.character(node_id))
relationships <- relationships %>%
  mutate(
    node_id_start = as.character(node_id_start),
    node_id_end = as.character(node_id_end)
  )

# Create nodes_all by combining all node types
nodes_all <- bind_rows(
  officers %>% select(node_id, name) %>% mutate(type = "officer"),
  intermediaries %>% select(node_id, name) %>% mutate(type = "intermediary"),
  entities %>% select(node_id, name) %>% mutate(type = "entity")
) %>%
  filter(!is.na(name)) %>%
  distinct(node_id, .keep_all = TRUE)

cat("Created nodes_all with", nrow(nodes_all), "nodes\n")
print(table(nodes_all$type))

# Find all officers sharing the same last name
normalize_name <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[-_]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# run test for Ali Allawi

target_lastname <- "allawi"
max_distance <- 1

family_nodes <- officers %>%
  mutate(
    name_norm = normalize_name(name),
    last_token = word(name_norm, -1)
  ) %>%
  filter(
    stringdist(last_token, target_lastname, method = "lv") <= max_distance
  ) %>%
  select(node_id, name)

cat("\nFamily nodes found:\n")
print(family_nodes)

# Define the network tracing function
# Note: start_nodes must be a dataframe with node_id and name columns
trace_network_with_family <- function(start_nodes,
                                      relationships,
                                      nodes_all,
                                      max_depth = 3) {
  
  # start_nodes is a dataframe, visited initialized from it
  visited <- tibble(node_id = as.character(start_nodes$node_id))
  frontier <- start_nodes %>%
    mutate(
      node_id = as.character(node_id),
      depth = 0
    )
  
  results <- tibble()
  
  # Record family links explicitly (same last name connections)
  if (nrow(start_nodes) > 1) {
    family_links <- tibble(
      from_node = start_nodes$node_id[1],
      to_node = start_nodes$node_id[-1],
      depth = 0,
      rel_type = "same_last_name"
    ) %>%
      left_join(nodes_all, by = c("to_node" = "node_id"))
    
    results <- bind_rows(results, family_links)
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
    
    if (nrow(links) == 0) next
    
    for (i in seq_len(nrow(links))) {
      
      nid <- links$next_node[i]
      
      if (nid %in% visited$node_id) next
      
      visited <- bind_rows(visited, tibble(node_id = nid))
      frontier <- bind_rows(
        frontier,
        tibble(node_id = nid, name = NA_character_, depth = current$depth + 1)
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

# Run the network trace
ali_family_network <- trace_network_with_family(
  start_nodes = family_nodes,
  relationships = relationships,
  nodes_all = nodes_all,
  max_depth = 3
)

# Display full results
cat("\nFull network results:\n")
ali_family_network %>%
  arrange(depth) %>%
  select(depth, name, type, rel_type) %>%
  print(n = 50)

# Filter to people only
ali_family_network_people <- ali_family_network %>%
  filter(
    type %in% c("officer", "intermediary"),
    !is.na(name),
    name != ""
  )

cat("\nPeople connected to Allawi family network:\n")
ali_family_network_people %>%
  arrange(depth) %>%
  select(depth, name, type, rel_type) %>%
  print(n = 50)

write.csv(ali_family_network, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/Ali2.csv")


###############################################################################
# Full run all names in officers_names (matches with political leaders, 100%) #
###############################################################################

# Function to extract last name
extract_lastname <- function(name) {
  name_norm <- normalize_name(name)
  word(name_norm, -1)
}

# Define the network tracing function
trace_network_with_family <- function(start_nodes,
                                      relationships,
                                      nodes_all,
                                      max_depth = 3) {
  
  visited <- tibble(node_id = as.character(start_nodes$node_id))
  frontier <- start_nodes %>%
    mutate(
      node_id = as.character(node_id),
      depth = 0
    )
  
  results <- tibble()
  
  # Record family links explicitly (same last name connections)
  if (nrow(start_nodes) > 1) {
    family_links <- tibble(
      from_node = start_nodes$node_id[1],
      to_node = start_nodes$node_id[-1],
      depth = 0,
      rel_type = "same_last_name"
    ) %>%
      left_join(nodes_all, by = c("to_node" = "node_id"))
    
    results <- bind_rows(results, family_links)
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
    
    if (nrow(links) == 0) next
    
    for (i in seq_len(nrow(links))) {
      
      nid <- links$next_node[i]
      
      if (nid %in% visited$node_id) next
      
      visited <- bind_rows(visited, tibble(node_id = nid))
      frontier <- bind_rows(
        frontier,
        tibble(node_id = nid, name = NA_character_, depth = current$depth + 1)
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

# Initialize list to store all results
all_family_networks <- list()
max_distance <- 1  # Fuzzy matching tolerance

# Loop through each officer in officers_filtered
for (i in 1:nrow(officers_filtered)) {
  
  original_name <- officers_filtered$name[i]
  original_id <- officers_filtered$node_id[i]
  
  cat("\n========================================\n")
  cat("Processing:", original_name, "(", i, "of", nrow(officers_filtered), ")\n")
  cat("========================================\n")
  
  # Extract target lastname
  target_lastname <- extract_lastname(original_name)
  
  cat("Target lastname:", target_lastname, "\n")
  
  # Find all officers with similar last name
  family_nodes <- officers %>%
    mutate(
      name_norm = normalize_name(name),
      last_token = word(name_norm, -1)
    ) %>%
    filter(
      stringdist(last_token, target_lastname, method = "lv") <= max_distance
    ) %>%
    select(node_id, name)
  
  cat("Found", nrow(family_nodes), "family members with similar last name\n")
  
  if (nrow(family_nodes) == 0) {
    cat("No family nodes found, skipping...\n")
    next
  }
  
  # Run the network trace
  family_network <- trace_network_with_family(
    start_nodes = family_nodes,
    relationships = relationships,
    nodes_all = nodes_all,
    max_depth = 3
  )
  
  # Filter to people only (officers and intermediaries)
  family_network_people <- family_network %>%
    filter(
      type %in% c("officer", "intermediary"),
      !is.na(name),
      name != ""
    )
  
  if (nrow(family_network_people) > 0) {
    
    # Extract unique connected names
    connected_names <- unique(family_network_people$name)
    
    # Remove the original person and their family members from connections
    family_member_names <- family_nodes$name
    connected_names <- setdiff(connected_names, family_member_names)
    
    cat("Found", length(connected_names), "non-family connections\n")
    
    # Create connection details
    connection_paths <- family_network_people %>%
      filter(!name %in% family_member_names) %>%
      mutate(
        connection_detail = paste0(name, " (depth ", depth, ", via: ", rel_type, ")")
      ) %>%
      pull(connection_detail)
    
    # Store result
    all_family_networks[[i]] <- data.frame(
      original_name = original_name,
      original_node_id = original_id,
      family_size = nrow(family_nodes),
      family_members = paste(family_nodes$name, collapse = "; "),
      connected_names = paste(connected_names, collapse = "; "),
      connection_details = paste(connection_paths, collapse = " | "),
      number_of_connections = length(connected_names),
      stringsAsFactors = FALSE
    )
    
  } else {
    
    cat("No non-family connections found\n")
    
    all_family_networks[[i]] <- data.frame(
      original_name = original_name,
      original_node_id = original_id,
      family_size = nrow(family_nodes),
      family_members = paste(family_nodes$name, collapse = "; "),
      connected_names = "No connections found",
      connection_details = "",
      number_of_connections = 0,
      stringsAsFactors = FALSE
    )
  }
}

# Combine all results
family_network_results <- bind_rows(all_family_networks)

# Display summary
cat("\n\n========================================\n")
cat("FINAL SUMMARY\n")
cat("========================================\n")
cat("Total officers processed:", nrow(officers_filtered), "\n")
cat("Officers with connections:", sum(family_network_results$number_of_connections > 0), "\n")
cat("Total unique connections:", sum(family_network_results$number_of_connections), "\n\n")

# Show top 10 officers by number of connections
cat("Top 10 officers by number of connections:\n")
family_network_results %>%
  arrange(desc(number_of_connections)) %>%
  select(original_name, family_size, number_of_connections) %>%
  head(10) %>%
  print()

# Save results
write.csv(family_network_results, 
          "family_network_connections.csv", 
          row.names = FALSE)

cat("\n\nResults saved to: family_network_connections.csv\n")

# View full results
View(family_network_results)










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



