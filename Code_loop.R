########################
# Upload the ICIJ data #
########################
library(data.table)
library(dplyr)
library(stringr)
library(stringdist)

intermediaries <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-intermediaries.csv") 
intermediaries <- select(intermediaries, -c(status, note, valid_until, sourceID))

entities <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-entities.csv")
entities <- select(entities, -c(status, sourceID, valid_until, note, service_provider, dorm_date))

# addresses <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-addresses.csv")
# addresses <- select(addresses, -c(sourceID, valid_until, note))

officers <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-officers.csv")
officers <- select(officers, -c(sourceID, valid_until, note))

# others <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/nodes-others.csv")
# others <- select(others, -c(sourceID, valid_until, note))

relationships <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Data input/ICIJ/relationships.csv") 
relationships <- select(relationships, -c(status, sourceID))

# DubaiDataAO <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiDataOA.csv")
# DubaiData <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiData.csv")

officers_names <- read.csv("C:/Users/wiedmann4/Documents/Aid and corruption/Out/ICIJmatches_officers_short.csv")

# --- Required helpers ---

entities_from_officers <- function(off_ids, rel_dt) {
  off_ids <- unique(as.character(off_ids))
  rel_dt[
    rel_type == "officer_of" & node_id_start %chin% off_ids,
    unique(node_id_end)
  ]
}

officers_from_entities <- function(ent_ids, rel_dt) {
  ent_ids <- unique(as.character(ent_ids))
  rel_dt[
    rel_type == "officer_of" & node_id_end %chin% ent_ids,
    unique(node_id_start)
  ]
}

only_officer_ids <- function(ids, officers_dt) {
  ids <- unique(as.character(ids))
  officers_dt[J(ids), nomatch = 0L, node_id]
}

only_entity_ids <- function(ids, entities_dt) {
  ids <- unique(as.character(ids))
  entities_dt[J(ids), nomatch = 0L, node_id]
}

entity_names_long <- function(entity_ids, entities_dt) {
  ent <- entities_dt[J(entity_ids), nomatch = 0L, .(node_id, name, original_name, former_name)]
  out <- melt(
    ent,
    id.vars = "node_id",
    measure.vars = c("name", "original_name", "former_name"),
    variable.name = "name_field",
    value.name = "connected_name",
    na.rm = TRUE
  )
  out[connected_name != "", .(connected_node_id = node_id, connected_name, name_field)]
}

# --- Main function ---

connected_names_iterative <- function(
    leaders_nodes,
    officers_dt, entities_dt, relationships_dt,
    include_seeds = TRUE,
    max_depth = Inf  # depth counted in "entity layers"
) {
  # basic coercions, leader nodes into dataframe, as character
  setDT(leaders_nodes)
  leaders_nodes[, leader_node_id := as.character(leader_node_id)]
  
  # ensure node ids are character and keyed
  officers_dt[, node_id := as.character(node_id)]
  entities_dt[, node_id := as.character(node_id)]
  relationships_dt[, `:=`(
    node_id_start = as.character(node_id_start),
    node_id_end   = as.character(node_id_end),
    rel_type      = as.character(rel_type)
  )]
  
  setkey(officers_dt, node_id)
  setkey(entities_dt, node_id)
  
  # create a sub-table for each leader_name of the data table leader_nodes
  out_list <- lapply(split(leaders_nodes, by = "leader_name", keep.by = TRUE), function(ld) {
    
    leader <- ld$leader_name[1]
    seed_off <- unique(ld$leader_node_id)
    
    # Track known sets, avoid "revisiting" nodes
    known_off <- unique(seed_off)
    known_ent <- character()
    
    # Frontier = officers discovered at previous depth, starting with seeds
    frontier_off <- known_off
    
    # Track discovery depth at NODE level
    discovered_nodes <- data.table(
      node_id = seed_off,
      node_type = "officer",
      first_depth = 0L
    )
    
    depth <- 1L
    while (length(frontier_off) > 0L && depth <= max_depth) {
      
      # 1) officers -> entities (depth = current entity-layer)
      ent_ids <- entities_from_officers(frontier_off, relationships_dt)
      ent_ids <- only_entity_ids(ent_ids, entities_dt)
      
      new_ent <- setdiff(ent_ids, known_ent)
      if (length(new_ent) == 0L) break
      
      known_ent <- c(known_ent, new_ent)
      
      # logs each new entity to the discovery table
      discovered_nodes <- rbind(
        discovered_nodes,
        data.table(node_id = new_ent, node_type = "entity", first_depth = as.integer(depth))
      )
      
      # 2) entities -> officers (same depth label; they are reached via "depth-th entity layer")
      off_ids <- officers_from_entities(new_ent, relationships_dt)
      off_ids <- only_officer_ids(off_ids, officers_dt)
      
      new_off <- setdiff(off_ids, known_off)
      if (length(new_off)) {
        known_off <- c(known_off, new_off)
        discovered_nodes <- rbind(
          discovered_nodes,
          data.table(node_id = new_off, node_type = "officer", first_depth = as.integer(depth))
        )
      }
      
      # next iteration expands from newly discovered officers, 1L digs one layer deeper
      frontier_off <- new_off
      depth <- depth + 1L
    }
    
    # Post-Loop Cleanup
    # If set, drop seeds from final officer list
    if (!include_seeds) {
      known_off <- setdiff(unique(known_off), seed_off)
    } else {
      known_off <- unique(known_off)
    }
    known_ent <- unique(known_ent)
    
    # ---- Attach depths to NAMES (by node_id) ----
    # Extracts two small lookup tables — one mapping officer node IDs to their discovery depth, one doing the same for entities.
    off_depths <- unique(discovered_nodes[node_type == "officer", .(node_id, first_depth)])
    ent_depths <- unique(discovered_nodes[node_type == "entity",  .(node_id, first_depth)])
    
    off_names_by_node <- officers_dt[J(known_off), nomatch = 0L,
                                     .(leader_name = leader,
                                       connected_type = "officer",
                                       connected_node_id = node_id,
                                       connected_name = name,
                                       countries, country_codes)
    ][off_depths, on = c(connected_node_id = "node_id")]
    
    ent_names_by_node <- entity_names_long(known_ent, entities_dt)[
      , .(leader_name = leader,
          connected_type = "entity",
          connected_node_id,
          connected_name,
          name_field)
    ][ent_depths, on = c(connected_node_id = "node_id")]
    
    names_by_node <- unique(
      rbindlist(list(off_names_by_node, ent_names_by_node), fill = TRUE),
      by = c("leader_name","connected_type","connected_node_id","connected_name")
    )
    
    # ---- Unique-name depth (min depth across all nodes that share the same name, e.g. if name appears at depth 2 and 4, it will only be reported at depth 2) ----
    # Results are sorted by leader, then depth, then type, then name alphabetically.
    unique_names <- names_by_node[
      , .(first_depth = min(first_depth, na.rm = TRUE)),
      by = .(leader_name, connected_type, connected_name)
    ][order(leader_name, first_depth, connected_type, connected_name)]
    
    list(
      names_by_node = names_by_node,
      unique_names  = unique_names,
      discovered_nodes = unique(discovered_nodes, by = c("node_id","node_type"))
    )
  })
  
  # Final Assembly
  list(
    names_by_node = rbindlist(lapply(out_list, `[[`, "names_by_node"), fill = TRUE),
    unique_names  = rbindlist(lapply(out_list, `[[`, "unique_names"),  fill = TRUE),
    per_leader    = out_list
  )
}

# Helper: strip non-alphabetical characters and extract LAST word (last name)
clean_last_name <- function(name) {
  name %>%
    str_replace_all("[^a-zA-Z ]", "") %>%
    str_trim() %>%
    str_squish() %>%
    str_extract("\\S+$") %>%        # take the last word = last name
    str_to_lower()
}

# Clean last names in both dfs
leader_names_clean <- officers_names %>%
  mutate(last_name_clean = clean_last_name(leader_name))

officers_clean <- officers %>%
  mutate(last_name_clean = clean_last_name(name))

# # For each leader_name, find all people in officers sharing a similar last name
# result <- leader_names_clean %>%
#   rowwise() %>%
#   reframe({
#     dists <- stringdist(last_name_clean, officers_clean$last_name_clean, method = "lv")
#     max_allowed <- pmin(2, floor(pmax(nchar(last_name_clean), nchar(officers_clean$last_name_clean)) * 0.2))
#     matched_idx <- which(dists <= max_allowed & officers_clean$name != leader_name)
#               # nchar(...) gets the character length of each name
#               # pmax(...) takes the longer of the two names being compared
#               # * 0.3 allows up to 30% of that length in edits
#               # floor(...) rounds down to a whole number
#               # pmin(3, ...) caps it at 3 — so the threshold is never more than 3 edits
#     if (length(matched_idx) == 0) {
#       tibble(original_name = leader_name, dependent = NA_character_, node_id = NA_character_)
#     } else {
#       matched_df <- officers_clean[matched_idx, ] %>%
#         distinct(name, node_id)
#       tibble(
#         leader_name = leader_name,
#         dependent     = matched_df$name,
#         node_id       = as.character(matched_df$node_id)
#       )
#     }
#   }) %>%
#   select(leader_name, dependent, node_id)

# Only exact last name matches
# For the current row's last_name_clean value, scans the entire last_name_clean 
# column in officers_clean and returns the row indices where there is an exact match.

result <- leader_names_clean %>%
  rowwise() %>%
  reframe({
    matched_idx <- which(officers_clean$last_name_clean == last_name_clean)
    
    if (length(matched_idx) == 0) {
      tibble(leader_name = leader_name, dependent = NA_character_, node_id = NA_character_)
    } else {
      matched_df <- officers_clean[matched_idx, ] %>%
        distinct(name, node_id)
      tibble(
        leader_name = leader_name,
        dependent   = matched_df$name,
        node_id     = as.character(matched_df$node_id)
      )
    }
  }) %>%
  select(leader_name, dependent, node_id)

result <- result[!duplicated(result$dependent), ]

#write.csv(result, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/Clans.csv")

# get the node_id for each original_name from the officers dataframe
self_rows <- leader_names_clean %>%
  left_join(officers %>% select(name, node_id), by = c("leader_name" = "name")) %>%
  transmute(
    leader_name = leader_name,
    dependent   = leader_name,
    node_id     = as.character(node_id)
  )

# bind to result
result <- bind_rows(result, self_rows) %>%
  arrange(leader_name)

selected_ids <- result %>%
  filter(!is.na(dependent), !is.na(node_id)) %>%
  distinct(dependent, node_id) %>%
  group_by(dependent) %>%
  summarise(node_ids = list(unique(node_id)), .groups = "drop") %>%
  { setNames(.$node_ids, .$dependent) }


### Run iterative
leaders_nodes <- result %>%
  filter(!is.na(node_id)) %>%
  distinct(leader_name, node_id) %>%
  rename(leader_node_id = node_id)

# max_depth = 1 means: seed officers -> 1st layer entities -> officers of those entities
res <- connected_names_iterative(
  leaders_nodes,
  officers, entities, relationships,
  max_depth = 1,
  include_seeds = TRUE
)


# Create csv files to download and search in Horizon/Sandcastles

# All node-level results
names_dt <- as.data.table(res$names_by_node)

# Unique names only (cleaner, less redundant)
unique_dt <- as.data.table(res$unique_names)

# Results for one specific leader
#unique_dt[leader_name == "Abdul Rahman"]

# How many connections per leader
#unique_dt[, .N, by = leader_name][order(-N)]

# How many entities per leader
#unique_dt[connected_type == "entity", .N, by = leader_name][order(-N)]

leaders <- read.csv("C:/Users/wiedmann4/Documents/Aid and corruption/Out/leaders.csv")
names(leaders)[names(leaders) == "LeadersName"] <- "leader_name"
setDT(leaders)

# join country onto unique_dt by leader_name
leader_country_lookup <- unique(leaders[, .(leader_name, LeadersCountry)])
unique_dt_country <- leader_country_lookup[unique_dt, on = "leader_name"]

unique_dt_country <- select(unique_dt_country, LeadersCountry, connected_name)

# split by country once, then write each chunk
splits <- split(unique_dt_country, by = "LeadersCountry", keep.by = TRUE)
lapply(names(splits), function(country) {
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", country)
  out <- splits[[country]][, .(name = sapply(connected_name, function(nm) {
    nm_clean <- gsub("[^a-zA-Z0-9 \u00C0-\u024F]", "", nm)# remove special characters, keep spaces
    words <- strsplit(nm_clean, " ")[[1]]
    words <- words[nchar(words) > 0]            # drop empty strings left by double spaces
    paste(paste0(words), collapse = " ")
  }))]
  fwrite(out, file = paste0("leader_exports/ICIJ_", safe_name, ".csv"))
})



all_leaders <- res[["unique_names"]]
per_leaderA <- res[["per_leader"]][["Ali Allawi"]][["unique_names"]]
per_leader2 <- res[["per_leader"]][["Abdul Rahman"]][["unique_names"]]


