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

# Change the letter in quotes to print names starting with that letter
letter <- "A"

cat("selected_ids <- list(\n")
nms <- names(selected_ids)[toupper(substr(names(selected_ids), 1, 1)) == toupper(letter)]
for (nm in nms) {
  ids <- paste0('"', selected_ids[[nm]], '"', collapse = ", ")
  cat(sprintf('  "%s" = c(%s),\n', nm, ids))
}
cat(")\n")

### Define leaders
#####
# A #
#####
selected_ids_A <- list("A'Hearn - Brian" = c("80027652"),
"AAL-ROYER, NELLY ANGELA CRUZ" = c("86003783"),
"AARON SIM KWANG LIANG" = c("12096874"),
"AASIM AZIM SIDDIQUI" = c("56071661"),
"ABASCAL MARIA MARGARITA SOSA" = c("110062674"),
"ABDALLAH KARIM" = c("110084088"),
"ABDEL I.G. IBRAHIM" = c("56034520"),
"ABDEL RAHMAN" = c("56012514"),
"ABDELLI SAAD" = c("12105762"),
"ABDOULA DEMBELE" = c("240130204"),
"ABDUL FATAH SHARIF" = c("56063866"),
"ABDUL GAFFAR IBRAHIM" = c("240040313", "240048990"),
"ABDUL JABBAR IBRAHIM" = c("56021974"),
"ABDULLAH AL MASRI" = c("56029194"),
"ABEL AUGUSTO GOMES DE SOUSA" = c("240041177"),
"ABOUZEID GEORGE AZIZ" = c("110107050"),
"ACILINO RAMIREZ MENDOZA" = c("240100299"),
"ACOSTA BERNARDO MARTINEZ" = c("110106331"),
"ADAM BROOKS" = c("56024449"),
"ADAM EBRAHIM" = c("110024306"),
"ADAM PETER CLARKE" = c("240381961", "240382010"),
"ADAM THEADORE YOUNG" = c("56008854"),
"ADAM THEODORE YOUNG" = c("56008855"),
"ADEEL KARAMAT MALIK" = c("56008588"),
"ADEL IBRAHIM" = c("56007730"),
"ADNAN ABDUL RAHMEN" = c("12190948"),
"ADNAN AHMED SIDIQUI" = c("240132100"),
"ADNAN NAIM IBRAHIM ABDUL KARIM" = c("12136339"),
"ADOLFO ALEXANDER MANAURE REYES" = c("240102921"),
"ADREA NICOLA SMITH" = c("12100640"),
"ADRIAN CLARKE" = c("56097207"),
"ADRIAN FRANCIS CLARKE" = c("56097206"),
"ADRIAN JOHNSON" = c("56007181"),
"ADRIAN PACE ROSS" = c("56039528"),
"ADRIAN VICTOR JOHNSON" = c("56048540"),
"ADRIAN WARD CLARKE" = c("56097546"),
"ADRIANO ROSSI" = c("13009252"),
"AFARA IBRAHIM" = c("240102859"),
"AGUDELO CORREA, GILMA DE LA CRUZ" = c("86005519"),
"AGUNBERO, MARION MALIK" = c("86023854"),
"AGUS SINDHU HARTANTO" = c("12157812"),
"AGYD PENGG" = c("56100681"),
"AHAMAD NAZIR" = c("110006179", "110092489"),
"AHMAD AZIZ" = c("56039365"),
"AHMAD FAUZI BIN ABDUL AZIZ" = c("12184667"),
"AHMAD J ABDULL RAHMAN" = c("56004604"),
"AHMED ABDUL KARIM" = c("56047165"),
"AHMED ANWAR KASAM" = c("12147936"),
"AHMED SALAH HASHIM IBRAHIM" = c("240052907"),
"AI DURK MING" = c("92742"),
"AI WEI (??)" = c("66526"),
"AI WEI CHEN" = c("12160645"),
"AIDA CAMPELO RIBEIRO" = c("240057169"),
"AIDA MANSOUR KARAM" = c("12121936"),
"AIHUA CHEN" = c("56094213"),
"AIRLANGGA HARTARTO" = c("13007923"),
"AIZHU CHEN" = c("56094268"),
"AKMAL BAHRI BIN BAKRI" = c("12102117"),
"AL BAZZAZ, SAAD" = c("86014769"),
"ALAIN CHUI YEW CHEONG" = c("56001467"),
"ALAN BABINGTON SMITH" = c("12101259"),
"ALAN CHARLES COCKBURN SMITH" = c("56011335", "56050695"),
"ALAN DAVID SMITH" = c("56063181"),
"ALAN FELIPE BRUDERER DELGADO" = c("240104541", "240041736", "240057748", "240048771", "240041997"),
"ALAN FRASER ROSS" = c("56057017"),
"ALAN GORDON SMITH" = c("12122741"),
"ALAN HILTON  DE LA HUNT" = c("12097852"),
"ALAN JAMES MICHAEL YOUNG" = c("56047845"),
"ALAN JOSEPH SMITH" = c("12118840"),
"ALAO IDRIS" = c("240040418"),
"ALARIC SMITH" = c("56044625"),
"ALBERNAZ NETO PAULO MANGABEIRA" = c("22005887"),
"ALBERT BRIAN ROSSO" = c("56030918"),
"ALBERT ROSSI" = c("56068665"),
"ALBERT, TAI-HANG WEN" = c("12103455"),
"ALBERTINA MARIA FERNANDES DE SOUSA" = c("240051058", "240051072", "240051073"),
"ALBERTO JESUS PEREZ MARTINEZ" = c("12049798"),
"ALBERTO MAGRI" = c("56028855"),
"ALBERTO ROSSI" = c("56050700"),
"ALBERTSZ-TOLENTINO MORLA, CARMEN VICENTA" = c("86026871"),
"ALBRIGHT, CHARLES WALTER" = c("86002906"),
"ALBUQUERQUE LUIZ CARLOS DA SILVA" = c("22004848"),
"ALCIDES BAPTISTA DA CRUZ" = c("56099243"),
"ALDA MARIA MARQUES PEDRALVA DELGADO" = c("56098365"),
"ALDO PESSAGNO NETO" = c("12048708"),
"ALDO URBINATI NETO" = c("240051628"),
"ALEEM SHARIFF" = c("240101975"),
"ALEJANDRO ANDRES & MELISSA GAYE SANTA CRUZ" = c("56008156"),
"ALEKSANDER KAGAN" = c("56061318"),
"ALEKSANDR BARANOV" = c("240131777"),
"ALEKSANDR POPKO" = c("56096846"),
"ALEKSEI POPOV" = c("12099893"),
"ALEKSEY MENG" = c("240131632"),
"ALEONG ALBERT JOHNSON" = c("110093591"),
"ALESSANDRO BORTMAN DE ALBUQUERQUE SILVA" = c("240051520"),
"ALESSANDRO ROSSI" = c("56036445"),
"ALESSANDRO ROSSO" = c("56105422"),
"ALEX FELICE" = c("56063754", "56063454"),
"ALEX IVAN HERNANDEZ NIETO" = c("12070907"),
"ALEX MAGRI" = c("56025655"),
"ALEX NAMIR" = c("56094683"),
"ALEX VAHMAN" = c("34971"),
"ALEX WALTER" = c("12155405"),
"ALEX WONG CHING" = c("12161955"),
"ALEX WONG CHING PING" = c("12161938", "13003766", "13001523"),
"ALEXANDEL CHANG" = c("66002516"),
"ALEXANDER BRIAN" = c("110037200", "110069208", "110119884"),
"ALEXANDER CHANG" = c("66002526"),
"ALEXANDER CHEN" = c("12100638"),
"ALEXANDER JESUS PAOLINI MARTINEZ" = c("12049796"),
"ALEXANDER NAUMOV" = c("240047483"),
"ALEXANDRA MAULI" = c("66002568"),
"ALEXANDRE LAMBERT DE BEAULIEU" = c("12189461", "12196269"),
"ALEXANDRU-COSMIN WALTER" = c("240382261"),
"ALEXEY BARANOV" = c("12137719"),
"ALEXIA ROSSI" = c("56021583", "56051141"),
"ALEYAMMA JOHNSON" = c("240131274"),
"ALFIO MAGRI" = c("56073375"),
"ALFONSO MARÍA ALGORTA YOUNG" = c("12197065"),
"ALFRED HUNT" = c("66001832"),
"ALFRED JOHN CLARK" = c("56060992"),
"ALFRED LEE MING CHEONG" = c("12121490"),
"ALFRED MAGRI" = c("56032897"),
"ALFRED WALKER" = c("56105110"),
"ALFREDO CARARSA NETTO" = c("12116676", "13012701"),
"ALFREDO CASARSA NETTO" = c("12177181", "12197672", "13008923"),
"ALFREDO CHANG" = c("12108369"),
"ALFREDO NICOLAS ABUGATTAS DELGADO" = c("12150089", "13008513"),
"ALFREDO SALVA'" = c("56074126"),
"ALI - Mahommed Bin" = c("80028494"),
"ALI H A EL SHARIF" = c("56032567"),
"ALI IBRAHIM" = c("56011709"),
"ALI ISMAIL BIN" = c("56006695"),
"ALICE CHEN JING" = c("12112891"),
"ALICE YAP KOOK CHENG" = c("12132987", "13003292"),
"ALICIA BASTO NIETO" = c("12069990"),
"ALICIA MATILDE REIMUNDO MARTINEZ" = c("240044968"),
"ALIE HUSSEIN Saad" = c("113965"),
"ALIREZA ALAEE RAHMANI" = c("56093349"),
"ALISTAIR CROSS" = c("56047034"),
"ALIYA ALPIYEVA" = c("56033080"),
"ALLAMBY GRAHAM DE LA TOUR" = c("110032441"),
"ALLAMBY REX DE LA TOUR" = c("110012360"),
"ALLAN CANTILLO MENDOZA" = c("12038908"),
"ALLAN DAVID WALTER" = c("110078027", "110120793", "110121214"),
"ALLAN RONALD WALKER" = c("56008771"),
"ALLAN YONG HENG CHONG" = c("76052"),
"ALLEYNE DAVID ANDREW CLARK" = c("110013591", "110048234", "110102222"),
"ALLEYNE LISA ADRIAN CLARK" = c("110034934"),
"ALLEYNE MICHAEL SMITH" = c("110030070", "110030292", "110076179", "110085595", "110054038"),
"ALLEYNE PETER JEREMY CLARK" = c("110103280"),
"ALLEYNE STEPHEN MARK CLARK" = c("110069011", "110126052"),
"ALLEYNE* DAVID ANDREW CLARK" = c("110092282"),
"ALLISON, DANNY BRIAN" = c("86012373"),
"ALMARY, ERIC MARTINES" = c("86012146"),
"ALMIR GHIARONI DE ALBUQUERQUE SILVA" = c("13000510"),
"ALMIR JOSE DA SILVA" = c("12046983"),
"ALTAGRACIA BEATRIZ ACOSTA Y/O LORAINE CRUZ" = c("12068164"),
"ALVAREZ JUAN JOSE DELGADO" = c("110083527"),
"ALVAREZ RUIZ-CHARRIS HURTADO, JUANA IRIS" = c("86018308"),
"ALVARO LOPES DA SILVA" = c("240050979"),
"ALVARO PAIPILLA MARTINEZ" = c("12154518"),
"AMAL NASR" = c("56018532"),
"AMANDA MARIA SANZ MARTINEZ" = c("56062514"),
"AMARAL WALTER" = c("110100292"),
"AMARO MAURO LUIZ DA SILVA" = c("110003636", "110029003"),
"AMID, FARID" = c("12096208"),
"AMIR ALIEV" = c("12139900"),
"AMIR HAMZAH BIN MD SHARIF" = c("12067059"),
"AMIR UALIYEV" = c("12054968"),
"AMIR-ALI AMIRI" = c("56094933"),
"AMMARI BRAHIM" = c("56003368"),
"AMOURY IRIS" = c("22016202"),
"AMPARO ARANGO DE CHENG" = c("12072282"),
"AMR AHMED ABDELAZIZ IBRAHIM" = c("240104376"),
"AN BAI QIANG" = c("12208175"),
"AN ChENG" = c("78203"),
"AN PING" = c("12168239"),
"AN XIANG" = c("12190352"),
"AN YIMIN ???" = c("79584"),
"AN ZHI QIANG" = c("12185595"),
"ANA BELEN CRESPO MARTINEZ" = c("56105299"),
"ANA ISABEL LOOR MARTINEZ" = c("240103280"),
"ANA ISABEL PEREZ LARA DE CRUZ" = c("12097498"),
"ANA LUCIA HOLGUIN ZAMBRANO" = c("240103300", "240103303", "240050064"),
"ANA MARIA DOS SANTOS BRANCO VARANDAS DE SOUSA" = c("56097450"),
"ANA MARIA MANGUERRA" = c("12095781", "12095782"),
"ANA SINISTERRA REYES" = c("12170032"),
"ANAD ALIYEV" = c("12122083"),
"ANANYAN ARAM" = c("240131917"),
"ANAS MOHAMMED RAYES" = c("240101656"),
"ANATOLIY ATROSHENKO" = c("240380032"),
"ANAYA DE LA HOZ, MARIA DE LA CRUZ" = c("86010390"),
"ANCELL JAMES ROSS" = c("110085669"),
"ANDERS JONSON" = c("56057044"),
"ANDERS NORMAN JOHNSON" = c("56046847"),
"ANDERSON MARTORANO AUGUSTO RIBEIRO" = c("240046176"),
"ANDRADE, LILIANA FORTES DA SILVA" = c("86007636"),
"ANDRE MAGRI" = c("56045018"),
"ANDREA GARRONI" = c("56006879", "56035021"),
"ANDREA LILIANA VAN REES" = c("240042240"),
"ANDREA NICOLA SMITH" = c("12100694"),
"ANDREA ROSSO" = c("56071082"),
"ANDREAS WALKER" = c("56090361"),
"ANDREI NAUMOV" = c("240101942"),
"ANDREI POPOV" = c("12025136"),
"ANDREIA PATRICIA BERBERAN FRAGOSO DOS SANTOS SILVA" = c("56014922"),
"ANDRELINA MARIA DOS RAMOS SILVA" = c("12171946"),
"ANDRES ALEJANDRO FUENTES MARTINEZ" = c("240101534", "240101579", "240051130", "240051264"),
"ANDRES BENZAZON SILVA" = c("240053361"),
"ANDRES CHEN" = c("12069764"),
"ANDRES DELGADO" = c("12021635"),
"ANDRES GOMEZ REYES" = c("12069974"),
"ANDRES MARTINEZ" = c("12030742"),
"ANDRES REINALDO ROSSI" = c("240045450", "240049152"),
"ANDREW BROOKE" = c("56012321"),
"ANDREW C CROSS" = c("56009033", "56035506"),
"ANDREW CHRISTOPHER CLARK" = c("56048257"),
"ANDREW COLIN CROSS" = c("56009032"),
"ANDREW DOUGLAS HUNT" = c("56051977"),
"ANDREW GUY SMITH" = c("56060951"),
"ANDREW IAN SMITH" = c("56064465"),
"ANDREW JOHN JOHNSON" = c("56060471"),
"ANDREW KAIYUAN CHEN" = c("10410"),
"ANDREW KEITH ANTHONY SMITH" = c("120768"),
"ANDREW MAGRI" = c("56041866", "56049053"),
"ANDREW MICHAEL SMITH" = c("56069991"),
"ANDREW ROSS" = c("66002881"),
"ANDREW SHARMA" = c("12027660"),
"ANDREW STEPHEN GRAY SMITH" = c("13012492"),
"ANDREW THOMAS WALKER" = c("12068204"),
"ANDREY POPOV" = c("240055074"),
"ANDREY VIKTOROVICH POPOV" = c("240043477"),
"ANDRII POPOV" = c("240053612", "240053887"),
"ANDRÉS ANTONIO CANELA ABEL DE LA CRUZ" = c("12154795", "13000174"),
"ANDY CHEN" = c("73081"),
"ANG  LONG  PHENG" = c("12145719"),
"ANG - Kiam Meng" = c("80030047"),
"ANG BOON PENG" = c("58521"),
"ANG HUI KIANG" = c("82266"),
"ANG KA BO" = c("98283"),
"ANGEL AVILA DE LA CRUZ" = c("12076500"),
"ANGEL RAFAEL RAIMAN" = c("13005194"),
"ANGEL SILVA" = c("66001314"),
"ANGELA WEN" = c("12095567"),
"ANGELO ALBERTO MAURI" = c("56093627"),
"ANGELO GIACOMINI NETO" = c("12175930"),
"ANGELO RAFFAELE MAGLI" = c("56071480"),
"ANGUS P.T. CHEN" = c("48666"),
"ANITA YOUNG" = c("66000680"),
"ANNA AIOLUPOTEA SILVA" = c("66002953"),
"ANNA CLAUDIA ROSSI" = c("56106036"),
"ANNA FELICE" = c("56047263"),
"ANNA MARIA FELICE" = c("56021056"),
"ANNA MARIA ROSSO" = c("56049741"),
"ANNA ROSSI" = c("56049860"),
"ANNE ANDREASEN FELICE" = c("56032321"),
"ANNETTE CHANG" = c("66001948"),
"ANNIE C. CHEN" = c("87527"),
"ANNY CHANG" = c("78756"),
"ANO LEO SE CHANG" = c("110033077"),
"ANTHONY BROOKES" = c("56060608"),
"ANTHONY FELICE" = c("56030417"),
"ANTHONY FOBEL" = c("56049467"),
"ANTHONY FRANCIS ALLES" = c("240380179"),
"ANTHONY JOSEPH SMYTH" = c("56101409"),
"ANTHONY JOSEPH WILLIAM BROOKES" = c("12098377"),
"ANTHONY LOWEN CLARKE" = c("56097529"),
"ANTHONY MAGRI" = c("56043387"),
"ANTHONY ROSSI" = c("56033829"),
"ANTO RADMAN" = c("56010226"),
"ANTON CORNEL WEBB" = c("12104611", "12105720"),
"ANTON MAGRI" = c("56033683"),
"ANTONELLO MARTINEZ" = c("56105552"),
"ANTONINA LAGUN" = c("240056243", "240041625", "240057861", "240053785", "240053869", "240054220", "240053983"),
"ANTONIO   RODRIGUES CARNEIRO NETO" = c("240440200"),
"ANTONIO ALEXANDER RODRIGUEZ MARTINEZ" = c("240104107"),
"ANTONIO AUGUSTO DE MESQUITA NETO" = c("56094022"),
"ANTONIO CARLOS PIRES DA SILVA" = c("12212693"),
"ANTONIO ESPINOZA CRUZ" = c("12036972"),
"ANTONIO GIL DELGADO" = c("56077612"),
"ANTONIO GOMES NETTO" = c("240054737"),
"ANTONIO HECTOR SOFIA TRONCOSO" = c("12219640"),
"ANTONIO JORGE AUGUSTO RODRIGUES DA SILVA" = c("240045912"),
"ANTONIO JOSE YAMMINE SAAD" = c("12088214"),
"ANTONIO JUAN BAUTISTA VIERCI MENDOZA" = c("12139506"),
"ANTONIO MIGUEL TELES DA SILVA" = c("56098687"),
"ANTONIO PEDRO MENDES RIBEIRO" = c("240045768"),
"ANTONIO PRUDENCIO DE SOUSA" = c("240040588"),
"ANTONIO RIERA MARI" = c("56070666"),
"ANTONIO RODRIGUES CARNEIRO NETO" = c("13005446"),
"ANTONIO, JOSE CARLO REYES" = c("240300026"),
"ANUJ SHARMA" = c("240101952"),
"ARDIANI KARTIKA SARI SUBIANTO" = c("89846"),
"ARDILA VISCONTI, JUAN VICENTE" = c("86002745"),
"ARENDS, FARIDA" = c("86012400"),
"ARENDS, WALTER" = c("86006766"),
"ARENDSZ, RANDOLPH WALTER" = c("86006210"),
"ARIANE ROSSI" = c("56016712"),
"ARIF AZIZ" = c("12113863"),
"ARINI SARASWATY SUBIANTO" = c("97409"),
"ARMANDO BRAGA RODRIGUES PIRES NETO" = c("12018697"),
"ARMANDO D' ALMEIDA NETO" = c("240101990"),
"ARMANDO MARCONDES MACHADO NETO" = c("12213559"),
"ARMANDO MOURA COSTA NETO" = c("240381969", "240381970"),
"ARMANDO ROSSI" = c("56062268"),
"ARMSTRONG FRANCES CLARE" = c("110049842", "110054907"),
"ARNAUD ROBERT ROSSI" = c("240047211", "240047384"),
"ARNOLD NICKLAUS D' CRUZ" = c("240380935"),
"ARTHUR DO NASCIMENTO SILVA" = c("240382301"),
"ARTHUR MAGRI" = c("56045896"),
"ARTHUR POPOV" = c("56029461", "56008995"),
"ARZU ILHAM QIZI ALIYEVA" = c("12119991"),
"ASAMI KANAE" = c("240221444", "240221518"),
"ASHANT BALWANT SHARMA" = c("12137788"),
"ASHBY BRIAN" = c("110009136"),
"ASHFORD BRIAN" = c("110039359"),
"ASHIBANI ASADIG SAAD" = c("56068215"),
"ASHIT BLAWANT SHARMA" = c("12190472"),
"ASHLEY GRANT WALKER" = c("56070017"),
"ASIA ABDUL AZIZ" = c("12014925"),
"ASIASTAR I.T. FUND L.P. (Represented by Tan Chuu Ming)" = c("70397"),
"ASIASTAR I.T. FUND L.P. (Represented by Tony Shao Liang)" = c("51410"),
"ASIASTAR I.T. FUND L.P.(Represented by Tan Chuu Ming)" = c("109109"),
"ASIF KARIM" = c("12075631", "56009098"),
"ASIM SIDDIQUI" = c("12222567"),
"ASOR AMIR" = c("110026760"),
"ASSER AHMED IBRAHIM" = c("56028676"),
"ATENCIO VELASQUEZ, RAFAEL VINCENTE" = c("86030165"),
"ATHENE MAULI" = c("66002571"),
"ATHIGAPANICH - Anan" = c("80031594"),
"ATTILA SZABO" = c("56107077"),
"AU - Malcolm Man Chung" = c("80031830"),
"AU CHING" = c("12034697"),
"AU PING YUN" = c("12211021", "12212132", "12224621"),
"AU Shuk Kwan, Clara" = c("94053"),
"AU WAI MING" = c("12149682"),
"AU WONG KIT MING" = c("13007693"),
"AU YAT CHING" = c("59571"),
"AU YEUNG HING CHEONG" = c("12163291"),
"AU YEUNG Wing Chung" = c("80257"),
"AU YEUNG YU CHING" = c("12203435", "12203518"),
"AU Yin Ping" = c("84526"),
"AU Yuk Ping (???)" = c("60030"),
"AU, DIANNA PUI CHING" = c("12043879"),
"AU, Shirley Yin Ming" = c("82486"),
"AU, TAI CHUNG" = c("26841"),
"AU, Yuen Ming" = c("87001"),
"AUGUSTINE JOSEPH WALKER" = c("56046201"),
"AURELIA SEMPERENA DELGADO" = c("12169913"),
"AURELIA SUPARDI" = c("79771"),
"AUSTIN J WALKER" = c("56050314"),
"AUSTIN SU CHI-SHIANG" = c("12111164"),
"AVAMARIA HUNT" = c("66003236"),
"AWAD ABDELRAHMAN AWAD IDRIS" = c("56089514"),
"AYAD ALLAWI" = c("12048878"),
"AYAD H. ALLAWI" = c("12179775"),
"AYOUCH SAAD" = c("110010383"),
"AZIZ ALIYEV" = c("12131380"),
"Abbas - Mohamed Yunus Ramli Bin" = c("80026680"),
"Abdallah Suliman Abdallah Ben Naser" = c("56099633"),
"Abdelhakim Amer A. ALLAFI" = c("56102464"),
"Abdelkawi - Ahmed Samir" = c("80026716"),
"Abdul Rahman" = c("54286"),
"Abdulaziz - Khaled Bin Sultan Bin" = c("80026725"),
"Abdulghani - Naser" = c("80026732"),
"Abdulhakim Mohammed Allawy" = c("3502"),
"Abdulziz - Bandar bin Sultan bin" = c("80026726"),
"Abhay Dev Sharma" = c("78"),
"Abhyankar - Samir" = c("80026788"),
"Aboubakare - Nasar" = c("80026848"),
"Abram David Smith" = c("115885"),
"Abuhamad - Karim" = c("80039529"),
"Aburizal Bakrie" = c("60323"),
"Acton - Brian" = c("80027132"),
"Adam - Brian" = c("80027165"),
"Adam Aliyev" = c("26342"),
"Adam B. Aliyev" = c("4688"),
"Adam Edward Ross" = c("62157"),
"Adam John Clark" = c("12109410", "12110525", "12110905"),
"Adama Toungara" = c("240552236"),
"Adarsh Kumar Sharma" = c("12096148"),
"Adcock - Brian" = c("80027248"),
"Adel Mahmoud Mohamed Ali Farid" = c("33049"),
"Adela Mendoza" = c("12195247"),
"Adeline Christopher Sousa" = c("64966"),
"Adeline Fong Mun Ping" = c("40237"),
"Adham - Kamal Ibrahim" = c("80027298"),
"Adi Sasono" = c("114918"),
"Adnan Haroon Siddiqui" = c("12156190", "12208831", "12222548", "12222563"),
"Adnani - Seyed Amir" = c("80027336"),
"Adolfo II Z. Reyes" = c("75138"),
"Adolfo Utor Martínez" = c("29000009"),
"Adrian John SANK (PREVIOUS BO)" = c("13004569"),
"Adriane Lafala Clarke" = c("61602"),
"Aga Khan - Karim" = c("80027530"),
"Ahmad - Nadir" = c("80027676"),
"Ahmad Riza Bin Basir" = c("80731"),
"Ahmadur Rahman" = c("14567"),
"Ahmed Abdullah M Al Malik" = c("240513879"),
"Ai-Lin Chang" = c("45594"),
"Aik Kwo Liang" = c("87610"),
"Aileen Josephine Mabasa Cheng" = c("95472"),
"Airlangga Hartarto" = c("240552641", "240552513"),
"Ait-Laoussine - Nadir" = c("80027847"),
"Aivaras Abromavicius" = c("240552048", "240552126"),
"Akmal Bahri Bin Bakri" = c("12101577", "12101791", "12102216"),
"Al Hokair - Fawaz Abdul Aziz" = c("80028459"),
"Al Ibrahim - Princess Al Jawhara Ibrahim" = c("80028481"),
"Al Nemer - Ibrahim" = c("80028921"),
"Al Rashid - Fahad Nasser Ibrahim" = c("80029014"),
"Al Tourki - Lina Abdul Rahman Ibrahim" = c("80029212"),
"Al-Fardan - Hussain Ibrahim" = c("80028351"),
"Al-Ghamdi - Dr Saad" = c("80028392"),
"Al-Khaldi - Ibrahim" = c("80028580"),
"Al-Rashid - Nasser Ibrahim" = c("80029016"),
"Al-Shihabi - Ali Samir" = c("80029106"),
"Al-Suwaidi - Jamal Ali Abdulla Sanad" = c("80029122"),
"Aladwani - Ibrahim" = c("80027939"),
"Alan Chung-ran YOUNG" = c("240303850"),
"Alan Clark" = c("114067"),
"Alan Mason Chenn" = c("103422"),
"Alan Robertson Clark" = c("86364"),
"Albert Chen" = c("42596"),
"Albert Cheng Hok Ming" = c("63066"),
"Albert Chia-Sheng Wei" = c("61739"),
"Albert Saadi" = c("12145907"),
"Albert Sugianto" = c("94683"),
"Albert Tai-Hang WEN" = c("12101271"),
"Albert, Au Siu Cheung                   " = c("44825"),
"Alberto Dahik Garzozi" = c("240552103", "240552112", "240552155", "240552173"),
"Albrahim - Jaber Saad" = c("80037049"),
"Alec MACRI" = c("240470099"),
"Aleem SHARIFF" = c("240470191"),
"Alejandro Jose SAYEGH MENDOZA" = c("240472138", "240472139"),
"Alejandro Young" = c("12194530"),
"Alesandro Dessimoni Vicente" = c("12089524"),
"Alessandro Dessioni Vicente" = c("12089696"),
"Alex Chen" = c("108357"),
"Alex Chung" = c("62332"),
"Alex Fang" = c("43607"),
"Alexander Ales" = c("9275"),
"Alexander Charles Lark" = c("112507"),
"Alexander Le" = c("57017"),
"Alexander NAUMOV" = c("26142"),
"Alexander Popov" = c("12213708", "12680"),
"Alexandre Le" = c("120142"),
"Alexei V. Popov" = c("6443"),
"Alfred Pingshan Chang" = c("101456"),
"Alfredo Casarsa Netto" = c("21487", "21488"),
"Ali - Jawad Nasir" = c("80028471"),
"Ali Allawi" = c("240552063", "240552128", "240552217"),
"Ali Ardalan" = c("95515"),
"Ali Bin Mohd Shariff" = c("15140", "15168"),
"Ali Golam Ardalan" = c("107293"),
"Ali Hammoud" = c("13006007", "13007659"),
"Ali Jehangir Siddiqui" = c("240552589", "240552590", "240552591", "240552592", "240552593", "240552594", "240552648"),
"Ali Otman Abdulgader Hammuda" = c("56011013"),
"Ali Saad" = c("12110047"),
"Ali Siddiqui" = c("12154906"),
"Alice CHEN" = c("240300412"),
"Alice Chang" = c("98046"),
"Alicia M. Martinez" = c("51047"),
"Alicia Shou-Jen Chen" = c("43891"),
"Alireza - Amira" = c("80028510"),
"Alireza - Zainal Ibrahim" = c("80028537"),
"Alkan KENAN" = c("240477406", "240477407"),
"Allan Gng Koon Liang" = c("70260"),
"Allan Nurichsan Rachman" = c("12167674"),
"Allana - Abdul Rehman" = c("80028613"),
"Allen Cheng" = c("91808"),
"Alotaibi - Ibrahim" = c("80028943"),
"Alternate Director to Ms Wong Ping" = c("28155"),
"Alvarado-de-Cordoba - Silvia" = c("80029258"),
"Alyasaa Younes KH Ibrahim" = c("56043919"),
"Aman Ur Rahman" = c("37724"),
"Amanda CHEN" = c("88960"),
"Amber Rudd" = c("29000078"),
"Amer Ishaq Malik" = c("240510686"),
"Amir Aliev" = c("13002420"),
"Amir Nasr" = c("12174378"),
"An Wei" = c("66080"),
"An, Xiao Jing" = c("9307"),
"Ana María de Sousa" = c("12199797"),
"Andrade - Miguel Kenehele de Sousa" = c("80029909", "80029910"),
"Andreas Young" = c("12219876"),
"Andrei Vladimirovich Popov" = c("12171280"),
"Andres Reyes" = c("96789"),
"Andrew Cheng" = c("116611"),
"Andrew D R Mendoza" = c("12128814"),
"Andrew James SMITH" = c("240474094", "240474095"),
"Andrew MA Chiu Cheung" = c("12118002", "13005281"),
"Andrew Malcolm HUNT" = c("56097979"),
"Andrew Noel Hunt" = c("115144"),
"Andrew Paul SMITH" = c("240478057", "240478058"),
"Andrew Samuel YOUNG" = c("240477961", "240477962"),
"Andrew Stephen HUNT" = c("240472998"),
"Andrey Baranov" = c("240513321"),
"Andrey Naumov" = c("4368"),
"Andrey Vladimir Yudin" = c("13006326"),
"Andrey Yudin" = c("12130000"),
"Andy Farouk Muhamad NASIM" = c("240302788"),
"Ang Lian Ping" = c("70596"),
"Ang Liong Peng" = c("54380"),
"Ang Sock Cheng" = c("40236"),
"Angel Chen" = c("64708"),
"Angela Chan Ha Ching" = c("111158"),
"Angela Chee Wen Ching Ching" = c("97969"),
"Angela Chen" = c("13355"),
"Angela Michelle Dixon-Lampitt and John James Smith" = c("80030007"),
"Angela Yuen Kwan Cheung" = c("47068"),
"Angelina Cecelia De Silva" = c("99421"),
"Angeline Cecelia De Silva" = c("85272"),
"Angeline Ng Peng Peng" = c("93337"),
"Angelo GENOESE NETO" = c("240476500"),
"Anh Le" = c("62900"),
"Anh-Thu Le" = c("37565"),
"Anh-Tu Le" = c("121623"),
"Anil De Silva" = c("69119"),
"Anindya N. Bakrie" = c("46267"),
"Anita Chow Yin Ping" = c("72921"),
"Ann Alicia Chenn" = c("120850"),
"Ann CHEN" = c("88158"),
"Ann Wam Tiang" = c("80174"),
"Anna Louise Johnson" = c("12096348"),
"Anne Bi-Lien Chen" = c("59967"),
"Anne Ye-Teh Hui Cheung" = c("63005"),
"Annie Wu Suk Ching" = c("117458"),
"Anthony Brookes" = c("12151666"),
"Anthony Charles John Lark" = c("59946"),
"Anthony Joseph William BROOKES" = c("12151664"),
"Anthony Liang" = c("44496"),
"Anthony Smith" = c("12104577"),
"Antoinette Smith" = c("93120"),
"Anton Cornel WEBB" = c("4549"),
"Antonio Jorge Augusto Rodrigues Da Silva" = c("12184069"),
"Antonio Marcus da Silva" = c("47185"),
"António Manuel Pinheiro Espírito Santo Silva" = c("12156315"),
"Anuar Abd. Aziz" = c("96064"),
"Anuar Bin Abd Aziz" = c("95890"),
"Anwar Bin Ibrahim" = c("10305"),
"Aqin QIAN" = c("240303043"),
"Aquiles Delgado" = c("12152708"),
"Araujo - Michael Brian" = c("80030582"),
"Arav - Amir" = c("80030584"),
"Armeilia Widayanti Subianto" = c("54046"),
"Arthur Chen" = c("93622"),
"Arthur Tugade" = c("240552637"),
"Artur Popkov" = c("41306"),
"Arzu Aliyeva" = c("15005003", "78392"),
"Arzu Ilham Qizi Aliyeva" = c("13007239"),
"Ashland - Brooke" = c("80031214"),
"Ashley Ching Sze Ming" = c("59770"),
"Ashley Smith" = c("121906"),
"Asim Ghayoor Siddiqui" = c("12178531", "12221959"),
"Assaubayev - Kanat" = c("80031396"),
"Assumpta Lan-Ruen Kiang" = c("83236"),
"Astrid Agudelo de De Sousa" = c("12120444"),
"Atag, Ernst & Young" = c("80031504"),
"Atkinson - Brian" = c("80031616"),
"Au - Man Ching" = c("80031827"),
"Au Mei Yi" = c("46069"),
"Au Sang Cheong" = c("58440"),
"Au TAk Cheong" = c("46012"),
"Au Wai Ming" = c("47380"),
"Au Yuk Ping" = c("67745"),
"Au kwok Ming" = c("60747"),
"Au, Lai Ping" = c("108272"),
"Augustine Gng (alias Gng Koon Hiang)" = c("116945"),
"Aung Kyaw Myint" = c("113516"),
"Austin - Brian" = c("80041113"),
"Austin Cheng" = c("91672"),
"Auw Gim Beng" = c("56129"),
"Avva - Venkata Subrahmanyeswawa Sarma" = c("80032051"),
"Ayers - Andrea Johnson" = c("80032113"),
"Aymin Mohamed  Matri" = c("56061348"),
"Azfar Bazli Bin Bakri" = c("12101634"),
"Azizah bt. Idris" = c("62937"),
"Azizan - Amir Hamzah Bin" = c("80032167"),
"Azlina Binti Abdul Aziz" = c("48494"),
"Azraq - Naram" = c("80032187"),
"al Rubaishi - Ali lbrahim" = c("80029028"),
"ames Frederick Smith" = c("12112464"),
"andrew kaiyuan chen" = c("10409"))

#####
# B #
#####


# selected_ids <- list(
#   "Ali Allawi"  = c("240552063", "240552128", "240552217"),
#   "Abdul Rahman" = c("54286"),
#   "Aburizal Bakrie" = c("60323")
# )

leaders_nodes <- rbindlist(lapply(names(selected_ids_A), function(nm) {
  data.table(leader_name = nm, leader_node_id = selected_ids_A[[nm]])
}))


### Run iterative
# max_depth = 1 means: seed officers -> 1st layer entities -> officers of those entities
res <- connected_names_iterative(
  leaders_nodes,
  officers, entities, relationships,
  max_depth = 1,
  include_seeds = TRUE
)

letter <- "A"

filtered_results <- leaders_nodes[
  toupper(substr(leader_name, 1, 1)) == toupper(letter)
] %>%
  left_join(
    result %>% distinct(original_name, dependent),
    by = c("leader_name" = "dependent")
  ) %>%
  select(leader_name, leader_node_id, original_name)

all_leaders <- res[["unique_names"]]
per_leaderA <- res[["per_leader"]][["Ali Allawi"]][["unique_names"]]
per_leader2 <- res[["per_leader"]][["Abdul Rahman"]][["unique_names"]]


