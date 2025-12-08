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

###################################################################
# Using the Paths to Power list to identify the political leaders #
###################################################################

leaders <- readRDS("~/Aid and corruption/Data input/PathstoPower_individuallevel_v1.0.rds")

# omitting NAs in column "name"
em_leaders <- leaders %>% drop_na(name)

#omitting developed countries, keeping only emerging markets
dc = c("Australia", "Austria", "Bahrain", "Belgium", "Canada", "Cyprus", "Czechia", "Denmark", "East Germany", "Estonia", "Finland", "France", "Germany", "Greece", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Kuwait", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "New Zealand", "Norway", "Portugal", "Saudi Arabia", "Singapore", "Slovakia", "Slovenia", "South Korea", "Soviet Union", "Spain", "Sweden", "Switzerland", "Taiwan", "Unted Kingdom", "United States")
leaders <- em_leaders %>%
  filter(!country_name %in% dc)

# Count how many positions each person held in each year
position_counts <- leaders %>%
  group_by(name, country_name, year) %>%
  summarise(num_positions = n(), .groups = "drop") %>%
  arrange(desc(num_positions))

# Get the maximum number of simultaneous positions
max_positions <- max(position_counts$num_positions)
print(paste("Maximum positions held at the same time:", max_positions))

# First, filter and prioritize positions
leaders_filtered <- leaders %>%
  group_by(name, country_name, year) %>%
  mutate(
    # Create priority order: high=1, medium=2, low=3, other=4
    prestige_priority = case_when(
      prestige_1 == "high" ~ 1,
      prestige_1 == "medium" ~ 2,
      prestige_1 == "low" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  arrange(name, country_name, year, prestige_priority) %>%
  # Keep only top 5 positions per person-year
  slice_head(n = 5) %>%
  ungroup() %>%
  select(-prestige_priority)  # Remove the helper column

# Count how many positions each person held in each year
position_counts2 <- leaders_filtered %>%
  group_by(name, country_name, year) %>%
  summarise(num_positions = n(), .groups = "drop") %>%
  arrange(desc(num_positions))

# Get the maximum number of simultaneous positions
max_positions <- max(position_counts2$num_positions)
print(paste("Maximum positions held at the same time:", max_positions))

# Get the constant columns
constant_cols <- setdiff(names(leaders_filtered), c("position", "prestige_1"))

# For each person-year, take the first value of the constant columns
leaders_constant <- leaders_filtered %>%
  group_by(name, country_name, year) %>%
  slice(1) %>%
  ungroup() %>%
  select(all_of(constant_cols))

# Create the wide format
leaders_wide <- leaders_filtered %>%
  select(name, country_name, year, position, prestige_1) %>%
  group_by(name, country_name, year) %>%
  mutate(position_number = letters[row_number()]) %>%
  ungroup() %>%
  pivot_wider(
    names_from = position_number,
    values_from = c(position, prestige_1),
    names_sep = ""
  ) %>%
  # Join back the constant columns
  left_join(leaders_constant, by = c("name", "country_name", "year"))
#View(leaders_wide)
 
leaders <- leaders_wide %>% relocate(country_isocode, gender, .before = positiona)
leaders <- leaders %>% relocate(prestige_1a, .before = positionb)

names(leaders)[names(leaders) == "country_name"] <- "LeadersCountry" 
names(leaders)[names(leaders) == "name"] <- "LeadersName" 

############################
# Introduce GODAD datasets #
############################

godad <- read.csv("~/Aid and corruption/Data input/GODAD/projectlevel_china_wb.csv")
subsetgodad <- subset(godad, select = c("name_0", "startyear", "comm", "comm_nominal", "disb", "disb_nominal"))
names(subsetgodad)[names(subsetgodad) == "name_0"] <- "LeadersCountry" 
names(subsetgodad)[names(subsetgodad) == "startyear"] <- "year" 
subsetgodad$year <- as.factor(subsetgodad$year)
subsetgodad <- subsetgodad %>% drop_na(year)
# create sums for each aid-variable to keep one row per country/year. 
godad_sum <- subsetgodad %>%
  group_by(LeadersCountry, year) %>%
  summarise(
    comm = sum(comm, na.rm = TRUE),
    comm_nominal = sum(comm_nominal, na.rm = TRUE),
    disb = sum(disb, na.rm = TRUE),
    disb_nominal = sum(disb_nominal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(LeadersCountry, year)

# merge GODAD and the Political Leaders data frames by country and year, keeping the one row per country and year and the wide format for the political positions, creating a variable for each year in office.
mlg <- full_join(leaders, godad_sum, by = c("LeadersCountry", "year"), suffix = c(".leaders", ".godad_sum"), keep = NULL) #use left_join to keep only country names from Sandcastles in dubai df
# shows NAs before 1991, as GODAD starts in 1991 only. 

mlg <- mlg %>% relocate(prestige_1b, .before = positionc)
merged <- mlg %>% relocate(prestige_1c, .before = positiond)
mlg <- merged %>% relocate(prestige_1d, .before = positione)
mlg <- mlg %>% relocate(prestige_1e, .before = id)
mlg <- mlg %>% relocate(comm, comm_nominal, disb, disb_nominal, .before = gender)

# divide amounts by 1.000.000
mlg <- mlg %>%
  mutate(
    comm = round(comm / 1000000),
    comm_nominal = round(comm_nominal / 1000000),
    disb = round(disb / 1000000),
    disb_nominal = round(disb_nominal / 1000000)
  )

mlg <- mlg %>% drop_na(comm)

# create the columns "tenure" and "tenure_period" to give information about the lenght and the time of the position held.
# This is the long format, since we first keep the year column still. 
mlg_long <- mlg %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%  # Create numeric version
  group_by(LeadersName, LeadersCountry) %>%
  mutate(
    tenure_period = paste0(min(year_numeric, na.rm = TRUE), "-", max(year_numeric, na.rm = TRUE)),
    tenure = max(year_numeric, na.rm = TRUE) - min(year_numeric, na.rm = TRUE) + 1
  ) %>%
  ungroup()

mlg_long <- mlg_long %>% relocate(tenure, tenure_period, .before = gender)

# This collapses the data frame by name and tenure, removing the year column, being "replaced" by the tenure_period column.
mlg_collapsed <- mlg_long %>%
  mutate(year_numeric = as.numeric(as.character(year))) %>%
  group_by(LeadersName, LeadersCountry) %>%
  mutate(
    tenure_period = paste0(min(year_numeric, na.rm = TRUE), "-", max(year_numeric, na.rm = TRUE)),
    tenure = max(year_numeric, na.rm = TRUE) - min(year_numeric, na.rm = TRUE) + 1
  ) %>%
  ungroup() %>%
  group_by(LeadersName, tenure_period, tenure) %>%
  summarise(
    across(c(comm, comm_nominal, disb, disb_nominal), ~sum(.x, na.rm = TRUE)),
    across(where(is.numeric) & !c(comm, comm_nominal, disb, disb_nominal) & !contains("year"), ~mean(.x, na.rm = TRUE)),
    across(where(is.character) | where(is.factor), ~first(na.omit(c(.x, NA)))[1]),  # Fixed line
    .groups = "drop"
  )

mlg_collapsed <- select(mlg_collapsed, -year)

mlg_collapsed <- mlg_collapsed %>% relocate(LeadersCountry, country_isocode,  .after = LeadersName)

###############################################
# Merging Horizons Output into one data frame #
###############################################

country_folders <- list.dirs(path = "C:/Users/wiedmann4/ownCloud/Aid and corruption_Dubai/Country's results",
                             full.names = TRUE, recursive = FALSE)

datalist <- lapply(country_folders, function(country_path) {
  folder_name <- basename(country_path)
  country_name <- str_split(folder_name, "_", n = 2, simplify = TRUE)[, 1]
  dataset_year <- str_extract(folder_name, "20\\d{2}")
  csv_files <- list.files(country_path, pattern = "\\.csv$", full.names = TRUE)
  lapply(csv_files, function(f) {
    df <- read_csv(
      f,
      show_col_types = FALSE,
      col_types = cols(.default = col_character()) #otherwise problem Phone number numeric and character
    )
    df$country   <- country_name  
    df$data_year <- dataset_year  
    df
  }) %>% bind_rows()   
  
}) %>% bind_rows()      

merged_data <- type_convert(datalist) # convert back / also type_convert()
sapply(merged_data, class) #info on class 
merged_data <- merged_data %>%
  relocate(country, data_year, .before = 1)
#saveRDS(merged_data, "merged_data.rds")

merged_data <- merged_data %>%
  rename(
    SandName = NAME,
    LeadersCountry = country,
    SandVersion = data_year)

merged_data <- merged_data %>%
  mutate(
    SandName_clean = SandName %>%
      str_squish() %>%      # extra space
      str_to_lower(),       # minusc
    LeadersName = NA_character_ 
  )

merged_data <- merged_data %>%
  relocate(SandName_clean, LeadersName, .before = DOB)
politicians <- c(
  "Dipak Patel",
  "Alawi Hasan Al Attas",
  "Osman Hussein",
  "Dang Thi",
  "Le Thi Thu",
  "Hoang Anh",
  "Gonzalo Fernandez",
  "Daniel Martinez",
  "Alvaro Alonso",
  "Andres Duran",
  "Oleksandr Moroz",
  "Mykola Azarov",
  "Viktor Petrov",
  "Yuriy Melnyk",
  "Ali Talip Ozdemir",
  "Ismail Cem", 
  "Mohamed Hamdi",
  "Omar Mansour",
  "Habib Mansour",
  "Mohamed Chaker",
  "Mohamed Trabelsi",
  "Mohamed Sayah",
  "Shavkat Umarov",
  "Ismail Ismail",
  "Ahmad Murad",
  "Malik Ali",
  "Adnan Mustafa",
  "George Radwan",
  "Ahmad Diab",
  "Issam Khalil",
  "Dib Al Masri",
  "Bassam Hanna",
  "Asad Mustafa",
  "Ibrahim Abdallah",
  "Ibrahim Yousef",
  "Faisal Saleh",
  "Adel Ibrahim",
  "Faisal Hassan Ibrahim",
  "Ibrahim Suleiman",
  "Ali Ahmad Ibrahim",
  "Yassin Ibrahim Yassin",
  "Hassan Ismael",
  "Farah Hasan",
  "Khalil Abdullah",
  "Osman Abdalla",
  "Ahmad Sulayman",
  "Mohammed Al Khalifa",
  "Ahmed Ibrahim Ali",
  "Jamal Mohammed Ahmad",
  "Bona Malwal Madut Ring",
  "Abdallah Ahmad Abdallah",
  "Hassan Omar",
  "Bashir Abbadi",
  "Ibrahim Gaber",
  "Ahmad Idris",
  "Ebrahim Rasool",
  "Ebrahim Patel",
  "Hassan Ali Mohamed",
  "Mohamed Ali Ibrahim",
  "Mohamed Qassim",
  "Mohamed Ibrahim Ahmed",
  "Mohamed Said Mohamed",
  "Omar Salah",
  "Ali Ahmed Jama",
  "Wais",
  "Omar Ahmed Omar",
  "Mohamed Sheikh Osman",
  "Ludovit Cernak",
  "Jan Mikolaj",
  "Bogoljub Karic",
  "Ali Haidar",
  "Adrian Stoica",
  "Cristian Ionescu",
  "Juan Manuel",
  "Jose De Jesus",
  "Felipe Castillo",
  "Julio Cesar Franco",
  "Mario Varela",
  "John Simon",
  "Mohammad Jaffar",
  "Mohammad Akbar",
  "Ahmed Rashid",
  "Abdul Aleem",
  "Arshad Hussain",
  "Asim Hussain",
  "Masood Khan",
  "Riaz Hussain",
  "Zahid Hamid",
  "Shaukat Aziz",
  "Mohammad Afzal",
  "Ghulam Hassan Khan",
  "Asif Ali Zardari",
  "Mohammad Ayub Khan",
  "Shahid Hassan Khan",
  "Salim Saifullah Khan",
  "Khalid Anwar",
  "Abdul Hamid Khan",
  "Muhammad Azam Khan",
  "Mohammad Sher Khan",
  "Tahir Iqbal",
  "Shamshad Ahmad",
  "Rafi Raza",
  "Ali Mahmud",
  "Zulfikar Ali Khan",
  "Amanullah Khan",
  "Mohammad Asghar",
  "Pervez Musharraf",
  "Asad Khan",
  "Waqar Ahmed Khan",
  "Mohammad Yaqub",
  "Mohammad Nasir Khan",
  "Murad Saeed",
  "Shamshad Akhtar",
  "Syed Ali Zafar",
  "Ali Akbar Khan",
  "Iqbal Ahmad Khan",
  "Shahid Hamid",
  "Mahmud Ali",
  "Amin Ul Haq",
  "Khurshid Hassan",
  "Mohamed Azhar",
  "Abdul Qayyum Khan",
  "Iftikhar Gilani",
  "Rehman Malik",
  "Syed Sharifuddin Pirzada",
  "Mohammad Afzal Khan",
  "Fida Mohammad Khan",
  "Ghulam Mustafa Shah",
  "Kamran Michael",
  "Shafqat Mahmood",
  "Iqbal Haider",
  "Mahmud Ali Durrani",
  "Sher Afghan",
  "Tariq Rahim",
  "David Mark",
  "Mohammed Musa",
  "Nura Imam",
  "Musa Mohammed",
  "Ibrahim Mohamed Hassan",
  "Zainab Ahmed",
  "Sule Lamido",
  "Ademola Adesina",
  "Bala Mohammed",
  "Abdullahi Ibrahim",
  "Idris Waziri",
  "Buba Ahmed",
  "Ibrahim Bunu",
  "Mohammed Bello Adoke",
  "Grace Ekpiwhre",
  "Clement Ikanade Agba",
  "Aisha Abubakar",
  "Ahmed Musa",
  "Uba Ahmed",
  "Mohamed Hamid",
  "Ibrahim Issa",
  "Oumar Diallo",
  "Ahmad Jafar",
  "Maung Maung",
  "Lwin",
  "Myint Thein",
  "Aung Thein",
  "Antonio Fernando",
  "Jalal Said",
  "Hassan Chami",
  "Nizar Baraka",
  "Mohamed Chafik",
  "Mohamed Naceur",
  "Mohamed Sajid",
  "Mourad Cherif",
  "Ahmed Bennani",
  "Kacem Zhiri",
  "Mustapha Mansouri",
  "Ahmed Ramzi",
  "Amadou Keita",
  "Mohamed Fall",
  "Mamadou Coulibaly",
  "Mamadou Traore",
  "Mohamed Nasir",
  "Suleiman Mohamed",
  "Zainal Abidin",
  "Abdul Wahid Omar",
  "Ismail Abdul Rahman",
  "Musa Mohamad",
  "Hussein Jaffar",
  "Anwar Ibrahim",
  "Soja",
  "Ahmade",
  "Basira",
  "Ahmad Sharif",
  "Peter Coleman",
  "Mohammed Sherif",
  "Charles Taylor",
  "John Bright",
  "Chahe Barsoumian",
  "Hamad Hasan",
  "Saeb Salam",
  "Elie Hobeika",
  "Adnan Mansour",
  "Youssef Saadeh",
  "Boutros Harb",
  "Hassan Diab",
  "Ali Hussein Abdullah",
  "Jean Obeid",
  "Ibrahim Daher",
  "Farid Abboud",
  "Laila Salah",
  "Marwan Hamadeh",
  "Serik Burkitbayev",
  "Adil Shayakhmetov",
  "Maha Ali",
  "Ali Bashir",
  "Jamal Nasir",
  "Khalid Saif",
  "Marwan Juma",
  "Ahmad Fawzi",
  "Talal Hassan",
  "Mohammad Khatib",
  "Mohammad Farhan",
  "Ayman Odeh",
  "Faleh Nasser",
  "Mohammad Dabbas",
  "Burhan Kamal",
  "Kamal Nasser",
  "Suhair Ali",
  "Ali Faqir",
  "Ibrahim Badran",
  "Marwan Kamal",
  "Marwan Hamad",
  "Mahmoud Kayed",
  "Jamal Hammad",
  "Husayn",
  "Sami Daoud",
  "Mohammad Abu Hammour",
  "Salah Juma",
  "Michael Henry",
  "Allan Douglas",
  "Peter Phillips",
  "Karim Mahmud",
  "Jafar Dhia Jafar",
  "Ali Hamid",
  "Muhammad Hashim",
  "Ismail Mustafa",
  "Tariq Aziz",
  "Ali Al-Shammari",
  "Amir Mahdi",
  "Izzat Mustafa",
  "Latif Rashid",
  "Manhal Aziz",
  "Khalid Makki Al-Hashimi",
  "Taha Ibrahim",
  "Jawad Hashim",
  "Mohammad Eslami",
  "Amir Hatami",
  "Morteza Bakhtiari",
  "Ali Akbar Salehi",
  "Ali Shamkhani",
  "Reza Salimi",
  "Mohammad Sam",
  "Mohammad Mir Mohammadi",
  "Reza Sadr",
  "Mohammad Ali Shahidi",
  "Abdul Gafur",
  "Subroto",
  "Ismail Saleh",
  "Singgih",
  "Jaswant Singh",
  "Karan Singh",
  "Dinesh Singh",
  "Ajit Singh",
  "Manmohan Singh",
  "Charan Singh",
  "Digvijay Singh",
  "Mahavir Prasad",
  "Harsh Vardhan",
  "Raj Bahadur",
  "Prakash Shah",
  "Amit Shah",
  "Rita Verma",
  "Abdul Kalam",
  "Naresh Chandra",
  "Praful Patel",
  "Arun Kumar Singh",
  "Zakir Husain",
  "Arjun Singh",
  "Dalbir Singh",
  "Nitish Kumar",
  "Bansi Lal",
  "Mohammad Hidayatullah",
  "Prithviraj Chavan",
  "Manubhai Shah",
  "Ravindra Varma",
  "Sunil Dutt",
  "Fakhruddin Ali Ahmed",
  "Abrar Ahmed Khan",
  "Fernando Garcia",
  "Jean Bernard",
  "Daniel Jean",
  "Andre Jean Pierre",
  "Raymond Joseph",
  "Reginald Paul",
  "Franck Charles",
  "Alpha Oumar Diallo",
  "Obeng",
  "Joseph Smith",
  "Mohamed Abdul Rahman",
  "Haile Giorgis Workneh",
  "Sufian Ahmed",
  "Addisu Legesse",
  "Mohamed Omer",
  "Ali Abdu",
  "Mohamed Tawfik",
  "Tarek Kabil",
  "Ahmed Imam",
  "Mohamed Ibrahim Soliman",
  "Mahmoud Sharif",
  "Mustafa Kamel",
  "Sabri Ali",
  "Ahmed Maher",
  "Nagwa Khalil",
  "Mustafa Khalil",
  "Ashraf Salman",
  "Mahmoud Fahmi",
  "Ahmad Badawi",
  "Mohamed Reda Ismail",
  "Ahmed Shafiq",
  "Ahmad Rushdi",
  "Sherif Fathi",
  "Atef Helmy",
  "Mohamed Shaker",
  "Ahmed Anis",
  "Muhammad Ahmad Muhammad",
  "Hasan Hasan Mustafa",
  "Hossam Kamal",
  "Mohamed Abbas Helmy",
  "Mohamed Ahmed Youssef",
  "Mustafa Abdel Qader",
  "Mohamed Saafan",
  "Samir Radwan",
  "Ibrahima Kone",
  "Maria Elena Carballo",
  "Pierre Andre",
  "Manuel Rodriguez",
  "Li Xue",
  "Li Qiang",
  "Bater",
  "Li Chen",
  "Wang Yi",
  "Liu Wei",
  "Rao Bin",
  "Xiao Yang",
  "Chen Lei",
  "Li Peng",
  "Chen Zhu",
  "Lu Dong",
  "Ahmed Issa",
  "Joseph Le",
  "Taiga",
  "Phuong",
  "Athanase Gahungu",
  "Norbert Ngendabanyikwa",
  "Come Manirakiza",
  "Emil Dimitrov",
  "Ivan Popov",
  "Eduardo Gomes",
  "Jamil Haddad",
  "Marcos Pereira",
  "Maria Do Rosario",
  "Abdur Razzak",
  "Akbar Hossain",
  "Abdul Mannan",
  "Sheikh Abdul Aziz",
  "Sharif Ahmed",
  "Imran Ahmad",
  "Abdul Quddus",
  "Zillur Rahman",
  "Tamim M",
  "Tariqul Islam",
  "Abdul Alim",
  "Muzaffar Ahmad",
  "Abdus Sattar",
  "Amina Rahman",
  "Mujibur Rahman",
  "Mohammad Ziauddin",
  "Abu Sayeed",
  "Syed Mohsin Ali",
  "Iqbal Hossain",
  "Tofael Ahmed",
  "Nazrul Islam",
  "Mohammad Nasim",
  "Azizur Rahman",
  "Abul Fazal",
  "Salahuddin Ahmed",
  "Mozammel Hossain",
  "Ali Ahsan",
  "Nural Islam",
  "Hasan Mahmud",
  "Rafiqul Islam",
  "Shamsul Haq",
  "Habibullah Khan",
  "Rahmat Ali",
  "Ismat Jahan",
  "Abdullah Al Noman",
  "Fazle Rabbi",
  "Abdur Rashid",
  "Zafrul Islam Chowdhury",
  "Mohammad Yunus",
  "Abdul Monim",
  "Mohammad Yunus Khan",
  "Shahabuddin Ahmed",
  "Riazuddin Ahmed",
  "Md. Shahab Uddin",
  "Abdul Mannan Bhuiyan",
  "Nurul Islam",
  "Mizanur Rahman Chowdhury",
  "Abdul Kashem",
  "Motiur Rahman",
  "Shahjahan Khan",
  "Humayun Kabir",
  "Ziaur Rahman",
  "Alamgir Kabir",
  "Kamal Hossain",
  "Anwar Zahid",
  "Mashiur Rahman",
  "Farhad Aliyev",
  "Jeyhun Bayramov",
  "Natiq Aliyev",
  "Mukhtar Babayev",
  "Yashar Aliyev",
  "Ilham Aliyev",
  "Gagik Harutyunyan",
  "Artur Grigorian",
  "Carlos Alberto Lopes",
  "Carlos Fernandes",
  "Mohamed Ghazi",
  "Ahmed Kaid",
  "Mohamed Amir",
  "Mohamed Nabi",
  "Abdullah Abdullah",
  "Mohammed Ayub",
  "Ismail Khan",
  "Abdul Qayum",
  "Mohammed Akram",
  "Khalilullah",
  "Asadullah Khalid",
  "Amirzai Sangin",
  "Abdul Zahir",
  "Ibrahim Adel",
  "Roya Rahmani",
  "Mohammed Aziz",
  "Mohammed Daoud",
  "Ghulam Rasul",
  "Abdul Ali",
  "Nazar Mohammed",
  "Mahmood Karzai",
  "Mohammed Yasir",
  "Faiz Mohammed",
  "Mohammed Anas",
  "Mohammed Zahir",
  "Abdul Wakil",
  "Ghulam Jelani Popal",
  "Ashraf Ghani",
  "Akbar Akbar",
  "Mohammed Kabir",
  "Ahmadullah Khan",
  "Wahidullah Nosher",
  "Anwar Ul Haq Ahady",
  "Hamidullah Rahimi",
  "Atiku Abubakar"
)
politicians_lower <- tolower(politicians)



for (i in seq_along(politicians_lower)) {
  
  p_low  <- politicians_lower[i] 
  p_orig <- politicians[i]
  
  tokens <- str_split(p_low, " +")[[1]]   
  
  if (length(tokens) == 1) {
    pattern <- paste0("\\b", tokens[1], "\\b")
    idx <- str_detect(merged_data$SandName_clean, regex(pattern))
    
  } else {
    # pattern normal
    pattern_fwd <- paste0("\\b", tokens, "\\b", collapse = ".*")
    
    # pattern inverted: 
    pattern_rev <- paste0("\\b", rev(tokens), "\\b", collapse = ".*")
    
    # match forward OR reverse
    idx <- str_detect(merged_data$SandName_clean, regex(pattern_fwd)) |
      str_detect(merged_data$SandName_clean, regex(pattern_rev))
  }
  
  merged_data$LeadersName[idx] <- p_orig
}

#sub_merge <- merged_data %>% filter(if_any(LeadersName, is.na)) was used to check which names from the Sandcastles db did not have a LeaderName

Sand <- merged_data %>%
  mutate(LeadersName = case_when(
    is.na(LeadersName) & SandName == "abdul aziz latif shaikh" ~ "Abdul Aziz",  # Shaikh part of street name, hence not directly matched with the Leaders name
    is.na(LeadersName) & SandName == "abdul aziz n a abu shanb" ~ "Abdul Aziz",
    is.na(LeadersName) & SandName == "heena kauser abdul aziz" ~ "Abdul Aziz",
    is.na(LeadersName) & SandName == "sajeena mullapilly abdul aziz" ~ "Abdul Aziz",
    is.na(LeadersName) & SandName == "abdul rahim shaikh" ~ NA_character_,      # accidental download
    is.na(LeadersName) & SandName == "manghnani murij jiwatram" ~ NA_character_,# accidental download
    is.na(LeadersName) & SandName == "mustapha zniber" ~ NA_character_,         # accidental download
    is.na(LeadersName) & SandName == "sami abdalla" ~ NA_character_,            # accidental download
    is.na(LeadersName) & SandName == "sana ullah khan" ~ NA_character_,         # accidental download
    is.na(LeadersName) & SandName == "shakeel alam" ~ NA_character_,            # accidental download
    is.na(LeadersName) & SandName == "tariq salah o abduljabbar" ~ NA_character_,# accidental download
    is.na(LeadersName) & SandName == "ahmad eljechi" ~ "Ahmade",                # name in Horizons: Ahmad Eljechi, email address: ahmade@mashreqbank.com
    is.na(LeadersName) & SandName == "mamadou dian diallo" ~ "Alpha Diallo",    # name in the address column with c/o
    is.na(LeadersName) & SandName == "thierno sadou diallo" ~ "Alpha Diallo",   # name in the address column with c/o
    is.na(LeadersName) & SandName == "mariam isaak abdul aziz al diqs" ~ "Abdul Aziz",
    is.na(LeadersName) & SandName == "muhammad abdul aziz atif" ~ "Abdul Aziz",
    is.na(LeadersName) & SandName == "ahmed khalid makki al hashimi" ~ "Khalid Makki Al-Hashimi", # could not match as there is a dash in the Leaders name
    is.na(LeadersName) & SandName == "nazhan qalam ali al shammari" ~ "Ali Al-Shammari",          # could not match as there is a dash in the Leaders name
    is.na(LeadersName) & SandName == "alpha tours (l.l.c)" ~ "Shell company likely",
    TRUE ~ LeadersName  # Keep existing values
  )) %>%
  filter(!is.na(LeadersName)) 

Sand <- Sand %>%
  relocate(LeadersName, .before = SandVersion) %>%
  relocate(SandName_clean, .before = ADDRESS)

sum(is.na(Sand$PLOT_NUMBER) & is.na(Sand$FLAT_NUMBER) & is.na(Sand$ESTIMATED_VALUE))

Sand <- Sand %>%
  mutate(SandID = case_when(
    # If all three are NA, just use "NA"
    is.na(PLOT_NUMBER) & is.na(FLAT_NUMBER) & is.na(ESTIMATED_VALUE) ~ "NA",
    # Otherwise, combine them with "NA" for missing values
    TRUE ~ paste(
      ifelse(is.na(PLOT_NUMBER), "NA", PLOT_NUMBER),
      ifelse(is.na(FLAT_NUMBER), "NA", FLAT_NUMBER),
      ifelse(is.na(ESTIMATED_VALUE), "NA", ESTIMATED_VALUE),
      sep = "-"
    )
  ))
Sand <- Sand %>% relocate(SandID, .before = DOB)

Sand <- Sand %>%
  group_by(LeadersName, LeadersCountry) %>%
  summarise(
    PropertyCount = n(),
    PropertyValueSum = sum(ESTIMATED_VALUE, na.rm = TRUE),
    across(everything(), first),
    .groups = "drop"
  )

Sand <- Sand %>% 
  relocate(PropertyCount, .before = DOB)%>%
  relocate(PropertyValueSum, .before = PropertyCount)

Sand$LeadersCountry <- gsub("Viet nam", "Vietnam", Sand$LeadersCountry)

SLG <- mlg_collapsed %>%
  left_join(
    Sand %>% 
      distinct(LeadersCountry, LeadersName) %>% 
      mutate(SandDummy = 1),
    by = c("LeadersCountry", "LeadersName")
  ) %>%
  mutate(SandDummy = replace_na(SandDummy, 0))

SLG <- SLG %>%
  mutate(iso3c = countrycode(LeadersCountry, origin = "country.name", destination = "iso3c"))
SLG <- SLG %>% relocate(SandDummy,iso3c, .before = tenure_period)
SLG <- select(SLG, -c(country_isocode, year))
SLG <- SLG %>% filter(!grepl("202[1-9]|20[3-9][0-9]", tenure_period))


log_model <- glm(SLG$SandDummy~SLG$comm, family = binomial)
summary(log_model)
# subleaders <- leaders[, c("id", "country_name", "name")]
# 
# #deleting duplicates in var name
# subleaders_NoNAD <- subleaders_NoNA[!duplicated(subleaders_NoNA$name), ]
# subleaders_NoNAD$country_name <-as.character(subleaders_NoNAD$country_name)
# subleaders_NoNAD$name <-as.character(subleaders_NoNAD$name)
# 
# subleaders_NoNAD$iso3_code <- countrycode(subleaders_NoNAD$country_name, 
#                                           origin = "country.name", 
#                                           destination = "iso3c")
# pollead <- subleaders_NoNAD
# 
# ##########################################################
# # Creating subsets for all countries from leaders database
# ##########################################################
# 
# # First, show all unique countries
# unique_countries <- unique(pollead$country_name)
# allcs <- unique_countries[unique_countries != "East Germany"]
# 
# cat("All unique countries in the dataset:\n")
# print(sort(unique(pollead$country_name)))
# cat("\n")
# 
# # for (country in allcs) {
# #   cat("Processing country:", country, "\n")
# #   
# #   # Get the ISO3C code for the country
# #   iso3c_code <- countrycode(country, origin = "country.name", destination = "iso3c")
# #   
# #   # Create the subset
# #   subset_data <- pollead %>% filter(country_name == country)
# #   
# #   # Find the maximum number of name parts in this subset
# #   max_parts <- max(lengths(strsplit(subset_data$name, " ")))
# #   
# #   # Create column names dynamically
# #   if (max_parts == 1) {
# #     col_names <- "First name"
# #   } else {
# #     col_names <- c(paste("Name part", 1:(max_parts-1)), "Last name")
# #   }
# #   
# #   # Split the name column into separate columns
# #   subset_data <- subset_data %>%
# #     separate(name, into = col_names, sep = " ", remove = FALSE, fill = "right")
# #   
# #   # Create variable name using ISO3C code and assign
# #   var_name <- paste0("subset_", iso3c_code)
# #   assign(var_name, subset_data)
# #   
# #   # Create CSV filename using ISO3C code
# #   csv_filename <- paste0("subset_", iso3c_code, ".csv")
# #   
# #   max_col <- min(7, ncol(subset_data))
# #   write.csv(subset_data[, 3:max_col, drop = FALSE], csv_filename, row.names = FALSE)  
# #   # Write columns 3-6 to CSV file
# #   write.csv(subset_data[, 3:6, drop = FALSE], csv_filename, row.names = FALSE)
# # }
# 
# 
# 
# for (country in allcs) {
#   cat("Processing country:", country, "\n")
#   
#   # Get the ISO3C code for the country
#   iso3c_code <- countrycode(country, origin = "country.name", destination = "iso3c")
#   
#   # Create the subset
#   subset_data <- pollead %>% filter(country_name == country)
#   
#   # Create variable name using ISO3C code and assign
#   var_name <- paste0("subset_", iso3c_code)
#   assign(var_name, subset_data)
#   
#   # Create CSV filename using ISO3C code
#   csv_filename <- paste0("subset_", iso3c_code, ".csv")
#   
#   write.csv(subset_data[, 3, drop = F], csv_filename, row.names = F)'
# }

