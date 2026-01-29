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


###############################################
# Merging Horizons Output into one data frame #
###############################################
# Merger with aid from OECD and AidData (1966-2021)
aidOA <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/Data_Aid_OECD_AidData.csv")

# Merger with aid from GODAD (1995-2023)

#aidG <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/Data_Aid_GODAD.csv")

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
#sapply(merged_data, class) #info on class 
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


#######################
# Long merge of names #
#######################

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

#################################
# To check when verifying names #
#################################

Sand_fullnames <- Sand %>%
  relocate(LeadersName, .before = SandVersion) %>%
  relocate(SandName_clean, .before = ADDRESS)

#write.csv(Sand_fullnames, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/Sand_fullnames.csv")


Sand <- Sand_fullnames %>%
  group_by(LeadersName, LeadersCountry) %>%
  mutate(SandName_num = row_number()) %>%
  summarise(
    PropertyCount = n(),
    PropertyValueSum = sum(ESTIMATED_VALUE, na.rm = TRUE),
    across(c(-SandName, -SandName_num), first),
    .groups = "drop")
#   ) %>%
#   # Pivot to create SandName1, SandName2, etc.
#   left_join(
#     Sand %>%
#       group_by(LeadersName, LeadersCountry) %>%
#       mutate(SandName_num = row_number()) %>%
#       select(LeadersName, LeadersCountry, SandName, SandName_num) %>%
#       pivot_wider(
#         names_from = SandName_num,
#         values_from = SandName,
#         names_prefix = "SandName"
#       ),
#     by = c("LeadersName", "LeadersCountry")
#   )

Sand <- Sand %>% 
  relocate(PropertyCount, .before = DOB)%>%
  relocate(PropertyValueSum, .before = PropertyCount)

Sand$LeadersCountry <- gsub("Viet nam", "Vietnam", Sand$LeadersCountry)
Sand$LeadersCountry <- gsub("Syrian Arab Republic", "Syria", Sand$LeadersCountry)
Sand$LeadersCountry <- gsub("Ivory coast", "Ivory Coast", Sand$LeadersCountry)


Sand <- Sand %>% 
  rename(SandGender = GENDER)%>%
  rename(SandCountry = NATIONALITY)
Sand <- Sand %>% 
  relocate(SandGender, .before = PropertyValueSum)

Sand <- Sand %>%
  mutate(across(where(is.character), str_squish))


Missing <- Sand %>%
  anti_join(aidOA, by = c("LeadersCountry", "LeadersName"))

# some values came into the data set erroneously, we need to delete them manually. An explanation is given for each case
# Abdul Ali, Bangladesh; Ali Haidar, Sudan; Ismail Khan, Morocco; Mohammed Aziz, Bangladesh; Murad Saeed, Syria; Pierre Andre, Haiti; Sheikh Abdul Aziz: there is no leader called as such
Sand <- Sand[-c(4, 65, 195, 312, 329, 361, 397), ]

# Oumar Diallo in Niger does not have the name Alpha, this only applies for the leader in Guinea
Sand[70, 1] = "Oumar Diallo"
Sand[173, 1] = "Adel Ibrahim"

SLO <- aidOA %>%
  left_join(
    Sand %>% 
      mutate(SandDummy = 1),
    by = c("LeadersCountry", "LeadersName"),
    suffix = c("_aid", "_sand")  # Add suffixes to overlapping columns
  ) %>%
  mutate(SandDummy = replace_na(SandDummy, 0))

names(SLO)[names(SLO) == 'Total.Comm.Constant.inclCN'] <- 'comm.cn'
names(SLO)[names(SLO) == 'Total.Disb.Constant'] <- 'disb'

SLO <- SLO %>%
  relocate(SandGender, .before = positiona)%>%
  relocate(SandDummy, .before = tenure_period) %>%
  relocate(SandCountry, .before = country_isocode)%>%
  #relocate(SandName, .before = LeadersCountry)%>%
  filter(!grepl("202[3-9]|20[3-9][0-9]", tenure_period))

# create the log of comm and add 1 as log(0) is undefined. 
SLO <- SLO %>%
  mutate(log_disb = log(disb + 1))%>%
  mutate(log_comm.cn = log(comm.cn + 1))
SLO <- SLO %>% 
  relocate(log_comm.cn, log_disb, .before = birthyear) %>%
  relocate(birthyear,LeadersGender, SandGender, SandName_clean,PropertyValueSum, PropertyCount, .before = pob_longitude)

write.csv(SLO, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiDataOA.csv")

SLO <- SLO %>%
  filter(SandDummy == 1)

#summary(SLO)
write.csv(SLO, "C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiDataOA_matchesonly.csv")
rm(datalist, merged_data, Missing, Sand, SLO, aidOA)
