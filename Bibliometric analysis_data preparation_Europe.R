# ##############################################################################
# # Bibliometric Analysis: Rural Cancer - European Data 
# # Author: Tanja Kleinhappel
# # Date: August 2025
# # Description: This script prepares a European Dataset for Rural Cancer
# #               Research. Two databases were searched (Web of Science & Scopus)
# #               on the 26th of August 2025. 
# ##############################################################################




# --- Setup: Package Management ---

# Install and load required packages.
# This block checks if packages are installed and installs them if missing,
# making the script more robust for new users.

# List of all packages required for the script
packages_required <- c(
  "bibliometrix","openxlsx", "tidyverse",
  "data.table", "reshape2", "RColorBrewer", "plyr", "tidyr",
  "igraph", "ggpubr", "magick", "scales", "ggpattern", "maps",
  "ggrepel", "rnaturalearth", "rnaturalearthdata", "here"
)

# Identify and install missing packages
new_packages <- packages_required[!(packages_required %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, dependencies = TRUE)
}

# Load all required packages silently
invisible(lapply(packages_required, library, character.only = TRUE))

# Clear all objects from the current workspace to ensure a clean environment
rm(list = ls())

# --- Setup: Project Directory ---
# If your project root is the folder containing "Biblio_Analysis",
# and your script is at Biblio_Analysis/EuropeDataAnalysis/

# NOTE: This line requires the script to be saved with this exact filename.
# If you rename the file, update the filename here to match, or here() will
# not be able to locate the project root correctly.

here::i_am("Bibliometric analysis_Data preparation_Europe.R")



################################################################################
###### LOAD DATABASES AND ONLY KEEP ARTICLES
################################################################################

#### Helper function to convert to dataframe
# This function will now handle the conversion, no meta-tag extraction.
process_database_conversion <- function(file_name, dbsource) {
  # Construct the full path using here()
  full_file_path <- here("Searches", file_name)
  
  # Determine format based on dbsource for convert2df
  if (dbsource == "wos") {
    format_type <- "bibtex"
  } else if (dbsource == "scopus") {
    format_type <- "bibtex"
  } else {
    # Default to "csv" for other sources if necessary, or throw an error
    format_type <- "csv"
  }
  
  df_full <- bibliometrix::convert2df(full_file_path, dbsource = dbsource, format = format_type)
  
  return(df_full)
}

#################### Web of Science database ########################
## Search terms in data base are: TS=(Cancer AND Rural) - no restriction on dates
## date of search: 26/08/2025
## downloaded as BibTex file format

wos_article_types <- c(
  "ARTICLE", "ARTICLE; EARLY ACCESS", "ARTICLE; PROCEEDINGS PAPER",
  "REVIEW", "REVIEW; EARLY ACCESS", "ARTICLE; DATA PAPER"
)

# Load and process WoS data initially
WoSdataset_raw <- process_database_conversion("WoS/WoSfullSearch.bib", dbsource = "wos")

# Now, perform the metaTagExtraction as a separate step.

# First, remove the columns to ensure they are re-created correctly
WoSdataset_raw <- WoSdataset_raw %>% 
  select(-any_of(c("AU1_UN", "AU_UN")))

# Now, re-add them using metaTagExtraction
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU_CO", sep = ";")
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU1_CO", sep = ";")
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU1_UN", sep = ";")
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU_UN", sep = ";")


# Filter for articles after initial processing
WoSdataset_articles <- WoSdataset_raw %>%
  filter(DT %in% wos_article_types)

message(paste0("Original WoS records: ", nrow(WoSdataset_raw)))
message(paste0("WoS Articles/Reviews kept: ", nrow(WoSdataset_articles)))
message(paste0("WoS Dropped non-articles: ", nrow(WoSdataset_raw) - nrow(WoSdataset_articles)))

# Initial save of articles
save(WoSdataset_articles, file = here("Searches", "WoS", "WoSdataset_articles.Rda"))
write.xlsx(WoSdataset_articles, file = here("Searches", "WoS", "WoSdataset_articles.xlsx"))

### CLEAN WoS DATASET
WoSdataset_final <- WoSdataset_articles %>%
  # 1. Remove Anonymous authors
  filter(AU != "[ANONYMOUS] A") %>%
  # 2. Clean RP column (using stringr for regex)
  mutate(
    RP = case_when(
      str_detect(RP, "CORRESPONDING AUTHOR") ~ str_replace_all(RP, "(CORRESPONDING AUTHOR),?", ";"),
      TRUE ~ "" # If no "CORRESPONDING AUTHOR", make it empty
    )
  ) %>%
  # 3. Corrected AU_UN cleaning: replicate original's split-remove-join logic
  mutate(
    AU_UN = map_chr(AU_UN, ~ {
      # Split the string by ";", find the element containing "CORRESPONDING AUTHOR",
      # remove that element, and paste the remaining ones back together.
      affiliations <- str_split(., pattern = ";", simplify = FALSE)[[1]]
      cleaned_affiliations <- affiliations[!grepl("CORRESPONDING AUTHOR", affiliations, fixed = TRUE)]
      str_c(cleaned_affiliations, collapse = ";")
    }),
    # 4. Correct AU1_UN cleaning: The original logic was to clear the entire cell, not just the text.
    AU1_UN = if_else(str_detect(AU1_UN, "CORRESPONDING AUTHOR"), "", AU1_UN)
  ) %>%
  # 5. Drop CR column
  select(-CR) %>%
  # Fix specific error for AU1_CO - using row-wise operation if truly a single fix
  mutate(
    AU1_CO = case_when(
      row_number() == 5250 ~ "ITALY",
      TRUE ~ AU1_CO
    )
  )

message(paste0("WoS records after cleaning: ", nrow(WoSdataset_final)))
save(WoSdataset_final, file = here("Searches", "WoS", "WoSdataset_final.Rda"))
write.xlsx(WoSdataset_final, file = here("Searches", "WoS", "WoSdataset_final.xlsx"))


#########################     Scopus      ############################################
## Search terms in data base: (TITLE-ABS-KEY (rural)) AND (TITLE-ABS-KEY (cancer ))
## date of search: 26/08/2025
## downloaded as csv file format

scopus_article_types <- c("ARTICLE", "REVIEW")

# Load and process Scopus data initially
ScopusDataset_raw <- process_database_conversion("Scopus/ScopusFullSearch.bib", dbsource = "scopus")


# Replicate the original's specific logic for meta-tag extraction.
# The original script first removes these columns before re-extracting them.

# NOTE: The row number below corresponds to a specific record identified
# manually in this dataset's Web of Science search (26 August 2025). If you
# rerun the search, the results may return a different number of records, or
# in a different order, so this row number will no longer point to the same
# entry. Check the affected record again after rerunning the search, rather
# than assuming this line still applies correctly.

ScopusDataset_raw <- ScopusDataset_raw %>%
  select(-any_of(c("AU1_UN", "AU_UN"))) %>%
  bibliometrix::metaTagExtraction(Field = "AU_CO", sep = ";") %>%
  bibliometrix::metaTagExtraction(Field = "AU1_CO", sep = ";") %>%
  bibliometrix::metaTagExtraction(Field = "AU1_UN", sep = ";") %>%
  bibliometrix::metaTagExtraction(Field = "AU_UN", sep = ";")

# Filter for articles and reviews after initial processing
ScopusDataset_articles <- ScopusDataset_raw %>%
  filter(DT %in% scopus_article_types)

message(paste0("Original Scopus records: ", nrow(ScopusDataset_raw)))
message(paste0("Scopus articles/reviews kept: ", nrow(ScopusDataset_articles)))
message(paste0("Scopus Dropped non-articles: ", nrow(ScopusDataset_raw) - nrow(ScopusDataset_articles)))

# Initial save of articles
save(ScopusDataset_articles, file = here("Searches", "Scopus", "ScopusDataset_articles.Rda"))
# write.xlsx(ScopusDataset_articles, file = here("Biblio_Analysis", "GlobalDataAnalysis", "Searches", "Scopus", "ScopusDataset_articles.xlsx"))


###### CLEAN SCOPUS DATASET
# Apply cleaning steps sequentially as per original logic
ScopusDataset_final <- ScopusDataset_articles %>%
  # 1. Remove empty author records
  filter(AU != "") %>%
  # 2. Fix fields with too long entries.
  mutate(CR = if_else(nchar(CR) > 30000, " ", CR)) %>%
  # 3. Change [No ABSTRACT] to blank.
  mutate(AB = str_replace_all(AB, fixed("[NO ABSTRACT AVAILABLE]"), " ")) %>%
  # 4. Clean RP column. Original clears the ENTIRE cell if it starts with a semicolon.
  mutate(RP = if_else(str_detect(RP, "^;"), "", RP)) %>%
  # 5. Clean AU_UN and AU1_UN columns: The original uses `sub` which only replaces
  # the FIRST occurrence of "NOTREPORTED".
  mutate(
    AU_UN = str_replace(AU_UN, "NOTREPORTED", ""),
    AU1_UN = str_replace(AU1_UN, "NOTREPORTED", "")
  ) %>%
  # 6. Replicate the redundant AU_UN cleaning step from the original script
  # The original script performs this step twice, so we will as well.
  mutate(AU_UN = str_replace(AU_UN, "NOTREPORTED", "")) %>%
  # Remove the temporary 'max' column if it was created
  select(-any_of("max"))

message(paste0("Scopus records after cleaning: ", nrow(ScopusDataset_final)))
save(ScopusDataset_final, file = here("Searches", "Scopus", "ScopusDataset_final.Rda"))
write.xlsx(ScopusDataset_final, file = here("Searches", "Scopus", "ScopusDataset_final.xlsx"))


################################################################################
###### MERGE WoS AND SCOPUS DATASETS
################################################################################

# Load datasets
load(file = here("Searches", "WoS", "WoSdataset_final.Rda"))
load(file = here("Searches", "Scopus", "ScopusDataset_final.Rda"))

# Merge datasets and remove duplicates
Merged_WoS_Scopus_Db <- bibliometrix::mergeDbSources(WoSdataset_final, ScopusDataset_final, remove.duplicated = TRUE)

message(paste0("Total WoS articles: ", nrow(WoSdataset_final)))
message(paste0("Total Scopus articles: ", nrow(ScopusDataset_final)))
message(paste0("Total articles after merging and removing duplicates: ", nrow(Merged_WoS_Scopus_Db)))

save(Merged_WoS_Scopus_Db, file = here("Searches", "Merged_WoS_Scopus_Db.rda"))
write.xlsx(Merged_WoS_Scopus_Db, file = here("Searches", "Merged_WoS_Scopus_Db.xlsx"))

#load(file = here("Searches", "Merged_WoS_Scopus_Db.Rda"))

## Select relevant columns for the final dataset
# The original code's select is a bit verbose, the new select works perfectly fine.
FullDataset <- Merged_WoS_Scopus_Db %>%
  select(
    AU, DE, ID, C1, AB, DI, SO, LA, TC, TI, DT, PY, SR, SR_FULL, CR, DB, RP,
    AU_UN, AU_CO, AU1_CO
  )

save(FullDataset, file = here("Searches", "FullDataset.rda"))
write.xlsx(FullDataset, file = here("Searches", "FullDataset.xlsx"))



############ Web of Science abbreviates the Affiliations, Scopus doesn't ##################
#### correct the Scopus Institution and Center names #####

# Load full dataset
load(file = here("Searches", "FullDataset.rda"))

# Load Web of Science address abbreviation file
WoSAbbrData <- read_csv(here("WebOfScienceAffiliationAbbreviations.csv"), show_col_types = FALSE) %>%
  mutate(
    Abbreviation = toupper(Abbreviation),
    FullName = toupper(FullName)
  )

# First, apply all abbreviations
for (j in 1:nrow(WoSAbbrData)) {
  FullDataset$AU_UN <- gsub(WoSAbbrData$FullName[j], WoSAbbrData$Abbreviation[j], FullDataset$AU_UN, fixed = TRUE)
  # FullDataset$AU1_UN <- gsub(WoSAbbrData$FullName[j], WoSAbbrData$Abbreviation[j], FullDataset$AU1_UN, fixed = TRUE)
}

# Second, apply the other cleaning steps
FullDataset <- FullDataset %>%
  mutate(
    AU_UN = str_replace_all(AU_UN, '\\bTHE\\b', ''),
    AU_UN = str_replace_all(AU_UN, '\\bOF\\b', ''),
    AU_UN = str_replace_all(AU_UN, '\\bAT\\b', ''),
    AU_UN = str_replace_all(AU_UN, '\\bSOUTH\\b', 'S'),
    AU_UN = str_replace_all(AU_UN, '\\bNORTH\\b', 'N'),
    AU_UN = str_replace_all(AU_UN, '\\bWEST\\b', 'W'),
    AU_UN = str_replace_all(AU_UN, '\\bEAST\\b', 'E'),
    AU_UN = str_replace_all(AU_UN, '\\s+', ' '),
    AU_UN = str_trim(AU_UN),
    
  )

save(FullDataset, file = here("Searches", "FullDataset.rda"))
write.xlsx(FullDataset, file = here("Searches", "FullDataset.xlsx"))






################################################################################

###### SUBSET DATA FOR EUROPE ALONE

################################################################################

# Abstract
load(file=here("Searches", "FullDataset.rda"))
dim(FullDataset)

## remove first funding bodies and journal from abstract:
words_to_remove <- c("CANCER RESEARCH UK", "UK RESEARCH AND INNOVATION", "BRITISH JOURNAL OF CANCER", "UK LIMITED")

# Loop through the words to remove and perform the substitution
for (word in words_to_remove) {
  FullDataset$AB <- gsub(pattern = word,
                         replacement = "",
                         x = FullDataset$AB,
                         fixed = TRUE)
}

save(FullDataset, file = here("Searches", "FullDataset.rda"))
write.xlsx(FullDataset, file = here("Searches", "FullDataset.xlsx"))


# To ensure the data is collected in European countries 
# Search for europe or european countries in title and abstract
# Author affiliations won't be useful as European authors can conduct research in Non-European countries


### Create a list of European countries
European_Countries = c("ALBANIA", "ANDORRA", "AUSTRIA", "BELARUS", "BELGIUM", "BOSNIA AND HERZEGOVINA",
                       "BULGARIA", "CROATIA", "CYPRUS", "CZECHIA", "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", 
                       "GREECE", "HUNGARY", "ICELAND", "IRELAND", "ITALY", "LATVIA", "LIECHTENSTEIN", "LITHUANIA",
                       "LUXEMBOURG", "MALTA", "MOLDOVA", "MONACO", "MONTENEGRO", "NETHERLANDS", "NORTH MACEDONIA",
                       "NORWAY", "POLAND", "PORTUGAL", "ROMANIA", "RUSSIA", "SAN MARINO", "SERBIA", "SLOVAKIA", "SLOVENIA",
                       "SPAIN", "SWEDEN", "SWITZERLAND", "UKRAINE", "UNITED KINGDOM")

### Create a list of European Population names
European_Population = c("AUSTRIAN", "ALBANIAN", "ANDORRANS", "BELORUSSIAN", "BELGAE", "BOSNIAN",
                        "BULGARIAN", "CROATIAN", "CYPRIOT", "CZECH", "DANE", "ESTONIAN", "FINN", "FRENCH", "GERMAN", 
                        "GREEK", "HUNGARIAN", "ICELANDER", "IRISH", "ITALIAN", "LATVIAN", "LIECHTENSTEINER", "LITHUANIAN",
                        "LUXEMBOURGER", "MALTESE", "MOLDOVA", "MONEGASQUES", "MONTENEGRIN", "DUTCH", "NORTH MACEDONIAN",
                        "NORWEGIAN", "POLISH", "PORTUGUESE", "ROMANIAN", "RUSSIAN", "SAMMARINESE", "SERBIAN", "SLOVAK", "SLOVENE",
                        "SPANISH", "SWEDISH", "SWISS", "UKRAINIAN", "ENGLISH")

# Create additional search words
search_words = c("EUROPE", "BRITAIN", "BRITISH", "SCOTTISH", "WELSH", "DANISH", "FLEMISH", "POLE", 
                 "SLAV", "SWEDE", "CORNISH", "CATALAN", "BAVARIAN", "CROAT", "FINNISH")

# 1. Search for European country names (Title & Abstract)
# 2. Search for European population names (Title & Abstract)
# 3. Search for additional words: Europe, Britain, British, England, Spanish
# 4. Search for UK (needs to be done separatly to restrict to only UK and not include words like Leukimea)
# 5. Search for EU (again needs to be done separatly)
# 6. Merge all datasets 

## 1. European countries in Abstract and Title
# Abstract
load(file=here("Searches", "FullDataset.rda"))
dim(FullDataset)

Europe_DB_Abstract = FullDataset[FALSE,]
for (i in 1:length(European_Countries)){
  Country = FullDataset[grepl(European_Countries[i], FullDataset$AB), ]
  print(dim(Country))
  Europe_DB_Abstract= rbind(Country, Europe_DB_Abstract)
}
Europe_DB_Abstract$CR <- NA 
Europe_DB_Abstract= mergeDbSources(Europe_DB_Abstract, remove.duplicated = TRUE)
dim(Europe_DB_Abstract)
Europe_DB_Abstract = subset(Europe_DB_Abstract, select= -c(KW_Merged))

save(Europe_DB_Abstract, file = here("Searches", "Europe_DB_Abstract.rda"))
write.xlsx(Europe_DB_Abstract, file = here("Searches", "Europe_DB_Abstract.xlsx"))


# Title
load(file=here("Searches", "FullDataset.rda"))
Europe_DB_Title = FullDataset[FALSE,]
for (i in 1:length(European_Countries)){
  Country = FullDataset[grepl(European_Countries[i], FullDataset$TI), ]
  print(dim(Country))
  Europe_DB_Title= rbind(Country, Europe_DB_Title)
}
Europe_DB_Title$CR <- NA 
Europe_DB_Title= mergeDbSources(Europe_DB_Title, remove.duplicated = TRUE)
Europe_DB_Title = subset(Europe_DB_Title, select= -c(KW_Merged))
dim(Europe_DB_Title)
save(Europe_DB_Title, file=here("Searches", "Europe_DB_Title.rda"))


# Keywords
load(file=here("Searches", "FullDataset.rda"))
Europe_DB_Keywords = FullDataset[FALSE,]
for (i in 1:length(European_Countries)){
  Country = FullDataset[grepl(European_Countries[i], FullDataset$DE), ]
  print(dim(Country))
  Europe_DB_Keywords= rbind(Country, Europe_DB_Keywords)
}
Europe_DB_Keywords$CR <- NA 
Europe_DB_Keywords= mergeDbSources(Europe_DB_Keywords, remove.duplicated = TRUE)
Europe_DB_Keywords = subset(Europe_DB_Keywords, select= -c(KW_Merged))
dim(Europe_DB_Keywords)
save(Europe_DB_Keywords, file=here("Searches", "Europe_DB_Keywords.rda"))
write.xlsx(Europe_DB_Keywords, file = here("Searches", "Europe_DB_Keywords.xlsx"))


## 2. European population names in Title and Abstract
#Title
load(file=here("Searches", "FullDataset.rda"))
Europe_DB_Title_Pop = FullDataset[FALSE,]
for (i in 1:length(European_Population)){
  Country = FullDataset[grepl(European_Population[i], FullDataset$TI), ]
  print(dim(Country))
  Europe_DB_Title_Pop= rbind(Country, Europe_DB_Title_Pop)
}
Europe_DB_Title_Pop$CR <- NA 
Europe_DB_Title_Pop = mergeDbSources(Europe_DB_Title_Pop, remove.duplicated=TRUE)
Europe_DB_Title_Pop = subset(Europe_DB_Title_Pop, select= -c(KW_Merged))
dim(Europe_DB_Title_Pop)
save(Europe_DB_Title_Pop, file=here("Searches", "Europe_DB_Title_Pop.rda"))


#Abstract
Europe_DB_Abstract_Pop = FullDataset[FALSE,]
for (i in 1:length(European_Population)){
  Country = FullDataset[grepl(European_Population[i], FullDataset$AB), ]
  print(dim(Country))
  Europe_DB_Abstract_Pop = rbind(Country, Europe_DB_Abstract_Pop)
}
Europe_DB_Abstract_Pop$CR <- NA 
Europe_DB_Abstract_Pop = mergeDbSources(Europe_DB_Abstract_Pop, remove.duplicated = TRUE)
Europe_DB_Abstract_Pop = subset(Europe_DB_Abstract_Pop, select= -c(KW_Merged))
dim(Europe_DB_Abstract_Pop)
save(Europe_DB_Abstract_Pop, file=here("Searches", "Europe_DB_Abstract_Pop.rda"))


# Keywords
Europe_DB_Keywords_Pop = FullDataset[FALSE,]
for (i in 1:length(European_Population)){
  Country = FullDataset[grepl(European_Population[i], FullDataset$DE), ]
  print(dim(Country))
  Europe_DB_Keywords_Pop = rbind(Country, Europe_DB_Keywords_Pop)
}
Europe_DB_Keywords_Pop$CR <- NA 
Europe_DB_Keywords_Pop = mergeDbSources(Europe_DB_Keywords_Pop, remove.duplicated = TRUE)
Europe_DB_Keywords_Pop = subset(Europe_DB_Keywords_Pop, select= -c(KW_Merged))
dim(Europe_DB_Keywords_Pop)
save(Europe_DB_Keywords_Pop, file=here("Searches", "Europe_DB_Keywords_Pop.rda"))



## 3. Additional words to search for in Title and Abstract
#Title
load(file=here("Searches", "FullDataset.rda"))
Europe_DB_Title_Extra = FullDataset[FALSE,]
for (i in 1:length(search_words)){
  Country = FullDataset[grepl(search_words[i], FullDataset$TI), ]
  print(dim(Country))
  Europe_DB_Title_Extra= rbind(Country, Europe_DB_Title_Extra)
}
Europe_DB_Title_Extra$CR <- NA
Europe_DB_Title_Extra = mergeDbSources(Europe_DB_Title_Extra, remove.duplicated = TRUE)
Europe_DB_Title_Extra = subset(Europe_DB_Title_Extra, select= -c(KW_Merged))
dim(Europe_DB_Title_Extra)
save(Europe_DB_Title_Extra, file=here("Searches", "Europe_DB_Title_Extra.rda"))


#Abstract
Europe_DB_Abstract_Extra = FullDataset[FALSE,]
for (i in 1:length(search_words)){
  Country = FullDataset[grepl(search_words[i], FullDataset$AB), ]
  print(dim(Country))
  Europe_DB_Abstract_Extra= rbind(Country, Europe_DB_Abstract_Extra)
}
Europe_DB_Abstract_Extra$CR <- NA
Europe_DB_Abstract_Extra = mergeDbSources(Europe_DB_Abstract_Extra, remove.duplicated = TRUE)
Europe_DB_Abstract_Extra = subset(Europe_DB_Abstract_Extra, select= -c(KW_Merged))
dim(Europe_DB_Abstract_Extra)
save(Europe_DB_Abstract_Extra, file=here("Searches", "Europe_DB_Abstract_Extra.rda"))
write.xlsx(Europe_DB_Abstract_Extra, file = here("Searches", "Europe_DB_Abstract_Extra.xlsx"))

#Keywords
Europe_DB_Keywords_Extra = FullDataset[FALSE,]
for (i in 1:length(search_words)){
  Country = FullDataset[grepl(search_words[i], FullDataset$DE), ]
  print(dim(Country))
  Europe_DB_Keywords_Extra= rbind(Country, Europe_DB_Keywords_Extra)
}
Europe_DB_Keywords_Extra$CR <- NA
Europe_DB_Keywords_Extra = mergeDbSources(Europe_DB_Keywords_Extra, remove.duplicated = TRUE)
Europe_DB_Keywords_Extra = subset(Europe_DB_Keywords_Extra, select= -c(KW_Merged))
dim(Europe_DB_Keywords_Extra)
save(Europe_DB_Keywords_Extra, file=here("Searches", "Europe_DB_Keywords_Extra.rda"))



## 4. Search or UK

#How to do this with the UK (e.g. Leukima comes up as UK)
load(file=here("Searches", "FullDataset.rda"))
UK_Title = FullDataset[grepl("\\bUK\\b", FullDataset$TI), ]
dim(UK_Title)
#add columns needed for later merge
UK_Title$CR <- NA
UK_Title$SR_FULL <- NA	
UK_Title$SR <- NA
save(UK_Title, file=here("Searches", "UK_Title.rda"))
write.xlsx(UK_Title, file = here("Searches", "UK_Title.xlsx"))

UK_Abstract = FullDataset[grepl("\\bUK\\b", FullDataset$AB), ]
dim(UK_Abstract)
save(UK_Abstract, file=here("Searches", "UK_Abstract.rda"))
write.xlsx(UK_Abstract, file = here("Searches", "UK_Abstract.xlsx"))


UK_Keywords = FullDataset[grepl("\\bUK\\b", FullDataset$DE), ]
dim(UK_Keywords)
save(UK_Keywords, file=here("Searches", "UK_Keywords.rda"))
write.xlsx(UK_Keywords, file = here("Searches", "UK_Keywords.xlsx"))



## 5. Search for EU
load(file=here("Searches", "FullDataset.rda"))
EU_Title = FullDataset[grepl("\\bEU\\b", FullDataset$TI), ]
dim(EU_Title)
#add columns needed for later merge
EU_Title$CR <- NA
EU_Title$SR_FULL <- NA	
EU_Title$SR <- NA
save(EU_Title, file=here("Searches", "EU_Title.rda"))
write.xlsx(EU_Title, file = here("Searches", "EU_Title.xlsx"))

EU_Abstract = FullDataset[grepl("\\bEU\\b", FullDataset$AB), ]
dim(EU_Abstract)
save(EU_Abstract, file=here("Searches", "EU_Abstract.rda"))
write.xlsx(EU_Abstract, file = here("Searches", "EU_Abstract.xlsx"))

#find manually and remove records that have not been conducted in the EU

# NOTE: The row numbers below were identified manually by reading each title
# and abstract in this dataset's EU search results, to exclude studies
# conducted outside Europe despite matching the search term "EU". These row
# numbers are tied to this specific dataset and search date, and will not
# necessarily point to the same records if the searches are rerun. Re-check
# the EU_Abstract results manually after rerunning the search, rather than
# reapplying these row numbers as is.

#3 conducted in Malaysia
EU_Abstract$TI[3]
EU_Abstract$AB[3]
#5 conducted in Iran
EU_Abstract$TI[5]
#10 conducted in Africa
EU_Abstract$TI[10]
#18 conducted in Malaysia
EU_Abstract$TI[18]
#19 conducted in Africa
EU_Abstract$TI[19]
#23 conducted in Korea
EU_Abstract$AB[24]
#26 conducted in Bangladesh
EU_Abstract$TI[27]

EU_Abstract = EU_Abstract[-c(3, 5, 10, 18, 19, 24, 27), ]
#add columns needed for later merge
EU_Abstract$CR <- NA
EU_Abstract$SR_FULL <- NA	
EU_Abstract$SR <- NA
save(EU_Abstract, file=here("Searches", "EU_Abstract.rda"))
write.xlsx(EU_Abstract, file = here("Searches", "EU_Abstract.xlsx"))
dim(EU_Abstract)


EU_Keywords = FullDataset[grepl("\\bEU\\b", FullDataset$DE), ]
dim(EU_Keywords)
save(EU_Keywords, file=here("Searches", "EU_Keywords.rda"))
write.xlsx(EU_Keywords, file = here("Searches", "EU_Keywords.xlsx"))


## 6. merge all Abstract/Title searches

load(file=here("Searches", "Europe_DB_Abstract.rda"))
dim(Europe_DB_Abstract)
load(file=here("Searches", "Europe_DB_Title.rda"))
dim(Europe_DB_Title)
load(file=here("Searches", "Europe_DB_Keywords.rda"))
dim(Europe_DB_Keywords)
load(file=here("Searches", "Europe_DB_Title_Pop.rda"))
dim(Europe_DB_Title_Pop)
load(file=here("Searches", "Europe_DB_Abstract_Pop.rda"))
dim(Europe_DB_Abstract_Pop)
load(file=here("Searches", "Europe_DB_Keywords_Pop.rda"))
dim(Europe_DB_Keywords_Pop)
load(file=here("Searches", "Europe_DB_Title_Extra.rda"))
dim(Europe_DB_Title_Extra)
load(file=here("Searches", "Europe_DB_Abstract_Extra.rda"))
dim(Europe_DB_Abstract_Extra)
load(file=here("Searches", "Europe_DB_Keywords_Extra.rda"))
dim(Europe_DB_Keywords_Extra)
load(file=here("Searches", "UK_Title.rda"))
dim(UK_Title)
load(file = here("Searches", "UK_Abstract.rda"))
dim(UK_Abstract)
load(file = here("Searches", "UK_Keywords.rda"))
dim(UK_Keywords)
load(file=here("Searches", "EU_Title.rda"))
dim(EU_Title)
load(file = here("Searches", "EU_Abstract.rda"))
dim(EU_Abstract)
load(file=here("Searches", "EU_Keywords.rda"))
dim(EU_Keywords)

Europe_Dataset = Europe_DB_Abstract[FALSE, ]
Europe_Dataset = rbind(Europe_DB_Abstract, Europe_DB_Title, Europe_DB_Keywords, Europe_DB_Title_Extra, 
                       Europe_DB_Abstract_Extra, Europe_DB_Keywords_Extra, Europe_DB_Title_Pop, Europe_DB_Abstract_Pop, Europe_DB_Keywords_Pop,
                       UK_Title, UK_Abstract, UK_Keywords, EU_Title, EU_Abstract, EU_Keywords)
dim(Europe_Dataset)

Europe_Dataset = mergeDbSources(Europe_Dataset, remove.duplicated = TRUE)

dim(Europe_Dataset)

save(Europe_Dataset, file=here("Searches", "Europe_Dataset.rda"))
write.xlsx(Europe_Dataset, file = here("Searches", "Europe_Dataset.xlsx"))


#### very unlikely that completely non-european authors are working on European studies. 
### Refine data to at least one author from Europe.

load(file=here("Searches", "Europe_Dataset.rda"))
head(Europe_Dataset)
dim(Europe_Dataset)

Europe_DB_Final = Europe_Dataset[FALSE,]
for (i in 1:length(European_Countries)){
  Country = Europe_Dataset[grepl(European_Countries[i], Europe_Dataset$AU_CO), ]
  print(dim(Country))
  Europe_DB_Final= rbind(Country, Europe_DB_Final)
}
dim(Europe_DB_Final)
Europe_Dataset = mergeDbSources(Europe_DB_Final, remove.duplicated = TRUE)



dim(Europe_Dataset)
save(Europe_Dataset, file=here("Searches", "Europe_Dataset.rda"))
write.xlsx(Europe_Dataset, file = here("Searches", "Europe_Dataset.xlsx"))




############ Correct some languages  ##########################
#### double check output of ROMANIAN;MOLDAVIAN;MOLDOVAN   #####


Europe_Dataset <- Europe_Dataset %>%
  dplyr::mutate(LA = dplyr::if_else(LA == "ROMANIAN;MOLDAVIAN;MOLDOVAN", "ROMANIAN", LA))

# NOTE: the Language field was blank for these records. Manually checked
# each one and confirmed they were Bulgarian, so relabelled here.
Europe_Dataset <- Europe_Dataset %>%
  dplyr::mutate(LA = dplyr::if_else(LA == "", "BULGARIAN", LA))

save(Europe_Dataset, file = here("Searches", "Europe_Dataset.rda"))
write.xlsx(Europe_Dataset, file = here("Searches", "Europe_Dataset.xlsx"))