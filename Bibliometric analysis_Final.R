# ##############################################################################
# # Bibliometric Analysis: Rural Cancer - European Dataset
# # Author: Tanja Kleinhappel
# # Date: August 2025
# # Description: This script performs a comprehensive bibliometric analysis
# #              on a European dataset related to rural cancer research.
# #              It covers data completeness, descriptive analysis, publication
# #              trends, country-level productivity, and
# #              adjustments for population and GDP.
# ##############################################################################

# --- Setup: Package Management ---

# Install and load required packages.
# This block checks if packages are installed and installs them if missing,
# making the script more robust for new users.

# List of all packages required for the script
packages_required <- c(
  "bibliometrix", "shiny", "openxlsx", "tidyverse", "ggplot2",
  "data.table", "reshape2", "RColorBrewer", "plyr", "tidyr",
  "igraph", "ggpubr", "magick", "scales", "ggpattern", "maps",
  "ggrepel", "rnaturalearth", "rnaturalearthdata", "here", "cowplot", "patchwork"
)

# Identify and install missing packages
new_packages <- packages_required[!(packages_required %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, dependencies = TRUE)
}

# Load all required packages silently
invisible(lapply(packages_required, library, character.only = TRUE))

# Clear all objects from the current workspace to ensure a clean environment
rm(list = ls())



# --- Data Loading and Initial Inspection ---

# Load full dataset using here::here() for robust path management.
message("Attempting to load Europe_Dataset.rda...")
tryCatch({
  load(here::here("Searches", "Europe_Dataset.rda"))
  message("Europe_Dataset.rda loaded successfully.")
}, error = function(e) {
  stop("Error loading Europe_Dataset.rda. Please ensure the file is in 'EuropeDataAnalysis/Searches/' relative to your project root. Details: ", e$message)
})

# Display a quick overview of the loaded data
message("\n--- Initial Data Inspection ---")
print(head(Europe_Dataset))
message("\nDimensions of FullDataset:")
print(dim(Europe_Dataset))


# --- Data Completeness Assessment ---

message("\n--- Data Completeness ---")
data_completeness <- bibliometrix::missingData(Europe_Dataset)
print(data_completeness)


# --- Descriptive Analysis: Main Bibliometric Measures ---

message("\n--- Descriptive Analysis: Main Bibliometric Measures ---")
# Perform bibliometric analysis of the dataset
results_europe <- bibliometrix::biblioAnalysis(Europe_Dataset, sep = ";")

# Obtain summary of the results
summary_europe <- summary(object = results_europe, k = 20, pause = FALSE)


# Get frequency and percentage for languages
message("\n--- Language Summary ---")
language_summary <- Europe_Dataset %>%
  dplyr::group_by(LA) %>% # LA is the Language field
  dplyr::summarise(Frequency = n(), .groups = 'drop') %>% # '.groups = 'drop'' prevents "ungrouping" messages
  dplyr::mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%
  dplyr::arrange(desc(Frequency))
print(language_summary)



# --- Document Age Statistics ---

message("\n--- Document Age Statistics ---")
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Calculate the age of each document, converting publication year robustly
document_ages <- current_year - as.numeric(Europe_Dataset$PY)
document_ages <- na.omit(document_ages) # Remove NAs if publication year is missing

# Calculate and print document age statistics
if (length(document_ages) > 0) {
  age_summary_stats <- quantile(document_ages, probs = c(0.25, 0.5, 0.75))
  
  cat("Q1:", age_summary_stats["25%"], "\n")
  cat("Median:", age_summary_stats["50%"], "\n")
  cat("Q3:", age_summary_stats["75%"], "\n")
  cat("IQR (Q3-Q1):", IQR(document_ages), "\n") # Using direct IQR function
  cat("Mean:", mean(document_ages), "\n")
  cat("Standard Deviation:", sd(document_ages), "\n\n")
} else {
  warning("No valid document ages found to calculate statistics.")
}


# --- Citations per Document Statistics ---

message("\n--- Citations per Document Statistics ---")
# Extract citations and handle NAs, converting robustly
citations_per_document <- as.numeric(Europe_Dataset$TC) # TC is Times Cited
citations_per_document <- na.omit(citations_per_document)

# Calculate and print citation statistics
if (length(citations_per_document) > 0) {
  citations_summary_stats <- quantile(citations_per_document, probs = c(0.25, 0.5, 0.75))
  
  cat("Q1:", citations_summary_stats["25%"], "\n")
  cat("Median:", citations_summary_stats["50%"], "\n")
  cat("Q3:", citations_summary_stats["75%"], "\n")
  cat("IQR (Q3-Q1):", IQR(citations_per_document), "\n")
  cat("Mean:", mean(citations_per_document), "\n")
  cat("Standard Deviation:", sd(citations_per_document), "\n\n")
} else {
  warning("No valid citation data found to calculate statistics.")
}


# --- Citations per Year per Document ---

message("\n--- Citations per Year per Document ---")
# Select relevant columns and convert to numeric, then calculate citations per year
document_data_citations <- Europe_Dataset %>%
  dplyr::select(PY, TC) %>%
  dplyr::mutate(
    PY = as.numeric(PY),
    TC = as.numeric(TC)
  ) %>%
  na.omit() %>% # Remove rows with NA in PY or TC
  dplyr::mutate(
    # Add 1 to age to ensure a minimum age of 1, preventing division by zero
    # and properly accounting for the publication year itself.
    Age = current_year - PY + 1,
    Age = pmax(Age, 1), # Ensure age is at least 1
    CitationsPerYear = TC / Age
  )

if (nrow(document_data_citations) > 0) {
  mean_citations_per_year <- mean(document_data_citations$CitationsPerYear)
  sd_citations_per_year <- sd(document_data_citations$CitationsPerYear)
  
  cat("Mean Citations per Year per Document:", round(mean_citations_per_year, 2), "\n")
  cat("SD Citations per Year per Document:", round(sd_citations_per_year, 2), "\n")
} else {
  warning("Not enough data to calculate Citations per Year per Document.")
}



# --- Additional Analysis: Growth Rate of Papers ---

message("\n--- Annual Publication Growth Rate ---")

# Access the AnnualProduction table from bibliometrix summary
annual_production_data <- summary_europe$AnnualProduction

# Ensure the 'Year' column name is cleaned immediately after assignment
names(annual_production_data)[names(annual_production_data) == "Year   "] <- "Year" # Remove extra spaces in column name

# Prepare data for growth rate calculation: ensure Year is numeric and filter out 0 articles
growth_data_filtered <- annual_production_data %>%
  dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
  dplyr::filter(Articles > 0)

if (nrow(growth_data_filtered) >= 2) { # Need at least two data points for growth calculation
  # Linear regression for exponential growth (log-linear model)
  growth_data_filtered <- growth_data_filtered %>%
    dplyr::mutate(
      Time = Year - min(Year), # Time variable starting from 0 for regression
      Log_N_Publications = log(Articles)
    )
  
  model_growth <- lm(Log_N_Publications ~ Time, data = growth_data_filtered)
  
  message("\nRegression Model Summary (Log-linear growth):")
  print(summary(model_growth))
  
  # Manual Compound Annual Growth Rate (CAGR) Calculation
  first_year_data <- min(growth_data_filtered$Year)
  last_year_data <- max(growth_data_filtered$Year)
  
  beginning_value <- growth_data_filtered$Articles[growth_data_filtered$Year == first_year_data]
  ending_value <- growth_data_filtered$Articles[growth_data_filtered$Year == last_year_data]
  
  number_of_periods <- last_year_data - first_year_data
  
  # Handle edge cases for CAGR calculation (e.g., beginning_value being zero)
  if (beginning_value == 0) {
    cagr_manual <- NA
    warning("Beginning value is 0. CAGR cannot be calculated.")
  } else if (number_of_periods <= 0) {
    cagr_manual <- NA
    warning("Not enough periods to calculate CAGR (need at least two years).")
  } else {
    cagr_manual <- (ending_value / beginning_value)^(1 / number_of_periods) - 1
  }
  
  cagr_percentage <- cagr_manual * 100
  
  cat("\n--- Manual Compound Annual Growth Rate (CAGR) ---\n")
  cat("Period:", first_year_data, "to", last_year_data, "\n")
  cat("Publications in First Year:", beginning_value, "\n")
  cat("Publications in Last Year:", ending_value, "\n")
  cat("Number of Periods (Years):", number_of_periods, "\n")
  cat("Calculated CAGR:", round(cagr_percentage, 2), "%\n")
  
} else {
  warning("Not enough data (less than 2 years with publications) to calculate meaningful growth rates.")
}



# --- Additional analysis: Year-normalised impact (MNCS) and its relationship with international collaboration (MCP) ---

message("\n--- Year-normalised impact (MNCS) and collaboration (MCP) ---")

# --- 1) Compute per-paper NCS (Year-Normalized Citation Score) ---
# Expected citations for year y = mean(TC) among papers published in y in your dataset.
NCS_docs <- Europe_Dataset %>%
  dplyr::mutate(PY = as.numeric(PY),
                TC = as.numeric(TC)) %>%
  dplyr::filter(!is.na(PY)) %>%
  dplyr::group_by(PY) %>%
  dplyr::mutate(expected_TC_year = mean(TC, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(NCS_global = ifelse(expected_TC_year > 0, TC / expected_TC_year, NA_real_))


# --- 2) Aggregate to country (Mean Normalized Citation Score - MNCS) ---
MNCS_country <- NCS_docs %>%
  # CLEANING STEPS (Crucial for correct grouping)
  dplyr::mutate( 
    AU1_CO = as.character(AU1_CO),
    AU1_CO = stringr::str_squish(AU1_CO),
    AU1_CO = stringr::str_to_upper(AU1_CO) # Standardize all country names
  ) %>%
  dplyr::filter(!is.na(AU1_CO), AU1_CO != "", AU1_CO != "NA") %>%
  
  # GROUP AND SUMMARISE
  dplyr::group_by(AU1_CO) %>% 
  dplyr::summarise( 
    MNCS = mean(NCS_global, na.rm = TRUE),
    MNCS_median = median(NCS_global, na.rm = TRUE),
    MNCS_iqr = IQR(NCS_global, na.rm = TRUE),
    MNCS_sd = sd(NCS_global, na.rm = TRUE),
    Total_Docs = dplyr::n() 
  ) %>%
  dplyr::ungroup()


# --- 3) Calculate MCP Ratio for ALL Corresponding Author Countries (Robust Join) ---
# Assuming 'results_europe' holds the bibliometrix output
MCP_df <- results_europe$Country 

# Calculate the MCP Ratio (MCP / (SCP + MCP))
MCP_df <- MCP_df %>%
  dplyr::mutate(
    Total_Articles = SCP + MCP,
    MCP_Ratio = MCP / Total_Articles 
  ) %>%
  dplyr::select(Country, MCP_Ratio, Articles = Total_Articles) %>% 
  dplyr::rename(AU1_CO = Country)


# --- 4) Merge DataFrames and Define Stability Filter ---
anal_df <- MNCS_country %>%
  # LEFT_JOIN to keep all MNCS data and fill in MCP data where available
  dplyr::left_join(MCP_df, by = c("AU1_CO")) %>%
  
  # Remove countries where the MCP data was missing/zero
  dplyr::filter(!is.na(MCP_Ratio)) %>% 
  
  # Define stability filter (20 outputs is the threshold for a stable MNCS)
  dplyr::mutate(enough_outputs = Total_Docs >= 20)


# --- 5) Correlation (Spearman; robust to skew) ---
# Correlation across all countries
ct_all  <- cor.test(anal_df$MCP_Ratio, anal_df$MNCS, method = "spearman", exact = FALSE)

# Correlation for stable research portfolios (>=20 outputs)
ct_20p  <- cor.test(anal_df$MCP_Ratio[anal_df$enough_outputs],
                    anal_df$MNCS[anal_df$enough_outputs],
                    method = "spearman", exact = FALSE)

# Correlation between Volume (Outputs) and Quality (MNCS)
ct_volume <- cor.test(anal_df$outputs, anal_df$MNCS, method = "spearman", exact = FALSE)


# --- 6) Output Results ---
cat("\n--- Spearman Correlation Results ---\n")
cat(sprintf("Spearman rho (All countries):   %.3f, p = %.4f\n", ct_all$estimate,  ct_all$p.value))
cat(sprintf("Spearman rho (>=20 outputs):    %.3f, p = %.4f\n", ct_20p$estimate,  ct_20p$p.value))
cat(sprintf("Spearman rho (Volume vs. MNCS): %.3f, p = %.4f\n", ct_volume$estimate, ct_volume$p.value))               




# ##############################################################################
# #               Publication Trends
# ##############################################################################

# --- Annual Scientific Production ---

message("\n--- Annual Scientific Production ---")

# Load global cancer research publications from Scopus
# Use here::here() for robust path management
message("Attempting to load ScopusCancerArticles.csv...")
tryCatch({
  CancerArticles <- readr::read_csv(here::here("ScopusCancerArticles.csv"), show_col_types = FALSE)
  message("ScopusCancerArticles.csv loaded successfully.")
}, error = function(e) {
  stop("Error loading ScopusCancerArticles.csv. Please ensure the file is in 'EuropeDataAnalysis/' relative to your project root. Details: ", e$message)
})
print(head(CancerArticles))


# Extract annual production data from summary and clean column name
AnnualProduction <- summary_europe$AnnualProduction
names(AnnualProduction)[names(AnnualProduction) == "Year   "] <- "Year" # Remove extra spaces in column name
print(head(AnnualProduction))

# Merge with all cancer data, filling NA values with 0 where no publications exist
# Remove articles before 1940 from the cancer data as the Europe dataset only starts in 1940
CancerArticles <- CancerArticles %>%
  filter(Year >= 1940)

AnnualProduction <- merge(CancerArticles, AnnualProduction, by = "Year", all = TRUE)
AnnualProduction[is.na(AnnualProduction)] <- 0
print(head(AnnualProduction))

# Convert the 'Year' column to a date format and add it to the dataframe.
# Also, calculate the scaled values for the secondary axis here.
AnnualProduction <- AnnualProduction %>%
  mutate(YearAsDate = as.Date(ISOdate(Year, 1, 1))) %>%
  mutate(CancerArticles_Scaled = CancerArticles * max(.$Articles, na.rm = TRUE) / max(.$CancerArticles, na.rm = TRUE))


# Set min, max, and breaks for the x-axis to make the plot scale reusable.
min_date <- as.Date("1940-01-01")
max_date <- as.Date("2025-01-01")
date_breaks <- as.Date(c("1940-01-01", "1954-01-01", "1964-01-01", "1974-01-01",
                         "1984-01-01", "1994-01-01", "2004-01-01", "2014-01-01",
                         "2025-01-01"))


# Initialise the plot with the main data frame.
ProdAndCancerPlot <- ggplot(data = AnnualProduction, aes(x = YearAsDate)) +
  # Add geom_line for the main 'Total Articles'
  geom_line(aes(y = Articles), color = "black", linewidth = 0.6) +
  # Add geom_line for the secondary 'Total Cancer Articles'.
  # The 'CancerArticles_Scaled' column is used for the y-aesthetic.
  geom_line(aes(y = CancerArticles_Scaled), color = "black", linewidth = 0.6, linetype = "dashed") +
  # Set axis scales and labels.
  scale_x_date(limits = c(min_date, max_date),
               guide = guide_axis(angle = 45),
               breaks = date_breaks,
               date_labels = "%Y") +
  scale_y_continuous(name = "Total Articles",
                     limits = c(0, 100),
                     breaks = seq(0, 100, by = 20),
                     sec.axis = sec_axis(~ . * max(AnnualProduction$CancerArticles, na.rm = TRUE) / max(AnnualProduction$Articles, na.rm = TRUE),
                                         name = "Total Cancer Articles")) +
  labs(x = "Year") +
  # Apply a classic theme and customize specific elements.
  theme_classic(base_size = 16) +
  theme(axis.text.y = element_text(color = 'black'),
        axis.title.y = element_text(color = 'black'),
        axis.text.y.right = element_text(color = 'black'),
        axis.title.y.right = element_text(color = 'black'),
        axis.line.y.right = element_line(color = 'black'),
        axis.ticks.y.right = element_line(color = 'black'),
        legend.position = "none") +
  # Use coord_cartesian to control the plot area.
  coord_cartesian(clip = "off")


# save the plot
ggsave(here::here("PlotsEurope", "ProdAndCancerPlot.png"),
       plot = ProdAndCancerPlot,
       width = 7.5,
       height = 4,
       dpi = 1200)

# save the plot as a PDF
ggsave(here::here("PlotsEurope", "ProdAndCancerPlot.pdf"),
       plot = ProdAndCancerPlot,
       width = 7.5, 
       height = 4)


# --- Average citations per year ---

message("\n--- Average Citations per Year ---")

# Extract years and total citations per year data
PubYear <- Europe_Dataset$PY
TCperYear <- results_europe$TCperYear

# Calculate the mean of total citations per year for each year
MeanAnnualCitations <- aggregate(TCperYear, list(PubYear), FUN = mean)
names(MeanAnnualCitations)[names(MeanAnnualCitations) == "Group.1"] <- "Year"
names(MeanAnnualCitations)[names(MeanAnnualCitations) == "x"] <- "MeanCitations"
MeanAnnualCitations$YearAsDate <- as.Date(ISOdate(MeanAnnualCitations$Year, 1, 1))
print(head(MeanAnnualCitations))

# Plot average annual citations
AvAnnualCitationsPlot <- ggplot(data = MeanAnnualCitations, aes(x = YearAsDate, y = MeanCitations, group = 1)) +
  geom_line(color = "black", linewidth = 0.6) +
  #geom_point(size = 1.0) +
  theme_classic(base_size = 16) +
  labs(x = "Year", y = "Average citations per year") +
  scale_x_date(limits = c(min_date, max_date), guide = guide_axis(angle = 45),
               breaks = date_breaks, date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1))

# Save the plot using here::here()
ggsave(here::here("PlotsEurope", "AvAnnualCitationsPlot.png"),
       plot = AvAnnualCitationsPlot,
       width = 7.5,
       height = 4,
       dpi = 1200)


# --- Combine graphs into a single publication trends plot ---

message("\n--- Combining Publication Trend Plots ---")

# This ensures your (a), (b), etc. tags are large and bold.
global_tag_style <- theme(
  plot.tag = element_text(size = 14)#, face = "bold")
)

# 2. Combine the figures
CombinedAnnualFigures <- (
  ProdAndCancerPlot / AvAnnualCitationsPlot + 
    plot_annotation(
      tag_levels = "a",
      tag_prefix = "(",
      tag_suffix = ")"
    )
) & global_tag_style

# 3. Save the final object
ggsave(here::here("PlotsEurope", "Combined_Annual_Plot.png"),
       plot = CombinedAnnualFigures,
       width = 10,
       height = 8, # Adjusted height for two vertically stacked plots
       dpi = 1200)


# 3. Save the final object as a PDF
ggsave(here::here("PlotsEurope", "Combined_Annual_Plot.pdf"),
       plot = CombinedAnnualFigures,
       width = 10,
       height = 8)




# ##############################################################################
# #               Who is publishing? (Country Analysis)
# ##############################################################################

# --- Most Productive Countries ---

message("\n--- Most Productive Countries ---")

# Extract most productive countries from Summary data
MostProdCountries <- summary_europe$MostProdCountries

# 1. CLEAN DATA & ADD LABEL TEXT
MostProdCountries <- data.frame(
  Country = as.character(MostProdCountries$Country), # Keep as char temporarily
  Articles = as.numeric(MostProdCountries$Articles),
  SCP = as.numeric(MostProdCountries$SCP), 
  MCP = as.numeric(MostProdCountries$MCP), 
  MCP_Ratio = as.numeric(MostProdCountries$MCP_Ratio)
) %>%
  mutate(
    Country = str_trim(Country),
    # Convert to Title Case *after* trimming
    Country = str_to_title(Country)
  ) %>%
  mutate(
    Country = case_when(
      Country == "Usa" ~ "USA", # Specific fix for title case conversion
      Country == "United Kingdom" ~ "UK",
      TRUE ~ Country
    )
  ) %>%
  # Create the combined label for the end of the bar: "Total [Ratio]"
  mutate(
    Formatted_Ratio = format(round(MCP_Ratio, 2), nsmall = 2),
    label_text = paste0(Articles, " [", Formatted_Ratio, "]")
  )

# Reshape data from wide to long format for geom_col
longMostProdCountries <- reshape2::melt(data.table::setDT(MostProdCountries),
                                        id.vars = c("Articles", "Country", "MCP_Ratio", "label_text", "Formatted_Ratio"),
                                        variable.name = "Type")

print(head(longMostProdCountries))

MostProdCountriesPlot <- ggplot(longMostProdCountries, aes(x = value, y = reorder(Country, +value))) +
  geom_col(aes(fill = Type), width = 0.7) +
  geom_text(
    data = MostProdCountries, 
    aes(x = Articles, y = Country, label = label_text), 
    hjust = -0.1, # Pushes the label just outside the end of the bar
    size = 4
    # Removed fontface = "bold"
  ) +
  scale_fill_manual(values = c("black", "lightblue")) +
  # Axis and Labels
  labs(x = "Total Articles", y = "Country") +
  scale_x_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 50)) + 
  # Theme Settings
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_text(margin = margin(r = -20)),
    axis.text.y = element_text(hjust = 1, margin = margin(r = 1)), # Use negative margin
    # Legend Settings
    legend.position = c(0.8, 0.2), 
    legend.background = element_rect(fill = alpha("white", 0.9), colour = "black"),
    legend.title = element_blank()
  )

# Check the plot
print(MostProdCountriesPlot)

# Save plot using here::here()
ggsave(here::here("PlotsEurope", "MostProdCountriesPlot.png"),
       plot = MostProdCountriesPlot,
       width = 10,
       height = 5,
       dpi = 1200)

# Save plot using here::here() as a PDF
ggsave(here::here("PlotsEurope", "MostProdCountriesPlot.pdf"),
       plot = MostProdCountriesPlot,
       width = 10,
       height = 5)


# --- Country Scientific Production for All Authors (Cumulative & World Map) ---

message("\n--- Country Scientific Production (All Authors) ---")

# Process Europe_Dataset to extract countries and associate with years
CountriesOverTime_Initial <- Europe_Dataset %>%
  mutate(
    # Ensure PY (Years) is numeric at the source
    Years = as.numeric(PY),
    # Split AU_CO (AllCountries string) into a list of country names for each row
    listCountries = str_split(AU_CO, ";")
  ) %>%
  # Add a unique row ID for later processing
  rowid_to_column(var = "record_identifier") %>%
  # Select only the columns needed for the next step: unique ID, year, and country list
  select(ID, Years, listCountries) %>%
  # Unnest the listCountries, creating a new row for each country in the list.
  # This efficiently transforms the data to a long format early.
  unnest(listCountries) %>%
  # Rename the unnested column to 'Countries'
  dplyr::rename(Countries = listCountries) %>%
  # Remove any rows where Countries is empty or NA (from empty strings after split)
  filter(!is.na(Countries) & Countries != "") %>%
  # Trim whitespace from country names if any
  mutate(Countries = str_trim(Countries))


# --- Aggregate and Calculate Cumulative Sum ---

# Group by Years & Countries and count occurrences
# Using `dplyr::summarise` explicitly to prevent masking issues
CountriesOverTime_Final <- CountriesOverTime_Initial %>%
  group_by(Years, Countries) %>%
  dplyr::summarise(total_count = n(), .groups = "drop")

# Ensure the full sequence of years for each country for consistent plotting
# Get min and max years from the aggregated data to define the sequence
min_year_data <- min(CountriesOverTime_Final$Years, na.rm = TRUE)
max_year_data <- max(CountriesOverTime_Final$Years, na.rm = TRUE)

# Years span 1940-2025 to ensure consistent plotting across all countries.
CountriesOverTime_Final <- CountriesOverTime_Final %>%
  group_by(Countries) %>%
  # Use `complete` to fill in missing years for each country, setting total_count to 0
  complete(Years = full_seq(1940:2025, 1), fill = list(total_count = 0)) %>%
  ungroup() %>% # Ungroup after completion
  # Calculate cumulative count of articles for each country over the years
  group_by(Countries) %>%
  mutate(cumulative_sum = cumsum(total_count)) %>%
  ungroup() # Final ungroup

# --- Identify Top Countries ---

# Find the maximum cumulative articles for each country to identify top performers
CountriesArticles <- CountriesOverTime_Final %>%
  group_by(Countries) %>%
  dplyr::summarise(Articles = max(cumulative_sum, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Articles)) # Arrange in descending order of articles



# Identify the top 6 countries from the 'CountriesArticles' data frame.
top6Countries <- head(CountriesArticles$Countries, 6)

# Filter the main data to include only the top 6 countries and add a date column.
# *** Apply str_to_title() to 'Countries' here ***
Top6Countries <- CountriesOverTime_Final %>%
  filter(Countries %in% top6Countries) %>%
  mutate(YearAsDate = as.Date(ISOdate(Years, 1, 1))) %>%
  mutate(Countries = str_to_title(Countries)) %>% # Capitalize first letter of each word
  # Change "United Kingdom" to "UK"
  mutate(
    Countries = if_else(
      Countries == "United Kingdom", 
      "UK", 
      Countries
    )
  ) 

# Display the head of the new data frame for verification.
print(head(Top6Countries))
print(paste("Top 6 Countries:", paste(str_to_title(top6Countries), collapse = ", "))) # Also update for print


# Define the breaks for the x-axis, consistent with previous plots.
date_breaks <- as.Date(c("1940-01-01", "1954-01-01", "1964-01-01", "1974-01-01",
                         "1984-01-01", "1994-01-01", "2004-01-01", "2014-01-01",
                         "2025-01-01"))
min_date <- as.Date("1940-01-01")
max_date <- as.Date("2025-01-01")

# Define the color palette for the lines.
my_palette <- brewer.pal(name = "Dark2", n = 8)[1:8]


CountriesProdTimePlot <- ggplot(data = Top6Countries, aes(x = YearAsDate, y = cumulative_sum,
                                                          group = Countries, color = Countries)) +
  # Add geom_line layer for the data
  geom_line(linewidth = 1) + # Added linewidth for better visibility
  # Set axis labels and titles
  labs(x = "Year", y = "Total Articles") +
  # Apply classic theme and customize legend box
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_text(margin = margin(r = -5)),
    # Move legend inside plot (x=0.05 is left, y=0.95 is top)
    # Adjust these numbers (0-1) to move the box around
    legend.position = c(0.15, 0.75), 
    # Create the white box with black border
    legend.background = element_rect(fill = alpha("white", 0.7), colour = "black"),
    # Legend styling
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.spacing.y = unit(0.2, "cm"),
    # Ensure plot text is legible
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  # Set x-axis properties
  scale_x_date(breaks = date_breaks,
               limits = c(min = min_date, max = max_date),
               date_labels = "%Y") +
  # Set y-axis properties
  scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, by = 100)) +
  # Manually set colors
  scale_color_manual(values = my_palette) +
  # Set legend to 1 column (vertical list)
  guides(color = guide_legend(ncol = 1))

# Save plot using here::here()
ggsave(here::here("PlotsEurope", "CountriesProdTimePlot.png"),
       plot = CountriesProdTimePlot,
       width = 10,
       height = 5,
       dpi = 1200)

# Save plot as a PDF
ggsave(here::here("PlotsEurope", "CountriesProdTimePlot.pdf"),
       plot = CountriesProdTimePlot,
       width = 10,
       height = 5)


# --- Combine graphs into a single plot ---

# 1. Define the global theme fix elements outside the patchwork call
global_tag_style <- theme(
  plot.tag = element_text(size = 14)#, face = "bold")
)

# 2. Combine the figures:
CombinedFigure <- (
  MostProdCountriesPlot / CountriesProdTimePlot +
    plot_annotation(
      tag_levels = "a",
      tag_prefix = "(",
      tag_suffix = ")"
    )
) & global_tag_style


# 3. Save the final object
ggsave(here::here("PlotsEurope", "Combined_Europe_Plot.png"),
       plot = CombinedFigure,
       width = 10,
       height = 9,
       dpi = 1200)

# 3. Save the final object as a PDF
ggsave(here::here("PlotsEurope", "Combined_Europe_Plot.pdf"),
       plot = CombinedFigure,
       width = 10,
       height = 9)



### Plot Europe Map with Total Country Production (All Authors) ###

message("\n--- Europe Map: Total Country Production (All Authors) ---")

# --- 1. Load Data and Initial Data Preparation ---
# ===============================================

# Load article data and rename columns using a pipe for clarity
# Also, apply title casing for country names
# Load map data for Europe
europe_map <- ne_countries(continent = "Europe", scale = "medium", returnclass = "sf")

# Load and clean article data
Articles_Countries_DF <- CountriesArticles %>%
  dplyr::rename(Country = Countries) %>%
  mutate(Country = str_to_title(Country)) %>%
  # Update: Standardize country names to match map data
  mutate(Country = case_when(
    Country == "Usa" ~ "United States",
    Country == "United Kingdom" ~ "United Kingdom",
    Country == "Czech Republic" ~ "Czechia",
    Country == "Russian Federation" ~ "Russia",
    Country == "Bosnia And Herzegovina" ~ "Bosnia and Herz.",
    Country == "Faroe Islands" ~ "Faeroe Is.",
    Country == "North Macedonia" ~ "North Macedonia",
    TRUE ~ Country
  ))

# Load and clean adjustment data, filtering to European countries
Df_adjustments <- read.csv(here("TotalPopulation.csv"), header = TRUE) %>%
  full_join(read.csv(here("GDP per Capita.csv")), by = "Country") %>%
  full_join(read.csv(here("RuralPop.csv")), by = "Country") %>%
  # Standardize names to match article data before joining
  mutate(
    Country = case_when(
      #Country == "Czechia" ~ "Czech Republic",
      Country == "Russian Federation" ~ "Russia",
      Country == "Slovak Republic" ~ "Slovakia",
      TRUE ~ Country
    )
  ) %>%
  filter(Country %in% unique(Articles_Countries_DF$Country))

# Join the article data with the cleaned adjustment data
Articles_Countries_DF <- Articles_Countries_DF %>%
  full_join(Df_adjustments, by = "Country") %>%
  filter(Country %in% unique(europe_map$name)) %>%
  mutate(
    RuralPop = as.numeric(as.character(RuralPop)),
    RuralPop_NA = ifelse(RuralPop == 0, NA, RuralPop)
  )

# Keep only European countries in the map data for a cleaner plot
europe_map_filtered <- europe_map %>%
  filter(name %in% unique(Articles_Countries_DF$Country))

# 2. Calculate Adjusted Variables
# ===============================

Articles_Countries_DF <- Articles_Countries_DF %>%
  mutate(
    # Articles adjusted for Total Population (per million people)
    adjArticlesPop = (Articles / TotalPopulation) * 1000000,
    # Articles adjusted for Rural Population (per 100,000 rural people)
    adjArticlesRuralPop = (Articles / RuralPop_NA) * 100000,
    # Articles adjusted for GDP per Capita (per $100 of GDP per Capita)
    adjArticlesGDPcapita = (Articles / GDPcapita) * 100
  )


# 3. Define Plotting Functions
# ============================

# Define a function for the common map theme
map_theme <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", color = "lightgray"),
      legend.key.size = unit(1, "cm"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      plot.subtitle = element_text(size = 14)
    )
}

# Define a reusable function to create each map
create_europe_map <- function(data, fill_var, legend_title, subtitle) {
  # Merge article data with the European map data
  europe_data <- europe_map %>%
    left_join(data, by = c("name" = "Country"))
  
  ggplot(data = europe_data) +
    geom_sf(aes(fill = {{fill_var}})) +
    # Use a clear color gradient for the non-log scale
    scale_fill_gradient(
      low = "lightskyblue1", high = "navy", name = legend_title,
      na.value = "gray75" # Change NA color to a lighter gray
    ) +
    # Zoom in on Europe's coordinates
    coord_sf(xlim = c(-25, 60), ylim = c(35, 75)) +
    # Add a subtitle for the panel label
    #labs(subtitle = subtitle) +
    map_theme()
}

# 4. Generate and Save Individual Plots
# =====================================

# Generate all four maps using the new function
map_a <- create_europe_map(Articles_Countries_DF, Articles, "Articles")
map_b <- create_europe_map(Articles_Countries_DF, adjArticlesPop, "Articles per\nmillion people")
map_c <- create_europe_map(Articles_Countries_DF, adjArticlesRuralPop, "Articles per\n100,000 rural\npeople")
map_d <- create_europe_map(Articles_Countries_DF, adjArticlesGDPcapita, "Articles per\n$100 GDP\nper Capita")


# 5. Combine and Save Final Figure
# ================================

# 1. Define the global theme object for consistent plot tag styling.
global_tag_style <- theme(
  plot.tag = element_text(size = 16)#, face = "bold")
)

# 2. Combine the four maps into a 2x2 grid:
combined_map <- (
  (map_a | map_b) / 
    (map_c | map_d)
) + # Use '+' for the main plot structure (not '&')
  plot_annotation(
    tag_levels = 'a', # Start tags at (a), (b), (c), (d)
    tag_prefix = '(',
    tag_suffix = ')'
  ) & global_tag_style # Apply tag style globally

# 3. Save the final combined plot
ggsave(here::here("PlotsEurope", "combined_euro_maps.png"), 
       plot = combined_map,
       width = 12, 
       height = 10, 
       units = "in", 
       dpi = 1200)

# Save the maps as a PDF
ggsave(here::here("PlotsEurope", "combined_euro_maps.pdf"), 
       plot = combined_map,
       width = 12, 
       height = 10, 
       units = "in")



# ##############################################################################
# #               Affiliations
# ##############################################################################

# Data preparation for Most Relevant Affiliations
# Data preparation: Percentages based ONLY on the Top 20
MostRelevantAffiliations <- results_europe$Affiliations %>%
  as.data.frame() %>% 
  dplyr::rename(Affiliations = AFF, Articles = Freq) %>% 
  dplyr::filter(!is.na(Affiliations) & Affiliations != "NA") %>% 
  dplyr::mutate(
    Affiliations = as.character(Affiliations) %>% stringr::str_trim(),
    Articles = as.numeric(Articles)
  ) %>%
  dplyr::arrange(desc(Articles)) %>% 
  dplyr::mutate(Affiliations = stringr::str_to_title(Affiliations)) %>%
  # Slice first so the total is limited to these 20
  dplyr::slice_head(n = 20) %>% 
  # Now calculate percentage based on the sum of these 20 rows
  dplyr::mutate(
    Percentage = Articles / sum(Articles),
    Percentage_Label = paste0(round(Percentage * 100, 1), "%")
  )
# Define a vector of replacements: Title Case Abbreviation -> Abbreviation.
# We are looking for the Title Case version that your code already generated.
abbreviation_replacements <- c(
  # Look for 'Univ' followed by a space (\s) OR the end of the string ($)
  "Univ(\\s|$)" = "Univ\\. ", 
  "Med(\\s|$)" = "Med\\. ",   
  "Coll(\\s|$)" = "Coll\\. ", 
  "Dept(\\s|$)" = "Dept\\. ", 
  "Inst(\\s|$)" = "Inst\\. ", 
  "Int(\\s|$)" = "Int\\. ",   
  "Agcy(\\s|$)" = "Agcy\\. ", 
  "Res(\\s|$)" = "Res\\. ",   
  "Canc(\\s|$)" = "Canc\\. "  
)

# Apply the replacements to the Affiliations column
MostRelevantAffiliations <- MostRelevantAffiliations %>%
  mutate(
    Affiliations = stringr::str_replace_all(Affiliations, abbreviation_replacements),
    # Add a final trim step to remove any extra spaces added at the end of the string.
    Affiliations = stringr::str_trim(Affiliations, side = "right")
  )


# Plotting the Most Relevant Affiliations
MostRelevantAffiliationsPlot <- ggplot(MostRelevantAffiliations, aes(x = Articles, y = reorder(Affiliations, Articles))) +
  geom_col(fill = "black") + # Create column chart with black bars
  geom_text(aes(label = Articles), color = "white", vjust = 0.5, hjust = 1.5) + # Add text labels inside/next to bars
  labs(x = "Articles", y = "Affiliations") + # Set axis labels
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) + # Define x-axis limits and breaks
  theme_classic(base_size = 20) + # Use a classic theme with a base font size
  theme(axis.text.y = element_text(hjust = 1)) # Adjust y-axis text justification

# Save the plot
ggsave(
  here("PlotsEurope", "MostRelevantAffiliationsPlot.eps"),
  plot = MostRelevantAffiliationsPlot, # Specify the plot object to save
  width = 8,
  height = 5,
  dpi = 1200
)

#### Affiliations over time for top 6

# 1. Data Preparation and Transformation
# =====================================

# Combine years and affiliations into a single data frame
AffOverTime <- data.frame(Years = as.numeric(Europe_Dataset$PY),
                          Affiliations = Europe_Dataset$AU_UN) %>%
  # Separate the affiliations
  mutate(Affiliations = str_split(Affiliations, ";", simplify = FALSE)) %>%
  # Reshape data to long format
  unnest(Affiliations) %>%
  # Remove leading/trailing whitespace
  mutate(Affiliations = str_trim(Affiliations)) %>%
  # Remove records where the affiliation is missing or empty
  filter(!is.na(Affiliations) & Affiliations != "") %>%
  # Group by years and affiliation to get article counts
  group_by(Years, Affiliations) %>%
  dplyr::summarise(total_count = n(), .groups = "drop")


# --- 2. FILL MISSING YEARS AND CALCULATE CUMULATIVE SUM (AffOverTime_Final) ---
AffOverTime_Final <- AffOverTime %>%
  group_by(Affiliations) %>%
  # Insert rows for missing years and fill with 0
  complete(Years = full_seq(1940:2025, 1), fill = list(total_count = 0)) %>%
  mutate(
    # Clean up weird spacing/non-standard characters
    Affiliations = stringr::str_squish(Affiliations), 
    # Force consistent capitalization (fixes ALL CAPS outliers)
    Affiliations = stringr::str_to_lower(Affiliations),
    Affiliations = stringr::str_to_title(Affiliations)
  ) %>%
  
  # Calculate the cumulative sum of articles
  mutate(cumulative_sum = cumsum(total_count)) %>%
  data.frame()

# 2. Select and Filter Data for Plotting
# ======================================

# 1. Get the names of the top 6 affiliations (based on Articles/Frequency)
# The names here are already Title Case and Period-Fixed from earlier steps.
top6Affs_with_periods <- head(MostRelevantAffiliations$Affiliations, 6)

# 2. CREATE A TEMPORARY, NORMALIZED LIST FOR FILTERING:
# This list is used as the 'perfect key' for matching.
top6Affs_normalized <- top6Affs_with_periods %>%
  stringr::str_remove_all("\\.\\s?") %>% 
  stringr::str_replace_all("[^A-Za-z0-9]", "") 


# 3. Filter AffOverTime_Final using the NORMALIZED key:
Top6Affiliations <- AffOverTime_Final %>%
  
  # Create a temporary, normalized key column in the data
  mutate(Affiliations_Normalized = Affiliations %>%
           # Since AffOverTime_Final was cleaned in the previous step,
           # these names should be ready to be normalized.
           stringr::str_replace_all("[^A-Za-z0-9]", "")) %>% 
  
  # Filter using the perfect normalized key
  filter(Affiliations_Normalized %in% top6Affs_normalized) %>%
  
  # Clean up temporary column
  select(-Affiliations_Normalized)

# 4. NOW, APPLY THE PERIOD CLEANING TO THE FILTERED DATA FRAME:
# This ensures the final output has the nice formatting, using the original names.
Top6Affiliations <- Top6Affiliations %>%
  mutate(
    Affiliations = stringr::str_replace_all(Affiliations, abbreviation_replacements),
    Affiliations = stringr::str_trim(Affiliations, side = "right")
  )

# 5. Continue with the year filtering (this section is robust):
firstPubTop6 <- Top6Affiliations[Top6Affiliations$cumulative_sum > 0, ]
firstYear <- min(firstPubTop6$Years) 

Top6Affiliations <- Top6Affiliations %>%
  filter(Years >= firstYear) %>%
  mutate(YearAsDate = as.Date(ISOdate(Years, 1, 1)))



# 3. Define Plot Variables and Theme
# ==================================

# Define the date limits and breaks for the x-axis
date_breaks <- as.Date(c("1975-01-01", "1985-01-01", "1995-01-01", "2005-01-01", 
                         "2015-01-01", "2025-01-01"))
min_date <- as.Date("1975-01-01")
max_date <- as.Date("2025-01-01")

# Define the color palette
my_palette <- brewer.pal(name = "Dark2", n = 8)[1:8]


# 4. Create the Plot
# ==================

AffProdTimePlot <- ggplot(data = Top6Affiliations, aes(x = YearAsDate, y = cumulative_sum,
                                                       group = Affiliations, color = Affiliations)) +
  geom_line() +
  labs(x = "Year", y = "Total Articles") +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.25, 0.8), 
        legend.background = element_rect(fill = alpha("white", 0.7), colour = "black"),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.spacing.y = unit(0.2, "cm")) + 
  scale_x_date(breaks = date_breaks,
               limits = c(min(Top6Affiliations$YearAsDate), max(Top6Affiliations$YearAsDate)),
               guide = guide_axis(angle = 45),
               date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  scale_color_manual(values = my_palette) +
  guides(color = guide_legend(ncol = 1))

# Save the plot using here() for a reproducible file path
ggsave(here("PlotsEurope", "AffProdTimePlot.png"),
       plot = AffProdTimePlot, width = 8, height = 7, dpi = 1200)



# --- Combine graphs into a single plot ---

global_tag_style <- theme(
  plot.tag = element_text(size = 20)#, face = "bold")
)

CombinedPlot <- 
  MostRelevantAffiliationsPlot + AffProdTimePlot +
  plot_annotation(
    tag_levels = "a",      # a, b, c ...
    tag_prefix = "(",      # (a
    tag_suffix = ")"       # (a)
  ) & global_tag_style

# Save combined figure
ggsave(
  here("PlotsEurope", "Combined_Affiliations.png"),
  plot = CombinedPlot,
  width = 16,  # adjust as needed
  height = 7,
  dpi = 1200
)


ggsave(
  here::here("PlotsEurope", "Combined_Affiliations.pdf"),
  plot = CombinedPlot,
  width = 16, 
  height = 7
)


# ##############################################################################
# #               Citations
# ##############################################################################


message("\n--- Citation Analysis: Per-Article and Country-Level Metrics ---")

# Define the current year and fraction of the year for up-to-date calculations
current_year <- 2025
fraction_of_year <- 8/12 # Represents end of August

# --- 1. Calculate the age of each publication and filter invalid records ---
EuropeDataset_aged <- Europe_Dataset %>%
  mutate(PY = as.numeric(PY)) %>%
  mutate(
    years_since_publication = case_when(
      PY < current_year ~ current_year - PY,
      PY == current_year ~ fraction_of_year,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(years_since_publication) & years_since_publication > 0)

# --- 2. Calculate average citations per year for each article ---
Individual_Citations_Per_Year <- EuropeDataset_aged %>%
  mutate(TC = as.numeric(TC)) %>%
  mutate(citations_per_year = TC / years_since_publication)

# --- 3. Summarize all citation metrics for all countries ---
# This step calculates both raw and normalized metrics for ALL countries first.
Full_Country_Summary <- Individual_Citations_Per_Year %>%
  mutate(AU1_CO = as.character(AU1_CO)) %>%
  group_by(AU1_CO) %>%
  filter(!is.na(AU1_CO) & AU1_CO != "NA") %>%
  dplyr::summarise(
    number_of_articles = n(),
    total_norm_citations = sum(citations_per_year, na.rm = TRUE),
    mean_norm_citations_per_article = mean(citations_per_year, na.rm = TRUE),
    median_norm_citations_per_article = median(citations_per_year, na.rm = TRUE),
    # RE-ADDED: IQR, Q1, and Q3 for normalized citations
    iqr_norm_citations_per_article = IQR(citations_per_year, na.rm = TRUE),
    q1_norm_citations_per_article = quantile(citations_per_year, 0.25, na.rm = TRUE),
    q3_norm_citations_per_article = quantile(citations_per_year, 0.75, na.rm = TRUE),
    sd_norm_citations_per_article = sd(citations_per_year, na.rm = TRUE),
    # Also calculate raw metrics here
    total_raw_citations = sum(TC, na.rm = TRUE),
    mean_raw_citations_per_article = mean(TC, na.rm = TRUE),
    sd_raw_citations_per_article = sd(TC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(desc(total_raw_citations))

# --- 4. Select the top 20 countries based on total raw citations ---
# The slice_max function is now applied to the 'total_raw_citations' column.
Top20_Country_Summary <- Full_Country_Summary %>%
  dplyr::slice_max(order_by = total_raw_citations, n = 20) %>%
  dplyr::arrange(desc(total_raw_citations)) # Final arrangement by raw citations

# --- 5. Print the resulting summary ---
# This single table now contains all the metrics for your top 20 countries.
print(Top20_Country_Summary)

write.xlsx(Top20_Country_Summary, file = here("Searches","Top20_Country_Summary.xlsx"))






# ##############################################################################
# #               Country & Institution Collaboration 
# ##############################################################################

message("\n--- Country Collaboration ---")

# 1. Create the collaboration network matrix (top 50 countries)
# Removed default arguments for cleaner code
CountryNetwork <- biblioNetwork(
  Europe_Dataset,
  analysis = "collaboration",
  network = "countries",
  n = 30, # Number of countries to include in the network
  sep = ";" # Separator for multiple country entries (if applicable)
)

# 2. Standardize country names for the network matrix
# This ensures consistency for labels within the network structure
country_labels_cleaned <- colnames(CountryNetwork) %>%
  str_to_title() %>% # Convert to Title Case (e.g., "united states" -> "United States")
  str_replace_all("Usa", "USA") # Correct specific instances like "Usa" to "USA"

colnames(CountryNetwork) <- country_labels_cleaned
rownames(CountryNetwork) <- country_labels_cleaned

# 3. Calculate comprehensive network statistics
network_stats <- networkStat(CountryNetwork, stat = "all")

# 4. Generate the network plot using bibliometrix's networkPlot
# Only essential and non-default parameters are kept for clarity.
CountryNetworkPlot <- networkPlot(
  CountryNetwork,
  normalize = "association", # Normalization method for edge weights
  n = 30,                    # Number of nodes (countries) to display in the plot
  label = TRUE,              # Display node labels
  labelsize = 1.2,           # Size of the node labels
  label.color = TRUE,        # Color labels by their community
  cluster = "walktrap",      # Community detection algorithm
  community.repulsion = 0.1, # Repulsion force between communities
  size = 8,                  # Base size of nodes
  size.cex = TRUE,           # Scale node size based on degree
  curved = FALSE,            # Edges are straight lines
  noloops = TRUE,            # Do not draw self-loops
  remove.multiple = TRUE,    # Remove multiple edges between the same two nodes
  weighted = TRUE,           # Use edge weights (collaboration strength)
  edgesize = 10,             # Base size of edges
  edges.min = 0,             # Minimum edge weight to display (0 shows all edges)
  alpha = 0.9,               # Transparency of network elements
  verbose = FALSE            # Suppress verbose output from networkPlot
)

# 5. Extract the igraph object for further custom visualization
networkGraph <- CountryNetworkPlot$graph

# 6. Apply cleaned labels directly to the igraph object's vertices
# This ensures the labels displayed on the plot are the cleaned names.
# V(networkGraph)$name should already contain the cleaned labels from step 2,
# but this line ensures V(networkGraph)$label is also set consistently.
V(networkGraph)$label <- V(networkGraph)$name

# 7. Customize node and edge colors based on community detection
# Get the communities detected by networkPlot
communities <- V(networkGraph)$community

# Define a color palette based on the number of communities.
# Max of "Paired" and "Set3" palettes is 12 colors. If more communities, consider other strategies.
num_communities <- max(communities)
v.colours <- brewer.pal(name = "Paired", n = max(3, min(12, num_communities))) # Ensure at least 3 colors, max 12

V(networkGraph)$color <- v.colours[communities] # Assign colors to nodes based on their community

# Color edges: within-community edges get community color, between-community edges get grey
E(networkGraph)$color <- apply(as.data.frame(as_edgelist(networkGraph, names = FALSE)), 1,
                               function(x) {
                                 node1_comm <- communities[x[1]]
                                 node2_comm <- communities[x[2]]
                                 if (node1_comm == node2_comm) {
                                   v.colours[node1_comm] # Same community color
                                 } else {
                                   'grey90' # Grey for between-community edges
                                 }
                               })

# Scale edge width based on collaboration strength (weight)
E(networkGraph)$width <- E(networkGraph)$weight / max(E(networkGraph)$weight) * 5 # Scale width (e.g., to a max of 5 units)

# 8. Plot the customized igraph network
# This uses the base R plot function for igraph objects.
plot(
  networkGraph,
  vertex.color = V(networkGraph)$color,
  edge.color = E(networkGraph)$color,
  vertex.label.color = "black", # Ensure labels are clearly visible
  vertex.label.cex = CountryNetworkPlot$labelsize # Use the label size set in networkPlot
)

# 9. Export network data for VOSviewer
# Use here() to construct the path to your VOSviewer.jar directory.
# This assumes VOSviewer_1.6.20_jar is a folder containing the VOSviewer.jar file.
net2VOSviewer(
  CountryNetworkPlot,
  vos.path = here("VOSviewer_1.6.20_jar")
)

# 10. Summary of network statistics (e.g., centrality measures)
# This provides insights into the network structure.
summary(network_stats, k = 30) # Summarize top 50 stats



###### Institution collaboration network


message("\n--- Institution Collaboration ---")

# 1. Create the collaboration network matrix (top 50 countries)
# Removed default arguments for cleaner code
InstitutionNetwork <- biblioNetwork(
  Europe_Dataset,
  analysis = "collaboration",
  network = "universities",
  n = 30, # Number of countries to include in the network
  sep = ";" # Separator for multiple country entries (if applicable)
)

# 2. Standardize country names for the network matrix
# This ensures consistency for labels within the network structure
institution_labels_cleaned <- colnames(InstitutionNetwork) %>%
  str_to_title() # Convert to Title Case 

colnames(InstitutionNetwork) <- institution_labels_cleaned
rownames(InstitutionNetwork) <- institution_labels_cleaned

# 3. Calculate comprehensive network statistics
network_stats <- networkStat(InstitutionNetwork, stat = "all")

# 4. Generate the network plot using bibliometrix's networkPlot
# Only essential and non-default parameters are kept for clarity.
InstitutionNetworkPlot <- networkPlot(
  InstitutionNetwork,
  normalize = "association", # Normalization method for edge weights
  n = 30,                    # Number of nodes (countries) to display in the plot
  label = TRUE,              # Display node labels
  labelsize = 1.2,           # Size of the node labels
  label.color = TRUE,        # Color labels by their community
  cluster = "walktrap",      # Community detection algorithm
  community.repulsion = 0.1, # Repulsion force between communities
  size = 8,                  # Base size of nodes
  size.cex = TRUE,           # Scale node size based on degree
  curved = FALSE,            # Edges are straight lines
  noloops = TRUE,            # Do not draw self-loops
  remove.multiple = TRUE,    # Remove multiple edges between the same two nodes
  weighted = TRUE,           # Use edge weights (collaboration strength)
  edgesize = 10,             # Base size of edges
  edges.min = 0,             # Minimum edge weight to display (0 shows all edges)
  alpha = 0.9,               # Transparency of network elements
  verbose = FALSE,            # Suppress verbose output from networkPlot
  remove.isolates = TRUE
)

# 5. Extract the igraph object for further custom visualization
networkGraph <- InstitutionNetworkPlot$graph

# 6. Apply cleaned labels directly to the igraph object's vertices
# This ensures the labels displayed on the plot are the cleaned names.
# V(networkGraph)$name should already contain the cleaned labels from step 2,
# but this line ensures V(networkGraph)$label is also set consistently.
V(networkGraph)$label <- V(networkGraph)$name

# 7. Customize node and edge colours based on community detection
# Get the communities detected by networkPlot
communities <- V(networkGraph)$community

# Define a color palette based on the number of communities.
# Max of "Paired" and "Set3" palettes is 12 colors. If more communities, consider other strategies.
num_communities <- max(communities)
v.colours <- brewer.pal(name = "Paired", n = max(3, min(12, num_communities))) # Ensure at least 3 colors, max 12

V(networkGraph)$color <- v.colours[communities] # Assign colors to nodes based on their community

# Color edges: within-community edges get community color, between-community edges get grey
E(networkGraph)$color <- apply(as.data.frame(as_edgelist(networkGraph, names = FALSE)), 1,
                               function(x) {
                                 node1_comm <- communities[x[1]]
                                 node2_comm <- communities[x[2]]
                                 if (node1_comm == node2_comm) {
                                   v.colours[node1_comm] # Same community color
                                 } else {
                                   'grey90' # Grey for between-community edges
                                 }
                               })

# Scale edge width based on collaboration strength (weight)
E(networkGraph)$width <- E(networkGraph)$weight / max(E(networkGraph)$weight) * 5 # Scale width (e.g., to a max of 5 units)

# 8. Plot the customized igraph network
# This uses the base R plot function for igraph objects.
plot(
  networkGraph,
  vertex.color = V(networkGraph)$color,
  edge.color = E(networkGraph)$color,
  vertex.label.color = "black", # Ensure labels are clearly visible
  vertex.label.cex = InstitutionNetworkPlot$labelsize # Use the label size set in networkPlot
)

# 9. Export network data for VOSviewer
# Use here() to construct the path to your VOSviewer.jar directory.
# This assumes VOSviewer_1.6.20_jar is a folder containing the VOSviewer.jar file.
net2VOSviewer(
  CountryNetworkPlot,
  vos.path = here("VOSviewer_1.6.20_jar")
)

# 10. Summary of network statistics (e.g., centrality measures)
# This provides insights into the network structure.
summary(network_stats, k = 30) # Summarize top 50 stats








# ##############################################################################
# #               Sources
# ##############################################################################

message("\n--- Sources ---")

# 1. Extract and clean "Most relevant sources" data
MostRelevantSources_cleaned <- summary_europe$MostRelSources %>%
  as_tibble() %>% # Convert the data to a tibble
  # Rename the column with trailing spaces to a clean 'Sources'
  dplyr::rename(Sources = `Sources       `) %>% # Use backticks for names with spaces
  dplyr::mutate(
    # Trim leading/trailing spaces from the 'Sources' values and convert to character
    Sources = as.character(Sources) %>% stringr::str_trim(),
    Articles = as.numeric(Articles) # Ensure 'Articles' column is numeric
  ) %>%
  dplyr::mutate(Sources = stringr::str_to_title(Sources)) %>% # Apply title casing
  dplyr::arrange(desc(Articles)) # Arrange by 'Articles' in descending order


# 2. Plot the results using ggplot2
MostRelevantSourcesPlot <- ggplot(
  MostRelevantSources_cleaned,
  aes(x = Articles, y = reorder(Sources, Articles)) # Reorder sources by article count for the plot
) +
  geom_col(fill = "black") + # Create a column chart with black bars
  # Add text labels for 'Articles' count on the bars.
  # 'color="white"' makes them visible on black bars, 'hjust=1' right-aligns them inside the bar.
  geom_text(aes(label = Articles), color = "white", hjust = 1) +
  # Adjust Y-axis text justification for better alignment if needed.
  theme(axis.text.y = element_text(hjust = 1.2)) +
  labs(x = "Articles", y = "Sources") + # Set x and y axis labels
  # Define x-axis limits and breaks for consistent scaling
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
  theme_classic(base_size = 16) # Apply a classic theme with a base font size for overall readability

# 3. Save the plot
ggsave(
  here("PlotsEurope", "MostRelevantSourcesPlot.eps"),
  plot = MostRelevantSourcesPlot, # Specify the ggplot object to save
  width = 13, # Set plot width
  height = 8, # Set plot height
  dpi = 300   # Set resolution for print quality
)



########   Sources' cumulative production over time  ########

# extract yearly published documents of the top sources (e.g. 6) with the function "sourceGrowth" from bibliometrix
# dft stands for cumulative
SourcesProductionTime <- bibliometrix::sourceGrowth(Europe_Dataset, top = 6, cdf = TRUE)

# Clean and prepare the data for plotting
# Use 'pivot_longer' to convert the data from a wide format (one column per source)
# to a long format, which is required for ggplot2.
SourcesProductionTime_PlotReady <- SourcesProductionTime %>%
  # Reshape the data from wide to long format.
  # The 'Year' column is the identifier.
  # New columns are created for 'Source' and 'Articles'.
  pivot_longer(
    cols = -Year,
    names_to = "Source",
    values_to = "Articles"
  ) %>%
  # Add a 'YearAsDate' column in a proper date format for the x-axis.
  # Also, clean up the source titles by converting them to title case.
  mutate(
    YearAsDate = as.Date(ISOdate(Year, 1, 1)),
    Source = str_to_title(Source)
  )


## Define Plot Variables and Theme 
# Define the date breaks and limits for the x-axis.
date_breaks <- as.Date(c("1940-01-01", "1955-01-01", "1965-01-01", "1975-01-01", 
                         "1985-01-01", "1995-01-01", "2005-01-01", "2015-01-01",
                         "2025-01-01"))
min_date <- as.Date("1940-01-01")
max_date <- as.Date("2025-01-01")

# Define a custom color palette using RColorBrewer.
my_palette <- brewer.pal(name = "Dark2", n = 8)[1:8]



# Create the line plot showing source production over time.
SourcesProdTimePlot <- ggplot(
  data = SourcesProductionTime_PlotReady, 
  aes(x = YearAsDate, y = Articles, group = Source, color = Source)
) +
  # Add the line geometry.
  geom_line() +
  # Add descriptive labels for the axes.
  labs(x = "Year", y = "Total Articles") +
  # Use a clean, classic theme with a larger base font size.
  theme_classic(base_size = 16) +
  # Customize the legend position and appearance.
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  # Customize the x-axis with defined date breaks and limits.
  scale_x_date(
    breaks = date_breaks,
    limits = c(min(SourcesProductionTime_PlotReady$YearAsDate), max(SourcesProductionTime_PlotReady$YearAsDate)),
    guide = guide_axis(angle = 45),
    date_labels = "%Y"
  ) +
  # Customize the y-axis with a clear range and breaks.
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10)
  ) +
  # Apply the custom color palette to the lines.
  scale_color_manual(values = my_palette) +
  # Specify the number of columns in the legend.
  guides(color = guide_legend(ncol = 2))


# ---  Save the Plot ---
ggsave(here("PlotsEurope", "SourcesProdTimePlot.eps"),
       plot = SourcesProdTimePlot, 
       width = 7.5,
       height = 5,
       dpi = 300)





# ##############################################################################
# #               Conceptual Structure 
# ##############################################################################


# --- 1. Co-word Analysis (Network Plot) ---

message("\n--- Co-word Analysis ---")

KeywordNetwork <- biblioNetwork(Europe_Dataset,
                                analysis = "co-occurrences",
                                network = "author_keywords",
                                n = 50,
                                sep = ";",
                                short = FALSE,
                                shortlabel = TRUE,
                                remove.terms = NULL,
                                synonyms = NULL)

networkPlot(KeywordNetwork,
            normalize = "association",
            n = 50,
            degree = NULL,
            Title = NULL,
            type = "auto",
            label = TRUE,
            labelsize = 1.2,
            label.cex = FALSE,
            label.color = TRUE,
            label.n = NULL,
            halo = FALSE,
            cluster = "walktrap",
            community.repulsion = 0.1,
            vos.path = NULL,
            size = 8,
            size.cex = TRUE,
            curved = FALSE,
            noloops = TRUE,
            remove.multiple = TRUE,
            remove.isolates = FALSE,
            weighted = TRUE,
            edgesize = 10,
            edges.min = 0,
            alpha = 0.5,
            verbose = FALSE)

# --- 2. Multiple Correspondence Analysis (MCA) ---

message("\n--- Multiple Correspondence Analysis (MCA) ---")

# remove not needed variables: 
Europe_Dataset$DE <- str_remove_all(Europe_Dataset$DE, "ENGLISH ABSTRACT")


suppressWarnings(
  CS <- conceptualStructure(
    Europe_Dataset,
    field = "DE",
    ngrams = 1,
    method = "MCA",
    quali.supp = NULL,
    quanti.supp = NULL,
    minDegree = 50,
    clust = 4,
    k.max = 5,
    stemming = FALSE,
    labelsize = 10,
    documents = 2,
    graph = TRUE,
    remove.terms = NULL,
    synonyms = NULL
  )
)

# Save and Load: Keep if CS object creation is very time-consuming.
# Using here::here() for the file path.
saveRDS(CS, file = here("Searches", "CS.RData"))
CS <- readRDS(file = here("Searches", "CS.RData"))


# --- Reusable Function for Plot Styling and Logo Removal ---
modify_bibliometrix_plot <- function(plot_obj, type = c("MCA", "ThematicMap")) {
  type <- match.arg(type)
  
  if (!inherits(plot_obj, "ggplot") || is.null(plot_obj$layers)) {
    warning(paste("Input is not a valid ggplot object or has no layers for type:", type))
    return(plot_obj)
  }
  
  modified_plot <- plot_obj +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, face = "plain"),
      axis.title.y = ggplot2::element_text(size = 16, face = "plain"),
      plot.margin = ggplot2::unit(c(0.5, 1, 0.5, 0.5), "cm")
    )
  
  if (type == "MCA") {
    if (length(modified_plot$layers) >= 6) {
      modified_plot$layers[[6]] <- NULL
    } else {
      message("Warning: Layer 6 (potential logo) not found or plot has fewer than 6 layers for MCA plot.")
    }
    
    if (length(modified_plot$layers) >= 3 && inherits(modified_plot$layers[[3]]$geom, "GeomTextRepel")) {
      modified_plot$layers[[3]]$aes_params$size <- 5.5
      message("MCA: Adjusted geom_text_repel size.")
    } else {
      message("MCA: GeomTextRepel not found at layer 3. Label size not modified.")
    }
    
  } else if (type == "ThematicMap") {
    modified_plot <- modified_plot +
      ggplot2::labs(
        x = "Relevance degree (Centrality)",
        y = "Development degree (Density)"
      )
    
    if (length(modified_plot$layers) >= 6) {
      modified_plot$layers[[6]] <- NULL
    } else {
      message("Warning: Layer 6 (potential logo) not found or plot has fewer than 6 layers for Thematic Map plot.")
    }
    
    if (length(modified_plot$layers) >= 2 && inherits(modified_plot$layers[[2]]$geom, "GeomLabelRepel")) {
      modified_plot$layers[[2]]$aes_params$fill <- NA
      modified_plot$layers[[2]]$aes_params$size <- 5.5
      modified_plot$layers[[2]]$aes_params$min.segment.length <- 0.3
      modified_plot$layers[[2]]$aes_params$force <- 1
      modified_plot$layers[[2]]$aes_params$box.padding <- 0.5
      modified_plot$layers[[2]]$aes_params$point.padding <- 0.5
      message("Thematic Map: Adjusted GeomLabelRepel parameters.")
    } else {
      message("Thematic Map: GeomLabelRepel not found at layer 2. Label parameters not modified.")
    }
    
    for (i in seq_along(modified_plot$layers)) {
      current_layer_geom <- class(modified_plot$layers[[i]]$geom)[1]
      if (current_layer_geom == "GeomVline" || current_layer_geom == "GeomHline") {
        modified_plot$layers[[i]]$aes_params$colour <- "grey80"
        message(paste("Thematic Map: Modified color of layer", i, "to a paler shade."))
      }
    }
  }
  return(modified_plot)
}


# Apply modifications to MCA plot
if ("graph_terms" %in% names(CS)) {
  MCAplot_final <- modify_bibliometrix_plot(CS$graph_terms, type = "MCA")
  print(MCAplot_final)
} else {
  warning("CS$graph_terms not found. MCA plot could not be generated or modified.")
  MCAplot_final <- NULL
}

# Save MCA plot
if (!is.null(MCAplot_final)) {
  ggsave(here("PlotsEurope", "MCAplot_no_logo.png"),
         plot = MCAplot_final,
         width = 8.5,
         height = 8.5,
         dpi = 300)
}


# --- 3. Thematic Analysis ---

message("\n--- Thematic Analysis ---")

# change keyword as both ways are used in the dataset
#Europe_Dataset$DE <- gsub(pattern = "POLYCYCLIC AROMATIC HYDROCARBONS",
#                       replacement = "PAHS",
#                       x = Europe_Dataset$DE)

Map <- thematicMap(
  Europe_Dataset,
  field = "DE",
  n = 300,
  minfreq = 5,
  ngrams = 1,
  stemming = FALSE,
  size = 0.5,
  n.labels = 4,
  community.repulsion = 0.1,
  repel = TRUE,
  remove.terms = NULL,
  synonyms = NULL,
  cluster = "walktrap",
  subgraphs = TRUE
)

# Apply modifications to Thematic Map plot
if ("map" %in% names(Map)) {
  ThematicMapPlot_final <- modify_bibliometrix_plot(Map$map, type = "ThematicMap")
  print(ThematicMapPlot_final)
} else {
  warning("Map$map not found. Thematic Map plot could not be generated or modified.")
  ThematicMapPlot_final <- NULL
}

# Save Thematic Map plot (PNG and SVG for Illustrator)
if (!is.null(ThematicMapPlot_final)) {
  ggsave(here("PlotsEurope", "ThematicMapPlot_no_logo.png"),
         plot = ThematicMapPlot_final,
         width = 8.5,
         height = 8.5,
         dpi = 1200)
  
  ggsave(here("PlotsEurope", "ThematicMapPlot_no_logo.svg"),
         plot = ThematicMapPlot_final,
         width = 8.5,
         height = 8.5,
         units = "in",
         dpi = 1200,
         device = "svg")
}

# --- 4. Combining Plots with patchwork ---

mca_plot_filename <- here("PlotsEurope", "MCAplot_no_logo.png")
thematic_plot_png_filename <- here("PlotsEurope", "ThematicMapPlot_no_logo.png")


# Only proceed if both individual PNG files were successfully created
if (file.exists(mca_plot_filename) && file.exists(thematic_plot_png_filename)) {
  message("Combining pre-saved images with magick...")
  
  image1 <- image_read(mca_plot_filename) # Use the variable holding the full path
  image2 <- image_read(thematic_plot_png_filename) # Use the variable holding the full path
  
  # Combine horizontally (stack = FALSE)
  combined_MCA_thematic <- image_append(c(image1, image2), stack = FALSE)
  
  # Save the combined image
  combined_image_filename <- here("PlotsEurope", "combined_MCA_thematic.png")
  image_write(combined_MCA_thematic, combined_image_filename)
  message(paste("Combined plot saved to:", combined_image_filename))
  
} else {
  warning("Skipping plot combination with magick as one or both required PNG files were not found.")
}


# save as pdf

# Only proceed if the plot objects exist in your R environment
if (!is.null(MCAplot_final) && !is.null(ThematicMapPlot_final)) {
  
  # Combine them side-by-side using patchwork syntax
  combined_vector_plot <- MCAplot_final + ThematicMapPlot_final
  
  # Save directly to PDF
  ggsave(
    here::here("PlotsEurope", "combined_MCA_thematic.pdf"),
    plot = combined_vector_plot,
    width = 17, # Doubled width (8.5 * 2) for side-by-side
    height = 8.5
  )
  
  message("Combined vector PDF saved successfully.")
}







# ---------------------------------------------------------
# Load and prepare the VOSviewer network
# ---------------------------------------------------------

vos_path <- here("PlotsEurope", "Network.pdf")
if (!file.exists(vos_path)) stop("VOSviewer PDF not found.")

vos_img <- image_read_pdf(vos_path, density = 300) |>
  image_trim(fuzz = 10)   # aggressive trim helps but won’t distort

vos_grob <- rasterGrob(
  as.raster(vos_img),
  interpolate = TRUE
)

NetworkPlot_Final <- wrap_elements(vos_grob) +
  theme(plot.margin = margin(0, 0, 0, 0))

# ---------------------------------------------------------
# OPTION A — FINAL VERSION
# Network fills RIGHT column, 1/3 width, full height
# ---------------------------------------------------------

Figure_Option_A <-
  MostProdCountriesPlot +
  CountriesProdTimePlot +
  NetworkPlot_Final +
  plot_layout(
    design = "
      AC
      BC
    ",
    widths = c(2, 2)   # network is exactly 1/3 width
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(size = 14, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

# ---------------------------------------------------------
# Save
# ---------------------------------------------------------

ggsave(
  here("PlotsEurope", "Manuscript_Figure_OptionA_OneThirdWidth.pdf"),
  plot = Figure_Option_A,
  width = 16,
  height = 8,
  device = "pdf"
)


# ---------------------------------------------------------
# OPTION B
# ALL THREE STACKED VERTICALLY: A / B / C
# ---------------------------------------------------------

Figure_Option_B <-
  (MostProdCountriesPlot /
     CountriesProdTimePlot /
     NetworkPlot_Final) +
  plot_layout(
    heights = c(1, 1, 1.2)  # give network a bit more space
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(size = 14, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

# ---------------------------------------------------------
# 3. SAVE – choose ONE (or both)
# ---------------------------------------------------------

# ---- OPTION A ----
ggsave(
  here("PlotsEurope", "Manuscript_Figure_OptionA_Corrected.pdf"),
  plot = Figure_Option_A,
  width = 16,
  height = 14,
  device = "pdf"
)

# ---- OPTION B ----
ggsave(
  here("PlotsEurope", "Manuscript_Figure_OptionB_Corrected.pdf"),
  plot = Figure_Option_B,
  width = 16,
  height = 18,
  device = "pdf"
)