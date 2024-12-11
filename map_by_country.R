#Load libraries
library(readr)
library(dplyr)
library(countrycode)

#Load data
posts <- read_csv("posts_map.csv")

#Keep only complete obs
posts <- filter(posts, Csm == "N.D")

# Rename "csm" to "target country"
colnames(posts)[colnames(posts) == "Csm"] <- "Target Country"

# Rename "country" to "origin country"
colnames(posts)[colnames(posts) == "country"] <- "Origin Country"


# Create a unique list of all countries in both source and target columns
all_countries <- unique(posts$`Target Country`)

# convert abbreviations to full names
replacement_mapping <- c("ARG" = "Argentina", "BOL" = "Bolivia", "BRA" = "Brazil", 
                         "CHL" = "Chile", "COL" = "Colombia", "CRI" = "Costa Rica", 
                         "CUB" = "Cuba", "DOM" = "Dominican Republic", "ECU" = "Ecuador", 
                         "EU" = "European Union", "GTM" = "Guatemala", "HND" = "Honduras", 
                         "INST" = "Institutions", "INT" = "International", "MEX" = "Mexico", 
                         "MUNDO" = "World", "N.A" = "North America", "N.D" = NA, 
                         "NIC" = "Nicaragua", "PAN" = "Panama", "PER" = "Peru", 
                         "PRI" = "Puerto Rico", "PRY" = "Paraguay", "SLV" = "El Salvador", 
                         "URY" = "Uruguay", "VEN" = "Venezuela")

# Replace target country abbreviations with full names
posts$`Target Country` <- ifelse(posts$`Target Country` %in% names(replacement_mapping),
                                 replacement_mapping[posts$`Target Country`],
                                 posts$`Target Country`)

# Replace origin country abbreviations with full names
posts$`Origin Country` <- ifelse(posts$`Origin Country` %in% names(replacement_mapping),
                                 replacement_mapping[posts$`Origin Country`],
                                 posts$`Origin Country`)



# Creating the dataframe for Country of Origin
df_origin <- posts %>%
  group_by(account_name) %>%
  summarise(Source = unique(`Origin Country`), Weight = 1) %>%
  select( Source, Target = account_name, Weight)  # Include Weight in the output
View(df_target)
# Creating the dataframe for Target Countries
df_target <- posts %>%
  group_by(account_name, `Target Country`) %>%
  summarise(Weight = n()) %>%
  select( Source = account_name,Target = `Target Country`, Weight)  # Include Weight in the output

View(df_origin)



# Combining the two dataframes
df_countries <- rbind(df_origin, df_target)
df_countries <- na.omit(df_countries)

# Ensure colnames are compatible with Gephi
colnames(df_countries) <- c("Source", "Target", "Weight")

# Export data
write.csv(df_countries, "map_by_country.csv", row.names = FALSE)





