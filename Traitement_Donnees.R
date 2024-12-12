#Load libraries

library(readr)
library(tidyverse)
library(tidytext)
library(tm)
library(dplyr)
library(sampler)
library(urltools)
library(countrycode)
library(utf8)
#import dataset
posts_1 <- read_csv("posts (1).csv")



#Identify and eliminate posts previously published and later reshared by the same page to avoid duplicates
# Extract relevant parts of URLs for comparison

link_1<- data.frame(str_split_fixed(posts_1$link, "/", n=6))
link_2<- str_c(link_1$X4, link_1$X5, sep = "/")

URL_1<- data.frame(str_split_fixed(posts_1$account_url, "/", n=6))
URL_2<- str_c(URL_1$X4, URL_1$X5, sep = "/")

posts_1$post_url_1<-URL_2
posts_1$link_1<-link_2

# Extract domain and top-level domain (TLD) from URLs

posts_1$domain <- domain(posts_1$link)
posts_1$tld <- tld_extract(posts_1$domain, tlds=NULL)

# Categorize posts based on TLD (filtering out generic TLDs like ".com")

posts_1 <- posts_1 %>%
  mutate(
    TLD = case_when(
      tld$tld %in% "com" ~ "NA",
      tld$tld != "com" ~ tld$tld))

posts_1$TLD[posts_1$TLD=="NA"] <- NA

# Save the cleaned data for hyperlink analysis

write.csv(post_f_1,"postsF1.csv", row.names = FALSE, fileEncoding = "UTF-8") # This is going to be used for hyperlink analysis

# Filter out reshared posts

post_f_1 <- filter(posts_1, post_url_1!=link_1)



# Find country of origin for every page and group

# Add country of origin for each page based on metadata
post_f_1$country <- post_f_1$account_page_admin_top_country

# Add a logical indicator (1 = metadata available, 0 = missing) for country metadata
post_f_1$Co <- ifelse(!is.na(post_f_1$country), 1, 0)


# Manually assign countries for specific account URLs where metadata is missing

post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/1606815329554276"] <- "MEX"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/303970143732912"] <- "EU"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/1614875702032929"] <- "ARG"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/975946939153291"] <- "PER"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/850969585455100"] <- "CUB"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/275911789207670"] <- "PER"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/198383487313673"] <- "ARG"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/893101194380807"] <- "BOL"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/149107612387145"] <- "ARG"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/1411250252226356"] <- "MEX"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/groups/259753348068603"] <- "MEX"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/402351143227209"] <- "PER"
post_f_1$country[post_f_1$account_url=="https://www.facebook.com/1055204584572916"] <- "MEX"

# Standardize country codes to ensure consistency
country_code_mapping <- list(
  "MX" = "MEX", "CUB" = "CUB", "PE" = "PER", "ES" = "EU",
  "SV" = "SLV", "AR" = "ARG", "EC" = "ECU", "BO" = "BOL",
  "CO" = "COL", "CR" = "CRI", "CL" = "CHL", "US" = "N.A", "BOL" ="BOL"
)

post_f_1$country <- ifelse(
  post_f_1$country %in% names(country_code_mapping),
  unlist(country_code_mapping[post_f_1$country]),
  NA
)

# Re-calculate the logical variable for tracking metadata availability
post_f_1$Co <- ifelse(!is.na(post_f_1$country), 1, 0)


# Add indicators for metadata completeness: message, description, and top-level domain (TLD)
post_f_1 <- post_f_1 %>%
  mutate(
    M = ifelse(!is.na(message), 1, 0),
    D = ifelse(!is.na(description), 1, 0),
    TL = ifelse(!is.na(TLD), 1, 0)
  )

# Summarize metadata completeness
post_f_1$sum <- rowSums(post_f_1[ , c(60,61,62)],)


#Clean dataset: uniform all text for further analysis
# Function to clean text
clean_text <- function(text) {
  text <- tolower(text) # Convert to lowercase
  text <- str_replace_all(text, "[áâàäąãæ]", "a")
  text <- str_replace_all(text, "[éêèëęēė]", "e")
  text <- str_replace_all(text, "[íîìïįī]", "i")
  text <- str_replace_all(text, "[óôòöõōœ]", "o")
  text <- str_replace_all(text, "[úûùüū]", "u")
  text <- str_replace_all(text, "[ñ]", "n")
  text <- str_replace_all(text, "[#¡¿?!]", " ") # Remove punctuation
  return(text)
}

# Apply the function to clean 'message' and 'description'
post_f_1$message_clean <- clean_text(post_f_1$message)
post_f_1$description_clean <- clean_text(post_f_1$description)


#Identify country related to hyperlinks by TLD
post_f_1$TLD <- paste0(".", post_f_1$TLD)

# Define the TLD to Country/Region mapping
tld_map <- data.frame(
  TLD = c(".mx", ".pe", ".ec", ".py", ".gt", ".ar", ".pr", ".co", ".cl", ".cu", ".uy", 
          ".bo", ".cr", ".hn", ".do", ".sv", ".ni", ".br", ".ve", ".pa", # Latin America
          ".eu", ".fr", ".it", ".uk", ".be", ".ru", ".es", ".de", ".me", ".au", ".al", 
          ".va", ".at", ".no", ".ch", ".ie", ".se", ".rs", ".pl",         # Europe
          ".us", ".vi", ".ca",                                           # North America
          ".ph", ".in", ".gd", ".nz", ".st", ".ag", ".io", ".ws", ".cn", 
          ".kr", ".fo", ".am", ".fm", ".ng", ".mp", ".gl", ".la"),       # Mundo (Rest of the World)
  TLD_1 = c("MEX", "PER", "ECU", "PRY", "GTM", "ARG", "PRI", "COL", "CHL", "CUB", 
              "URY", "BOL", "CRI", "HND", "DOM", "SLV", "NIC", "BRA", "VEN", "PAN", 
              "EU", "EU", "EU", "EU", "EU", "EU", "EU", "EU", "EU", "EU", "EU", 
              "EU", "EU", "EU", "EU", "EU", "EU", "EU", "EU",
              "N.A", "N.A", "N.A", 
              "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO",
              "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO", "MUNDO")
)

# Merge to map TLDs to Countries/Regions
post_f_1 <- merge(post_f_1, tld_map, by.x = "TLD", by.y = "TLD", all.x = TRUE)




## Identify which countries are mentioned in text content 
# Create bag of words for all latin american countries to identify which countries are mentioned in the discourse

mx <- c("mx", "mexic", "amlo", "guadalaj", "jalisc", 
        "michoacan", "chiapas", "sinaloa", "quintana", 
        "guanajuato", "veracruz", "oaxaca", "queretaro")


slv <- c("el salvador", "nayib", "buklele", "elsalvador")

nic <- c("nicaragua", "managua", "tipitapa", "masaya", "chinandega", 
         "matagalpa", "ortega", "nicaragu")

cri <- c("costa rica", "costaric", "alajuela", "cartago", "alvarado")

hnd <- c("honduras", "choloma", "danli", "orlando hernandez" )

pan <- c("panama", "tocumen", "cortizo")

dom <- c("republica dominicana", "dominic", "abinader", "medina")

pe <- c("peru", "lima", "cusco", "cajamarca", "fujimori", "callao", 
        "lambayeque", "piura", "trujillo", "arequipa", "chiclayo")


ec <- c("ecuato", "ecuador", "quito", "guayaquil", 
        "cuenca", "pichincha", "chimborazo", "esmeraldas", 
        "guayas", "correa", "lasso", "bucaram")


co <- c("colombia", "cali", "bogota", "medellin", "cartagena", 
        "pereira", "popayan", "barranquilla", "soacha", 
        "cucuta", "farc", "duque")


chl <- c("chile", "santiago", "iquique", "talca", "antofagasta", 
         "valparaiso", "vitacura", "pinera", "kast", 
         "boric", "bachelet", "mapuche")

ven <- c("venezuela", "maduro", "chavez", "caracas", "merida", 
         "maracaibo", "valencia", "maracay")

bra <- c("brasil", "janeiro", "manaos", "curitiba", "sao paulo", 
         "fortaleza", "bolsonaro", "lula", "rousseff")


arg <- c("argentin", "buenos aires", "mendoza", "córdoba", "neuquen", 
         "jujuy", "rosario", "macri", "kirchner", "peron", "catamarca")

bol <- c("bolivia", "la paz", "cochabamba", "potosi", "catacora", 
         "morales", "evo", "luis arce", "jeanine anez")

pry <- c("paraguay", "asuncion", "luque", "capiata", "abdo")

ury <- c("uruguay", "montevideo", "paysandu", "tacuarembo", "lacalle", 
         "tabare vazquez", "mujica")

la <- c("america", "latina", "centroamerica", "sudamerica")


# Create bag of words for other world regions

N.A <- c("eeuu", "washington", "biden", "trump", "york", 
         "california", "texas", "michigan", "canada", 
         "ontario", "quebec", "montreal", "trudeau", "fda", "ice")


eu <- c("europ", "franc", "aleman", "espan", "inglaterra", 
        "ukrania", "russ", "polonia", "orban", 
        "hungria", "macron", "londres", "putin","soviet", "italia", 
        "grecia", "portugal", "austria", "irlanda",
        "bruxellas", "suiza", "suecia", "norueg", "uk", "rumania",
        "luxemburgo", "lituania", "letonia", "holanda", "brexit", "britan",
        "madrid", "paris", "berlin")

mundo <- c("africa", "asia", "japon", "china", "india", 
           "filipinas", "zeland", "australia", "comunista", 
           "duterte", "afgan", "saudi", "siria","israel", "palestina", 
           "jerusalem", "corea", "malasia", "singapur", "tailandia", "vietnam",
           "camboya", "myanmar","libano", "qatar", "egipt", 
           "yemen", "nigeria", "marruecos", "etiopia", "uganda", "sudan",
           "zimbabue", "madagascar", "angola", "ruanda", "botsuana", "nigeria")


inst.int <- c("onu", "cidh", "unesco", "mercosur", "oms", 
              "omc", "cepal", "bid", "fmi", 
              "oit", "pfizer", "sinovac", "oea")


# Combine text from the 'message_clean' and 'description_clean' columns into a single variable

post_f_1$text <- paste(post_f_1$message_clean, post_f_1$description_clean)

# Define a list of country codes and their associated variables (e.g., word lists made before)

categories <- list(
  MX = mx, SLV = slv, NIC = nic, CRI = cri, HND = hnd, PAN = pan,
  DOM = dom, PE = pe, EC = ec, CO = co, CHL = chl, VEN = ven,
  BRA = bra, ARG = arg, BOL = bol, PRY = pry, URY = ury, LA = la,
  N_A = N.A, EU = eu, MUNDO = mundo, INST = inst.int
)

# Use the bag-of-words (BoW) method to identify if a country is mentioned in the post text
# For each category, create a new binary column (e.g., 'MXd', 'SLVd') indicating whether
# the associated keywords for that category appear in the text

for (cat in names(categories)) {
  var_name <- paste0(cat, "d")  # e.g., MXm, SLVm each variable reflects wether the BoW associated to that country code is identified
  post_f_1[[var_name]] <- as.integer(grepl(paste(categories[[cat]], collapse = "|"), 
                                           x = post_f_1$text))
}



# Calculate if any country is identified in the text using the BoW method


post_f_1$Ct <- rowSums(post_f_1[68:89]) # Identify if any country was identified in text
post_f_1$Ctld <- !is.na(post_f_1$TLD_1)  # Identify if any country was identified in TLD

# Determine if no country is identified by any method (BoW or TLD)
# 'Cs' = 0 if no country is identified, 1 otherwise

post_f_1$Cs <- ifelse(post_f_1$Ctld == FALSE &
                post_f_1$Ct==0, 0,1)

# Assign values to 'Csm' (Country Summary) based on country identification
# 'N.D' = No data, 'INT' = More than one country mentioned, otherwise specific country codes

post_f_1$Csm <- post_f_1$TLD_1 # Default variable to country identified in TLD
post_f_1$Csm[post_f_1$Cs == 0] <- "N.D" #assign no data for relevant columns
post_f_1$Csm[post_f_1$Ct > 1] <- "INT" #Assign international for observations that mention more than one country

# Assign specific country codes to 'Csm' if only one country is mentioned in the text (this takes precedence over TLD)

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$MXd == 1 ] <- "MEX" # example: if there's a country found in the text (Ct) and that country is Mexico (MXd) then assign Mexico

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$ARGd == 1 ] <- "ARG" #repeat for every country

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$BOLd == 1 ] <- "BOL"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$BRAd == 1 ] <- "BRA"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$CHLd == 1 ] <- "CHL"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$COd == 1 ] <- "COL"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$CRId == 1 ] <- "CRI"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$DOMd == 1 ] <- "DOM"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$ECd == 1 ] <- "ECU"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$EUd == 1 ] <- "EU"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$HNDd == 1 ] <- "HND"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$INSTd == 1 ] <- "INST"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$LAd == 1 ] <- "LA"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$MUNDOd == 1 ] <- "MUNDO"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$N.Ad == 1 ] <- "N.A"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$NICd == 1 ] <- "NIC"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$PANd == 1 ] <- "PAN"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$PEd == 1 ] <- "PER"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$PRYd == 1 ] <- "PRY"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$SLVd == 1 ] <- "SLV"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$URYd == 1 ] <- "URY"

post_f_1$Csm[post_f_1$Ct == 1 & post_f_1$VENd == 1 ] <- "VEN"


#Create data frame only with the relevant data for geography-based mapping
posts_map <- post_f_1[,c(13,14,43,44,47,50,51,52,54,55,58,93)]

#Export data to csv
write.csv(posts_map,"posts_map.csv", row.names = FALSE, fileEncoding = "UTF-8")


#Create sample data for transparency
sample_data <- posts_map %>% sample_n(1000)
write.csv(sample_data,"sample_data.csv", row.names = FALSE, fileEncoding = "UTF-8")

