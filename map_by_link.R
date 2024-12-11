library(dplyr)
library(readr)

# Read data
posts <- read_csv("postsF1.csv")

# Define a function to extract page name or group identifier
extract_info <- function(link) {
  if (grepl("facebook.com/groups/", link) && grepl("/permalink/", link)) {
    sub("https://www.facebook.com/groups/([^/]+)/permalink/.*", "\\1", link)
  } else if (grepl("facebook.com", link) && !grepl("photo.php", link)) {
    sub("https://www.facebook.com/([^/]+)/.*", "\\1", link)
  } else {
    NA
  }
}

# Extract information 
posts1 <- posts %>%
  mutate(shared_info = sapply(link, extract_info),
         account_info = sub(".*/([^/]+)$", "\\1", account_url))

#ensure uniformity of nomenclature between account handles and account info
for (name in unique(posts1$account_handle)){
  posts1$account_info[posts1$account_handle==name] <- name
}

#subset to only keep complete observations that are not self-shares
posts1<- subset(posts1, !is.na(posts1$shared_info))
posts1<- subset(posts1, posts1$shared_info!=posts1$account_info)

#create new df to map information on the shared links
df <- posts1 %>%
  count(account_info, shared_info, name = "Weight")
colnames(df) <- c("Source", "Target", "Weight")

# Create a table of Target frequencies
t<-as.data.frame(table(df$Target))

# Extract Target values with frequencies greater than 1
gnames <- t$Var1[t$Freq>1]

# Create df1 by filtering based on conditions (original df too big to be processed effectively)
df1 <- df[df$Target %in% gnames | df$Weight > 5, ]

#Export data
write.csv(df1, "map_by_links.csv", row.names = FALSE)






#Exercises in visualization

# compute frequency for all tables, independently from account info
link_frequency <- table(posts1$shared_info)

# Compute the frequency of frequencies
frequency_of_frequencies <- table(link_frequency)

# Bar plot with log-log scale
barplot(log(frequency_of_frequencies), 
        main = "Distribution of Shared Links (Log-Log Scale)",
        xlab = "Log(Number of Times Link is Shared)",
        ylab = "Log(Number of Unique Links)",
        col = "skyblue",
        border = "black",
        cex.names = 0.7,
        las = 1)  # Rotate x-axis labels for better readability

