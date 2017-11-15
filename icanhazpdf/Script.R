library(jsonlite)
library(stringr)
library(ggplot2)
library(scales)

# Windows loves Unicode. Not.
options(encoding = "UTF-8")

tweets <- stream_in(file("stream_icanhazpdf.json", open = "r"))

# Filter out tweets by me
tweets <- subset(tweets, user$id_str != "867090278104039426")


# geo is deprecated
# extended_entities only contains media attributes
# favorite_count is zero anyway
# retweet_count too as is reply_count


drop <- c("geo", "possibly_sensitive", "filter_level", "retweeted", "favorited", "extended_entities",
          "favorite_count", "retweet_count", "reply_count", "quote_count", "quoted_status", "quoted_status_id",
          "coordinates", "in_reply_to_id", "in_reply_to_screen_name", "in_reply_to_user_id", "in_reply_to_status_id", "truncated", "id")

tweets <- tweets[, !(names(tweets) %in% drop)]

tweets$source <- str_extract(tweets$source, ">(.*?)</a>$") %>%
    gsub("</a>", "", .) %>%
    gsub(">", "", .)

tweets <- within(tweets, {
                 type <- ifelse(!(is.na(retweeted_status$id)), "retweet",
                                ifelse(is_quote_status, "quote",
                                ifelse(!(is.na(in_reply_to_status_id_str)), "reply", "tweet")))
})

# Plot Type Partition

partition <- as.data.frame(table(tweets$type), stringsAsFactors = FALSE)
colnames(partition) <- c("Type", "Frequency")
ggplot(partition, aes(x = "", y = Frequency, fill = Type)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)

# Plot User Partition

partition <- as.data.frame(table(tweets$user$id_str), stringsAsFactors = FALSE)
colnames(partition) <- c("User", "Tweets")
partition <- partition[order(-partition$Tweets),]
ggplot(head(partition, 10), aes(x = "", y = Tweets, fill = User)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)

# Remove @heshecanhazpdf

tweets <- tweets[which(tweets$user$id_str != "2924860811"),]

# Plot Type Partition (again)

partition <- as.data.frame(table(tweets$type), stringsAsFactors = FALSE)
colnames(partition) <- c("Type", "Frequency")
ggplot(partition, aes(x = "", y = Frequency, fill = Type)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + ylab("") + xlab("") + ggtitle("Distribution of tweet types")

unique.tweets <- tweets[which(tweets$type == "tweet"),]
nrow(unique.tweets) # 357

# Date

Sys.setlocale("LC_TIME", "C")
tweets$created_at <- strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y", tz = "GMT") %>%
    format(as.POSIXct(., format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")

datefreq <- as.data.frame(table(tweets$created_at), stringsAsFactors = FALSE)

colnames(datefreq) <- c("Date", "Frequency")


datefreq$Date <- as.Date(datefreq$Date)


ggplot(datefreq, aes(Date, Frequency)) + geom_line() + scale_x_date(labels = date_format("%m-%d")) + xlab("Date") + ylab("Tweets per day")

# Stats

length(unique(tweets$user$id_str)) #482

length(unique(tweets[which(tweets$type == "tweet"),]$user$id_str)) # 250

length(unique(tweets[which(tweets$type != "tweet"),]$user$id_str)) # 263

nrow(tweets[which(tweets$type == "tweet"),]) # 357 so 304 not 'originall'


# Device Sources

sources <- as.data.frame(table(tweets$source), stringsAsFactors = FALSE)

colnames(sources) <- c("Platform", "Use")

sources <- sources[order(sources$Platform),]

sources <- sources[order(-sources$Use),]

others.df <- data.frame(sum(sources[sources$Use <= 10,]$Use), "Others")
names(others.df) <- c("Use", "Platform")

sources.collapsed <- rbind(sources[sources$Use >= 10,], others.df)

ggplot(sources.collapsed, aes(x = "", y = Use, fill = Platform)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)

# Lang distribution

langs <- as.data.frame(table(tweets$user$lang), stringsAsFactors = FALSE)

colnames(langs) <- c("Language", "Freq")

# Merge en and en-gb
langs[3, 2] <- langs[3, 2] + langs[4, 2] + langs[5,2]
langs <- langs[-c(4, 5),]

otherlangs <- data.frame(sum(langs[langs$Freq < 30,]$Freq), "others")
names(otherlangs) <- c("Freq", "Language")

langs <- rbind(langs[langs$Freq >= 30,], otherlangs)

ggplot(langs, aes(x = "", y = Freq, fill = Language)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + ylab("") + xlab("")

# Location querys

length(tweets$place$country[!(is.na(tweets$place$country))])

length(tweets$user$location[!(is.na(tweets$user$location))])