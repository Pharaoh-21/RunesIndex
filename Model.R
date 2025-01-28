

setwd("~/pharaoh/data")
library(dplyr)
library(httr)
library(jsonlite)
options(scipen = 99999)


api <- "UNISAT API KEY"

###################Runes


url <- "https://open-api.unisat.io/v3/market/runes/auction/runes_types"

headers <- c(
  'accept' = 'application/json',
  'Authorization' = 'Bearer UNISAT API KEY',
  'Content-Type' = 'application/json'
)

body <- toJSON(list(
  timeType = "day1",
  start = 0,
  limit = 100000
), auto_unbox = TRUE)

response <- POST(url, add_headers(.headers = headers), body = body)

data <- fromJSON(content(response, "text", encoding = "UTF-8"))
runes <- as.data.frame(data[["data"]][["list"]])


################################################################################

### K - means


runes$cap <- as.numeric(as.character(runes$cap))
runes$capUSD <- as.numeric(as.character(runes$capUSD))
runes <- subset(runes,runes$holders > 500)
runes <- subset(runes,runes$cap > 0)
runes <- subset(runes,runes$transactions > 150)


df <- runes[,c(8,9,10)]

df$capUSD <- scale(df$capUSD)
df$holders <- scale(df$holders)
df$transactions <- scale(df$transactions)

library(factoextra)   
?fviz_nbclust
#fviz_nbclust(df, kmeans, k.max = 40, method = "wss")
set.seed(690)

km <- kmeans(df, 6)
km

runes$km <- km$cluster

clusters <- as.data.frame(table(runes$km))
clusters <- subset(clusters, clusters$Freq < max(clusters$Freq))


fin <- subset(runes, runes$km %in% clusters$Var1)


################################################################################

fin$cap <- ifelse(fin$tick == "Z•Z•Z•Z•Z•FEHU•Z•Z•Z•Z•Z", fin$curPrice*fin$totalMinted, fin$cap)

fin$pond <- fin$cap/(sum(fin$cap))
fin$index_rune <- fin$curPrice*fin$pond



sum(fin$index_rune)


fin$date <- Sys.Date()

fin <- fin[,c(1,20,21)]
name <- paste0("GPIR_",Sys.Date())

write.csv(fin, name, row.names = FALSE)


################################################################################
setwd("~/pharaoh/data")


library(ggplot2)

files <- list.files()

one <- read.csv(files[1])
two <- read.csv(files[2])

df <- rbind(one, two)

index <- aggregate(df$index_rune, by = list(df$date), sum)
colnames(index) <- c("Date", "GPIR")

max_date <- max(df$date)
yesterday_date <- as.Date(max(df$date))-1

which(my_data == 5) 


today <- index[which(index$Date == max(index$Date)), "GPIR"] 

yesterday <- index[which(index$Date == as.Date(max(index$Date))-1), "GPIR"] 

variacion <- ((today - yesterday)/yesterday)*100


library(ggplot2)
library(dplyr)
library(hrbrthemes)
# Most basic bubble plot
p <- ggplot(index, aes(x=Date, y=GPIR)) +
  geom_line(color="#69b3a2") + 
  xlab("Date")
p

index$grupo <- "Gr"

p <- ggplot(index, aes(x=Date, y=GPIR, group = grupo)) +
  geom_line( color="red", linewidth=0.71) + 
  geom_point() +
  xlab("Date") +
  #theme_ipsum() +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11"))) +
  ylim(0,20)

p


plot(as.Date(dat$Date), dat$GPIR, type = "l")


tod_run <- subset(df, df$date == Sys.Date())

tod_run$tick
