#PARESH GUPTA

install.packages("xlsx")
install.packages("rworldmap")
install.packages("ggmap")
install.packages("mapproj")
install.packages("mapdata")
install.packages("revgeo")
install.packages("googleway")
install.packages("e1071")
install.packages("caret")
install.packages("rpart.plot")


library(rpart)
library(rpart.plot)
library(stringi)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(qdap)
library(text2vec)
library(ggmap)
library(mapproj)
library(rworldmap)
library(maps)
library(mapdata)
library(revgeo)
library(googleway)
library(e1071)
library(caret)
library(geosphere)

#Reverse - geocoding the geographical points

latlong <- read.csv("latlong.csv")

for (i in 1:nrow(latlong)){
  latlong$Radius.in.miles[i] <- sqrt(latlong$Area.in.mi.sq.[i]/3.14)
}

for (i in 1:nrow(latlong)){
  latlong$radius[i] <-  1609.34*latlong$Radius.in.miles[i]
}


twitter <- read_csv("uk_tweets_1.csv")


tweets_english <- subset(twitter, twitter$`Tweet language (ISO 639-1)`=="en")

tweets_GB <- subset(tweets_english, tweets_english$Country == "GB")


tweets_all <- tweets_GB$`Tweet content`
tweets_all <- as.data.frame(tweets_all, stringasFactors = FALSE)
tweets_all$lat <- tweets_GB$Latitude
tweets_all$long <- tweets_GB$Longitude
tweets_all$country <- tweets_GB$Country
tweets_all$tweets_all <- as.character(tweets_all$tweets_all)

tweets3 <- na.omit(tweets_all)

#1st_city

city <- NULL
for (i in 1:nrow(tweets3)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[1],latlong$latitude[1]), c(tweets3$long[i],tweets3$lat[i])) < latlong$radius[1],"Bath","")
  
}  
city <- as.data.frame(city)
tweets3$city <- city$city

#2nd_city

tweets4 <- subset(tweets3, tweets3$city == "")

city <- NULL
for(i in 1:nrow(tweets4)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[2],latlong$latitude[2]), c(tweets4$long[i],tweets4$lat[i])) < latlong$radius[2],"Birmingham","")
}
city <- as.data.frame(city)

tweets4$city <- city$city

#3rd_city

tweets5 <- subset(tweets4, tweets4$city == "")

city <- NULL
for(i in 1:nrow(tweets5)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[3],latlong$latitude[3]), c(tweets5$long[i],tweets5$lat[i])) < latlong$radius[3],"Bradford","")
}
city <- as.data.frame(city)

tweets5$city <- city$city


#4th_city

tweets6 <- subset(tweets5, tweets5$city == "")

city <- NULL
for(i in 1:nrow(tweets6)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[4],latlong$latitude[4]), c(tweets6$long[i],tweets6$lat[i])) < latlong$radius[4],"Brighton & Hove","")
}
city <- as.data.frame(city)

tweets6$city <- city$city

#5th_city

tweets7 <- subset(tweets6, tweets6$city == "")

city <- NULL
for(i in 1:nrow(tweets7)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[5],latlong$latitude[5]), c(tweets7$long[i],tweets7$lat[i])) < latlong$radius[5],"Bristol","")
}
city <- as.data.frame(city)

tweets7$city <- city$city

#6th_city

tweets8 <- subset(tweets7, tweets7$city == "")

city <- NULL
for(i in 1:nrow(tweets8)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[6],latlong$latitude[6]), c(tweets8$long[i],tweets8$lat[i])) < latlong$radius[6],"Cambridge","")
}
city <- as.data.frame(city)

tweets8$city <- city$city

#7th_city

tweets9 <- subset(tweets8, tweets8$city == "")

city <- NULL
for(i in 1:nrow(tweets9)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[7],latlong$latitude[7]), c(tweets9$long[i],tweets9$lat[i])) < latlong$radius[7],"Canterbury","")
}
city <- as.data.frame(city)

tweets9$city <- city$city

#8th_city

tweets10 <- subset(tweets9, tweets9$city == "")

city <- NULL
for(i in 1:nrow(tweets10)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[8],latlong$latitude[8]), c(tweets10$long[i],tweets10$lat[i])) < latlong$radius[8],"Carlisle","")
}
city <- as.data.frame(city)

tweets10$city <- city$city


#9th_city

tweets11 <- subset(tweets10, tweets10$city == "")

city <- NULL
for(i in 1:nrow(tweets11)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[9],latlong$latitude[9]), c(tweets11$long[i],tweets11$lat[i])) < latlong$radius[9],"Chelmsford","")
}
city <- as.data.frame(city)

tweets11$city <- city$city



#10th_city

tweets12 <- subset(tweets11, tweets11$city == "")

city <- NULL
for(i in 1:nrow(tweets12)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[10],latlong$latitude[10]), c(tweets12$long[i],tweets12$lat[i])) < latlong$radius[10],"Chester","")
}
city <- as.data.frame(city)

tweets12$city <- city$city



#11th_city

tweets13 <- subset(tweets12, tweets12$city == "")

city <- NULL
for(i in 1:nrow(tweets13)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[11],latlong$latitude[11]), c(tweets13$long[i],tweets13$lat[i])) < latlong$radius[11],"Chichester","")
}
city <- as.data.frame(city)

tweets13$city <- city$city


#12th_city

tweets14 <- subset(tweets13, tweets13$city == "")

city <- NULL
for(i in 1:nrow(tweets14)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[12],latlong$latitude[12]), c(tweets14$long[i],tweets14$lat[i])) < latlong$radius[12],"Coventry","")
}
city <- as.data.frame(city)

tweets14$city <- city$city

#13th_city

tweets15 <- subset(tweets14, tweets14$city == "")

city <- NULL
for(i in 1:nrow(tweets15)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[13],latlong$latitude[13]), c(tweets15$long[i],tweets15$lat[i])) < latlong$radius[13],"Derby","")
}
city <- as.data.frame(city)

tweets15$city <- city$city

#14th_city

tweets16 <- subset(tweets15, tweets15$city == "")

city <- NULL
for(i in 1:nrow(tweets16)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[14],latlong$latitude[14]), c(tweets16$long[i],tweets16$lat[i])) < latlong$radius[14],"Durham","")
}
city <- as.data.frame(city)

tweets16$city <- city$city

#15th_city

tweets17 <- subset(tweets16, tweets16$city == "")

city <- NULL
for(i in 1:nrow(tweets17)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[15],latlong$latitude[15]), c(tweets17$long[i],tweets17$lat[i])) < latlong$radius[15],"Ely","")
}
city <- as.data.frame(city)

tweets17$city <- city$city

#16th_city

tweets18 <- subset(tweets17, tweets17$city == "")

city <- NULL
for(i in 1:nrow(tweets18)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[16],latlong$latitude[16]), c(tweets18$long[i],tweets18$lat[i])) < latlong$radius[16],"Exeter","")
}
city <- as.data.frame(city)

tweets18$city <- city$city

#17th_city

tweets19 <- subset(tweets18, tweets18$city == "")

city <- NULL
for(i in 1:nrow(tweets19)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[17],latlong$latitude[17]), c(tweets19$long[i],tweets19$lat[i])) < latlong$radius[17],"Gloucester","")
}
city <- as.data.frame(city)

tweets19$city <- city$city

#18th_city

tweets20 <- subset(tweets19, tweets19$city == "")

city <- NULL
for(i in 1:nrow(tweets20)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[18],latlong$latitude[18]), c(tweets20$long[i],tweets20$lat[i])) < latlong$radius[18],"Hereford","")
}
city <- as.data.frame(city)

tweets20$city <- city$city

#19th_city

tweets21 <- subset(tweets20, tweets20$city == "")

city <- NULL
for(i in 1:nrow(tweets21)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[19],latlong$latitude[19]), c(tweets21$long[i],tweets21$lat[i])) < latlong$radius[19],"Kingston-upon-Hull","")
}
city <- as.data.frame(city)

tweets21$city <- city$city

#20th_city

tweets22 <- subset(tweets21, tweets21$city == "")

city <- NULL
for(i in 1:nrow(tweets22)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[20],latlong$latitude[20]), c(tweets22$long[i],tweets22$lat[i])) < latlong$radius[20],"Lancaster","")
}
city <- as.data.frame(city)

tweets22$city <- city$city

#21st_city

tweets23 <- subset(tweets22, tweets22$city == "")

city <- NULL
for(i in 1:nrow(tweets23)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[21],latlong$latitude[21]), c(tweets23$long[i],tweets23$lat[i])) < latlong$radius[21],"Leeds","")
}
city <- as.data.frame(city)

tweets23$city <- city$city



#22nd_city

tweets24 <- subset(tweets23, tweets23$city == "")

city <- NULL
for(i in 1:nrow(tweets24)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[22],latlong$latitude[22]), c(tweets24$long[i],tweets24$lat[i])) < latlong$radius[22],"Leicester","")
}
city <- as.data.frame(city)

tweets24$city <- city$city

#23rd_city

tweets25 <- subset(tweets24, tweets24$city == "")

city <- NULL
for(i in 1:nrow(tweets25)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[23],latlong$latitude[23]), c(tweets25$long[i],tweets25$lat[i])) < latlong$radius[23],"Lichfield","")
}
city <- as.data.frame(city)

tweets25$city <- city$city

#24thcity

tweets26 <- subset(tweets25, tweets25$city == "")

city <- NULL
for(i in 1:nrow(tweets26)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[24],latlong$latitude[24]), c(tweets26$long[i],tweets26$lat[i])) < latlong$radius[24],"Lincoln","")
}
city <- as.data.frame(city)

tweets26$city <- city$city

#25thcity

tweets27 <- subset(tweets26, tweets26$city == "")

city <- NULL
for(i in 1:nrow(tweets27)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[25],latlong$latitude[25]), c(tweets27$long[i],tweets27$lat[i])) < latlong$radius[25],"Liverpool","")
}
city <- as.data.frame(city)

tweets27$city <- city$city

#26thcity

tweets28 <- subset(tweets27, tweets27$city == "")

city <- NULL
for(i in 1:nrow(tweets28)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[26],latlong$latitude[26]), c(tweets28$long[i],tweets28$lat[i])) < latlong$radius[26],"City of London","")
}
city <- as.data.frame(city)

tweets28$city <- city$city

#27thcity

tweets29 <- subset(tweets28, tweets28$city == "")

city <- NULL
for(i in 1:nrow(tweets29)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[27],latlong$latitude[27]), c(tweets29$long[i],tweets29$lat[i])) < latlong$radius[27],"Manchester","")
}
city <- as.data.frame(city)

tweets29$city <- city$city

#28thcity

tweets30 <- subset(tweets29, tweets29$city == "")

city <- NULL
for(i in 1:nrow(tweets30)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[28],latlong$latitude[28]), c(tweets30$long[i],tweets30$lat[i])) < latlong$radius[28],"Newcastle-upon-Tyne","")
}
city <- as.data.frame(city)

tweets30$city <- city$city

#29thcity

tweets31 <- subset(tweets30, tweets30$city == "")

city <- NULL
for(i in 1:nrow(tweets31)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[29],latlong$latitude[29]), c(tweets31$long[i],tweets31$lat[i])) < latlong$radius[29],"Norwich","")
}
city <- as.data.frame(city)

tweets31$city <- city$city

#30thcity

tweets32 <- subset(tweets31, tweets31$city == "")

city <- NULL
for(i in 1:nrow(tweets32)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[30],latlong$latitude[30]), c(tweets32$long[i],tweets32$lat[i])) < latlong$radius[30],"Nottingham","")
}
city <- as.data.frame(city)

tweets32$city <- city$city

#31stcity

tweets33 <- subset(tweets32, tweets32$city == "")

city <- NULL
for(i in 1:nrow(tweets33)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[31],latlong$latitude[31]), c(tweets33$long[i],tweets33$lat[i])) < latlong$radius[31],"Oxford","")
}
city <- as.data.frame(city)

tweets33$city <- city$city


#32ndcity

tweets34 <- subset(tweets33, tweets33$city == "")

city <- NULL
for(i in 1:nrow(tweets34)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[32],latlong$latitude[32]), c(tweets34$long[i],tweets34$lat[i])) < latlong$radius[32],"Peterborough","")
}
city <- as.data.frame(city)

tweets34$city <- city$city

#33rdcity
tweets35 <- subset(tweets34, tweets34$city == "")

city <- NULL
for(i in 1:nrow(tweets35)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[33],latlong$latitude[33]), c(tweets35$long[i],tweets35$lat[i])) < latlong$radius[33],"Plymouth","")
}
city <- as.data.frame(city)

tweets35$city <- city$city

#34thcity

tweets36 <- subset(tweets35, tweets35$city == "")

city <- NULL
for(i in 1:nrow(tweets36)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[34],latlong$latitude[34]), c(tweets36$long[i],tweets36$lat[i])) < latlong$radius[34],"Portsmouth","")
}
city <- as.data.frame(city)

tweets36$city <- city$city

#35thcity

tweets37 <- subset(tweets36, tweets36$city == "")

city <- NULL
for(i in 1:nrow(tweets37)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[35],latlong$latitude[35]), c(tweets37$long[i],tweets37$lat[i])) < latlong$radius[35],"Preston","")
}
city <- as.data.frame(city)

tweets37$city <- city$city

#36thcity

tweets38 <- subset(tweets37, tweets37$city == "")

city <- NULL
for(i in 1:nrow(tweets38)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[36],latlong$latitude[36]), c(tweets38$long[i],tweets38$lat[i])) < latlong$radius[36],"Ripon","")
}
city <- as.data.frame(city)

tweets38$city <- city$city

#37thcity

tweets39 <- subset(tweets38, tweets38$city == "")

city <- NULL
for(i in 1:nrow(tweets39)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[37],latlong$latitude[37]), c(tweets39$long[i],tweets39$lat[i])) < latlong$radius[37],"Salford","")
}
city <- as.data.frame(city)

tweets39$city <- city$city

#38thcity

tweets40 <- subset(tweets39, tweets39$city == "")

city <- NULL
for(i in 1:nrow(tweets40)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[38],latlong$latitude[38]), c(tweets40$long[i],tweets40$lat[i])) < latlong$radius[38],"Salisbury","")
}
city <- as.data.frame(city)

tweets40$city <- city$city

#39thcity

tweets41 <- subset(tweets40, tweets40$city == "")

city <- NULL
for(i in 1:nrow(tweets41)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[39],latlong$latitude[39]), c(tweets41$long[i],tweets41$lat[i])) < latlong$radius[39],"Sheffield","")
}
city <- as.data.frame(city)

tweets41$city <- city$city

#40thcity

tweets42 <- subset(tweets41, tweets41$city == "")

city <- NULL
for(i in 1:nrow(tweets42)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[40],latlong$latitude[40]), c(tweets42$long[i],tweets42$lat[i])) < latlong$radius[40],"Southampton","")
}
city <- as.data.frame(city)

tweets42$city <- city$city

#41stcity

tweets43 <- subset(tweets42, tweets42$city == "")

city <- NULL
for(i in 1:nrow(tweets43)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[41],latlong$latitude[41]), c(tweets43$long[i],tweets43$lat[i])) < latlong$radius[41],"St Albans","")
}
city <- as.data.frame(city)

tweets43$city <- city$city



#42ndcity
tweets44 <- subset(tweets43, tweets43$city == "")

city <- NULL
for(i in 1:nrow(tweets44)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[42],latlong$latitude[42]), c(tweets44$long[i],tweets44$lat[i])) < latlong$radius[42],"Stoke-on-Trent","")
}
city <- as.data.frame(city)

tweets44$city <- city$city

#43rdcity
tweets45 <- subset(tweets44, tweets44$city == "")

city <- NULL
for(i in 1:nrow(tweets45)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[43],latlong$latitude[43]), c(tweets45$long[i],tweets45$lat[i])) < latlong$radius[43],"Sunderland","")
}
city <- as.data.frame(city)

tweets45$city <- city$city

#44thcity

tweets46 <- subset(tweets45, tweets45$city == "")

city <- NULL
for(i in 1:nrow(tweets46)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[44],latlong$latitude[44]), c(tweets46$long[i],tweets46$lat[i])) < latlong$radius[44],"Truro","")
}
city <- as.data.frame(city)

tweets46$city <- city$city

#45thcity

tweets47 <- subset(tweets46, tweets46$city == "")

city <- NULL
for(i in 1:nrow(tweets47)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[45],latlong$latitude[45]), c(tweets47$long[i],tweets47$lat[i])) < latlong$radius[45],"Wakefield","")
}
city <- as.data.frame(city)

tweets47$city <- city$city

#46thcity

tweets48 <- subset(tweets47, tweets47$city == "")

city <- NULL
for(i in 1:nrow(tweets48)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[46],latlong$latitude[46]), c(tweets48$long[i],tweets48$lat[i])) < latlong$radius[46],"Wells","")
}
city <- as.data.frame(city)

tweets48$city <- city$city

#47thcity

tweets49 <- subset(tweets48, tweets48$city == "")

city <- NULL
for(i in 1:nrow(tweets49)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[47],latlong$latitude[47]), c(tweets49$long[i],tweets49$lat[i])) < latlong$radius[47],"(City of) Westminster","")
}
city <- as.data.frame(city)

tweets49$city <- city$city

#48thcity

tweets50 <- subset(tweets49, tweets49$city == "")

city <- NULL
for(i in 1:nrow(tweets50)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[48],latlong$latitude[48]), c(tweets50$long[i],tweets50$lat[i])) < latlong$radius[48],"Winchester","")
}
city <- as.data.frame(city)

tweets50$city <- city$city

#49thcity

tweets51 <- subset(tweets50, tweets50$city == "")

city <- NULL
for(i in 1:nrow(tweets51)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[49],latlong$latitude[49]), c(tweets51$long[i],tweets51$lat[i])) < latlong$radius[49],"Wolverhampton","")
}
city <- as.data.frame(city)

tweets51$city <- city$city

#50thcity

tweets52 <- subset(tweets51, tweets51$city == "")

city <- NULL
for(i in 1:nrow(tweets52)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[50],latlong$latitude[50]), c(tweets52$long[i],tweets52$lat[i])) < latlong$radius[50],"Worcester","")
}
city <- as.data.frame(city)

tweets52$city <- city$city

#51stcity

tweets53 <- subset(tweets52, tweets52$city == "")

city <- NULL
for(i in 1:nrow(tweets53)){
  city[i] <- ifelse(distHaversine(c(latlong$longitude[51],latlong$latitude[51]), c(tweets53$long[i],tweets53$lat[i])) < latlong$radius[51],"York","")
}
city <- as.data.frame(city)

tweets53$city <- city$city

#All_other points
tweets54 <- subset(tweets53, tweets53$city == "")
tweets54$city <- "Not in England"

#Combining all the classified points into a single dataframe

tweets3 <- subset(tweets3, tweets3$city != "")
tweets4 <- subset(tweets4, tweets4$city != "")
tweets5 <- subset(tweets5, tweets5$city != "")
tweets6 <- subset(tweets6, tweets6$city != "")
tweets7 <- subset(tweets7, tweets7$city != "")
tweets8 <- subset(tweets8, tweets8$city != "")
tweets9 <- subset(tweets9, tweets9$city != "")
tweets10 <- subset(tweets10, tweets10$city != "")
tweets11 <- subset(tweets11, tweets11$city != "")
tweets12 <- subset(tweets12, tweets12$city != "")
tweets13 <- subset(tweets13, tweets13$city != "")
tweets14 <- subset(tweets14, tweets14$city != "")
tweets15 <- subset(tweets15, tweets15$city != "")
tweets16 <- subset(tweets16, tweets16$city != "")
tweets17 <- subset(tweets17, tweets17$city != "")
tweets18 <- subset(tweets18, tweets18$city != "")
tweets19 <- subset(tweets19, tweets19$city != "")
tweets20 <- subset(tweets20, tweets20$city != "")
tweets21 <- subset(tweets21, tweets21$city != "")
tweets22 <- subset(tweets22, tweets22$city != "")
tweets23 <- subset(tweets23, tweets23$city != "")
tweets24 <- subset(tweets24, tweets24$city != "")
tweets25 <- subset(tweets25, tweets25$city != "")
tweets26 <- subset(tweets26, tweets26$city != "")
tweets27 <- subset(tweets27, tweets27$city != "")
tweets28 <- subset(tweets28, tweets28$city != "")
tweets29 <- subset(tweets29, tweets29$city != "")
tweets30 <- subset(tweets30, tweets30$city != "")
tweets31 <- subset(tweets31, tweets31$city != "")
tweets32 <- subset(tweets32, tweets32$city != "")
tweets33 <- subset(tweets33, tweets33$city != "")
tweets34 <- subset(tweets34, tweets34$city != "")
tweets35 <- subset(tweets35, tweets35$city != "")
tweets36 <- subset(tweets36, tweets36$city != "")
tweets37 <- subset(tweets37, tweets37$city != "")
tweets38 <- subset(tweets38, tweets38$city != "")
tweets39 <- subset(tweets39, tweets39$city != "")
tweets40 <- subset(tweets40, tweets40$city != "")
tweets41 <- subset(tweets41, tweets41$city != "")
tweets42 <- subset(tweets42, tweets42$city != "")
tweets43 <- subset(tweets43, tweets43$city != "")
tweets44 <- subset(tweets44, tweets44$city != "")
tweets45 <- subset(tweets45, tweets45$city != "")
tweets46 <- subset(tweets46, tweets46$city != "")
tweets47 <- subset(tweets47, tweets47$city != "")
tweets48 <- subset(tweets48, tweets48$city != "")
tweets49 <- subset(tweets49, tweets49$city != "")
tweets50 <- subset(tweets50, tweets50$city != "")
tweets51 <- subset(tweets51, tweets51$city != "")
tweets52 <- subset(tweets52, tweets52$city != "")
tweets53 <- subset(tweets53, tweets53$city != "")
tweets54 <- subset(tweets54, tweets54$city != "")


class_tweet <- rbind(tweets3,tweets4,tweets5,tweets6,tweets7,tweets8,tweets9,tweets10,tweets11,tweets12,tweets13,tweets14,tweets15,tweets16,tweets17,tweets18,tweets19,tweets20,tweets21,tweets22,tweets23,tweets24,tweets25,tweets26,tweets27,tweets28,tweets29,tweets30,tweets31,tweets32,tweets33,tweets34,tweets35,tweets36,tweets37,tweets38,tweets39,tweets40,tweets41,tweets42,tweets43,tweets44,tweets45,tweets46,tweets47,tweets48,tweets49,tweets50,tweets51,tweets52,tweets53,tweets54)

final <- subset(class_tweet, class_tweet$city != "Not in England")
table(final$city)

tweets <- droplevels(final, exclude = levels("State Not Found"))
tweets2 <- tweets



#Cleaning the tweets

tweets$tweets <- str_replace(tweets$tweets,"[a-z]*(://)[a-z]*[.]*[a-z]*/[a-z|A-Z|0-9]*","")

stopwords <- c("a","able","about","above","abst","accordance","according","accordingly","across","act","actually","added","adj","affected","affecting","affects","after","afterwards","again","against","ah","all","almost","alone","along","already","also","although","always","am","among","amongst","an","and","announce","another","any","anybody","anyhow","anymore","anyone","anything","anyway","anyways","anywhere","apparently","approximately","are","aren","arent","arise","around","as","aside","ask","asking","at","auth","available","away","awfully","b","back","be","became","because","become","becomes","becoming","been","before","beforehand","begin","beginning","beginnings","begins","behind","being","believe","below","beside","besides","between","beyond","biol","both","brief","briefly","but","by","c","ca","came","can","cannot","can't","cause","causes","certain","certainly","co","com","come","comes","contain","containing","contains","could","couldnt","d","date","did","didn't","different","do","does","doesn't","doing","done","don't","down","downwards","due","during","e","each","ed","edu","effect","eg","eight","eighty","either","else","elsewhere","end","ending","enough","especially","et","et-al","etc","even","ever","every","everybody","everyone","everything","everywhere","ex","except","f","far","few","ff","fifth","first","five","fix","followed","following","follows","for","former","formerly","forth","found","four","from","further","furthermore","g","gave","get","gets","getting","give","given","gives","giving","go","goes","gone","got","gotten","h","had","happens","hardly","has","hasn't","have","haven't","having","he","hed","hence","her","here","hereafter","hereby","herein","heres","hereupon","hers","herself","hes","hi","hid","him","himself","his","hither","home","how","howbeit","however","hundred","i","id","ie","if","i'll","im","immediate","immediately","importance","important","in","inc","indeed","index","information","instead","into","invention","inward","is","isn't","it","itd","it'll","its","itself","i've","j","just","k","keep",	"keeps","kept","kg","km","know","known","knows","l","largely","last","lately","later","latter","latterly","least","less","lest","let","lets","like","liked","likely","line","little","'ll","look","looking","looks","ltd","m","made","mainly","make","makes","many","may","maybe","me","mean","means","meantime","meanwhile","merely","mg","might","million","miss","ml","more","moreover","most","mostly","mr","mrs","much","mug","must","my","myself","n","na","name","namely","nay","nd","near","nearly","necessarily","necessary","need","needs","neither","never","nevertheless","new","next","nine","ninety","no","nobody","non","none","nonetheless","noone","nor","normally","nos","not","noted","nothing","now","nowhere","o","obtain","obtained","obviously","of","off","often","oh","ok","okay","old","omitted","on","once","one","ones","only","onto","or","ord","other","others","otherwise","ought","our","ours","ourselves","out","outside","over","overall","owing","own","p","page","pages","part","particular","particularly","past","per","perhaps","placed","please","plus","poorly","possible","possibly","potentially","pp","predominantly","present","previously","primarily","probably","promptly","proud","provides","put","q","que","quickly","quite","qv","r","ran","rather","rd","re","readily","really","recent","recently","ref","refs","regarding","regardless","regards","related","relatively","research","respectively","resulted","resulting","results","right","run","s","said","same","saw","say","saying","says","sec","section","see","seeing","seem","seemed","seeming","seems","seen","self","selves","sent","seven","several","shall","she","shed","she'll","shes","should","shouldn't","show","showed","shown","showns","shows","significant","significantly",
               "similar","similarly","since","six","slightly","so","some","somebody","somehow","someone","somethan","something","sometime","sometimes","somewhat","somewhere","soon","sorry","specifically","specified","specify","specifying","still","stop","strongly","sub","substantially","successfully","such","sufficiently","suggest","sup","sure",	"t","take","taken","taking","tell","tends","th","than","thank","thanks","thanx","that","that'll","thats","that've","the","their","theirs","them","themselves","then","thence","there","thereafter","thereby","thered","therefore","therein","there'll","thereof","therere","theres","thereto","thereupon","there've","these","they","theyd","they'll","theyre","they've","think","this","those","thou","though","thoughh","thousand","throug","through","throughout","thru","thus","til","tip","to","together","too","took","toward","towards","tried","tries","truly","try","trying","ts","twice","two","u","un","under","unfortunately","unless","unlike","unlikely","until","unto","up","upon","ups","us","use","used","useful","usefully","usefulness","uses","using","usually","v","value","various","'ve","very","via","viz","vol","vols","vs","w","want","wants","was","wasnt","way","we","wed","welcome","we'll","went","were","werent","we've","what","whatever","what'll","whats","when","whence","whenever","where","whereafter","whereas","whereby","wherein","wheres","whereupon","wherever","whether","which","while","whim","whither","who","whod","whoever","whole","who'll","whom","whomever","whos","whose","why","widely","willing","wish","with","within","without","wont","words","world","would","wouldnt","www","x","y","yes","yet","you","youd","you'll","your","youre","yours","yourself","yourselves","you've","z","zero")


tweets <- tolower(tweets$tweets)

tweets <- str_replace_all(tweets,"@[a-z|A-Z|0-9]*","")

tweets <- iconv(tweets, to = "ASCII//TRANSLIT")

tweets <- removePunctuation(tweets)
tweets <- removeWords(tweets, stopwords)

x <- as.data.frame(freq_terms(tweets,30000,at.least = 3))
x <- subset(x, x$FREQ > 10)
x <- as.vector(x$WORD)

tweets <- as.data.frame(tweets)
tweets$tweets <- as.character(tweets$tweets)

tweets$lat <- tweets2$lat
tweets$long <- tweets2$long
tweets$city <- tweets2$city
tweets$city <- as.factor(tweets$city)
str(tweets)

tweets$tweets <- str_replace_all(tweets$tweets,"[^[:graph:]]", " ")


#Forming a corpus and finding the term-document matrix

corpus <- Corpus(VectorSource(tweets$tweets))
tdm <- TermDocumentMatrix(corpus, list(dictionary = x))

tdm_matrix <- as.matrix(tdm)


#Calulating the TF-IDF scores for each term

idf <- log(ncol(tdm_matrix)/( 1 + rowSums(tdm_matrix != 0)))
idf <- diag(idf)

tf_idf <- crossprod(tdm_matrix,idf)

memory.limit(size = 40000)
colnames(tf_idf) <- rownames(tdm_matrix)

df_tf_idf <- as.data.frame(tf_idf)
View(df_tf_idf)

df_tf_idf$location <- tweets2$city 


#classificiation

spl <- sample(1:nrow(df_tf_idf), size = 0.7*nrow(df_tf_idf))

train_set <- df_tf_idf[spl,]
test <- df_tf_idf[-spl,]


#naive bayes

model_cv <- train(location ~ ., data = train_set, method = "nb", trControl = trainControl(method = "cv", number = 10))
pred_cv <- predict(model_cv, newdata = test)

model <- naiveBayes(location ~ ., data = train_set)

pred <- predict(model, newdata = test)

mat <- confusionMatrix(pred, test$location)
mat <- as.matrix(mat)

accuracy <- sum(diag(mat))/sum(mat)

accuracy*100


#Decision Tree

train(location ~ ., data = train_set, method = "rpart", trControl = trainControl(method = "cv", number = 10), tuneGrid = expand.grid(.cp = seq(0.01,0.5,0.01)))

model <- rpart(location ~., data = train_set, method = "class", cp = 0.01)
prp(model)
pred <- predict(model, newdata = test, type = "class")


mat1 <- confusionMatrix(pred, test$location)
mat1 <- as.matrix(mat1)

accuracy1 <- sum(diag(mat1))/sum(mat1)
accuracy1*100


#SVM

fit <- svm(location ~ ., data = train_set)

predicted <- predict(fit, test)

mat2 <- confusionMatrix(predicted, test$location)
mat2 <- as.matrix(mat2)

accuracy2 <- sum(diag(mat2))/sum(mat2)
accuracy2*100


#Mapping and other graphs

#Plotting all points in England on the map

maps::map(
  database = "worldHires",
  regions = c("uk"),
  xlim = c(-12,2),
  ylim = c(49,59),
  col = "gray90",
  fill = TRUE
)

points(tweets$long,tweets$lat, col = "red")


#Frequency distribution of tweets across all cities in England

histo <- ggplot(tweets, aes(x = city)) 
histo + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +ggtitle("Frequency Distribution of tweets") + theme(axis.title.y = element_blank()) + theme(plot.title = element_text(hjust = 0.5))
