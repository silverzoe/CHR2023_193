
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
######################################
####gdp data added#######
######################################

#read pre processed files
d = read.csv('/Users/silverzoe/Documents/STUDY AT ENS/project - chinese fiction descriptions of baidu/重要文件/analysis/computational humanity conferences/documents/data/pre_analysis_love_discriptions.csv')
#using maddison's 2020 project
#round the Gdp per capita and fill in the missing values
#missing gdp fill in:
# 1- 1000 :1225 (gdp is the same since 1AD - 1000AD)
# 1000 - 1400: liner point for every year (get intercept from the line between the year)
# after 1400: get the closet gdp numbers for every year
# 1620 - 1661 ：liner point for every year (get intercept from the line between the year)
# 1661 -  : get the closet gdp for every year
# 1911 - 1912 - 1913: for missing value 1912, get mean of 1911 and 1913
# 1913 - 2023 : for missing values, get the closet gdp for every year(get intercept from the line between the year)
# using world bank data for 2019,2020,2021,2022
gdp_rounded <- read.csv('gdp_rounded.csv')
d$Year <- as.numeric(d$Year)
library(dplyr)
gdp_rounded$Year <- as.numeric(gdp_rounded$Year)
gdp_rounded <- gdp_rounded[, -1]
d <- left_join(d, gdp_rounded, by = "Year")

#here we started with data processing
# Preprocessing
d_baidu = d[d$baidu_length > 0, ]
d_baidu$Source = "Baidu"
d_wiki = d[d$wiki_length > 0, ]
d_wiki$Source = "Wiki"
d_scholar = d[d$scholar_length > 0, ]
d_scholar$Source = "Scholar"

d_baidu$length = d_baidu$baidu_length
d_wiki$length = d_wiki$wiki_length
d_scholar$length = d_scholar$scholar_length

d_baidu$love = d_baidu$love_baidu
d_wiki$love = d_wiki$love_wiki
d_scholar$love = d_scholar$love_scholar

d_tot = rbind(d_baidu, d_wiki, d_scholar)

#remove video games
d_tot <- d_tot[d_tot$Type != "video game", ] 
d_tot <- d_tot[d_tot$Type != "game", ] 
d_tot <- d_tot[d_tot$Type != "Game", ] 
d_tot <- d_tot[d_tot$Type != "mobile game ", ] 

#clean the type into textual and video
is_literary = (d_tot$Type == "chuanqi")|(d_tot$Type == "collection")|
  (d_tot$Type == "chuanqi ")|(d_tot$Type == "Literary work")|
  (d_tot$Type == "novel")|(d_tot$Type == "manga")|
  (d_tot$Type == "hua ben ")|(d_tot$Type == "play")|
  (d_tot$Type == "play ")|(d_tot$Type == "qu")|
  (d_tot$Type == "story")

is_video = (d_tot$Type == "film")|(d_tot$Type == "movie")|
  (d_tot$Type == "Tv program")|(d_tot$Type == "series")|
  (d_tot$Type == "seires")|(d_tot$Type == "animation")


d_tot$Type[is_literary] = "textual"
d_tot$Type[is_video] = "video"

d_tot = d_tot[is_literary|is_video,]

d_tot$Year = as.numeric(d_tot$Year)
# the processing part end up here 
######################################
####model analysis      #######
######################################
d_tot$love_ratio <- d_tot$love / d_tot$length


#for love of ancient 618 ~ 1799
ancient <- d_tot[d_tot$Year < 1800, ] 
ancient <- ancient[ancient$Year > 618, ] 

model1 = glm(love ~ Year  + length + Source, family = poisson(), data = ancient)
summary(model1)
AIC(model1)

#for love during 1978 post
modern <- d_tot[d_tot$Year > 1978, ] 
model2 = glm(love ~ Year + length + Source + Type, family = poisson(), data = modern)
summary(model2)
AIC(model2)


#for love during whole history and explained by GDP 
d_tot <- d_tot[d_tot$Year > 618, ] 
model3 = glm(love ~ Gdp + Year  + length + Source, family = poisson(), data = d_tot)
summary(model3)

#using gam model to predict the general evolution pattern of romantic love in history 
# Fit a GAM with a smooth term for Year through history
library(mgcv)
model4 <- gam(love ~ s(Year) + length + Type + Source, family = poisson(), data = d_tot)
summary(model4)
# Plot the relationship between Year and harm from the GAM
plot(model4, select = 1, se = TRUE, rug = TRUE,ylab = "Estimated effect of the Year on love")



modern <- d_tot[d_tot$Year > 1900, ]
model5 <- gam(love ~ s(Year) + length + Type + Source, family = poisson(), data = modern)
summary(model5)
# Plot the relationship between Year and harm from the GAM
plot(model5, select = 1, se = TRUE, rug = TRUE,ylab = "Estimated effect of the Year on love")

######################################
####adaptation analysis      #######
######################################

#based on the two columns from where adapted and DBID, we can use the node id and links between two 
library(dplyr)
df <- d %>% replace(.=="", NA) # replace with NA
#keep only raws that are adapted
adaptations <- df[!(is.na(df$From_where_adapted_1)),]
adaptations <- adaptations[ , -which(names(adaptations) %in% c("X","X.1"))]
# Using dplyr::filter
original <- dplyr::filter(df, DBID %in% c("1958863","nodb_20","nodb_27","nodb_8","nodb_17","2308676","nodb_30","25734257","nodb_1 " , 
                                          "nodb_1174" ,"nodb_8 ","nodb_284","3765099","nodb_37","nodb_169","nodb_25","3200108" ,"25889744" , "nodb_32" , 
                                          "nodb_28" , "nodb_173" , "nodb_203"  ,"nodb_11"  , "nodb_212" , "nodb_200" , "nodb_211" , "nodb_199" , "nodb_242" ,
                                          "1142794","1029553"  , "nodb_219" , "nodb_168", "1921294" ,  "nodb_298" , "3810699" ,  "nodb_283" , "nodb_341" , "nodb_18" ,
                                          "nodb_293","nodb_187" , "3191229","nodb_1172" ,"nodb_178", "nodb_234","nodb_349" , "nodb_255" , "nodb_35" ,  "nodb_254" ,
                                          "nodb_7","nodb_344","nodb_378","nodb_351","3567384" , "nodb_264" , "nodb_464" , "1439197" ,  "3409576" ,  "nodb_271",  "nodb_474" 
                                          ,"nodb_33" ,  "nodb_480" , "nodb_489" , "nodb_472"  ,"nodb_235" , "1956966"  , "1007305" ,  "nodb_127" , "nodb_116" , "nodb_45" , 
                                          "3206343"  , "nodb_192" , "nodb_579" , "nodb_205" , "nodb_582" , "1121449"  , "nodb_21" ,  "nodb_1173" ,"nodb_367" , "nodb_568", 
                                          "nodb_610" , "nodb_238" , "nodb_473" , "nodb_295" , "nodb_651" , "nodb_646" , "nodb_346" , "nodb_241" , "4832865"  , "nodb_181", 
                                          "nodb_678" , "nodb_684" , "1965031"  , "nodb_591" , "nodb_711" , "nodb_627" , "nodb_716" , "nodb_201" , "nodb_688" , "3409573" , 
                                          "nodb_687" , "nodb_690"  ,"nodb_691" , "nodb_686" , "nodb_657" , "nodb_689" , "nodb_658" , "nodb_732" , "1042053"  , "nodb_549", 
                                          "nodb_695" , "1019568" ,  "nodb_741" , "nodb_880" , "nodb_645" , "11629279" , "1705591" ,  "1262336" ,  "nodb_794",  "1144385" , 
                                          "nodb_693" , "nodb_692" , "nodb_1175" ,"nodb_377" , "nodb_12",   "nodb_1033" ,"26277313" , "1016631" ,  "nodb_491" , "3037380"  ,
                                          "3789901" ,  "1027488"  , "1742054"  , "nodb_373" , "26314458" , "nodb_999" , "nodb_130" , "6533514" ,  "1926289" ,  "nodb_91" , 
                                          "26904320" , "11591355" , "nodb_216" , "5380746" ,  "3341128" ,  "1493256" ,  "2153527" ,  "nodb_7 "  , "1391191"  , "3574529" , 
                                          "1003000" ,  "30719360" , "25791071" , "Nodb_2 " ,  "nodb_481"))
original <- original[ , -which(names(original) %in% c("X","X.1"))]
#merge original dataset with adaptations
t <- rbind(original, adaptations)
t$From_where_adapted_1 <- trimws(t$From_where_adapted_1)
t$From_where_adapted_2 <- trimws(t$From_where_adapted_2)
t$DBID <- trimws(t$DBID)

#plot the adaptation networks 
library(igraph)
library(showtext)
showtext_auto()
# Specify the font file path
font_path <- "yahei.ttf"  # Replace with the actual file path to your font file
# Add the font using the specified file path
font_add("Microsoft YaHei", regular = font_path)
# Create a new data frame with only relevant columns
adaptations <- t[!is.na(t$From_where_adapted_1) , 
                 c("DBID", "From_where_adapted_1", "From_where_adapted_2")]
# Create a directed graph
G <- graph_from_data_frame(adaptations, directed = TRUE)
V(G)$label <- name_mapping[match(V(G)$name, name_mapping$DBID), "Name_chinese"]

# Get the corresponding Name_chinese values for each DBID
name_mapping <- t[, c("DBID", "Name_chinese")]

# Set the vertex labels and sizes
V(G)$label <- name_mapping[match(V(G)$name, name_mapping$DBID), "Name_chinese"]
V(G)$size <- 3  # Adjust the node size as desired

# Set the edge width
E(G)$width <- 3  # Adjust the edge width as desired
E(G)$arrow.size <- 0.1

# Plot the network graph
plot(
  G, 
  edge.arrow.size = 0.1, 
  vertex.label.cex = 0.2,
  edge.label.cex = 0.1, 
  vertex.label.dist = 0,  # Increase the label distance from nodes
  vertex.frame.color = "white",  # Add a white frame around the nodes for better visibility
  vertex.label.color = "black"  ,
  vertex.label.family = "SimHei",
  width = 1500,  # Adjust the width of the plot window
  height = 1500
)




#here we statitically test whether adaptation combine more love than original (based on their differences in GDP per capita)

t <- d_tot %>% replace(.=="", NA) # replace with NA

adaptations <- subset(t, !is.na(From_where_adapted_1) | !is.na(From_where_adapted_2))
merged_data <- merge(adaptations, t, by.x = "From_where_adapted_1", by.y = "DBID", all.x = TRUE)
merged_data <- merged_data[!duplicated(merged_data), ]

# Compute the variables we have 
# remove the data except for mainland china 
# Create a vector of valid eras that should be kept (e.g., ancient and modern China)
valid_eras <- c("CN", "Qing", "Ming", "Tang", "Yuan", "Song")
# Create a new dataframe with only the rows having era.x values in the valid_eras vector
merged_data <- merged_data[merged_data$Era.x %in% valid_eras, ]

#the delta of love in adaptations and original works 
merged_data$delta_love <- merged_data$love.x - merged_data$love.y
#the delta of love in adaptations and original works 
merged_data$delta_year <- merged_data$Year.x - merged_data$Year.y
#the delta of length in adaptations and original works 
merged_data$delta_length <- merged_data$length.x - merged_data$length.y
#the delta of Gdp in adaptations and original works 
merged_data$delta_gdp <- log(merged_data$Gdp.x) - log(merged_data$Gdp.y)

#the delta of length in adaptations and original works 
merged_data$delta_type <- ifelse(merged_data$Type.x == merged_data$Type.y, "not change",
                                 ifelse(merged_data$Type.x == "textual", "vid to txt", "txt to vid"))
#the delta of length in adaptations and original works 
merged_data$wtob <- with(merged_data,
                         ifelse(Source.x == "Baidu" & Source.y == "Wiki", 1,
                                ifelse(Source.x == "Scholar" & Source.y == "Wiki", 0,
                                       ifelse(Source.x == "Wiki" & Source.y == "Wiki", 0,
                                              ifelse(Source.x == "Wiki" & Source.y == "Baidu", -1, NA)
                                       )
                                )
                         )
)
merged_data$btos <- with(merged_data,
                         ifelse(Source.x == "Baidu" & Source.y == "Wiki", 0,
                                ifelse(Source.x == "Scholar" & Source.y == "Wiki", 0,
                                       ifelse(Source.x == "Wiki" & Source.y == "Wiki", 0,
                                              ifelse(Source.x == "Wiki" & Source.y == "Baidu", 0, NA)
                                       )
                                )
                         )
)


# Assuming merged_data$delta_love is a numeric variable
hist(merged_data$delta_love, main = "Distribution of delta_love",
     xlab = "Delta Love", ylab = "Frequency", col = "lightblue", breaks = 50)
#test the regression model to control source, type, length with gdp and love. 
model15 <- lm(delta_love ~ delta_gdp + delta_length, data = merged_data)
summary(model15)
model16 <- lm(delta_love ~ delta_gdp + delta_length + delta_type + delta_source, data = merged_data)
summary(model16)



