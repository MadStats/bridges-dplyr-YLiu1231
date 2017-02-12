# install packages we need
install.packages("ggplot2") 
install.packages("plyr")
install.packages("choroplethr")
install.packages("dplyr")
install.packages("readr")

# load all packages 
library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(choroplethrMaps)

# save all abbreviated names of states in the USA
states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()

# get all types of variables from the bridge dataset.
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)

# download all the bridge data from web.
dest= rep("", 52)
for(i in 1:52) 
  dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
data16 = ldply(dest, fread, colClasses = classes) 

# save data to my computer.
#save(data16,file = "allStates16.RData")

# create a new data frame 
M<-data16

wisc = filter(M, STATE_CODE_001 == 55)

# extract the latitude, longitude, the scores for distinguishing whether one bridge is qualified.
safety<-wisc[,c(1,2,9,20,21,27,37:40)]
M1<-M[,c(1,2,9,20,21,27,37:40)]

# delete the rows which have missing values.
safety<-as.data.frame(na.omit(safety))
M1<-as.data.frame(na.omit(M1))

# use filter to get rid of not interpretable bridges.
sa<-filter(safety,safety[,7]!="N"&safety[,8]!="N"&
             safety[,9]!="N"&safety[,10]!="N")
M11<-filter(M1,M1[,7]!="N"&M1[,8]!="N"&
              M1[,9]!="N"&M1[,10]!="N")

sa<-data.frame(sa[,1:6],as.numeric(sa[,7]),as.numeric(sa[,8]),
               as.numeric(sa[,9]),as.numeric(sa[,10]))
M11<-data.frame(M11[,1:6],as.numeric(M11[,7]),as.numeric(M11[,8]),
                as.numeric(M11[,9]),as.numeric(M11[,10]))

# rename this dataset
names(sa)<-c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003",
             "latitude","longitude","year","RAILINGS","TRANSITIONS","APPR_RAIL","APPR_RAIL_END")
sa<-na.omit(sa)
names(M11)<-c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003",
             "latitude","longitude","year","RAILINGS","TRANSITIONS","APPR_RAIL","APPR_RAIL_END")
M11<-na.omit(M11)

# add all scores getting the rank of these bridges.
rank<-sa[,7]+sa[,8]+sa[,9]+sa[,10]
rank1<-M11[,7]+M11[,8]+M11[,9]+M11[,10]

# add this variable to sa dataframe.
sa<-mutate(sa,rank=rank)
M11<-mutate(M11,rank=rank1)

# now I want to draw the plot about the bridges in USA grouped by their quality level.
min2dec = function(x){
  n = nchar(x)
  as.numeric(substr(x,1,n-6)) + as.numeric(substr(x,n-5,n))/6e+05 %>% return
}

# plot the raw data 
ggplot(data = sa) +geom_point(mapping = aes(y = latitude, x = longitude))

# plot after transformation
sa = mutate(sa,lat = min2dec(latitude), lon = - min2dec(longitude))
sa<-na.omit(sa)
M11<- mutate(M11,lat = min2dec(latitude), lon = - min2dec(longitude))
M11<-na.omit(M11)

sa = filter(sa,lat>41,lat<47)
ggplot(data = sa) +geom_point(mapping = aes(y = lat, x = lon))

M11<- M11 %>% filter(lon>-150,lat<50,lat>24.5)
M11<- M11 %>% filter(lon<(-50))
ggplot(data = M11) +geom_point(mapping = aes(y = lat, x = lon))

# Bridges with good quality are dyed by light blue. Dark blue means this kind of bridges are bad.
sa<- mutate(sa, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
M11<- mutate(M11,fips = STATE_CODE_001*1000+COUNTY_CODE_003)

ggplot(data = sa) +geom_point(mapping = aes(y = lat, x = lon,col =rank))
ggplot(data = M11) + geom_point(mapping=aes(y = lat, x = lon,col = rank)) 

pdf("safety level of bridges in Wisconsin and the whole country.pdf")

sa %>% group_by(fips) %>%
  summarise(rank = mean(rank)) %>%
  transmute(region = fips, value = rank) %>% 
  county_choropleth(state_zoom ="wisconsin",num_colors=9) +
  ggtitle("The average bridges safety level in Wisconsin in history")

sa1<-filter(sa,year<1983)
sa1 %>% group_by(fips) %>%
  summarise(rank = mean(rank)) %>%
  transmute(region = fips, value = rank) %>% 
  county_choropleth(state_zoom ="wisconsin",num_colors=9) +
  ggtitle("The average bridges safety level in Wisconsin before 1983")

sa2<-filter(sa,year>=1983)
sa2 %>% group_by(fips) %>%
  summarise(rank = mean(rank)) %>%
  transmute(region = fips, value = rank) %>% 
  county_choropleth(state_zoom ="wisconsin",num_colors=9) +
  ggtitle("The average bridges safety level in Wisconsin after 1983")

a <- c(1,3:10,12:50)
M11<- filter(M11,fips!=32025)
M11 %>% group_by(fips) %>%
  summarise(rank = mean(rank)) %>%
  transmute(region = fips, value = rank) %>% 
  county_choropleth(state_zoom = as.vector(tolower(states$Alberta[a])),num_colors=9) +
  ggtitle("The average bridges safety level in USA in history")

M111<-filter(M11,year<1973)
M111 %>% group_by(fips) %>%
  summarise(rank = mean(rank)) %>%
  transmute(region = fips, value = rank) %>% 
  county_choropleth(state_zoom = as.vector(tolower(states$Alberta[a])),num_colors=9) +
  ggtitle("The average bridges safety level in the USA before 1973")

M112<-filter(M11,year>=1973)
M112 %>% group_by(fips) %>%
  summarise(rank = mean(rank)) %>%
  transmute(region = fips, value = rank) %>% 
  county_choropleth(state_zoom = as.vector(tolower(states$Alberta[a])),num_colors=9) +
  ggtitle("The average bridges safety level in the USA after 1973")
dev.off()


