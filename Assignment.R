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
tmp1 = read_csv(dest)
tmp2 = read_csv(dest, col_types = "c")  # could make them all characters...
classes = sapply(tmp, class)

# download all the bridge data from web.
dest= rep("", 52)
for(i in 1:52) 
  dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
data16 = ldply(dest, fread, colClasses = classes) 

# save data to my computer.
save(data16,file = "allStates16.RData")

# create a new data frame 
M<-data16

wisc = filter(M, STATE_CODE_001 == 55)
# extract the latitude, longitude, the scores for distinguishing whether one bridge is qualified.
safety<-wisc[,c(20,21,37:40)]

# delete the rows which have missing values.
safety<-as.data.frame(na.omit(safety))

# use filter to get rid of not interpretable bridges.
sa<-filter(safety,safety[,3]!="N"&safety[,4]!="N"&
             safety[,5]!="N"&safety[,6]!="N")

sa<-data.frame(sa[,1],sa[,2],as.numeric(sa[,3]),as.numeric(sa[,4]),
               as.numeric(sa[,5]),as.numeric(sa[,6]))
# rename this dataset
names(sa)<-c("latitude","longitude","RAILINGS","TRANSITIONS","APPR_RAIL","APPR_RAIL_END")
sa<-na.omit(sa)

# add all scores getting the level of these bridges.
rank<-sa[,3]+sa[,4]+sa[,5]+sa[,6]

# create one new dataset with only the level column and their location.
newsa<-data.frame(sa[,1],sa[,2],rank)
names(newsa)<-c("lat","lon","level")

# now I want to draw the plot about the bridges in USA grouped by their quality level.
min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}

# plot the raw data 
ggplot(data = newsa) +geom_point(mapping = aes(y = lat, x = lon))

# plot after transformation
newsa = mutate(newsa,lat = min2dec(lat), lon = - min2dec(lon))
newsa<-na.omit(newsa)

newsa = filter(newsa,lat>41,lat<47)
ggplot(data = newsa) +geom_point(mapping = aes(y = lat, x = lon))

# Bridges with good quality are dyed by light blue. Dark blue means this kind of bridges are bad.
ggplot(data = newsa) +geom_point(mapping = aes(y = lat, x = lon,col =level))
