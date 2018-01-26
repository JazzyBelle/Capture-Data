library(tidyverse)

setwd("C:/Users/lherb/Documents/PhD/Data/Capture Data/")

capt_data = read.table("allcaptures_allcaves.txt", h=T, colClasses = c('factor','factor','factor','factor','factor','factor','factor','character','factor'))

# Remove duplicated observations based on ID, season and year
# Meaning that fish captured more than once in one season-year are kept only once
# No work with cave. /!\/!\/!\/!\ VERY unlikely that an individual was caught in 2 
# different caves within a 2(max 3)-weeks interval /!\/!\/!\/!\ 
capt_data = capt_data[!duplicated(capt_data[3:5]),]


# HOW MANY INDIVIDUALS ARE BEING FOLLOWED TOTAL?

length(levels(capt_data$ID)) # 3298 different IDs overall

# HOW MANY INDIVIDUALS PER CAVE ? HOW MANY WITH A CERTAIN NUMBER OF CAPTURES ?

n.caves=length(levels(capt_data$cave))
detail_per_cave = as.data.frame(matrix(data=NA,nrow=n.caves, ncol=2))
colnames(detail_per_cave) = c('cave', 'n.fish')
detail_per_cave[,1] = as.factor(levels(capt_data$cave))

for (i in 1:n.caves){
  # counting the number of fish per cave
  a = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
  detail_per_cave[i,2] = length(levels(droplevels(a$ID)))
  
  # counting the number of fish with a certain number of recaptures
  b = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
  per_cave_levels = as.data.frame(table(droplevels(a$ID)))
  
  detail_per_cave$n.1capture[i] = length(which(per_cave_levels$Freq == 1))
  detail_per_cave$percent.1capture[i] = 100 * detail_per_cave$n.1capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.2capture[i] = length(which(per_cave_levels$Freq == 2))
  detail_per_cave$percent.2capture[i] = 100 * detail_per_cave$n.2capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.3capture[i] = length(which(per_cave_levels$Freq == 3))
  detail_per_cave$percent.3capture[i] = 100 * detail_per_cave$n.3capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.4capture[i] = length(which(per_cave_levels$Freq == 4))
  detail_per_cave$percent.4capture[i] = 100 * detail_per_cave$n.4capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.5capture[i] = length(which(per_cave_levels$Freq == 5))
  detail_per_cave$percent.5capture[i] = 100 * detail_per_cave$n.5capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.6capture[i] = length(which(per_cave_levels$Freq == 6))
  detail_per_cave$percent.6capture[i] = 100 * detail_per_cave$n.6capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.7capture[i] = length(which(per_cave_levels$Freq == 7))
  detail_per_cave$percent.7capture[i] = 100 * detail_per_cave$n.7capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.8capture[i] = length(which(per_cave_levels$Freq == 8))
  detail_per_cave$percent.8capture[i] = 100 * detail_per_cave$n.8capture[i] / detail_per_cave$n.fish[i]
  
  detail_per_cave$n.9capture[i] = length(which(per_cave_levels$Freq == 9))
  detail_per_cave$percent.9capture[i] = 100 * detail_per_cave$n.9capture[i] / detail_per_cave$n.fish[i]
}

# n = 3401. It is more than the overall number of IDs because some fish moved between caves and 
# HOW MANY HAVE BEEN CAPTURED 1, 2, 3, ... IN TOTAL? PER CAVE?



# THINK OF A WAY TO COUNT FISH THAT HAVE POTENTIALLY LOST THEIR TAGS..