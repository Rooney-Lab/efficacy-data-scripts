# Packages  ---------------------------------------------------------------

library(tidyverse) #ggplot, data organization
library(vegan) # diversity metrics


# Load Data ---------------------------------------------------------------

data <- read.csv("Data/vegetation_data.csv")
str(data) # quick look at it
colnames(data) # grab the column names


species <- data %>% select(ACHMILLE:Seedling) # Just the living cover
env <- data %>% select(Site:TotalStem) # Environmental variables


# If you want to remove rares

sapply(species, function(col) length(unique(col))) # how many unique values in column

rares <- species[, sapply(species, function(col) length(unique(col))) > 2] # remove those with fewer than 2 occurrences 
sapply(rares, function(col) length(unique(col))) # check to make sure that worked

write.csv(rares, "Data/vegetation_data_rares.csv") # save it as a csv if you want to use it later

# Diversity Indices -------------------------------------------------------

# I will do this with all the columns, repeat with rares removed if wanted
# using vegan

rich <- rowSums(species > 0) # same as excel countif > 0, can also use "specnumber" from vegan
H <- diversity(species, index = "shannon", base = exp(1)) # Shannon Weiner
D1 <- diversity(species, index = "simpson") #default is base log, but can change it
J <- H/log(specnumber(species))



# Create Univariate Dataframe ---------------------------------------------

# add diversity metrics to other environmental variables
env$richness <- rich
env$H <- H
env$D1 <- D1
env$J <- J

colnames(env)

write.csv(env, "Data/efficacy_univariate.csv")






