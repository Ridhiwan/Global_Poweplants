# Display the global power plant schema

library(tibble)
library(readr)
library(tidyverse)

gppb <- read.csv("global_power_plant_database.csv", header = TRUE)

gppb1 <- tibble(gppb)

head(gppb1)

# Choose columns wit the most essential data

gppb2 <- gppb1 %>% select(gppd_idnr,capacity_mw,latitude,longitude,primary_fuel,
                          generation_gwh_2013,generation_gwh_2014,generation_gwh_2015,generation_gwh_2016,generation_gwh_2017)

head(gppb2)

# Assume the energy production was zero in the years that it was not recorded
gppb2[is.na(gppb2)] <- 0
head(gppb2)

library(corrplot)

# factorize the character columns
gppb3 <- gppb2
gppb3$gppd_idnr <- as.numeric(as.factor(gppb3$gppd_idnr))
gppb3$primary_fuel <- as.numeric(as.factor(gppb3$primary_fuel))
head(gppb3)

# Plot the correlation figure
corrplot(cor(gppb3),method = "color",type = "upper")

# Average the energy generation columns into one

gppb3$avg_gwh <- rowMeans(gppb3[,c(6:11)], na.rm = TRUE)

gppb4 <- gppb3 %>% select(-c(generation_gwh_2013,generation_gwh_2014,generation_gwh_2015,generation_gwh_2016,generation_gwh_2017,estimated_generation_gwh))
head(gppb4)

# remove rows with zero average energy generation
gppb5 <- gppb4[gppb4$avg_gwh != 0,]

#plot new correlation plot
corrplot(cor(gppb5),method = "color",type = "upper")

