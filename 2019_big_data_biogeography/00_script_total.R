#' ---
#' title: 2019 big databiogeography
#' author: mauricio vancine
#' date: 2019-01-16
#' ---

# download gbif data ------------------------------------------------------
## Library setup
library(rgbif)
library(tidyverse)
library(rgdal)
library(rgeos)

## 1. Download data for a single species we saw in the filedand save them in a data.frame
# Search occurrence records
dat <- occ_search(scientificName = "Wittmackia patentissima", 
                  return =  "data", limit = 1000)
dat

## 2. Explore the downloaded data
nrow(dat) # Check the number of records
head(dat) # Check the data
plot(dat$decimalLatitude ~ dat$decimalLongitude) # Look at the georeferenced records

## 3. How many records are available for your group of interest?
#Use the name_suggest function to get the gbif taxon key
tax_key <- name_suggest(q = "Magnoliopsida", rank = "Class")
tax_key

#Sometimes groups have multiple taxon keys, in this case three, so we will check how many records are available for them
lapply(tax_key$key, "occ_count")

#Here the firsrt one is relevant, check for your group!
tax_key <- tax_key$key[1]
tax_key
occ_count(tax_key, country = "DE")

##4. Download all data for your group from Brazil, or if the group is very large from part of that area.
dat  <- occ_search(taxonKey = tax_key, return = "data",
                   country = "BR", hasCoordinate = T, limit = 1000)
dat

study_a <- "POLYGON((-35 -4.5, -38.5 -4.5, -38.5 -7, -35 -7, -35 -4.5))"
study_a

dat_ne  <- occ_search(taxonKey = tax_key, return = "data", hasCoordinate = T,
                      geometry = study_a, limit = 1000) 
dat_ne


gen_list <- c("Ceiba", "Eriotheca")

tax_key <- lapply(gen_list, function(k){name_suggest(q = k, rank = "Genus")})
tax_key <- unlist(lapply(tax_key, "[[", "key"))

unlist(lapply(tax_key, "occ_count"))

dat_ne  <- occ_search(taxonKey = tax_key, return = "data", hasCoordinate = T,
                      limit = 1000, country = "BR") 
dat_ne <- lapply(dat_ne, "as.data.frame")

dat_ne <- bind_rows(dat_ne)
dat_ne

##5. Save the downloaded data as .csv to the working directory.
dir.create("inst")
write_csv(dat_ne, path = "inst/gbif_occurrences.csv")


# cleaning geographic data --------------------------------------------

## Library setup
library(tidyverse)
library(rgbif)
library(sp)
library(countrycode)
library(CoordinateCleaner)

## 1. Load your occurrence data downloaded from GBIF
dat <- read_csv("inst/gbif_occurrences.csv", guess_max = 25000)%>%
  dplyr::mutate(dataset = "GBIF")

names(dat) #a lot of columns

dat <- dat %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount,
         gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
         basisOfRecord, institutionCode, datasetName, dataset)%>% # you might find other ones useful depending on your downstream analyses
  dplyr::mutate(countryCode = countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c'))
dat

## 3. Visualize the coordinates on a map
world.inp  <- map_data("world")

ggplot()+
  geom_map(data=world.inp, map=world.inp, aes(x=long, y=lat, map_id=region), fill = "grey80")+
  xlim(min(dat$decimalLongitude, na.rm = T), max(dat$decimalLongitude, na.rm = T))+
  ylim(min(dat$decimalLatitude, na.rm = T), max(dat$decimalLatitude, na.rm = T))+
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude, color = dataset),
             size = 1)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank())

## 4. Clean the coordinates based on available meta-data
# remove records without coordinates
dat_cl <- dat%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))

#remove records with low coordinate precision
hist(dat_cl$coordinateUncertaintyInMeters/1000, breaks = 30)

dat_cl <- dat_cl %>%
  filter(coordinateUncertaintyInMeters/1000 <= 100 | is.na(coordinateUncertaintyInMeters))

#remove unsuitable data sources, especially fossils
table(dat$basisOfRecord)

dat_cl <- filter(dat_cl, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "OBSERVATION" |
                   basisOfRecord == "PRESERVED_SPECIMEN" | is.na(basisOfRecord))
dat_cl

#Individual count
table(dat_cl$individualCount)

dat_cl <- dat_cl%>%
  filter(individualCount > 0 | is.na(individualCount))%>%
  filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem
dat_cl

#Age of records
table(dat_cl$year)

dat_cl <- dat_cl%>%
  filter(year > 1945) # remove records from before second world war
dat_cl

table(dat_cl$family) #that looks good
table(dat_cl$taxonRank) # We will only include records identified to species level
dat_cl <- dat_cl%>%
  filter(taxonRank == "SPECIES" | is.na(taxonRank))

##5. Apply automated flagging to identify potentially problematic records
#flag problems
dat_cl <- data.frame(dat_cl)
dat_cl
flags <- clean_coordinates(x = dat_cl, lon = "decimalLongitude", lat = "decimalLatitude",
                           countries = "countryCode", 
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", 
                                     "zeros", "seas"),
                           seas_ref = buffland) # most test are on by default

plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

## 6. Visualize the difference between the uncleaned and cleaned dataset (`plot`)
world.inp  <- map_data("world")

ggplot()+
  geom_map(data=world.inp, map=world.inp, aes(x=long, y=lat, map_id=region), fill = "grey80")+
  xlim(min(dat$decimalLongitude, na.rm = T), max(dat$decimalLongitude, na.rm = T))+
  ylim(min(dat$decimalLatitude, na.rm = T), max(dat$decimalLatitude, na.rm = T))+
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 1)+
  geom_point(data = dat_cl, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkgreen", size = 1)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank())


ggplot()+
  geom_map(data=world.inp, map=world.inp, aes(x=long, y=lat, map_id=region), fill = "grey80")+
  xlim(min(dat$decimalLongitude, na.rm = T), max(dat$decimalLongitude, na.rm = T))+
  ylim(min(dat$decimalLatitude, na.rm = T), max(dat$decimalLatitude, na.rm = T))+
  geom_point(data = dat_cl, aes(x = decimalLongitude, y = decimalLatitude, colour = dataset),
             size = 1)+
  coord_fixed()+
  theme_bw()+
  theme(axis.title = element_blank())

## 8. Write to disk
write_csv(dat_cl, "inst/occurrence_records_clean.csv")


# tue_accesibility_bias ---------------------------------------------------

## Library setup
library(tidyverse)
library(sampbias)

## 1. Load the example distribution data from your data
occ <- read_csv("inst/occurrence_records_clean.csv")%>%
  mutate(decimallongitude = decimalLongitude)%>%
  mutate(decimallatitude = decimalLatitude)
occ

## 2. Run sampbias with the default settings. (SamplingBias)
bias.out <- SamplingBias(x = occ, res = 1)
bias.out

## 3. Look at the run summary and visualize the results. How informative are the results?
#summarize results
summary(bias.out)
  
#Visualize
plot(bias.out)

## 4. Explore `?SamplingBias` and try to change the relevant arguments to improve the results. Summarize and visualize again.
bias.det <- SamplingBias(x = occ, 
                         res = .1,
                         verbose = TRUE)

#summarize results
summary(bias.det)

#Visualize
par(mfrow = c(3,2))
plot(bias.det)

# -------------------------------------------------------------------------
