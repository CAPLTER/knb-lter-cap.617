
# README ----

# use mdbtools-gmdb (sudo apt-get install mdbtools-gmdb) to view the mdb and export data

# reml slots ----
getSlots("dataset")
  getSlots("distribution")
  getSlots("keywordSet")
    getSlots("keyword")
getSlots("dataTable")
getSlots("physical")
  getSlots("dataFormat")
    getSlots("textFormat")
  getSlots("size")
  getSlots("distribution")
    getSlots("online")
      getSlots("url")
getSlots("additionalInfo")
  getSlots("section")
  getSlots("para")
getSlots("metadataProvider")
  getSlots("individualName")
  getSlots("userId")
getSlots("creator")
  getSlots("individualName")
  getSlots("userId")

# libraries ----
library("EML")
library('RPostgreSQL')
library('RMySQL')
library('tidyverse')
library("tools")
library("readr")
library("readxl")
library("stringr")

# reml-helper-functions ----
# source('~/localRepos/reml-helper-tools/createdataTableFn.R')
source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createKMLFn.R')
source('~/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/localRepos/reml-helper-tools/createOtherEntityFn.R')
source('~/localRepos/reml-helper-tools/createPeople.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')

# DB connections ----

prod <- dbConnect(MySQL(),
                  user='srearl',
                  password=.rs.askForPassword("Enter password:"),
                  dbname='gios2_production',
                  host='mysql.prod.aws.gios.asu.edu')


# dataset details to set first ----
projectid <- 617
packageIdent <- 'knb-lter-cap.617.2'
pubDate <- '2017-05-05'

# data entity ----

# data processing ----

# file processing ----

files <- list.files(path=".", pattern="*.csv", full.names=T, recursive=FALSE) # identify directory with files (not full.names=T)
batchLoad <- function(file) {data <- read_csv(file) } # function to bring files into R
data <- lapply(files, batchLoad) # import files from target directory
fileNames <- list.files(path=".", pattern="*.csv", full.names=F, recursive=FALSE) # get filenames of list items from directory (note full.names=F); change to lower case on import

# modify and assign filenames
rmtxt <- function(file) { sub(".csv", "", file) } # function to remove txt file extension
fileNames <- lapply(fileNames, rmtxt) # apply function rmtxt to all filenames
names(data) <- fileNames # assign file names to list of datasets

# omit empty columns, originally doing this only for quarterlyMetals where there
# were missing fields but why not do it for all? Could not figure out how to do
# this with apply, so looped it.
for (i in 1:length(data)) { data[[i]] <- Filter(function(x)!all(is.na(x)), data[[i]]) }
# data[['quarterlyMetals']] <- Filter(function(x)!all(is.na(x)), data[['quarterlyMetals']])

# convert empty values to NAs 
data <- lapply(data, function(x) {
  # do the replacing
  x[x=='']<-NA
  return(x)
})

# arsenic ----

arsenic <- data[['arsenic']] %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(`Arsenic (ug/L)` = as.numeric(`Arsenic (ug/L)`)) %>% 
  mutate(date = format(as.POSIXct(`month/date`, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>% 
  select(-ID, -`month/date`) %>% 
  arrange(date, `Site Number`) %>% 
  filter(!is.na(date))
  
writeAttributes(arsenic) # write data frame attributes to a csv in current dir to edit metadata
arsenic_desc <- "arsenic and perchlorate concentrations in canals and reservoirs, 2004-2011"

# address factors if needed
Cluster <- c(cap = "Central Arizona Project canal",
             salt = "Salt River",
             srp = "Salt River Project canal",
             tempe = "Tempe, AZ area canal",
             verde = "Verde River",
             `az canal` = "Arizona canal",
             chandler = "Chandler Water Treatment Plant (WTP)",
             `mesa turnout` = "Mesa, AZ area canal")

arsenic_factors <- factorsToFrame(arsenic)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
arsenic_DT <- createDTFF(dfname = arsenic,
                         factors = arsenic_factors,
                         description = arsenic_desc)

# algae (now called conductivity in the Access database) ----

algae <- data[['conductivity']] %>% 
  rename(Cluster = `cluster name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(date = format(as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>%
  select(-ID, -month) %>% 
  arrange(date, `site number`) %>% 
  filter(!is.na(date))

writeAttributes(algae) # write data frame attributes to a csv in current dir to edit metadata
algae_desc <- "algal characteristics (chlorophyll concentration, cell counts) and specific conductance in canals and reservoirs, 1999-2016"

algae_factors <- factorsToFrame(algae)
  
algae_DT <- createDTFF(dfname = algae,
                       factors = algae_factors,
                       description = algae_desc)


# carbon (docMonth in Access database) ----

carbon <- data[['docMonth']] %>% 
 rename(Cluster = `cluster name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(date = format(as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>%
  select(-ID, -month) %>% 
  arrange(date, `site number`) %>% 
  filter(!is.na(date))

writeAttributes(carbon) # write data frame attributes to a csv in current dir to edit metadata
carbon_desc <- "approximately monthly water carbon data in canals and reservoirs, 1999-2016"

carbon_factors <- factorsToFrame(carbon)
  
carbon_DT <- createDTFF(dfname = carbon,
                        factors = carbon_factors,
                        description = carbon_desc)


# fieldMeasurements ----

fieldMeasurements <- data[['fieldMeasurements']] %>% 
  rename(Cluster = `cluster name`) %>% 
  rename(`Secchi Disk (m)` = `Secchi Disk, m`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(date = format(as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>%
  select(-ID, -month) %>% 
  arrange(date, `site number`) %>% 
  filter(!is.na(date))
  
writeAttributes(fieldMeasurements) # write data frame attributes to a csv in current dir to edit metadata
fieldMeasurements_desc <- "water quality parameters as measured in the field in canals and reservoirs, 1998-2016"

fieldMeasurements_factors <- factorsToFrame(fieldMeasurements)
  
fieldMeasurements_DT <- createDTFF(dfname = fieldMeasurements,
                                   factors = fieldMeasurements_factors,
                                   description = fieldMeasurements_desc)


# gpscoord ----

sampling_locations <- data[['gpscoord']] %>%
  rename(Cluster = `cluster name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(date = format(as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>%
  mutate(long_deg = as.numeric(str_match(longitude, "\\d+"))) %>% 
  mutate(long_min = as.numeric(str_match(longitude, "\\d+\\.\\d+$"))) %>%
  mutate(long_dd = long_min/60) %>% 
  mutate(long = long_deg + (long_min/60)) %>% 
  mutate(lat_deg = as.numeric(str_match(laditude, "\\d+"))) %>% 
  mutate(lat_min = as.numeric(str_match(laditude, "\\d+\\.\\d+$"))) %>%
  mutate(lat_dd = lat_min/60) %>% 
  mutate(lat = lat_deg + (lat_min/60)) %>% 
  mutate(altitude = `altitude (ft)` * 0.3048) %>%
  filter(!is.na(long)) %>% 
  select(`site number`:date,long, lat, altitude)
  
writeAttributes(sampling_locations) # write data frame attributes to a csv in current dir to edit metadata
sampling_locations_desc <- "latitude, longitude, and elevation of select canal, lake, and river water-quality sampling locations"

sampling_locations_factors <- factorsToFrame(sampling_locations)
  
sampling_locations_DT <- createDTFF(dfname = sampling_locations,
                                    factors = sampling_locations_factors,
                                    description = sampling_locations_desc)
  

# algalByproducts ----

algalByproducts <- data[['mibAndGeosmin']] %>% 
  rename(Cluster = `cluster name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(date = format(as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>%
  select(-ID, -month) %>% 
  arrange(date, `site number`) %>% 
  filter(!is.na(date))
  
writeAttributes(algalByproducts) # write data frame attributes to a csv in current dir to edit metadata
algalByproducts_desc <- "concentration of algal by-products in canals and reservoirs, 1999-2016"

algalByproducts_factors <- factorsToFrame(algalByproducts)
  
algalByproducts_DT <- createDTFF(dfname = algalByproducts,
                                 factors = algalByproducts_factors,
                                 description = algalByproducts_desc)


# microbial ----

# microbial was an absolute mess. Marisa has flags in the Coliform and
# Mycobacterium count fields. This information is lost if converted to numeric.
# So, I have pulled those text flags out of the count fields and put them in new
# flag fields as factors. Note other things here too like changing some N/A
# entries to NA, and since I was doing so much manipulating anyway, I went ahead
# and made the field names a bit more pleasant.

microbial <- data[['microbial']] %>% 
  rename(Cluster = `Cluster Name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(E_coli = as.numeric(`E coli(colonies per 100mL)`)) %>% 
  rename(Coliform = `Coliform(colonies per 100mL)`) %>% 
  mutate(Coliform = replace(Coliform, Coliform == "N/A", NA)) %>% 
  mutate(Coliform_flag = as.factor(str_match(Coliform, "[A-z]+"))) %>% 
  mutate(Coliform = as.numeric(Coliform)) %>% 
  rename(Mycobacterium = `Mycobacterium(colonies per 100mL)`) %>% 
  mutate(Mycobacterium = replace(Mycobacterium, Mycobacterium == "N/A", NA)) %>% 
  mutate(Mycobacterium_flag = as.factor(str_match(Mycobacterium, "[A-z]+.*"))) %>% 
  mutate(Mycobacterium = as.numeric(Mycobacterium)) %>% 
  select(`Site Number`, 
         `Site Location`,
         Cluster,
         `Site Acronym`,
         date,
         E_coli, 
         Coliform, 
         Coliform_flag, 
         Mycobacterium, 
         Mycobacterium_flag, 
         Comments) %>% 
  filter(!is.na(date)) %>% 
  arrange(date, `Site Number`)
  
Coliform_flag <- c(TNTC = "Too Numerous to Count")
Mycobacterium_flag <- c(NCDC = "Not Countable Due to Contamination",
                        TNTC = "Too Numerous to Count",
                        `Not detected` = " absent/too few to count")

writeAttributes(microbial) # write data frame attributes to a csv in current dir to edit metadata
microbial_desc <- "E coli, Coliform, and Mycobacterium colony counts in canals and reservoirs, 2014-2016"

microbial_factors <- factorsToFrame(microbial)
  
microbial_DT <- createDTFF(dfname = microbial,
                           factors = microbial_factors,
                           description = microbial_desc)


# nutrients ----

nutrients <- data[['nutrients']] %>% 
  rename(Cluster = `cluster name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(date = format(as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>%
  mutate(total_nitrogen = as.numeric(`Tot N (mg/L (0<mdl))`)) %>% 
  mutate(total_dissolved_nitrogen = as.numeric(`Diss N (mg/L (0<mdl))`)) %>% 
  mutate(total_phosphorus = as.numeric(`Tot P (ug/L (0<mdl))`)) %>% 
  mutate(total_dissolved_phosphorus = as.numeric(`Diss P (ug/L (0<mdl))`)) %>% 
  mutate(nitrate_nitrogen = as.numeric(`NO3-N, mg/l`)) %>% 
  mutate(ammonium_nitrogen = as.numeric(`NH4-N, ppm`)) %>% 
  mutate(dissolved_organic_nitrogen = as.numeric(`DON, ppm`)) %>% 
  select(-ID, 
         -month, 
         -`Tot N (mg/L (0<mdl))`,
         -`Diss N (mg/L (0<mdl))`,
         -`Tot P (ug/L (0<mdl))`,
         -`Diss P (ug/L (0<mdl))`,
         -`NO3-N, mg/l`,
         -`NH4-N, ppm`,
         -`DON, ppm`) %>% 
  arrange(date, `site number`) %>% 
  filter(!is.na(date))
  
writeAttributes(nutrients) # write data frame attributes to a csv in current dir to edit metadata
nutrients_desc <- "concentration of algal by-products in canals and reservoirs, 1999-2016"

nutrients_factors <- factorsToFrame(nutrients)
  
nutrients_DT <- createDTFF(dfname = nutrients,
                           factors = nutrients_factors,
                           description = nutrients_desc)


# quarterlyLakeSampling ----

quarterlyLakeSampling <- data[['quarterlyLakeSampling']] %>% 
  rename(Cluster = `Cluster Name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(`Conductance(us/cm)` = as.numeric(`Conductance(us/cm)`)) %>% 
  mutate(date = format(as.POSIXct(Date, format = "%m/%d/%Y %H:%M:%S %p"), "%Y-%m-%d")) %>%
  select(-ID, -Date) %>% 
  arrange(date, `Site Name`) %>% 
  filter(!is.na(date))
  
writeAttributes(quarterlyLakeSampling) # write data frame attributes to a csv in current dir to edit metadata
quarterlyLakeSampling_desc <- "concentration of nutrients and algal by-products, and other water quality parameters measured in central-Arizona area lakes, 2012-2016"

quarterlyLakeSampling_factors <- factorsToFrame(quarterlyLakeSampling)
  
quarterlyLakeSampling_DT <- createDTFF(dfname = quarterlyLakeSampling,
                                       factors = quarterlyLakeSampling_factors,
                                       description = quarterlyLakeSampling_desc)


# quarterlyMetals ----

quarterlyMetals <- data[['quarterlyMetals']] %>% 
  rename(Cluster = `Cluster Name`) %>% 
  mutate(Cluster = tolower(Cluster)) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(`23Na` = as.numeric(`23Na`)) %>% 
  mutate(`24Mg` = as.numeric(`24Mg`)) %>% 
  mutate(`39K` = as.numeric(`39K`)) %>% 
  mutate(`44Ca` = as.numeric(`44Ca`)) %>% 
  mutate(date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  select(-ID, -Date, -Units) %>% 
  arrange(date, `Site Name`) %>% 
  filter(!is.na(date))
  
writeAttributes(quarterlyMetals) # write data frame attributes to a csv in current dir to edit metadata
quarterlyMetals_desc <- "concentration of dissolved metals measured in central-Arizona area lakes and canals, 2012-2015"

quarterlyMetals_factors <- factorsToFrame(quarterlyMetals)
  
quarterlyMetals_DT <- createDTFF(dfname = quarterlyMetals,
                                 factors = quarterlyMetals_factors,
                                 description = quarterlyMetals_desc)


# sucralose ----

# so little data here as to be pointless


# title and abstract ----
title <- 'Regional drinking water quality monitoring program: long-term monitoring of water quality in select canals, reservoirs, and treatment plants of the greater Phoenix metropolitan area drinking water system, ongoing since 1998'

# abstract from file or directly as text
abstract <- as(set_TextType("knb-lter-cap.617_abstract.md"), "abstract") 
# abstract <- 'abstract text'


# people ----

# creators
paulWesterhoff <- addCreator('p', 'westerhoff')
miltonSommerfeld <- addCreator('m', 'sommerfeld')
peterFox <- addCreator('p', 'fox')
morteza <- addCreator('m', 'abbaszadegan')
creators <- c(as(paulWesterhoff, 'creator'),
              as(miltonSommerfeld, 'creator'),
              as(peterFox, 'creator'),
              as(morteza, 'creator'))

# associated party
danChilders <- addAssocParty('d', 'childers', 'CAP LTER Principal Investigator')
associatedParty <- c(as(danChilders, 'associatedParty'))

# metadata provider
marisaMasles <- addMetadataProvider('m', 'masles')
metadataProvider <-c(as(marisaMasles, 'metadataProvider'))


# keywords ----

# CAP IRTs for reference: https://sustainability.asu.edu/caplter/research/
# be sure to include these as appropriate

keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("urban",
                     "dissolved organic carbon",
                     "chlorophyll",
                     "nutrients",
                     "water quality",
                     "total phosphorus",
                     "total nitrogen",
                     "total dissolved nitrogen")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("water and fluxes",
                     "disturbance patterns",
                     "movement of organic matter",
                     "movement of inorganic matter")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("total dissolved phosphorus",
                     "geosmin",
                     "canal",
                     "water treatment")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "arid land"))
    )

# methods and coverages ----
methods <- set_methods("knb-lter-cap.617_methods.md")

begindate <- "2005-11-05"
enddate <- "2016-12-05"
geographicDescription <- "canals, water treatment plants, and reservoirs in the greater phoenix metropolitan area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         geographicDescription = geographicDescription,
                         west = -111.1250, east = -111.1235,
                         north = +33.4900, south = +33.2917)


# construct the dataset ----

# address, publisher, contact, and rights come from a sourced file

# XML DISTRUBUTION
  xml_url <- new("online",
                 onlineDescription = "CAPLTER Metadata URL",
                 url = paste0("https://sustainability.asu.edu/caplter/data/data-catalog/view/", packageIdent, "/xml/"))
metadata_dist <- new("distribution",
                 online = xml_url)

# DATASET
dataset <- new("dataset",
               title = title,
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               associatedParty = associatedParty,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(algae_DT,
                             algalByproducts_DT,
                             arsenic_DT,
                             carbon_DT,
                             fieldMeasurements_DT,
                             microbial_DT,
                             nutrients_DT,
                             quarterlyLakeSampling_DT,
                             quarterlyMetals_DT,
                             sampling_locations_DT))


# ls(pattern= "_DT") # can help to pull out DTs

# construct the eml ----

# ACCESS
allow_cap <- new("allow",
                 principal = "uid=CAP,o=LTER,dc=ecoinformatics,dc=org",
                 permission = "all")
allow_public <- new("allow",
                    principal = "public",
                    permission = "read")
lter_access <- new("access",
                   authSystem = "knb",
                   order = "allowFirst",
                   scope = "document",
                   allow = c(allow_cap,
                             allow_public))

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

custom_units <- rbind(
  data.frame(id = "microsiemenPerCentimeter",
             unitType = "conductance",
             parentSI = "siemen",
             multiplierToSI = 0.000001,
             description = "electric conductance of lake water in the units of microsiemenPerCentimeter"),
  data.frame(id = "perCentimeter",
             unitType = "unknown",
             parentSI = "unknown",
             multiplierToSI = "unknown",
             description = "absorbance per centimeter of path length at 254 nm"),
  data.frame(id = "literPerMilligramMeter",
             unitType = "unknown",
             parentSI = "unknown",
             multiplierToSI = "unknown",
             description = "the UV absorbance at 254 nanometers measured in inverse meters divided by the DOC concentration measured in milligrams per liter"),
  data.frame(id = "nanogramsPerLiter",
             unitType = "amountOfSubstanceConcentration",
             parentSI = "kilogram/cubic meter",
             multiplierToSI = 0.000001,
             description = "nanograms of material per liter of solute"),
  data.frame(id = "coloniesPer100milliliter",
             unitType = "unknown",
             parentSI = "unknown",
             multiplierToSI = "unknown",
             description = "number of bacterial colonies formed per 100 millileters of water"))
unitList <- set_unitList(custom_units)

eml <- new("eml",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))

# write the xml to file ----
write_eml(eml, "knb-lter-cap.617.2.xml")
