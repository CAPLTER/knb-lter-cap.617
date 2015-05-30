# early stuff ----

dates <- function(df) {
  if ("Month" %in% names(df)) {df$Month <- as.POSIXct(df$Month, format="%Y/%m/%d")
  df$Month <- format(df$Month, format="%m")
  }
  if ("Date" %in% names(df)) {df$Date <- as.POSIXct(df$Date, format="%Y/%m/%d")
  }
  if ("Sample_Date" %in% names(df)) {df$Sample_Date <- as.POSIXct(df$Sample_Date, format="%Y/%m/%d")
  }
  return(df)
}


if ("Date" %in% names(df)) {df$Date <- as.POSIXct(df$Date, format="%Y/%m/%d")

names(data[['algae']])



for (i in 1:length(filesNames)) { filesNames[[i]] <- sub("\\.[[:alnum:]]+$", "", filesNames[[i]]) } # remove '.csv'

fileNames <- list.files(path=".", pattern="*.csv", full.names=F, recursive=FALSE)
rmxlsx <- function(file) { sub(".xlsx", "", file) }
alpha <- llply(fileNames, rmxlsx)

names(data)<-alpha


for (i in 1:length(data)) {if ("Month" %in% names(data[[i]])) data[[i]]$Month<- as.POSIXct(data[[i]]$Month, format="%Y/%M/%s")}
n


for (i in 1:length(data)) {if ("end" %in% names(data[[i]])) data[[i]]$end <- as.POSIXct(data[[i]]$end, format="%b %e, %Y %I:%M:%S %p")}
for (i in 1:length(data)) {if ("today" %in% names(data[[i]])) data[[i]]$today <- as.POSIXct(data[[i]]$today, format="%b %e, %Y")}
for (i in 1:length(data)) {if ("se_samp_date" %in% names(data[[i]])) data[[i]]$se_samp_date <- as.POSIXct(data[[i]]$se_samp_date, format="%b %e, %Y")}

month <- function(file) { as.POSIXct(file['[[', 'Month'], format="%Y/%M/%d") }
alpha <- llply(data, month)
llply(as.POSIXct(data, "[[", 'Month', format="%Y/%M/%s"))


month <- function(list, field) llply(list, '[[', field) { as.POSIXct(field, format="%Y/%M/%s") }

head(data[['algae.csv']]['Month'])
head(data[])


llply

as.POSIXct(llply(data, "[[", 'Month'), format="%Y/%M/%d")

%c%` <- function(x, n)sapply(x, `[[`, n)

month <- function(x) { x <- as.POSIXct(x, format="%Y/%m/%d")
                       return(x) }
llply(data, '[[', 'Month')

names(data)
data[['algae.csv']]$Month

head(data[['algae.csv']][['Month']])
as.POSIXct(llply(data, "[[", 'Month'), format="%Y/%m/%d")
head(llply(data, "[[", 'Month'))

my.list<-llply(.data=data,
               function(x) { x$Month <- as.POSIXct(x$Month, format="%Y/%m/%d")
                             return(x)
               })

head(data['algae.csv'], '[[', 'Month')

# did not work
for (i in 1:length(data)) { data[[i]]$Month <- as.POSIXct(data[[i]]$Month, format="%Y/%m/%d") }

# worked
for (i in 1:length(data)) {if ("Month" %in% names(data[[i]])) data[[i]]$Month<- as.POSIXct(data[[i]]$Month, format="%Y/%m/%d")}

as.POSIXct('1999/08/01', format="%Y/%m/%d")
jm$Month <- as.POSIXct(jm$Month, format="%Y/%m/%d")

lapply(data, FUN=head)


str(data[['algae.csv']][['Month']])
data <- data.bak

lapply(data, FUN=as.POSIXct(Month, format="%Y/%m/%d"))

month <- function(x) { x <- as.POSIXct(x, format="%Y/%m/%d") }
alpha <- lapply(data, '[[', 'Month', FUN =  month())
lapply(data, '[[', 'Month')

alpha <- as.POSIXct(lapply(data, '[[', 'Month'), format="%Y/%m/%d")

alpha <- lapply(.data=data, function(x) {  x$Month <- as.POSIXct(x$Month, format="%Y/%m/%d")
                                    return(x)
               })
lapply(data$Month, FUN = head)


alpha <- lapply(.data=(data, '[[', 'Month'), FUN = as.POSIXct(x, format="%Y/%m/%d")
                                           return(x)
})

dates <- function(df) {
  if ("Month" %in% names(df)) {df$Month <- as.POSIXct(df$Month, format="%Y/%m/%d")
  df$Month <- format(df$Month, format="%m")
  }
  if ("Date" %in% names(df)) {df$Date <- as.POSIXct(df$Date, format="%Y/%m/%d")
  }
  if ("Sample_Date" %in% names(df)) {df$Sample_Date <- as.POSIXct(df$Sample_Date, format="%Y/%m/%d")
  }
  return(df)
}

data <- lapply(data, dates)

dataframe$rundate <- format(dataframe$rundate, format="%m/%d/%Y %H:%M:%S")


# single dataframe ##########

fluor <- data.frame(data[['3D_Fluorescence']])

names(fluor) <- c(
  'ID',
  'Site_Number',
  'Month',
  'Exc_A',
  'Em_A',
  'Fl_A',
  'Exc_B',
  'Em_B',
  'Fl_B')

# column defs for 3D fluoresence (aka fluor in dataframe)
# with specific col assignment
col.defs <- c(
  'ID' = 'Record number',
  'Site_Number' = 'Site number or name',
  'Month' = 'Month sample was collected',
  'Exc_A' = 'Excitation A wavelength',
  'Em_A' = 'Emission A wavelength',
  'Fl_A' = 'Fluorescence A',
  'Exc_B' = 'Excitation B wavelength',
  'Em_B' = 'Emission B wavelength',
  'Fl_B' = 'Fluorescence B')

unit.defs <- c(
  'ID' = "number",
  'Site_Number' = "'a' designates the epilimnion",
  'Month' = 'nominalMonth',
  'Exc_A' = 'nanometer',
  'Em_A' = 'nanometer',
  'Fl_A' = 'dimensionless',
  'Exc_B' = 'nanometer',
  'Em_B' = 'nanometer',
  'Fl_B' = 'dimensionless')

unit <- eml_define_unit(id = "nominalMonth",
                        parentSI = "dimensionless",
                        unitType = "dimensionless",
                        multiplierToSI = "NA",
                        description = "Calculated value for use in comparison")

# this works but not sure about the multiplierToSI, set to NA (text, not object) as the package requires that input but it it not clear that will be valid

# note that you can create a list of multiple custom units and call them in eml_write !!!
# so, do we HAVE to put them in some type of list or container???

eml_write(fluor,
          col.defs = col.defs,
          unit.defs = unit.defs,
          custom_units = c(unit),
          file = "~/Desktop/fluor3.xml",
          contact = "Carl Boettiger <cboettig@ropensci.org>")

# multiple tables ----

[1] "algae"                   "Arsenic"                 "brushexp"                "canal_data"              "doc_month"
[6] "doc_quarter"             "field_measurements"      "Fluorescence"            "gpscoord"                "Intensive_Lake_Sampling"
[11] "IntensiveSampling"       "mib_and_geosmin"         "nutrients"               "quality"                 "Quarterly_Lake_Sampling"
[16] "Quarterly_Metals"        "Sample_Names"            "sample_summary"          "Sucralose"               "wtp_data"

Fluorescence <- data.frame(data[['Fluorescence']])
algae <- data.frame(data[['algae']])
doc_month <- data.frame(data[['doc_month']])

# create two data tables
Fluorescence.DT <- eml_dataTable(data.frame(data[['Fluorescence']]),
                                 col.defs = col.defs.Fluorescence,
                                 unit.defs = unit.defs.Fluorescence,
                                 description = "metadata documentation for Fluorescence",
                                 filename = 'Flourescence.csv')

algae.DT <- eml_dataTable(algae,
                          col.defs = col.defs.algae,
                          unit.defs = unit.defs.algae,
                          description = "metadata documentation for algae",
                          filename = 'plain text filename for algae')

doc_month.DT <- eml_dataTable(doc_month,
                          col.defs = col.defs.doc_month,
                          unit.defs = unit.defs.doc_month,
                          description = "metadata documentation for doc_month",
                          filename = 'plain text filename for doc_month')

# create a data set of the two tables
testSet <- new("dataset",
               title = "tryingMultTables",
               dataTable = c(Fluorescence.DT,
                             algae.DT))

# create a new eml object then generate xml from it
testEML <- new("eml",
               dataset = testSet)
eml_write(testEML, file = '~/Desktop/testEML.xml')

# this one did not work, generated a file but just header info
# testEML_2 <- eml(dataset = testSet,
#                  title = "titleFromEng")
# eml_write(testEML_2, file = "~/Desktop/testEML_2.xml")

# generate dataset ----

# AWESOME !!!
# extracting from other xml
davies <- eml_read('../Davies_616')
scope <- davies@scope
system <- davies@system
contact <- davies@dataset@contact
creator <- c(paulwesterhoff, miltsommmerfeld)
keywordSet <- davies@dataset@keywordSet
pubDate <- '2015-06-01'
publisher <- davies@dataset@publisher
rights <- davies@dataset@intellectualRights
title <- 'Reducing Taste and Odor and Other Algae-Related Problems for Surface Water Supplies in Arid Environments'
metadataProvider <- davies@dataset@metadataProvider
additionalInfo <- davies@dataset@dataTable@additionalInfo@section

paulwesterhoff <- person(given = 'Paul', family = 'Westerhoff', email = 'p.westerhoff@asu.edu')
paul <- as.person('Paul Westerhoff <p.westerhoff@asu.edu>')
miltsommerfeld <- person(given = 'Milton', family = 'Sommerfeld', email = 'Milton.Sommerfeld@asu.edu')
milton <- as.person('Milton Sommerfeld <Milton.Sommerfeld@asu.edu>')
marisamasles <- person(given = 'Marisa', family = 'Masles', email = 'Marisa.Masles@asu.edu')
marisa <- as.person('Marisa Masles <Marisa.Masles@asu.edu>')
metadataProvider <- c(as('marisa', 'metadataProvider'))
metadataProvider <- c(marisamasles)

creator <- c(as('Paul Westerhoff', 'creator'),
             as('Milton Sommerfeld', 'creator'))

ssebe <- new('address',
             deliveryPoint = 'School of Sustainable Engineering and the Built Environment',
             city = 'Tempe',
             administrativeArea = 'AZ',
             postalCode = '85287',
             country = 'USA')

methods <- new('methods', methodStep = c(new('methodStep', description = 'Perform field measurements for dissolved oxygen, pH, temperature, conductance, and Secchi disk.  Collect algae for chlorophyll measurement, identification and enumeration.  Collect and analyze water samples for total phosphorus, total nitrogen, DOC, SUVA, THM, MIB and Geosmin, and metals measurements.')))

abstract <- 'Develop a comprehensive management strategy to reduce algae-related water quality problems for drinking water supplies in arid environments.'

coverage <- eml_coverage(dates = c('1999-08-17', '2015-06-01'),
                         geographic_description = 'Canals, Water Treatment Plants, and Reservoirs in Phoenix and surrounding areas',
                         NSEWbox = c(34.4900, 33.2917, -111.1235, -112.1250))


# build order
- dataset
- creator
- metadataProvider
- contact
- publisher
- title
- pubDate
- keywords
- abstract
- intellectualRights
- methods
- coverage
- dataTable
- physical
- attributeList
- additionalMetadata

# working eml write but without custom_units integration ----
delta <- new('dataset',
               scope = scope, # not sure the order of this one
               system = system, # not sure the order of this one
               creator = creator,
               metadataProvider = metadataProvider,
               contact = contact,
               publisher = publisher,
               title = title,
               pubDate = pubDate,
               keywordSet = keywordSet,
               abstract = abstract,
               intellectualRights = rights,
               methods = methods,
               coverage = coverage,
               dataTable = c(algae.DT,
                             Arsenic.DT,
                             brushexp.DT,
                             canal_data.DT,
                             doc_month.DT,
                             doc_quarter.DT,
                             field_measurements.DT,
                             Fluorescence.DT,
                             gpscoord.DT,
                             Intensive_Lake_Sampling.DT,
                             IntensiveSampling.DT,
                             mib_and_geosmin.DT,
                             nutrients.DT,
                             quality.DT,
                             Quarterly_Lake_Sampling.DT,
                             Quarterly_Metals.DT,
                             Sample_Names.DT))

frank <- new('eml',
            dataset = delta)

eml_write(frank,
          file = "./outFile.xml")

# did not work at all (well, produced xml file with header and additional metadata and that was all)
testhelper <- eml(dataset = c(algae.DT,
                              Arsenic.DT,
                              brushexp.DT,
                              canal_data.DT,
                              doc_month.DT,
                              doc_quarter.DT,
                              field_measurements.DT,
                              Fluorescence.DT,
                              gpscoord.DT,
                              Intensive_Lake_Sampling.DT,
                              IntensiveSampling.DT,
                              mib_and_geosmin.DT,
                              nutrients.DT,
                              quality.DT,
                              Quarterly_Lake_Sampling.DT,
                              Quarterly_Metals.DT,
                              Sample_Names.DT),
                  scope = scope, # not sure the order of this one
                  system = system, # not sure the order of this one
                  creator = creator,
                  metadataProvider = metadataProvider,
                  contact = contact,
                  publisher = publisher,
                  title = title,
                  pubDate = pubDate,
                  keywordSet = keywordSet,
                  abstract = abstract,
                  intellectualRights = rights,
                  methods = methods,
                  coverage = coverage,
                  custom_units = c(microgramPerCentimeterCubed,
                                   SUVA_254nm,
                                   microsiemensPerCentimeter,
                                   nominalMonth))

eml_write(testhelper, file = "./indiaB.xml")

# worked beautifully one time, now only the produces an xml with the stmml
hotel <- new('dataset',
               dataTable = c(algae.DT,
                             Arsenic.DT,
                             brushexp.DT,
                             canal_data.DT,
                             doc_month.DT,
                             doc_quarter.DT,
                             field_measurements.DT,
                             Fluorescence.DT,
                             gpscoord.DT,
                             Intensive_Lake_Sampling.DT,
                             IntensiveSampling.DT,
                             mib_and_geosmin.DT,
                             nutrients.DT,
                             quality.DT,
                             Quarterly_Lake_Sampling.DT,
                             Quarterly_Metals.DT,
                             Sample_Names.DT))

oscar <- eml(dataset = hotel,
             scope = scope, # not sure the order of this one
             system = system, # not sure the order of this one
             creator = creator,
             metadataProvider = metadataProvider,
             contact = contact,
             publisher = publisher,
             title = title,
             pubDate = pubDate,
             keywordSet = keywordSet,
             abstract = abstract,
             intellectualRights = rights,
             methods = methods,
             coverage = coverage,
             custom_units = c(microgramPerCentimeterCubed,
                              SUVA_254nm,
                              microsiemensPerCentimeter,
                              nominalMonth))

eml_write(oscar, file = "./oscar.xml")

# separate eml write just for the custom units, really hope it does not come to this
onlycustom <- eml(custom_units = c(microgramPerCentimeterCubed,
                                SUVA_254nm,
                                microsiemensPerCentimeter,
                                nominalMonth))

# actually do not even need the eml construciton, this can be done directly in eml_write
eml_write(onlycustom,
          custom_units = c(microgramPerCentimeterCubed,
                           SUVA_254nm,
                           microsiemensPerCentimeter,
                           nominalMonth),
          file = "~/Desktop/onlycustomA.xml")

# check out list2env() for converting list dfs to dfs

Fluorescence.DT <- eml_dataTable(data.frame(data[['Fluorescence']]),
                                 col.defs = col.defs.Fluorescence,
                                 unit.defs = unit.defs.Fluorescence,
                                 description = "metadata documentation for Fluorescence",
                                 additionalInfo = additionalInfo,
                                 filename = ref_id)

eml_write(custom_units = c(microgramPerCentimeterCubed,
                           SUVA_254nm,
                           microsiemensPerCentimeter,
                           nominalMonth),
          file = "./units.xml")