---
title: "taste_odor_processing"
author: "SRE"
date: "04/10/2015"
output: html_document
---


library("devtools")
library("EML")
library("rapport")

# file processing ----

# identify directory with files (not full.names=T)
files <- list.files(path=".", pattern="*.txt", full.names=T, recursive=FALSE)

# function to bring files into R
BatchLoad <- function(file) {data <- read.csv(file, header=T, stringsAsFactors=F) }

# import files from target directory
data <- lapply(files, BatchLoad)

# get filenames of list items from directory (note full.names=F); change to lower case on import
fileNames <- list.files(path=".", pattern="*.txt", full.names=F, recursive=FALSE)

# modify and assign filenames
# rmspac <- function(file) { gsub(" ", "_", file) } # function to change spaces to underscores
# fileNames <- lapply(fileNames, rmspac) # apply function rmspac to all filenames
rmtxt <- function(file) { sub(".txt", "", file) } # function to remove txt file extension
fileNames <- lapply(fileNames, rmtxt) # apply function rmtxt to all filenames
fileNames <- lapply(fileNames, tolower)
fileNames <- lapply(fileNames, tocamel)
names(data) <- fileNames # assign file names to list of datasets

# convert empty values to NAs ----
# method one
data <- lapply(data, function(x) {
  # do the replacing
  x[x=='']<-NA
  return(x)
})

# omit empty columns from quarterlyMetals
data[['quarterlyMetals']] <- data[['quarterlyMetals']][,c(1:34)]

# NAME fields according to metadata ----
# this step is necessary here owing to the non-standardized naming conventions
# in the future, we would give instruction to the investigator for proper naming

# algae
names(data[['algae']]) <- c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'ClusterName',
  'SiteAcronym',
  'Date',
  'Month',
  'SampleType',
  'Conductance',
  'ChlA',
  'Phaeophytin',
  'PhaeophytinChlA',
  'Chlorophyta',
  'Cyanophyta',
  'Bacillariophyta',
  'Total',
  'AlgaeComments')

# arsenic
names(data[['arsenic']])<-c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'Cluster',
  'SiteAcronym',
  'Date',
  'Arsenic',
  'Perchlorate')

# docMonth
names(data[['docMonth']])<-c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'ClusterName',
  'SiteAcronym',
  'Date',
  'Month',
  'SampleType',
  'TOC',
  'DOC',
  'UVA',
  'SUVA',
  'PeakInt',
  'PeakIntWL',
  'Intat450',
  'Intat460',
  'Intat500',
  'Fl',
  'docm_comments')

# fieldMeasurements
names(data[['fieldMeasurements']])<-c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'ClusterName',
  'SiteAcronym',
  'Date',
  'Month',
  'Depth',
  'Temperature',
  'DO',
  'pH',
  'SecchiDisk',
  'SamplesCollected',
  'SampleType',
  'Comments')

# gpscoord
names(data[['gpscoord']])<-c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'ClusterName',
  'SiteAcronym',
  'Date',
  'Longitude',
  'Latitude',
  'Altitude')

# mibAndGeosmin
names(data[['mibAndGeosmin']])<-c(
  'ID',
  'Site_Number',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Month',
  'Sample_Type',
  'MIB',
  'Geosmin',
  'Beta_Cyclocitral',
  'IPMP_Recovery',
  'MIB_Phoenix',
  'Geosmin_Phoenix',
  'Comments')

# nutrients
names(data[['nutrients']])<-c(
  'ID',
  'Site_Number',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Month',
  'Sample_Type',
  'Tot_N',
  'Diss_N',
  'Tot_P',
  'Diss_P',
  'NO3_N',
  'NH4_N',
  'DON',
  'Nutrient_comments')

# quarterlyLakeSampling
names(data[['quarterlyLakeSampling']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'MIB',
  'Geosmin',
  'Cyclocitral',
  'TP',
  'DOC',
  'UVA',
  'SUVA',
  'Conductance',
  'TDN')

# quarterlyMetals
names(data[['quarterlyMetals']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Sample_Date',
  'Units',
  'Li7',
  'Be9',
  'Na23',
  'Mg24',
  'Al27',
  'K39',
  'Ca44',
  'V51',
  'Cr52',
  'Mn55',
  'Fe56',
  'Co59',
  'Ni60',
  'Cu65',
  'Zn66',
  'As75',
  'Se82',
  'Sr88',
  'Mo95',
  'Ag107',
  'Cd111',
  'In115',
  'Sb121',
  'Ba138',
  'Hg202',
  'Pb207',
  #'Th232',
  'U238')

# sampleNames
names(data[['sampleNames']])<-c(
  'Site_ID',
  'Site_Number',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym')

# sucralose
names(data[['sucralose']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Sucralose')

# microbial
names(data[['microbial']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'E_coli',
  'Coliform',
  'Mycobacterium',
  'Comments')

# format dates, time, and ID (row count, basically) ----

dates <- function(df) {
  if ("Month" %in% names(df)) {df$Month <- as.POSIXct(df$Month, format="%m/%d/%Y %H:%M:%S")
                               df$Month <- as.numeric(format(df$Month, format="%m"))
                               # added as numeric here to change it from text to int; as text it will be treated as nominal
  }
  if ("Time" %in% names(df)) {df$Time <- as.POSIXct(df$Time, format="%Y/%m/%d %H:%M:%S")
  df$Time <- format(df$Time, format="%H:%M")
  }
  if ("Date" %in% names(df)) {df$Date <- as.POSIXct(df$Date, format="%m/%d/%Y %H:%M:%S")
  }
  if ("Sample_Date" %in% names(df)) {df$Sample_Date <- as.POSIXct(df$Sample_Date, format="%m/%d/%Y") # name and this if tailored specifically for quarterly metals
  }
  if ("ID" %in% names(df)) {df$ID <- as.character(df$ID) # these are arbitrary numerical values that need to be converted to nominal
  }
  return(df)
}

data <- lapply(data, dates)

# convert lone numeric -> nominal column not addressed by "ID" in the dates function
data[["sampleNames"]]$Site_Number <- as.character(data[["sampleNames"]]$Site_Number) # meaningless numeric -> nominal

# provide col.defs (field descriptions) and unit.defs according to metadata ----

# algae
col.defs.algae <- c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name; ‘A’ designates epilimnion, ‘B’ hypolimnion',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'SampleType' = 'Type of water sample collected',
  'Conductance' = 'Conductivity of water sample',
  'ChlA' = 'Total Chlorophyll a measurement',
  'Phaeophytin' = 'Phaeophytin measurement',
  'PhaeophytinChlA' = 'Chlorophyll a measurement corrected for Phaeophytin',
  'Chlorophyta' = 'Chlorophyta count',
  'Cyanophyta' = 'Cyanophyta count',
  'Bacillariophyta' = 'Bacillariophyta count',
  'Total' = 'Total of Chlorophyta, Cyanophyta, and Bacillariophyta measurements',
  'AlgaeComments' = 'Sampling comments about canal, lake or water treatment plant')

unit.defs.algae <- list(
  'ID' = 'Record number',
  'SiteNumber' = '‘A’ designates epilimnion, ‘B’ hypolimnion',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'either Verde, Salt, CAP, SRP, Tempe',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Month' = 'nominalMonth',
  'SampleType' = '“field blank”,” lab blank”, or “duplicate”; “grab” if not noted',
  'Conductance' = 'microsiemensPerCentimeter',
  'ChlA' = 'microgramsPerLiter',
  'Phaeophytin' = 'microgramsPerLiter',
  'PhaeophytinChlA' = 'microgramsPerLiter',
  'Chlorophyta' = 'numberPerMilliliter',
  'Cyanophyta' = 'numberPerMilliliter',
  'Bacillariophyta' = 'numberPerMilliliter',
  'Total' = 'numberPerMilliliter',
  'AlgaeComments' = 'e.g. “musty odor”, “turbid”, “algal growth”, “dry canal”, etc.')

# arsenic
col.defs.arsenic <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'Cluster' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Arsenic' = 'Arsenic measurement; measurement by graphite furnace',
  'Perchlorate' = 'Perchlorate measurement; measurement by ion chromatography')

unit.defs.arsenic <-c(
  'ID' = 'Record number',
  'SiteNumber' = '‘A’ designates epilimnion, ‘B’ hypolimnion',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'Cluster' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Arsenic' = 'microgramsPerLiter',
  'Perchlorate' = 'microgramsPerLiter')

# docMonth
col.defs.docMonth <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'SampleType' = 'Type of water sample collected',
  'TOC' = 'Total organic carbon measurement',
  'DOC' = 'Dissolved organic carbon measurement',
  'UVA' = 'UV absorbance measurement; measured at 254 nanometers',
  'SUVA' = 'SUVA calculation',
  'PeakInt' = 'Intensity of peak',
  'PeakIntWL' = 'Wavelength at the highest peak intensity',
  'Intat450' = 'Intensity at 450 nanometers',
  'Intat460' = 'Intensity at 460 nanometers',
  'Intat500' = 'Intensity at 500 nanometers',
  'Fl' = 'Fluorescence ratio of 450/500',
  'docm_comments' = 'No metadata provided')

unit.defs.docMonth <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Month' = 'nominalMonth',
  'SampleType' = 'either “field blank”, “lab blank”, or “duplicate”; “grab” if not noted',
  'TOC' = 'milligramsPerLiter',
  'DOC' = 'milligramsPerLiter',
  'UVA' = 'dimensionless',
  'SUVA' = 'SUVA_254nm',
  'PeakInt' = 'dimensionless',
  'PeakIntWL' = 'nanometer',
  'Intat450' = 'dimensionless',
  'Intat460' = 'dimensionless',
  'Intat500' = 'dimensionless',
  'Fl' = 'dimensionless',
  'docm_comments' = 'METADATA_NOT_PROVIDED')

# fieldMeasurements
col.defs.fieldMeasurements <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'Depth' = 'Water depth where measurement was taken; “0” designates just below surface',
  'Temperature' = 'Water temperature',
  'DO' = 'Dissolved oxygen measurement',
  'pH' = 'pH measurement',
  'SecchiDisk' = 'Water transparency measurement; Secchi disk lowered from side of boat',
  'SamplesCollected' = 'Individual bottles collected for various analyses',
  'SampleType' = 'Type of water sample collected',
  'Comments' = 'Sampling comments about canal, lake or water treatment plant')

unit.defs.fieldMeasurements <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Month' = 'nominalMonth',
  'Depth' = 'meter',
  'Temperature' = 'celsius',
  'DO' = 'milligramsPerLiter',
  'pH' = 'dimensionless',
  'SecchiDisk' = 'meter',
  'SamplesCollected' = 'bottles for:  “NUT(nutrients)”, “DOCM”, “MIB/GEO”, “PHYT(phytoplankton)”, or “PERI(Periphyton)”',
  'SampleType' = 'either “field blank”, “lab blank”, or “duplicate”; "grab" if not noted',
  'Comments' = 'e.g. “musty odor”, “turbid”, “algal growth”, “dry canal”, etc.')

# gpscoord
col.defs.gpscoord <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Longitude' = 'Longitude coordinates',
  'Latitude' = 'Latitude coordinates',
  'Altitude' = 'Elevation above sea level')

unit.defs.gpscoord <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Longitude' = 'W deg min.sec',
  'Latitude' = 'N deg min.sec',
  'Altitude' = 'foot')

# mibAndGeosmin
col.defs.mibAndGeosmin <-c(
  'ID' = 'Record number',
  'Site_Number' = 'Site number or name',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'Sample_Type' = 'Type of water sample collected',
  'MIB' = '2-methyisoborneol measurement',
  'Geosmin' = 'Geosmin measurement',
  'Beta_Cyclocitral' = 'Beta-Cyclocitral measurement',
  'IPMP_Recovery' = 'Recovery of 2-isopropyl-3-methoxy pyrazine; IPMP used as an internal standard',
  'MIB_Phoenix' = 'MIB comparison with Phoenix (?)',
  'Geosmin_Phoenix' = 'Geosmin comparison with Phoenix (?)',
  'Comments' = 'Sampling comments about canal, lake or water treatment plant')

unit.defs.mibAndGeosmin <-c(
  'ID' = 'Record number',
  'Site_Number' = 'METADATA_NOT_PROVIDED',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Month' = 'nominalMonth',
  'Sample_Type' = 'either “field blank”, “lab blank”, or “duplicate”; "grab" if not noted',
  'MIB' = 'milligramsPerCubicMeter',
  'Geosmin' = 'milligramsPerCubicMeter',
  'Beta_Cyclocitral' = 'milligramsPerCubicMeter',
  'IPMP_Recovery' = 'dimensionless',
  'MIB_Phoenix' = 'milligramsPerCubicMeter',
  'Geosmin_Phoenix' = 'milligramsPerCubicMeter',
  'Comments' = 'e.g. “musty odor”, “turbid”, “algal growth”, “dry canal”, etc.')

# nutrients
col.defs.nutrients <-c(
  'ID' = 'Record number',
  'Site_Number' = 'Site number or name',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'Sample_Type' = 'Type of water sample collected',
  'Tot_N' = 'Total nitrogen measurement; if “0” or negative value, then below MDL;if no value entered, then not measured',
  'Diss_N' = 'Dissolved nitrogen measurement; if “0” or negative value, then below MDL; if no value entered, then not measured',
  'Tot_P' = 'Total phosphorus measurement; if “0” or negative value, then below MDL; if no value entered, then not measured',
  'Diss_P' = 'Dissolved phosphorus measurement; if “0” or negative value, then below MDL; if no value entered, then not measured',
  'NO3_N' = 'Nitrogen as nitrate measurement',
  'NH4_N' = 'Nitrogen as ammonia measurement',
  'DON' = 'Dissolved organic nitrogen measurement',
  'Nutrient_comments' = 'Sampling comments about canal, lake or water treatment plant')

unit.defs.nutrients <-c(
  'ID' = 'Record number',
  'Site_Number' = 'METADATA_NOT_PROVIDED',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Month' = 'nominalMonth',
  'Sample_Type' = 'either “field blank”, “lab blank”, or “duplicate”; "grab" if not noted',
  'Tot_N' = 'milligramsPerLiter',
  'Diss_N' = 'microgramsPerLiter',
  'Tot_P' = 'microgramsPerLiter',
  'Diss_P' = 'microgramsPerLiter',
  'NO3_N' = 'milligramsPerLiter',
  'NH4_N' = 'milligramsPerLiter',
  'DON' = 'milligramsPerLiter',
  'Nutrient_comments' = 'e.g. “musty odor”, “turbid”, “algal growth”, “dry canal”, etc.')

# quarterlyLakeSampling
col.defs.quarterlyLakeSampling <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'MIB' = '2-methyisoborneol measurement',
  'Geosmin' = 'Geosmin measurement',
  'Cyclocitral' = 'beta-Cyclocitral measurement',
  'Conductance' = 'Conductivity of sample',
  'TP' = 'Total phosphorus measurement',
  'TDN' = 'Total dissolved nitrogen measurement',
  'DOC' = 'Dissolved organic carbon measurement',
  'UVA' = 'UV absorbance measurement',
  'SUVA' = 'SUVA calculation')

unit.defs.quarterlyLakeSampling <-c(
  'ID' = 'Record number',
  'Site_Name' = '“A” denotes epilimnion, “B” denotes hypolimnion',
  'Site_Location' = '“upper” denotes upstream lake location, “lower” denotes downstream',
  'Cluster_Name' = '“Salt River”',
  'Site_Acronym' = '“epi” = epilimnion, “hypo” = hypolimnion',
  'Date' = c(format = 'YYYY-MM-DD'),
  'MIB' = 'milligramsPerCubicMeter',
  'Geosmin' = 'milligramsPerCubicMeter',
  'Cyclocitral' = 'milligramsPerCubicMeter',
  'Conductance' = 'microsiemensPerCentimeter',
  'TP' = 'microgramsPerLiter',
  'TDN' = 'milligramsPerLiter',
  'DOC' = 'milligramsPerLiter',
  'UVA' = 'dimensionless',
  'SUVA' = 'SUVA_254nm')

# quarterlyMetals
col.defs.quarterlyMetals <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Units' = 'Measurement units',
  'Li7' = 'Lithium measurement by ICP-MS',
  'Be9' = 'Beryllium measurement by ICP-MS',
  'Na23' = 'Sodium measurement by ICP-MS',
  'Mg24' = 'Magnesium measurement by ICP-MS',
  'Al27' = 'Aluminum measurement by ICP-MS',
  'K39' = 'Potassium measurement by ICP-MS',
  'Ca44' = 'Calcium measurement by ICP-MS',
  'V51' = 'Vanadium measurement by ICP-MS',
  'Cr52' = 'Chromium measurement by ICP-MS',
  'Mn55' = 'Manganese measurement by ICP-MS',
  'Fe56' = 'Iron measurement by ICP-MS',
  'Co59' = 'Cobalt measurement by ICP-MS',
  'Ni60' = 'Nickle measurement by ICP-MS',
  'Cu65' = 'Copper measurement by ICP-MS',
  'Zn66' = 'Zinc measurement by ICP-MS',
  'As75' = 'Arsenic measurement by ICP-MS',
  'Se82' = 'Selenium measurement by ICP-MS',
  'Sr88' = 'Strontium measurement by ICP-MS',
  'Mo95' = 'Molybdenum measurement by ICP-MS',
  'Ag107' = 'Silver measurement by ICP-MS',
  'Cd111' = 'Cadmium measurement by ICP-MS',
  'In115' = 'Indium measurement by ICP-MS',
  'Sb121' = 'Antimony measurement by ICP-MS',
  'Ba138' = 'Barium measurement by ICP-MS',
  'Hg202' = 'Mercury measurement by ICP-MS',
  'Pb207' = 'Lead measurement by ICP-MS',
  #'Th232' = 'Thorium measurement by ICP-MS',
  'U238' = 'Uranium measurement by ICP-MS')

unit.defs.quarterlyMetals <-c(
  'ID' = 'Record number',
  'Site_Name' = '“A” denotes epilimnion, “B” denotes hypolimnion',
  'Site_Location' = '“upper” denotes upstream lake location, “lower” denotes downstream',
  'Cluster_Name' = 'either “Salt River”,” Cap”, or “Verde”',
  'Site_Acronym' = '“epi” = epilimnion, “hypo” = hypolimnion',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Units' = 'micrograms per liter',
  'Li7' = 'microgramsPerLiter',
  'Be9' = 'microgramsPerLiter',
  'Na23' = 'microgramsPerLiter',
  'Mg24' = 'microgramsPerLiter',
  'Al27' = 'microgramsPerLiter',
  'K39' = 'microgramsPerLiter',
  'Ca44' = 'microgramsPerLiter',
  'V51' = 'microgramsPerLiter',
  'Cr52' = 'microgramsPerLiter',
  'Mn55' = 'microgramsPerLiter',
  'Fe56' = 'microgramsPerLiter',
  'Co59' = 'microgramsPerLiter',
  'Ni60' = 'microgramsPerLiter',
  'Cu65' = 'microgramsPerLiter',
  'Zn66' = 'microgramsPerLiter',
  'As75' = 'microgramsPerLiter',
  'Se82' = 'microgramsPerLiter',
  'Sr88' = 'microgramsPerLiter',
  'Mo95' = 'microgramsPerLiter',
  'Ag107' = 'microgramsPerLiter',
  'Cd111' = 'microgramsPerLiter',
  'In115' = 'microgramsPerLiter',
  'Sb121' = 'microgramsPerLiter',
  'Ba138' = 'microgramsPerLiter',
  'Hg202' = 'microgramsPerLiter',
  'Pb207' = 'microgramsPerLiter',
  #'Th232' = 'microgramsPerLiter',
  'U238' = 'microgramsPerLiter')

# sampleNames
col.defs.sampleNames <-c(
  'Site_ID' = 'Record number',
  'Site_Number' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location')

unit.defs.sampleNames <-c(
  'Site_ID' = '“A” denotes epilimnion, “B” denotes hypolimnion',
  'Site_Number' = 'number',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = '“epi”= epilimnion, “hypo”= hypolimnion,  “dup”= duplicate, “x con”= cross connect')

# sucralose
col.defs.sucralose <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Sucralose' = 'Sucralose measurement by LC-MS; Samples concentrated first by SPE')

unit.defs.sucralose <-c(
  'ID' = 'number',
  'Site_Name' = '“A” denotes epilimnion',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = '“epi” = epilimnion',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Sucralose' = 'microgramsPerLiter')

# microbial
col.defs.microbial <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'E_coli' = 'Number of colonies per 100mL of sample',
  'Coliform' = 'Number of colonies per 100mL of sample',
  'Mycobacterium' = 'Number of colonies per 100mL of sample',
  'Comments' = 'comments regarding microbial sample')

unit.defs.microbial <-c(
  'ID' = 'number',
  'Site_Name' = 'METADATA_NOT_PROVIDED',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”, “CAP”, “SRP", or “Tempe”',
  'Site_Acronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'E_coli' = 'coloniesPer100milliliter',
  'Coliform' = 'coloniesPer100milliliter',
  'Mycobacterium' = 'coloniesPer100milliliter',
  'Comments' = '“TNTC” = Too Numerous To Count; “NCDC” = Not Countable Due to Contamination')

# custom units ----
# note that you can create a list of multiple custom units and call them in eml_write
# able to generate these in an eml doc as additional metadata, but they are not being called by eml_dataTable
# that is a big problem as all custom unit field are going to bring up the dialog boxes every single time
# however anything can be entered for those values a these values overwrite the stmml in additional metadata

nominalMonth <- eml_define_unit(id = "nominalMonth",
                                parentSI = "unknown month",
                                unitType = "date",
                                multiplierToSI = "1", # not sure about this
                                description = "month in which sample was collected")

microsiemensPerCentimeter <- eml_define_unit(id = "microsiemensPerCentimeter",
                                             parentSI = "siemen",
                                             unitType = "conductance",
                                             multiplierToSI = "0.0001",
                                             description = "microsiemensPerCentimeter")

SUVA_254nm <- eml_define_unit(id = "SUVA_254nm",
                              parentSI = "unknown (SUVA[254nm] literPerMilligramPerMeter)",
                              unitType = "specific ultra violet  absorbance",
                              multiplierToSI = "1",
                              description = "Specific Ultra Violet Absorbance (liter per milligram per meter)")

microgramPerCentimeterCubed <- eml_define_unit(id = "microgramPerCentimeterCubed",
                                               parentSI = "kilogramsPerCubicMeter",
                                               unitType = "massDensity",
                                               multiplierToSI = "1000000",
                                               description = "micrograms per cubic centimeter")

coloniesPer100milliliter <- eml_define_unit(id = "coloniesPer100milliliter ",
                                               parentSI = "unknown",
                                               unitType = "concentration",
                                               multiplierToSI = "1",
                                               description = "Number of colonies per 100mL of sample")

# generate datatables ----
# dataTable slotnames
# slotNames(new('dataTable'))
[1] "entityName"          "entityDescription"   "alternateIdentifier" "physical"            "coverage"            "methods"
[7] "additionalInfo"      "attributeList"       "caseSensitive"       "numberOfRecords"     "id"                  "system"
[13] "scope"

# do this set one-by-one owing to custom units
algae.DT <- eml_dataTable(data.frame(data[['algae']]),
                          col.defs = col.defs.algae,
                          unit.defs = unit.defs.algae,
                          description = "metadata documentation for algae",
                          filename = 'algae.csv')

docMonth.DT <- eml_dataTable(data.frame(data[['docMonth']]),
                              col.defs = col.defs.docMonth,
                              unit.defs = unit.defs.docMonth,
                              description = "metadata documentation for docMonth",
                              filename = 'docMonth.csv')

quarterlyLakeSampling.DT <- eml_dataTable(data.frame(data[['quarterlyLakeSampling']]),
                                            col.defs = col.defs.quarterlyLakeSampling,
                                            unit.defs = unit.defs.quarterlyLakeSampling,
                                            description = "metadata documentation for quarterlyLakeSampling",
                                            filename = 'quarterlyLakeSampling.csv')

microbial.DT <- eml_dataTable(data.frame(data[['microbial']]),
                                            col.defs = col.defs.microbial,
                                            unit.defs = unit.defs.microbial,
                                            description = "metadata documentation for microbial",
                                            filename = 'microbial.csv')

nutrients.DT <- eml_dataTable(data.frame(data[['nutrients']]),
                              col.defs = col.defs.nutrients,
                              unit.defs = unit.defs.nutrients,
                              description = "metadata documentation for nutrients",
                              filename = 'nutrients.csv')

# okay to highlight all and run
arsenic.DT <- eml_dataTable(data.frame(data[['arsenic']]),
                            col.defs = col.defs.arsenic,
                            unit.defs = unit.defs.arsenic,
                            description = "metadata documentation for arsenic",
                            filename = 'arsenic.csv')

fieldMeasurements.DT <- eml_dataTable(data.frame(data[['fieldMeasurements']]),
                                       col.defs = col.defs.fieldMeasurements,
                                       unit.defs = unit.defs.fieldMeasurements,
                                       description = "metadata documentation for fieldMeasurements",
                                       filename = 'fieldMeasurements.csv')

gpscoord.DT <- eml_dataTable(data.frame(data[['gpscoord']]),
                             col.defs = col.defs.gpscoord,
                             unit.defs = unit.defs.gpscoord,
                             description = "metadata documentation for gpscoord",
                             filename = 'gpscoord.csv')

mibAndGeosmin.DT <- eml_dataTable(data.frame(data[['mibAndGeosmin']]),
                                    col.defs = col.defs.mibAndGeosmin,
                                    unit.defs = unit.defs.mibAndGeosmin,
                                    description = "metadata documentation for mibAndGeosmin",
                                    filename = 'mibAndGeosmin.csv')

quarterlyMetals.DT <- eml_dataTable(data.frame(data[['quarterlyMetals']]),
                                     col.defs = col.defs.quarterlyMetals,
                                     unit.defs = unit.defs.quarterlyMetals,
                                     description = "metadata documentation for quarterlyMetals",
                                     filename = 'quarterlyMetals.csv')

sampleNames.DT <- eml_dataTable(data.frame(data[['sampleNames']]),
                                 col.defs = col.defs.sampleNames,
                                 unit.defs = unit.defs.sampleNames,
                                 description = "metadata documentation for sampleNames",
                                 filename = 'sampleNames.csv')

sucralose.DT <- eml_dataTable(data.frame(data[['sucralose']]),
                              col.defs = col.defs.sucralose,
                              unit.defs = unit.defs.sucralose,
                              description = "metadata documentation for sucralose",
                              filename = 'sucralose.csv')

# generate dataset ----

# dataset slotNames
# slotNames(new('dataset'))
[1] "alternateIdentifier" "shortName"           "title"               "creator"             "metadataProvider"    "associatedParty"
[7] "pubDate"             "language"            "series"              "abstract"            "keywordSet"          "additionalInfo"
[13] "intellectualRights"  "distribution"        "coverage"            "purpose"             "contact"             "publisher"
[19] "methods"             "dataTable"           "otherEntity"         "id"                  "system"              "scope"
[25] "references"

# pull relevant info from an existing EML
davies <- eml_read('../Davies_616')
scope <- davies@scope
system <- davies@system
language <- davies@dataset@language
distribution <- davies@dataset@distribution
alternateIdentifier <- davies@dataset@alternateIdentifier
contact <- davies@dataset@contact
keywordSet <- davies@dataset@keywordSet
publisher <- davies@dataset@publisher
rights <- davies@dataset@intellectualRights
metadataProvider <- davies@dataset@metadataProvider
# additionalInfo <- davies@dataset@dataTable@additionalInfo@section

# text based entities
pubDate <- '2015-07-17'
title <- 'Regional Drinking Water Quality Monitoring Program'
abstract <- 'ASU has been working with regional water providers (SRP, CAP) and metropolitan Phoenix cities since 1998 on algae-related issues affecting drinking water supplies, treatment, and distribution.  The results have improved the understanding of taste and odor (T&O) occurrence, control, and treatment, improved the understanding of dissolved organic and algae dynamics, and initiated a forum to discuss and address regional water quality issues.  The monitoring benefits local WTPs  by optimizing ongoing operations (i.e., reducing operating costs), improving the quality of municipal water for consumers, facilitating long-term water quality planning, and providing information on potentially future-regulated compounds. ASU has been monitoring water quality in terminal reservoirs (Lake Pleasant, Saguaro Lake, and Bartlett Lake) continuously from 1998 to the present for algae-related constituents (taste and odors, and more recently cyanotoxins), nutrients, and disinfection by-product precursors (i.e., total and dissolved organic carbon and organic nitrogen). Additional monitoring has been conducted in the SRP and CAP canal systems and in water treatment plants in Phoenix, Tempe and Peoria. During this work the Valley has been in a prolonged drought and recently one above average wet year, and this data provides important baseline data for development of new or expanded WTPs and management of existing WTPs in the future.  The current work has improved the understanding of T&O sources and treatment, but additional research and monitoring into the future is necessary.'

# person details but none used in this example
# I could not get metadataProvider to work here, so pulled it from Davies_616 after adding those data to the file
# paulwesterhoff <- person(given = 'Paul', family = 'Westerhoff', email = 'p.westerhoff@asu.edu')
# paul <- as.person('Paul Westerhoff <p.westerhoff@asu.edu>')
#
# miltsommerfeld <- person(given = 'Milton', family = 'Sommerfeld', email = 'Milton.Sommerfeld@asu.edu')
# milton <- as.person('Milton Sommerfeld <Milton.Sommerfeld@asu.edu>')
#
# marisamasles <- person(given = 'Marisa', family = 'Masles', email = 'Marisa.Masles@asu.edu')
# marisa <- as.person('Marisa Masles <Marisa.Masles@asu.edu>')
# metadataProvider <- c(as('marisa', 'metadataProvider'))
# metadataProvider <- c(marisamasles)

# not used here
# ssebe <- new('address',
#              deliveryPoint = 'School of Sustainable Engineering and the Built Environment',
#              city = 'Tempe',
#              administrativeArea = 'AZ',
#              postalCode = '85287',
#              country = 'USA')

# creator
creator <- c(as('Paul Westerhoff', 'creator'),
             as('Milton Sommerfeld', 'creator'))

# methods: will need to develop this, esp. project methods versus dataTable methods
methods <- new('methods', methodStep = c(new('methodStep', description = 'Grab water samples were collected for analysis from canals and water treatment plants in headspace-free, amber glass containers.  Water samples for metals analysis were collected in centrifuge tubes and acidified with nitric acid for stability.  Lake samples were obtained from the epilimnion and hypolimnion by way of a Kemmerer sampler.  All samples were kept on ice in the field and stored at 4 degrees Celsius until analyzed.  Nutrient samples that could not be analyzed within 24 hours were frozen. Field measurements for dissolved oxygen, pH, and temperature were made using a portable meter.  Water clarity for lake samples was measured using a Secchi Disk. Taste and odor compounds were measured using solid phase micro extraction and injected onto a GC-MS/MS.  Total nutrients were obtained by way of persulfate digestion and then analysis on a continuous flow colorimetric analyzer.  Metals data were obtained by ICP-MS.')))

# coverage
# note that end date for this on-going study is arbitrarily defined
coverage <- eml_coverage(dates = c('1999-08-17', '2015-06-02'),
                         geographic_description = 'Canals, Water Treatment Plants, and Reservoirs in Phoenix and surrounding areas',
                         NSEWbox = c(34.4900, 33.2917, -111.1235, -112.1250))


# build order per REML
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

# works but cannot incorporate custom_units
delta <- new('dataset',
             scope = scope, # not sure the order of this one
             system = system, # not sure the order of this one
             alternateIdentifier = alternateIdentifier,
             language = language,
             creator = creator,
             metadataProvider = metadataProvider,
             contact = contact,
             distribution = distribution,
             publisher = publisher,
             title = title,
             pubDate = pubDate,
             keywordSet = keywordSet,
             abstract = abstract,
             intellectualRights = rights,
             methods = methods,
             coverage = coverage,
             dataTable = c(algae.DT,
                           arsenic.DT,
                           docMonth.DT,
                           fieldMeasurements.DT,
                           gpscoord.DT,
                           mibAndGeosmin.DT,
                           microbial.DT,
                           nutrients.DT,
                           quarterlyLakeSampling.DT,
                           quarterlyMetals.DT,
                           sampleNames.DT,
                           sucralose.DT))

frank <- new('eml',
             dataset = delta)

eml_write(frank,
          file = "./outFile.xml")

# custom_units > eml
eml_write(custom_units = c(microgramPerCentimeterCubed,
                           SUVA_254nm,
                           microsiemensPerCentimeter,
                           nominalMonth,
                           coloniesPer100milliliter),
          file = "./units.xml")

#### incorporate separate table writing
sapply(names(data), function (x) write.table(data[[x]], file=paste(x, "csv", sep="."), sep = "," ) )
