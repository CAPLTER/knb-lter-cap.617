---
title: "taste_odor_processing"
author: "SRE"
date: "04/10/2015"
output: html_document
---

# BEGIN ----

# the list approach ----

# identify directory with files (not full.names=T)
files <- list.files(path=".", pattern="*.csv", full.names=T, recursive=FALSE)

# function to bring files into R
BatchLoad <- function(file) {data <- read.csv(file, header=T, stringsAsFactors=F) }

# import files from target directory
data <- lapply(files, BatchLoad)

# get filenames of list items from directory (note full.names=F); change to lower case on import
fileNames <- list.files(path=".", pattern="*.csv", full.names=F, recursive=FALSE)

# modify and assign filenames
rmxlsx <- function(file) { sub(".xlsx", "", file) } # function to remove xlsx
rmspac <- function(file) { gsub(" ", "_", file) } # function to change spaces to underscores
fileNames <- lapply(fileNames, rmspac) # apply function rmspac to all filenames
fileNames <- lapply(fileNames, rmxlsx) # apply function rmxlsx to all filenames
names(data) <- fileNames # assign file names to list of datasets

# convert empty values to NAs ----
# method one
data <- lapply(data, function(x) {
  # do the replacing
  x[x=='']<-NA
  return(x)
})

# method two
tona <- function(x) { x[x==''] <- NA 
                      return(x)}
data <- lapply(data, tona)


# name fields according to metadata ----
# 3D_Fluorescence.csv
names(data[['3D_Fluorescence.csv']]) <- c(
  'ID',
  'Site_Number',
  'Month',
  'Exc_A',
  'Em_A',
  'Fl_A',
  'Exc_B',
  'Em_B',
  'Fl_B')

# algae.csv
names(data[['algae.csv']]) <- c(
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

# Arsenic.csv
names(data[['Arsenic.csv']])<-c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'Cluster',
  'SiteAcronym',
  'Date',
  'Arsenic',
  'Perchlorate')

# brushexp.csv
names(data[['brushexp.csv']])<-c(
  'SiteNumber',
  'SiteLocation',
  'Date',
  'TotN',
  'DissN',
  'TotP',
  'DissP',
  'MIB',
  'Geosmin',
  '664nm',
  '647nm',
  '630nm',
  'Chl_a_vol',
  'Chl_b_vol',
  'Chl_c_vol',
  'Chl_a_area',
  'Chl_b_area',
  'Chl_c_area',
  'DWT')

# canal_data.csv
names(data[['canal_data.csv']])<-c(
  'SiteNumber',
  'SiteLocation',
  'ClusterName',
  'SiteAcronym',
  'Date',
  'PeriphytonDescription',
  'Thickness_shallow',
  'MatColor_shallow',
  'Thickness_mid',
  'MatColor_mid',
  'Thickness_deep',
  'MatColor_deep',
  'CanalNotes')

# doc_month.csv
names(data[['doc_month.csv']])<-c(
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
  'Fl')

# doc_quarter.csv
names(data[['doc_quarter.csv']])<-c(
  'SiteNumber',
  'SiteLocation',
  'ClusterName',
  'SiteAcronym',
  'Date',
  'Month',
  'UVAf_Alsorp',
  'DOCf_Alsorp',
  'SUVAf',
  '%_UVA_Rem',
  '%_DOC_Rem',
  'CHCl3',
  'CHBrCl2',
  'CHClBr2',
  'CHBr3',
  'TTHM',
  'MCAA',
  'MBAA',
  'DCAA',
  'TCAA',
  'BCAA',
  'DBAA',
  'BDCAA',
  'CDBAA',
  'TBAA',
  'DiHAA',
  '%DBA_Recovery',
  'THAA9',
  'MCAA_COP',
  'MBAA_COP',
  'DCAA_COP',
  'TCAA_COP',
  'BCAA_COP',
  'DBAA_COP',
  'BDCAA_COP',
  'CDBAA_COP',
  'TAA_COP',
  'DiHAA_COP',
  'THAA9_COP',
  '%DBA_COP',
  'SampleType',
  'DocqComments')

# field_measurements.csv
names(data[['field_measurements.csv']])<-c(
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

# gpscoord.csv
names(data[['gpscoord.csv']])<-c(
  'ID',
  'SiteNumber',
  'ClusterName',
  'SiteAcronym',
  'Date',
  'Longitude',
  'Latitude',
  'Altitude')

# Intensive_Lake_Sampling.csv
names(data[['Intensive_Lake_Sampling.csv']])<-c(
  'ID',
  'SiteNumber',
  'Depth',
  'Depth',
  'Lake',
  'Month',
  'MIB',
  'Geosmin',
  'T',
  'DO_conc',
  'DO_percent',
  'AlgaeCount',
  'Comments')


# provide field descriptions according to metadata ----
# 3D_Fluorescence.csv
fields.fluor <- c(
  'Record number',
  'Site number or name',
  'Month sample was collected',
  'Excitation A wavelength',
  'Emission A wavelength',
  'Fluorescence A',
  'Excitation B wavelength',
  'Emission B wavelength',
  'Fluorescence B')

# algae.csv
fields.algae <- c(
  'Record number',
  'Site number or name',
  'Description of where site is located',
  'Source water',
  'Abbreviation of site location',
  'Date sample was collected',
  'Month sample was collected',
  'Type of water sample collected',
  'Conductivity of water sample',
  'Total Chlorophyll a measurement',
  'Phaeophytin measurement',
  'Chlorophyll a measurement corrected for Phaeophytin',
  'Chlorophyta count',
  'Cyanophyta count',
  'Bacillariophyta count')

# format data frames ----
# format dates ----
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

