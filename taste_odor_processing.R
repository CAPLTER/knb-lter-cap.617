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
rmspac <- function(file) { gsub(" ", "_", file) } # function to remove xlsx
fileNames <- llply(fileNames, rmspac) # apply function to all filenames
fileNames <- llply(fileNames, rmxlsx) # apply function to all filenames
names(data) <- fileNames # assign file names to list of datasets

# convert empty values to NAs DOES NOT SEEM TO BE NECC FOR THESE DATA ----
data <- llply(data, function(x) {
  # do the replacing
  x[x=='']<-NA
  return(x)
})

tona <- function(list) { x[x==''] <- NA }
data <- llply(data, tona)

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

# Algae.csv
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
  'SiteNumber',
  'SiteLocation',
  'Cluster',
  'SiteAcronym',
  'Date',
  'Arsenic',
  'Perchlorate')

# Brushexp.csv
names(data[['Brushexp.csv']])<-c(
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

# Canal_data.csv
names(data[['Canal_data.csv']])<-c(
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

# DOC_month.csv
names(data[['DOC_month.csv']])<-c(
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

# DOC_quarter.csv
names(data[['DOC_quarter.csv']])<-c(
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
  'THAA9(ug/L)',
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



# old code from s200 stuff ----

# format site code names (should have made this a drop down list) ----
data[["gpi"]][['s_site_code']] <- toupper(data[["gpi"]][['s_site_code']]) # to uppercase
data[["gpi"]][['s_site_code']] <- paste0(data[["gpi"]][['s_site_code']],'1') # add a trailing '1'
data[["gpi"]][['s_site_code']] <- gsub("-","", data[["gpi"]][['s_site_code']]) # omit any '-'
data[["gpi"]][['s_site_code']] <- sub("\\s+$", "", data[["gpi"]][['s_site_code']]) # omit any trailing spaces

# remove spaces from extra site crew names
# note this will also remove spaces between crew members if there are more than one, need to assess
data[['gpi_additional_crew']][['se_other_crew_member']] <- sub("\\s", "", data[['gpi_additional_crew']][['se_other_crew_member']])

# need to change plot to survey200; this seems to work but throws an error, need to test when there are parcel data else do in PG
# if ( data[["gpi"]][['s_research_focus']] == 'plot' ) data[["gpi"]][['s_research_focus']] <- 'survey200'

# format dates
for (i in 1:length(data)) {if ("start" %in% names(data[[i]])) data[[i]]$start<- as.POSIXct(data[[i]]$start, format="%b %e, %Y %I:%M:%S %p")}
for (i in 1:length(data)) {if ("end" %in% names(data[[i]])) data[[i]]$end <- as.POSIXct(data[[i]]$end, format="%b %e, %Y %I:%M:%S %p")}
for (i in 1:length(data)) {if ("today" %in% names(data[[i]])) data[[i]]$today <- as.POSIXct(data[[i]]$today, format="%b %e, %Y")}
for (i in 1:length(data)) {if ("se_samp_date" %in% names(data[[i]])) data[[i]]$se_samp_date <- as.POSIXct(data[[i]]$se_samp_date, format="%b %e, %Y")}

# strip "/media" prefix from photo names
for (i in 1:length(data)) {if ("vs_annuals_photo" %in% names(data[[i]])) data[[i]]$vs_annuals_photo <- gsub("media/", "", data[[i]]$vs_annuals_photo)}
for (i in 1:length(data)) {if ("synoptic_picture" %in% names(data[[i]])) data[[i]]$synoptic_picture<- gsub("media/", "", data[[i]]$synoptic_picture)}
for (i in 1:length(data)) {if ("vs_hedgeSurv_photo" %in% names(data[[i]])) data[[i]]$vs_hedgeSurv_photo<- gsub("media/", "", data[[i]]$vs_hedgeSurv_photo)}
for (i in 1:length(data)) {if ("plot_shrub_cnt_photo" %in% names(data[[i]])) data[[i]]$plot_shrub_cnt_photo<- gsub("media/", "", data[[i]]$plot_shrub_cnt_photo)}
for (i in 1:length(data)) {if ("vs_shrubSurv_photo" %in% names(data[[i]])) data[[i]]$vs_shrubSurv_photo<- gsub("media/", "", data[[i]]$vs_shrubSurv_photo)}
for (i in 1:length(data)) {if ("vs_trees_photo" %in% names(data[[i]])) data[[i]]$vs_trees_photo<- gsub("media/", "", data[[i]]$vs_trees_photo)}
for (i in 1:length(data)) {if ("vs_trees_photo" %in% names(data[[i]])) data[[i]]$vs_trees_photo<- gsub("media/", "", data[[i]]$vs_trees_photo)}

# format column names
for (i in 1:length(data)) { names(data[[i]]) <- gsub("\\.", "_", names(data[[i]])) } # change '.' to '_' for good practice
for (i in 1:length(data)) { names(data[[i]]) <- tolower(names(data[[i]])) } # change all to lowercase

# loop through list items to put push them to postgresql
# note we need to add an if else for appending to existing tables
# dbWriteTable(con, c("s200_tablet","s200_annuals_annualsSurveyTaxa"), value=s200_annuals_annualsSurveyTaxa, row.names=F) 
for (i in 1:length(data)) {dbWriteTable(con, c("s200_tablet",names(data)[[i]]), value=data[[i]], row.names=F)}
