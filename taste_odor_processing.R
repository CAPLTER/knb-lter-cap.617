---
title: "taste_odor_processing"
author: "SRE"
date: "04/10/2015"
output: html_document
---

# file processing ----

# identify directory with files (not full.names=T)
files <- list.files(path=".", pattern="*.csv", full.names=T, recursive=FALSE)

# function to bring files into R
BatchLoad <- function(file) {data <- read.csv(file, header=T, stringsAsFactors=F) }

# import files from target directory
data <- lapply(files, BatchLoad)

# get filenames of list items from directory (note full.names=F); change to lower case on import
fileNames <- list.files(path=".", pattern="*.csv", full.names=F, recursive=FALSE)

# modify and assign filenames
rmxlsx <- function(file) { sub(".xlsx.csv", "", file) } # function to remove xlsx.csv file extension(s)
rmspac <- function(file) { gsub(" ", "_", file) } # function to change spaces to underscores
fileNames <- lapply(fileNames, rmspac) # apply function rmspac to all filenames
fileNames <- lapply(fileNames, rmxlsx) # apply function rmxlsx to all filenames
names(data) <- fileNames # assign file names to list of datasets
# be sure to change '3D_Fluorescence' to 'Fluorescence' - done manually outside of R in this case

# convert empty values to NAs ----
# method one
data <- lapply(data, function(x) {
  # do the replacing
  x[x=='']<-NA
  return(x)
})

# method two
# tona <- function(x) { x[x==''] <- NA
#                      return(x)}
# data <- lapply(data, tona)

# NAME fields according to metadata ----
# this step is necessary here owing to the non-standardized naming conventions
# in the future, we would give instruction to the investigator for proper naming

# Fluorescence
names(data[['Fluorescence']]) <- c(
  'ID',
  'Site_Number',
  'Month',
  'Exc_A',
  'Em_A',
  'Fl_A',
  'Exc_B',
  'Em_B',
  'Fl_B')

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

# Arsenic
names(data[['Arsenic']])<-c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'Cluster',
  'SiteAcronym',
  'Date',
  'Arsenic',
  'Perchlorate')

# brushexp
names(data[['brushexp']])<-c(
  'ID',
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

# canal_data
names(data[['canal_data']])<-c(
  'ID',
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

# doc_month
names(data[['doc_month']])<-c(
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

# doc_quarter
names(data[['doc_quarter']])<-c(
  'ID',
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

# field_measurements
names(data[['field_measurements']])<-c(
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

# Intensive_Lake_Sampling
names(data[['Intensive_Lake_Sampling']])<-c(
  'ID',
  'SiteNumber',
  'Depth_ft',
  'Depth_m',
  'Lake',
  'Month',
  'MIB',
  'Geosmin',
  'T',
  'DO_conc',
  'DO_percent',
  'AlgaeCount',
  'Comments')

# IntensiveSampling
names(data[['IntensiveSampling']])<-c(
  'ID',
  'Miles_from_Granite_Reef_Dam',
  'Miles_from_24th_St',
  'Site_Number',
  'Location',
  'Date',
  'MIB',
  'Chlor_A',
  'Total_Nitrogen',
  'Dissolved_Nitrogen',
  'Total_Phosphate',
  'Dissolved_Phosphate',
  'Geosmin')

# mib_and_geosmin
names(data[['mib_and_geosmin']])<-c(
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

# quality
names(data[['quality']])<-c(
  'ID',
  'Sample_date',
  'Analyses_type',
  'Samples_Analyzed',
  'Calibration_slope',
  'Calibration_Intercept',
  'Correlation_Coefficient',
  'Measure_units',
  'MDL',
  'Standard_range_low',
  'Standard_range_high',
  'External_theoretical',
  'External_measured',
  'Bias',
  'Digestion_theoretical',
  'Digestion_measured',
  'Digestion_recovery',
  'Sample_spiked',
  'Sample_value',
  'Spike_amount',
  'Spike_theoretical',
  'Spike_measured',
  'Spike_recovery',
  'QC_comment')

# Quarterly_Lake_Sampling
names(data[['Quarterly_Lake_Sampling']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'MIB',
  'Geosmin',
  'Cyclocitral',
  'Conductance',
  'TP',
  'TDN',
  'DOC',
  'UVA',
  'SUVA')

# Quarterly_Metals
names(data[['Quarterly_Metals']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Units',
  '7Li',
  '9Be',
  '23Na',
  '24Mg',
  '27Al',
  '39K',
  '44Ca',
  '51V',
  '52Cr',
  '55Mn',
  '56Fe',
  '59Co',
  '60Ni',
  '65Cu',
  '66Zn',
  '75As',
  '82Se',
  '88Sr',
  '95Mo',
  '107Ag',
  '111Cd',
  '115In',
  '121Sb',
  '138Ba',
  '202Hg',
  '207Pb',
  '232Th',
  '238U')

# Sample_Names
names(data[['Sample_Names']])<-c(
  'Site_ID',
  'Site_Number',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym')

# sample_summary
names(data[['sample_summary']])<-c(
  'ID',
  'Site_Number',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Time',
  'Personnel',
  'Weather',
  'Water_Appearance',
  'Water_Odor_Strength',
  'Water_Odor',
  'Phytoplankton_Color',
  'Phytoplankton_clumps',
  'Comments')

# Sucralose
names(data[['Sucralose']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Sucralose')

# wtp_data
names(data[['wtp_data']])<-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Sample_at_lab?',
  'Pre_chlorination',
  'Conc_CL2',
  'Duration_CL2',
  'Add_CuSO4',
  'Conc_CuSO4',
  'Duration_CuSO4',
  'Alum_in_pre-sed?',
  'Conc_Alum',
  'Duration_Alum',
  'Alum_dose_floc',
  'pH_in_floc',
  'pH_adjuster?',
  'PAC_added?',
  'Conc_PAC',
  'Duration_PAC',
  'WTP_comments')

# format dates and time ----

dates <- function(df) {
  if ("Month" %in% names(df)) {df$Month <- as.POSIXct(df$Month, format="%Y/%m/%d")
                               df$Month <- format(df$Month, format="%m")
  }
  if ("Time" %in% names(df)) {df$Time <- as.POSIXct(df$Time, format="%Y/%m/%d %H:%M:%S")
  df$Time <- format(df$Time, format="%H:%M")
  }
  if ("Date" %in% names(df)) {df$Date <- as.POSIXct(df$Date, format="%Y/%m/%d")
  }
  if ("Sample_Date" %in% names(df)) {df$Sample_Date <- as.POSIXct(df$Sample_Date, format="%Y/%m/%d")
  }
  return(df)
}

data <- lapply(data, dates)

####
# be sure to modify format of time field in sample_summary table
# 'Time' = c(format = '%H:%M'),

# provide col.defs (field descriptions) and unit.defs according to metadata ----

# Fluorescence
col.defs.Fluorescence <- c(
  'ID' = 'Record number',
  'Site_Number' = 'Site number or name',
  'Month' = 'Month sample was collected',
  'Exc_A' = 'Excitation A wavelength',
  'Em_A' = 'Emission A wavelength',
  'Fl_A' = 'Fluorescence A',
  'Exc_B' = 'Excitation B wavelength',
  'Em_B' = 'Emission B wavelength',
  'Fl_B' = 'Fluorescence B')

unit.defs.Fluorescence <- c(
  'ID' = 'number',
  'Site_Number' = "'a' designates the epilimnion",
  'Month' = 'nominalMonth',
  'Exc_A' = 'nanometer',
  'Em_A' = 'nanometer',
  'Fl_A' = 'dimensionless',
  'Exc_B' = 'nanometer',
  'Em_B' = 'nanometer',
  'Fl_B' = 'dimensionless')

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
  'ID' = 'number',
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

# Arsenic
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
  'ID' = 'number',
  'SiteNumber' = '‘A’ designates epilimnion, ‘B’ hypolimnion',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'Cluster' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Arsenic' = 'microgramsPerLiter',
  'Perchlorate' = 'microgramsPerLiter')

# brushexp
col.defs.brushexp <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'Date' = 'Date sample was collected',
  'TotN' = 'Total nitrogen measurement; no data indicates measurement was below the MDL',
  'DissN' = 'Dissolved nitrogen measurement; no data indicates measurement was below the MDL',
  'TotP' = 'Total phosphorus measurement; no data indicates measurement was below the MDL',
  'DissP' = 'Dissolved phosphorus measurement; no data indicates measurement was below the MDL',
  'MIB' = '2-methylisoborneol measurement; no data indicates measurement was below the MDL',
  'Geosmin' = 'Geosmin measurement; no data indicates measurement was below the MDL',
  '664nm' = 'UV-Vis measurement at a particular wavelength',
  '647nm' = 'UV-Vis measurement at a particular wavelength',
  '630nm' = 'UV-Vis measurement at a particular wavelength',
  'Chl_a_vol' = 'Chlorophyll a measurement',
  'Chl_b_vol' = 'Chlorophyll b measurement',
  'Chl_c_vol' = 'Chlorophyll c measurement',
  'Chl_a_area' = 'Chlorophyll a measurement',
  'Chl_b_area' = 'Chlorophyll b measurement',
  'Chl_c_area' = 'Chlorophyll c measurement',
  'DWT' = 'Dry weight of biomass')

unit.defs.brushexp <-c(
  'ID' = 'number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'TotN' = 'milligramsPerLiter',
  'DissN' = 'milligramsPerLiter',
  'TotP' = 'microgramsPerLiter',
  'DissP' = 'microgramsPerLiter',
  'MIB' = 'milligramsPerCubicMeter',
  'Geosmin' = 'milligramsPerCubicMeter',
  '664nm' = 'dimensionless',
  '647nm' = 'dimensionless',
  '630nm' = 'dimensionless',
  'Chl_a_vol' = 'microgramsPerLiter',
  'Chl_b_vol' = 'microgramsPerLiter',
  'Chl_c_vol' = 'microgramsPerLiter',
  'Chl_a_area' = 'milligramsPerSquareMeter',
  'Chl_b_area' = 'milligramsPerSquareMeter',
  'Chl_c_area' = 'milligramsPerSquareMeter',
  'DWT' = 'milligramsPerSquareMeter')

# canal_data
col.defs.canal_data <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'PeriphytonDescription' = 'Color and thickness of mat, if present',
  'Thickness_shallow' = 'Thickness of Periphyton mat(s) less than 6 inches deep',
  'MatColor_shallow' = 'Color of Periphyton mat(s) less than 6 inches deep',
  'Thickness_mid' = 'Thickness of Periphyton mat(s) between 6 and 12 inches deep',
  'MatColor_mid' = 'Color of Periphyton mat(s) between 6 and 12 inches deep',
  'Thickness_deep' = 'Thickness of Periphyton mat(s) greater than 12 inches deep',
  'MatColor_deep' = 'Color of Periphyton mat(s) greater than 12 inches deep',
  'CanalNotes' = 'Description of canal odor, turbidity, algal growth, etc.')

unit.defs.canal_data <-c(
  'ID' = 'number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'PeriphytonDescription' = 'e.g.  “green”, “brown”, “blue-green”, “thick”, “thin”',
  'Thickness_shallow' = 'inch',
  'MatColor_shallow' = 'e.g. “green”,” brown”, “gold”, “blue-green”',
  'Thickness_mid' = 'inch',
  'MatColor_mid' = 'e.g. “green”,” brown”, “gold”, “blue-green”',
  'Thickness_deep' = 'inch',
  'MatColor_deep' = 'e.g. “green”,” brown”, “gold”, “blue-green”',
  'CanalNotes' = 'METADATA_NOT_PROVIDED')

# doc_month
col.defs.doc_month <-c(
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

unit.defs.doc_month <-c(
  'ID' = 'number',
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

# doc_quarter
col.defs.doc_quarter <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'UVAf_Alsorp' = 'METADATA_NOT_PROVIDED',
  'DOCf_Alsorp' = 'METADATA_NOT_PROVIDED',
  'SUVAf' = 'METADATA_NOT_PROVIDED',
  '%_UVA_Rem' = 'Percentage of UVA removed',
  '%_DOC_Rem' = 'Percentage of DOC removed',
  'CHCl3' = 'Chloroform measurement',
  'CHBrCl2' = 'Dichloromethane measurement',
  'CHClBr2' = 'Dibromochloromethane measurement',
  'CHBr3' = 'Bromoform measurement',
  'TTHM' = 'Trihalomethanes measurement',
  'MCAA' = 'Monochloroacetic acid measurement',
  'MBAA' = 'Monobromoacetic acid measurement',
  'DCAA' = 'Dichloroacetic acid measurement',
  'TCAA' = 'Trichloroacetic acid measurement',
  'BCAA' = 'Bromochloroacetic acid measurement',
  'DBAA' = 'Dibromoacetic acid measurement',
  'BDCAA' = 'Bromodichloroacetic acid measurement',
  'CDBAA' = 'Chlorodibromoacetic acid measurement',
  'TBAA' = 'Tribromoacetic acid measurement',
  'DiHAA' = 'di-haloacetic acids measurement',
  '%DBA_Recovery' = 'Percent recovery of Dibromopropionic Acid',
  'THAA9' = 'Sum measurement of Haloacetic acids',
  'MCAA_COP' = 'METADATA_NOT_PROVIDED',
  'MBAA_COP' = 'METADATA_NOT_PROVIDED',
  'DCAA_COP' = 'METADATA_NOT_PROVIDED',
  'TCAA_COP' = 'METADATA_NOT_PROVIDED',
  'BCAA_COP' = 'METADATA_NOT_PROVIDED',
  'DBAA_COP' = 'METADATA_NOT_PROVIDED',
  'BDCAA_COP' = 'METADATA_NOT_PROVIDED',
  'CDBAA_COP' = 'METADATA_NOT_PROVIDED',
  'TAA_COP' = 'METADATA_NOT_PROVIDED',
  'DiHAA_COP' = 'METADATA_NOT_PROVIDED',
  'THAA9_COP' = 'METADATA_NOT_PROVIDED',
  '%DBA_COP' = 'METADATA_NOT_PROVIDED',
  'SampleType' = 'Type of water sample collected',
  'DocqComments' = 'Sampling comments about canal, lake or water treatment plant')

unit.defs.doc_quarter <-c(
  'ID' = 'number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Month' = 'nominalMonth',
  'UVAf_Alsorp' = 'METADATA_NOT_PROVIDED',
  'DOCf_Alsorp' = 'METADATA_NOT_PROVIDED',
  'SUVAf' = 'METADATA_NOT_PROVIDED',
  '%_UVA_Rem' = 'dimensionless',
  '%_DOC_Rem' = 'dimensionless',
  'CHCl3' = 'microgramsPerLiter',
  'CHBrCl2' = 'microgramsPerLiter',
  'CHClBr2' = 'microgramsPerLiter',
  'CHBr3' = 'microgramsPerLiter',
  'TTHM' = 'microgramsPerLiter',
  'MCAA' = 'microgramsPerLiter',
  'MBAA' = 'microgramsPerLiter',
  'DCAA' = 'microgramsPerLiter',
  'TCAA' = 'microgramsPerLiter',
  'BCAA' = 'microgramsPerLiter',
  'DBAA' = 'microgramsPerLiter',
  'BDCAA' = 'microgramsPerLiter',
  'CDBAA' = 'microgramsPerLiter',
  'TBAA' = 'microgramsPerLiter',
  'DiHAA' = 'microgramsPerLiter',
  '%DBA_Recovery' = 'dimensionless',
  'THAA9' = 'microgramsPerLiter',
  'MCAA_COP' = 'METADATA_NOT_PROVIDED',
  'MBAA_COP' = 'METADATA_NOT_PROVIDED',
  'DCAA_COP' = 'METADATA_NOT_PROVIDED',
  'TCAA_COP' = 'METADATA_NOT_PROVIDED',
  'BCAA_COP' = 'METADATA_NOT_PROVIDED',
  'DBAA_COP' = 'METADATA_NOT_PROVIDED',
  'BDCAA_COP' = 'METADATA_NOT_PROVIDED',
  'CDBAA_COP' = 'METADATA_NOT_PROVIDED',
  'TAA_COP' = 'METADATA_NOT_PROVIDED',
  'DiHAA_COP' = 'METADATA_NOT_PROVIDED',
  'THAA9_COP' = 'METADATA_NOT_PROVIDED',
  '%DBA_COP' = 'METADATA_NOT_PROVIDED',
  'SampleType' = 'either “field blank”, “lab blank”, or “duplicate”; "grab" if not noted',
  'DocqComments' = 'e.g. “musty odor”, “turbid”, “algal growth”, “dry canal”, etc.')

# field_measurements
col.defs.field_measurements <-c(
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

unit.defs.field_measurements <-c(
  'ID' = 'number',
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
  'ID' = 'number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'SiteLocation' = 'METADATA_NOT_PROVIDED',
  'ClusterName' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'SiteAcronym' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Longitude' = 'dimensionless',
  'Latitude' = 'dimensionless',
  'Altitude' = 'METADATA_NOT_PROVIDED')

# Intensive_Lake_Sampling
col.defs.Intensive_Lake_Sampling <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'Depth_ft' = 'Sampling depth below water surface',
  'Depth_m' = 'Sampling depth below water surface',
  'Lake' = 'Lake sample was collected at',
  'Month' = 'Month sample was collected',
  'MIB' = 'MIB measurement',
  'Geosmin' = 'Geosmin measurement',
  'T' = 'Temperature measurement',
  'DO_conc' = 'Dissolved oxygen',
  'DO_percent' = 'Dissolved oxygen percent saturation',
  'AlgaeCount' = 'Number of algae counted',
  'Comments' = 'Sampling comments about canal, lake or water treatment plant')

unit.defs.Intensive_Lake_Sampling <-c(
  'ID' = 'number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
  'Depth_ft' = 'foot',
  'Depth_m' = 'meter',
  'Lake' = 'either “Saguaro”, “Bartlett”, or “Pleasant”',
  'Month' = 'nominalMonth',
  'MIB' = 'milligramsPerCubicMeter',
  'Geosmin' = 'milligramsPerCubicMeter',
  'T' = 'celsius',
  'DO_conc' = 'milligramsPerLiter',
  'DO_percent' = 'dimensionless',
  'AlgaeCount' = 'number',
  'Comments' = 'e.g. “musty odor”, “turbid”, “algal growth”, “dry canal”, etc.')

# IntensiveSampling
col.defs.IntensiveSampling <-c(
  'ID' = 'Record number',
  'Miles_from_Granite_Reef_Dam' = 'Location of sampling site relative to Granite Reef Dam',
  'Miles_from_24th_St' = 'Location of sampling site relative to 24th St.',
  'Site_Number' = 'Site number or name',
  'Location' = 'Description of where site is located',
  'Date' = 'Date sample was collected',
  'MIB' = 'MIB measurement',
  'Chlor_A' = 'Chlorophyll a measurement',
  'Total_Nitrogen' = 'Total nitrogen measurement',
  'Dissolved_Nitrogen' = 'Dissolved nitrogen',
  'Total_Phosphate' = 'Total phosphorus measurement',
  'Dissolved_Phosphate' = 'Total phosphorus measurement',
  'Geosmin' = 'Geosmin measurement')

unit.defs.IntensiveSampling <-c(
  'ID' = 'number',
  'Miles_from_Granite_Reef_Dam' = 'mile',
  'Miles_from_24th_St' = 'mile',
  'Site_Number' = 'METADATA_NOT_PROVIDED',
  'Location' = 'METADATA_NOT_PROVIDED',
  'Date' = c(format = 'YYYY-MM-DD'),
  'MIB' = 'milligramsPerCubicMeter',
  'Chlor_A' = 'microgramPerCentimeterCubed',
  'Total_Nitrogen' = 'microgramsPerLiter',
  'Dissolved_Nitrogen' = 'microgramsPerLiter',
  'Total_Phosphate' = 'microgramsPerLiter',
  'Dissolved_Phosphate' = 'microgramsPerLiter',
  'Geosmin' = 'milligramsPerCubicMeter')

# mib_and_geosmin
col.defs.mib_and_geosmin <-c(
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

unit.defs.mib_and_geosmin <-c(
  'ID' = 'number',
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
  'ID' = 'number',
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

# quality
col.defs.quality <-c(
  'ID' = 'Record number',
  'Sample_date' = 'Date sample was collected',
  'Analyses_type' = 'Type of analyses done',
  'Samples_Analyzed' = 'Name of sample analyzed',
  'Calibration_slope' = 'Slope of linear equation',
  'Calibration_Intercept' = 'Calibration intercept',
  'Correlation_Coefficient' = 'Measure of the linear correlation',
  'Measure_units' = 'Units of measurement',
  'MDL' = 'Method detection limit',
  'Standard_range_low' = 'Concentration of lowest standard',
  'Standard_range_high' = 'Concentration of highest standards',
  'External_theoretical' = 'Known concentration of externally measured quality control standard',
  'External_measured' = 'Observed concentration of externally measured quality control standard',
  'Bias' = 'Difference between external theoretical and measured concentrations',
  'Digestion_theoretical' = 'Expected concentration after digestion of analyte',
  'Digestion_measured' = 'Measured concentration after digestion of analyte',
  'Digestion_recovery' = 'Percent recovery of analyte after digestion',
  'Sample_spiked' = 'Name of sample a spike was added to',
  'Sample_value' = 'Concentration of MIB or Geosmin',
  'Spike_amount' = 'Amount of MIB or Geosmin added to sample',
  'Spike_theoretical' = 'Total MIB or Geosmin measured in sample  plus theoretical spike concentration',
  'Spike_measured' = 'Measured total MIB or Geosmin in sample including spike',
  'Spike_recovery' = '“Spike measured” divided by “spike theoretical”',
  'QC_comment' = 'Quality control comment')

unit.defs.quality <-c(
  'ID' = 'number',
  'Sample_date' = c(format = 'YYYY-MM-DD'),
  'Analyses_type' = 'either nitrogen, phosphorus, MIB, Geosmin, DOC,  or conductance',
  'Samples_Analyzed' = 'METADATA_NOT_PROVIDED',
  'Calibration_slope' = 'dimensionless',
  'Calibration_Intercept' = 'dimensionless',
  'Correlation_Coefficient' = 'dimensionless',
  'Measure_units' = 'depending on the parameter measured',
  'MDL' = 'lowest limit of detection for given method',
  'Standard_range_low' = 'low  calibration standard',
  'Standard_range_high' = 'high calibration standard',
  'External_theoretical' = 'varies depending on parameter measured',
  'External_measured' = 'varies depending on parameter measured',
  'Bias' = 'METADATA_NOT_PROVIDED',
  'Digestion_theoretical' = 'either “1” for Nitrogen or “100” for phosphorus',
  'Digestion_measured' = 'either “1” for Nitrogen or “100” for phosphorus',
  'Digestion_recovery' = 'dimensionless',
  'Sample_spiked' = 'METADATA_NOT_PROVIDED',
  'Sample_value' = 'milligramsPerCubicMeter',
  'Spike_amount' = 'milligramsPerCubicMeter',
  'Spike_theoretical' = 'milligramsPerCubicMeter',
  'Spike_measured' = 'milligramsPerCubicMeter',
  'Spike_recovery' = 'dimensionless',
  'QC_comment' = 'METADATA_NOT_PROVIDED')

# Quarterly_Lake_Sampling
col.defs.Quarterly_Lake_Sampling <-c(
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

unit.defs.Quarterly_Lake_Sampling <-c(
  'ID' = 'number',
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

# Quarterly_Metals
col.defs.Quarterly_Metals <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Units' = 'Measurement units',
  '7Li' = 'Lithium measurement by ICP-MS',
  '9Be' = 'Beryllium measurement by ICP-MS',
  '23Na' = 'Sodium measurement by ICP-MS',
  '24Mg' = 'Magnesium measurement by ICP-MS',
  '27Al' = 'Aluminum measurement by ICP-MS',
  '39K' = 'Potassium measurement by ICP-MS',
  '44Ca' = 'Calcium measurement by ICP-MS',
  '51V' = 'Vanadium measurement by ICP-MS',
  '52Cr' = 'Chromium measurement by ICP-MS',
  '55Mn' = 'Manganese measurement by ICP-MS',
  '56Fe' = 'Iron measurement by ICP-MS',
  '59Co' = 'Cobalt measurement by ICP-MS',
  '60Ni' = 'Nickle measurement by ICP-MS',
  '65Cu' = 'Copper measurement by ICP-MS',
  '66Zn' = 'Zinc measurement by ICP-MS',
  '75As' = 'Arsenic measurement by ICP-MS',
  '82Se' = 'Selenium measurement by ICP-MS',
  '88Sr' = 'Strontium measurement by ICP-MS',
  '95Mo' = 'Molybdenum measurement by ICP-MS',
  '107Ag' = 'Silver measurement by ICP-MS',
  '111Cd' = 'Cadmium measurement by ICP-MS',
  '115In' = 'Indium measurement by ICP-MS',
  '121Sb' = 'Antimony measurement by ICP-MS',
  '138Ba' = 'Barium measurement by ICP-MS',
  '202Hg' = 'Mercury measurement by ICP-MS',
  '207Pb' = 'Lead measurement by ICP-MS',
  '232Th' = 'Thorium measurement by ICP-MS',
  '238U' = 'Uranium measurement by ICP-MS')

unit.defs.Quarterly_Metals <-c(
  'ID' = 'number',
  'Site_Name' = '“A” denotes epilimnion, “B” denotes hypolimnion',
  'Site_Location' = '“upper” denotes upstream lake location, “lower” denotes downstream',
  'Cluster_Name' = 'either “Salt River”,” Cap”, or “Verde”',
  'Site_Acronym' = '“epi” = epilimnion, “hypo” = hypolimnion',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Units' = 'micrograms per liter',
  '7Li' = 'microgramsPerLiter',
  '9Be' = 'microgramsPerLiter',
  '23Na' = 'microgramsPerLiter',
  '24Mg' = 'microgramsPerLiter',
  '27Al' = 'microgramsPerLiter',
  '39K' = 'microgramsPerLiter',
  '44Ca' = 'microgramsPerLiter',
  '51V' = 'microgramsPerLiter',
  '52Cr' = 'microgramsPerLiter',
  '55Mn' = 'microgramsPerLiter',
  '56Fe' = 'microgramsPerLiter',
  '59Co' = 'microgramsPerLiter',
  '60Ni' = 'microgramsPerLiter',
  '65Cu' = 'microgramsPerLiter',
  '66Zn' = 'microgramsPerLiter',
  '75As' = 'microgramsPerLiter',
  '82Se' = 'microgramsPerLiter',
  '88Sr' = 'microgramsPerLiter',
  '95Mo' = 'microgramsPerLiter',
  '107Ag' = 'microgramsPerLiter',
  '111Cd' = 'microgramsPerLiter',
  '115In' = 'microgramsPerLiter',
  '121Sb' = 'microgramsPerLiter',
  '138Ba' = 'microgramsPerLiter',
  '202Hg' = 'microgramsPerLiter',
  '207Pb' = 'microgramsPerLiter',
  '232Th' = 'microgramsPerLiter',
  '238U' = 'microgramsPerLiter')

# Sample_Names
col.defs.Sample_Names <-c(
  'Site_ID' = 'Record number',
  'Site_Number' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location')

unit.defs.Sample_Names <-c(
  'Site_ID' = 'METADATA_NOT_PROVIDED',
  'Site_Number' = '“A” denotes epilimnion, “B” denotes hypolimnion',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = '“epi”= epilimnion, “hypo”= hypolimnion,  “dup”= duplicate, “x con”= cross connect')

# sample_summary
col.defs.sample_summary <-c(
  'ID' = 'Record number',
  'Site_Number' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Time' = 'Time sample was collected',
  'Personnel' = 'Initials of people who went sampling',
  'Weather' = 'Weather during sampling',
  'Water_Appearance' = 'Color and degree of turbidity of water in lake, canal or water treatment plant',
  'Water_Odor_Strength' = 'Degree of odor from water in lake, canal or water treatment plant',
  'Water_Odor' = 'METADATA_NOT_PROVIDED',
  'Phytoplankton_Color' = 'Color of phytoplankton, if present',
  'Phytoplankton_clumps' = 'Presence or absence of phytoplankton and description',
  'Comments' = 'Additional notes on water description')

unit.defs.sample_summary <-c(
  'ID' = 'number',
  'Site_Number' = '“A” denotes epilimnion, “B” denotes hypolimnion',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = '“epi”= epilimnion, “hypo”= hypolimnion,  “dup”= duplicate, “x con”= cross connect',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Time' = c(format = '%H:%M'),
  'Personnel' = 'METADATA_NOT_PROVIDED',
  'Weather' = 'e.g. “clear”, “partly cloudy”, “sunny”, “rain”',
  'Water_Appearance' = 'e.g. “clear”,” turbid”, “stagnant”,” green”, “brown”',
  'Water_Odor_Strength' = 'e.g. “absent”,” weak”,” medium”, “strong”',
  'Water_Odor' = 'Metadata not provided',
  'Phytoplankton_Color' = 'e.g. “green”,” yellow”,” blue-green”,”brown”',
  'Phytoplankton_clumps' = 'either “no” or “yes”, with description',
  'Comments' = 'METADATA_NOT_PROVIDED')

# Sucralose
col.defs.Sucralose <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Sucralose' = 'Sucralose measurement by LC-MS; Samples concentrated first by SPE')

unit.defs.Sucralose <-c(
  'ID' = 'number',
  'Site_Name' = '“A” denotes epilimnion',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = '“epi” = epilimnion',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Sucralose' = 'microgramsPerLiter')

# wtp_data
col.defs.wtp_data <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Sample_at_lab?' = 'Was sample collected from lab at the water treatment plant?',
  'Pre_chlorination' = 'Was sample pre-chlorinated?',
  'Conc_CL2' = 'Chlorine concentration in milligrams per liter',
  'Duration_CL2' = 'Period when chlorine was added',
  'Add_CuSO4' = 'Was copper sulphate added?',
  'Conc_CuSO4' = 'Concentration of copper sulphate added',
  'Duration_CuSO4' = 'Period when copper sulfate was added',
  'Alum_in_pre-sed?' = 'Was alum in the pre-sedimentation basin',
  'Conc_Alum' = 'Concentration of alum in pre-sedimentation basin',
  'Duration_Alum' = 'Period when alum was added',
  'Alum_dose_floc' = 'Alum dose added to floc',
  'pH_in_floc' = 'pH of the floc',
  'pH_adjuster?' = 'Note on whether the pH was adjusted and acid used',
  'PAC_added?' = 'Note on whether powder activated carbon was added',
  'Conc_PAC' = 'Concentration of powder activated carbon added',
  'Duration_PAC' = 'Period when powder activated carbon was added',
  'WTP_comments' = 'Sampling conditions noted at the water treatment plant')

unit.defs.wtp_data <-c(
  'ID' = 'number',
  'Site_Name' = '“A” denotes epilimnion',
  'Site_Location' = 'METADATA_NOT_PROVIDED',
  'Cluster_Name' = 'either “Verde”,” Salt”, “CAP”, “SRP",  or “Tempe”',
  'Site_Acronym' = '“epi” = epilimnion',
  'Date' = c(format = 'YYYY-MM-DD'),
  'Sample_at_lab?' = 'either “yes” or “no”, if “no” where from',
  'Pre_chlorination' = 'either “yes” or “no”',
  'Conc_CL2' = 'milligramsPerLiter',
  'Duration_CL2' = 'e.g. “starting”',
  'Add_CuSO4' = 'either “yes” or “no”',
  'Conc_CuSO4' = 'milligramsPerLiter',
  'Duration_CuSO4' = 'METADATA_NOT_PROVIDED',
  'Alum_in_pre-sed?' = 'either “yes” or “no”',
  'Conc_Alum' = 'milligramsPerLiter',
  'Duration_Alum' = 'METADATA_NOT_PROVIDED',
  'Alum_dose_floc' = 'milligramsPerLiter',
  'pH_in_floc' = 'dimensionless',
  'pH_adjuster?' = 'either “yes”, “sulfuric” or “no”',
  'PAC_added?' = 'either “yes” or “no”',
  'Conc_PAC' = 'milligramsPerLiter',
  'Duration_PAC' = 'METADATA_NOT_PROVIDED',
  'WTP_comments' = 'e.g. “water treatment plant off-line”, “canal shutdown”')

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
                              description = "Specific Ultra Violet  Absorbance (liter per milligram per meter)")

microgramPerCentimeterCubed <- eml_define_unit(id = "microgramPerCentimeterCubed",
                                               parentSI = "kilogramsPerCubicMeter",
                                               unitType = "massDensity",
                                               multiplierToSI = "1000000",
                                               description = "Smicrograms per cubic centimeter")

# generate datatables ----

Fluorescence.DT <- eml_dataTable(Fluorescence,
                                 col.defs = col.defs.Fluorescence,
                                 unit.defs = unit.defs.Fluorescence,
                                 description = "metadata documentation for Fluorescence",
                                 filename = "Fluorescence.csv")

algae.DT <- eml_dataTable(algae,
                          col.defs = col.defs.algae,
                          unit.defs = unit.defs.algae,
                          description = "metadata documentation for algae",
                          filename = algae.csv)

Arsenic.DT <- eml_dataTable(Arsenic,
                            col.defs = col.defs.Arsenic,
                            unit.defs = unit.defs.Arsenic,
                            description = "metadata documentation for Arsenic",
                            filename = Arsenic.csv)

brushexp.DT <- eml_dataTable(brushexp,
                             col.defs = col.defs.brushexp,
                             unit.defs = unit.defs.brushexp,
                             description = "metadata documentation for brushexp",
                             filename = brushexp.csv)

canal_data.DT <- eml_dataTable(canal_data,
                               col.defs = col.defs.canal_data,
                               unit.defs = unit.defs.canal_data,
                               description = "metadata documentation for canal_data",
                               filename = canal_data.csv)

doc_month.DT <- eml_dataTable(doc_month,
                              col.defs = col.defs.doc_month,
                              unit.defs = unit.defs.doc_month,
                              description = "metadata documentation for doc_month",
                              filename = doc_month.csv)

doc_quarter.DT <- eml_dataTable(doc_quarter,
                                col.defs = col.defs.doc_quarter,
                                unit.defs = unit.defs.doc_quarter,
                                description = "metadata documentation for doc_quarter",
                                filename = doc_quarter.csv)

field_measurements.DT <- eml_dataTable(field_measurements,
                                       col.defs = col.defs.field_measurements,
                                       unit.defs = unit.defs.field_measurements,
                                       description = "metadata documentation for field_measurements",
                                       filename = field_measurements.csv)

gpscoord.DT <- eml_dataTable(gpscoord,
                             col.defs = col.defs.gpscoord,
                             unit.defs = unit.defs.gpscoord,
                             description = "metadata documentation for gpscoord",
                             filename = gpscoord.csv)

Intensive_Lake_Sampling.DT <- eml_dataTable(Intensive_Lake_Sampling,
                                            col.defs = col.defs.Intensive_Lake_Sampling,
                                            unit.defs = unit.defs.Intensive_Lake_Sampling,
                                            description = "metadata documentation for Intensive_Lake_Sampling",
                                            filename = Intensive_Lake_Sampling.csv)

IntensiveSampling.DT <- eml_dataTable(IntensiveSampling,
                                      col.defs = col.defs.IntensiveSampling,
                                      unit.defs = unit.defs.IntensiveSampling,
                                      description = "metadata documentation for IntensiveSampling",
                                      filename = IntensiveSampling.csv)

mib_and_geosmin.DT <- eml_dataTable(mib_and_geosmin,
                                    col.defs = col.defs.mib_and_geosmin,
                                    unit.defs = unit.defs.mib_and_geosmin,
                                    description = "metadata documentation for mib_and_geosmin",
                                    filename = mib_and_geosmin.csv)

nutrients.DT <- eml_dataTable(nutrients,
                              col.defs = col.defs.nutrients,
                              unit.defs = unit.defs.nutrients,
                              description = "metadata documentation for nutrients",
                              filename = nutrients.csv)

quality.DT <- eml_dataTable(quality,
                            col.defs = col.defs.quality,
                            unit.defs = unit.defs.quality,
                            description = "metadata documentation for quality",
                            filename = quality.csv)

Quarterly_Lake_Sampling.DT <- eml_dataTable(Quarterly_Lake_Sampling,
                                            col.defs = col.defs.Quarterly_Lake_Sampling,
                                            unit.defs = unit.defs.Quarterly_Lake_Sampling,
                                            description = "metadata documentation for Quarterly_Lake_Sampling",
                                            filename = Quarterly_Lake_Sampling.csv)

Quarterly_Metals.DT <- eml_dataTable(Quarterly_Metals,
                                     col.defs = col.defs.Quarterly_Metals,
                                     unit.defs = unit.defs.Quarterly_Metals,
                                     description = "metadata documentation for Quarterly_Metals",
                                     filename = Quarterly_Metals.csv)

Sample_Names.DT <- eml_dataTable(Sample_Names,
                                 col.defs = col.defs.Sample_Names,
                                 unit.defs = unit.defs.Sample_Names,
                                 description = "metadata documentation for Sample_Names",
                                 filename = Sample_Names.csv)

sample_summary.DT <- eml_dataTable(sample_summary,
                                   col.defs = col.defs.sample_summary,
                                   unit.defs = unit.defs.sample_summary,
                                   description = "metadata documentation for sample_summary",
                                   filename = sample_summary.csv)

Sucralose.DT <- eml_dataTable(Sucralose,
                              col.defs = col.defs.Sucralose,
                              unit.defs = unit.defs.Sucralose,
                              description = "metadata documentation for Sucralose",
                              filename = Sucralose.csv)

wtp_data.DT <- eml_dataTable(wtp_data,
                             col.defs = col.defs.wtp_data,
                             unit.defs = unit.defs.wtp_data,
                             description = "metadata documentation for wtp_data",
                             filename = wtp_data.csv)

# generate dataset ----

# all possible dataset slotNames
[1] "alternateIdentifier" "shortName"           "title"               "creator"             "metadataProvider"    "associatedParty"
[7] "pubDate"             "language"            "series"              "abstract"            "keywordSet"          "additionalInfo"
[13] "intellectualRights"  "distribution"        "coverage"            "purpose"             "contact"             "publisher"
[19] "methods"             "dataTable"           "otherEntity"         "id"                  "system"              "scope"
[25] "references"