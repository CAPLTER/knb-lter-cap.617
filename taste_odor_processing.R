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

# 3D_Fluorescence
names(data[['3D_Fluorescence']]) <- c(
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

# provide col.defs (field descriptions) according to metadata ----

# 3D_Fluorescence
col.defs.3D_Fluorescence <- c(
  'ID' = 'Record number',
  'Site_Number' = 'Site number or name; “a” designates the epilimnion',
  'Month' = 'Month sample was collected',
  'Exc_A' = 'Excitation A wavelength',
  'Em_A' = 'Emission A wavelength',
  'Fl_A' = 'Fluorescence A',
  'Exc_B' = 'Excitation B wavelength',
  'Em_B' = 'Emission B wavelength',
  'Fl_B' = 'Fluorescence B')

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

# Arsenic
col.defs.arsenic <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'Cluster' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Arsenic' = 'Arsenic measurement',
  'Perchlorate' = 'Perchlorate measurement')

# brushexp
col.defs.brushexp <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'Date' = 'Date sample was collected',
  'TotN' = 'Total nitrogen measurement',
  'DissN' = 'Dissolved nitrogen measurement',
  'TotP' = 'Total phosphorus measurement',
  'DissP' = 'Dissolved phosphorus measurement',
  'MIB' = '2-methylisoborneol measurement',
  'Geosmin' = 'Geosmin measurement',
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

# canal_data
col.defs.canal_data <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'PeriphytonDescription' = 'Color and thickness of mat, if present',
  'Thickness_shallow' = 'Thickness of Periphyton mat(s)  less than 6 inches deep',
  'MatColor_shallow' = 'Color of Periphyton mat(s)  less than 6 inches deep',
  'Thickness_mid' = 'Thickness of Periphyton mat(s) between 6 and 12 inches deep',
  'MatColor_mid' = 'Color of Periphyton mat(s) between 6 and 12 inches deep',
  'Thickness_deep' = 'Thickness of Periphyton mat(s) greater than 12 inches deep',
  'MatColor_deep' = 'Color of Periphyton mat(s) greater than 12 inches deep',
  'CanalNotes' = 'Description of canal odor, turbidity, algal growth, etc.')

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
  'UVA' = 'UV absorbance measurement',
  'SUVA' = 'SUVA calculation',
  'PeakInt' = 'Intensity of peak',
  'PeakIntWL' = 'Wavelength at the highest peak intensity',
  'Intat450' = 'Intensity at 450 nanometers',
  'Intat460' = 'Intensity at 460 nanometers',
  'Intat500' = 'Intensity at 500 nanometers',
  'Fl' = 'Fluorescence ratio of 450/500',
  'docm_comments' = 'No metadata provided')

# doc_quarter
col.defs.doc_quarter <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'UVAf_Alsorp' = 'metadata not provided',
  'DOCf_Alsorp' = 'metadata not provided',
  'SUVAf' = 'metadata not provided',
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
  'MCAA_COP' = 'metadata not provided',
  'MBAA_COP' = 'metadata not provided',
  'DCAA_COP' = 'metadata not provided',
  'TCAA_COP' = 'metadata not provided',
  'BCAA_COP' = 'metadata not provided',
  'DBAA_COP' = 'metadata not provided',
  'BDCAA_COP' = 'metadata not provided',
  'CDBAA_COP' = 'metadata not provided',
  'TAA_COP' = 'metadata not provided',
  'DiHAA_COP' = 'metadata not provided',
  'THAA9_COP' = 'metadata not provided',
  '%DBA_COP' = 'metadata not provided',
  'SampleType' = 'Type of water sample collected',
  'DocqComments' = 'Sampling comments about canal, lake or water treatment plant')

# field_measurements
col.defs.field_measurements <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = 'Description of where site is located',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Month' = 'Month sample was collected',
  'Depth' = 'Water depth where measurement was taken',
  'Temperature' = 'Water temperature',
  'DO' = 'Dissolved oxygen measurement',
  'pH' = 'pH measurement',
  'SecchiDisk' = 'Water transparency measurement',
  'SamplesCollected' = 'Individual bottles collected for various analyses',
  'SampleType' = 'Type of water sample collected',
  'Comments' = 'Sampling comments about canal, lake or water treatment plant')

# gpscoord
col.defs.gpscoord <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'SiteLocation' = '',
  'ClusterName' = 'Source water',
  'SiteAcronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Longitude' = 'Longitude coordinates',
  'Latitude' = 'Latitude coordinates',
  'Altitude' = 'Elevation above sea level')

# Intensive_Lake_Sampling
col.defs.Intensive_Lake_Sampling <-c(
  'ID' = 'Record number',
  'SiteNumber' = 'Site number or name',
  'Depth' = 'Sampling depth below water surface',
  'Depth' = 'Sampling depth below water surface',
  'Lake' = 'Lake sample was collected at',
  'Month' = 'Month sample was collected',
  'MIB' = 'MIB measurement',
  'Geosmin' = 'Geosmin measurement',
  'T' = 'Temperature measurement',
  'DO_conc' = 'Dissolved oxygen',
  'DO_percent' = 'Dissolved oxygen percent saturation',
  'AlgaeCount' = 'Number of algae counted',
  'Comments' = 'Sampling comments about canal, lake or water treatment plant')

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
  'IPMP_Recovery' = 'Recovery of 2-isopropyl-3-methoxy pyrazine',
  'MIB_Phoenix' = 'MIB comparison with Phoenix (?)',
  'Geosmin_Phoenix' = 'Geosmin comparison with Phoenix (?)',
  'Comments' = 'Sampling comments about canal, lake or water treatment plant')

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
  'Tot_N' = 'Total nitrogen measurement',
  'Diss_N' = 'Dissolved nitrogen measurement',
  'Tot_P' = 'Total phosphorus measurement',
  'Diss_P' = 'Dissolved phosphorus measurement',
  'NO3_N' = 'Nitrogen as nitrate measurement',
  'NH4_N' = 'Nitrogen as ammonia measurement',
  'DON' = 'Dissolved organic nitrogen measurement',
  'Nutrient_comments' = 'Sampling comments about canal, lake or water treatment plant')

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

# Sample_Names
col.defs.Sample_Names <-c(
  'Site_ID' = 'Record number',
  'Site_Number' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location')

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
  'Water_Odor' = 'Metadata not provided',
  'Phytoplankton_Color' = 'Color of phytoplankton, if present',
  'Phytoplankton_clumps' = 'Presence or absence of phytoplankton and description',
  'Comments' = 'Additional notes on water description')

# Sucralose
col.defs.Sucralose <-c(
  'ID' = 'Record number',
  'Site_Name' = 'Name of sampling site',
  'Site_Location' = 'Description of where site is located',
  'Cluster_Name' = 'Source water',
  'Site_Acronym' = 'Abbreviation of site location',
  'Date' = 'Date sample was collected',
  'Sucralose' = 'Sucralose measurement by LC-MS')

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

# provide unit.defs (units) according to metadata ----

# 3D_Fluorescence
unit.defs.3D_Fluorescence <- c(
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
unit.defs.algae <- list(
  'ID' = 'number',
  'SiteNumber' = 'METADATA_NOT_PROVIDED',
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
unit.defs.arsenic <-c(
  'ID',
  'SiteNumber',
  'SiteLocation',
  'Cluster',
  'SiteAcronym',
  'Date',
  'Arsenic',
  'Perchlorate')

# brushexp
unit.defs.brushexp <-c(
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
unit.defs.canal_data <-c(
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
unit.defs.doc_month <-c(
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
  'docm_comments',
  'Fl')

# doc_quarter
unit.defs.doc_quarter <-c(
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
unit.defs.field_measurements <-c(
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
unit.defs.gpscoord <-c(
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
unit.defs.Intensive_Lake_Sampling <-c(
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

# IntensiveSampling
unit.defs.IntensiveSampling <-c(
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
unit.defs.mib_and_geosmin <-c(
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
unit.defs.nutrients <-c(
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
unit.defs.quality <-c(
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
unit.defs.Quarterly_Lake_Sampling <-c(
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
unit.defs.Quarterly_Metals <-c(
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
unit.defs.Sample_Names <-c(
  'Site_ID',
  'Site_Number',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym')

# sample_summary
unit.defs.sample_summary <-c(
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
unit.defs.Sucralose <-c(
  'ID',
  'Site_Name',
  'Site_Location',
  'Cluster_Name',
  'Site_Acronym',
  'Date',
  'Sucralose')

# wtp_data
unit.defs.wtp_data <-c(
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

# custom units ----

nominalMonth <- eml_define_unit(id = "nominalMonth",
                                parentSI = "dimensionless",
                                unitType = "dimensionless",
                                multiplierToSI = "NA",
                                description = "month in which sample was collected")

microsiemensPerCentimeter <- eml_define_unit(id = "microsiemensPerCentimeter",
                                             parentSI = "siemen",
                                             unitType = "conductance",
                                             multiplierToSI = "0.0001",
                                             description = "microsiemensPerCentimeter")


# this works but not sure about the multiplierToSI, set to NA (text, not object) as the package requires that input but it it not clear that will be valid
# note that you can create a list of multiple custom units and call them in eml_write