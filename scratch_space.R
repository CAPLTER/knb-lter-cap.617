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


########### single dataframe ##########

fluor <- as.data.frame(data[1])

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

# not necessary if dates function already run
# frame$Month <- as.POSIXct(frame$Month, format="%Y/%m/%d")
# frame$Month <- format(frame$Month, format="%m")

# column defs for 3D fluoresence (aka fluor in dataframe)
col.defs <- c(
  'Record number',
  'Site number or name',
  'Month sample was collected',
  'Excitation A wavelength',
  'Emission A wavelength',
  'Fluorescence A',
  'Excitation B wavelength',
  'Emission B wavelength',
  'Fluorescence B')

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
# note that you can create a list of multiple custom units and call them in eml_write

eml_write(fluor,
          col.defs = col.defs,
          unit.defs = unit.defs,
          custom_units = c(unit),
          file = "~/Desktop/fluor2.xml",
          contact = "Carl Boettiger <cboettig@ropensci.org>")

