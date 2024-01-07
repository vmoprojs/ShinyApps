rm(list = ls())

# Libraries:
library(impute)
library(vegan)
library(rio)
library(shiny)
library(plotly)
library(networkD3)
library(cluster)
library(DT)
library(dplyr)
library(plyr)
require(shinydashboard)
require(ggplot2)
require(highcharter) #to plot amazing time series plots

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
validate_args <- function(name, lstargs) {
  lstargsnn <- lstargs[which(names(lstargs) == "")]
  lenlst <- length(lstargsnn)
  
  if (lenlst != 0) {
    chrargs <- lstargsnn %>%
      unlist() %>%
      as.character()
    
    chrargs <- paste0("'", chrargs, "'", collapse = ", ")
    
    txt <- ifelse(lenlst == 1, " is ", "s are ")
    
    stop(chrargs, " argument", txt, "not named in ", paste0("hc_", name),
         call. = FALSE
    )
  }
}
hc_add_series.default <- function(hc, ...) {
  
  # assertthat::assert_that(is.highchart(hc))
  
  if (getOption("highcharter.verbose")) {
    message("hc_add_series.default")
  }
  
  validate_args("add_series", eval(substitute(alist(...))))
  
  hc$x$hc_opts$series <- append(hc$x$hc_opts$series, list(list(...)))
  
  hc
}
fix_map_name1 <- function(x = "custom/world") {
  x <- stringr::str_replace(x, "\\.js$", "")
  x <- stringr::str_replace(x, "https://code\\.highcharts\\.com/mapdata/", "")
  x <- sprintf("%s.js", x)
  x
}
download_map_data1 <- function (url = "custom/world.js", showinfo = FALSE, quiet = FALSE) 
{
  url = "custom/world.js";showinfo = FALSE;quiet = FALSE
  
  url <- sprintf("https://code.highcharts.com/mapdata/%s", 
                 fix_map_name1(url))
  tmpfile <- tempfile(fileext = ".js")
  download.file(url, tmpfile, quiet = quiet)
  mapdata <- readLines(tmpfile, warn = FALSE, encoding = "UTF-8")
  mapdata[1] <- gsub(".* = ", "", mapdata[1])
  
  mapdata <- paste(mapdata, collapse = "\n")
  mapdata <- gsub(";", "", mapdata[1])
  mapdata <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
  if (showinfo) {
    glimpse(get_data_from_map(mapdata))
  }
  mapdata
}


hcmap1 <- function (map = "custom/world", download_map_data = getOption("highcharter.download_map_data"), 
          data = NULL, value = NULL, joinBy = NULL, ...) 
{
  map <- fix_map_name1(map)
  hc <- highchart(type = "map")
  if (download_map_data) {
    mapdata <- download_map_data1(map)
  }
  else {
    dep <- htmlDependency(name = basename(map), version = "0.1.0", 
                          src = c(href = "https://code.highcharts.com/mapdata"), 
                          script = map)
    hc$dependencies <- c(hc$dependencies, list(dep))
    mapdata <- JS(sprintf("Highcharts.maps['%s']", str_replace(map, 
                                                               "\\.js$", "")))
  }
  if (is.null(data)) {
    hc <- hc %>% hc_add_series.default(mapData = mapdata, 
                                       ...)
  }
  else {
    data <- dplyr::rename(data, `:=`(value, value))
    hc <- hc %>% hc_add_series.default(mapData = mapdata, 
                                       data = list_parse(data), joinBy = joinBy, ...) %>% 
      hc_colorAxis(auxpar = NULL)
  }
  hc %>% hc_credits(enabled = TRUE)
}

# usage
packages <- c("rio", "vegan","plotly","shiny","networkD3","cluster")
ipak(packages)

# if(!"impute"%in%installed.packages()[, "Package"])
# {
#   install.packages("impute",repos = "https://bioconductor.org/packages/3.8/bioc")
# }

# source("app.R")
options(repos = BiocManager::repositories())
getOption("repos")


# Auxiliar functions:

# Import data:

# dataset <- import("data/ctryCodes.xlsx")

# pw = rio::import("d:/Users_info/MORALESV/My Documents/Proyectos/Varios/AppClus/App/data/pwt91.xlsx",which = "Data")




indicador <- c("Real GDP, employment and population levels",
               "Current price GDP, capital and TFP",
               "National accounts-based variables",
               "Exchange rates and GDP price levels",
               "Data information variables",
               "Shares in CGDPo",
               "Price levels, expenditure categories and capital")

# ctcode = c("ABW" ,"AGO", "AIA", "ALB" ,"ARE", "ARG", "ARM","ATG", "AUS" ,"AUT", "AZE", "BDI" ,"BEL", "BEN", "BFA"
#            ,"BGD", "BGR", "BHR", "BHS" ,"BIH" ,"BLR", "BLZ", "BMU", "BOL" ,"BRA", "BRB", "BRN", "BTN", "BWA", "CAF"
#            ,"CAN" ,"CHE", "CHL" ,"CHN" ,"CIV", "CMR" ,"COD" ,"COG", "COL", "COM" ,"CPV" ,"CRI" ,"CUW", "CYM", "CYP"
#            ,"CZE" ,"DEU", "DJI" ,"DMA", "DNK" ,"DOM", "DZA" ,"ECU", "EGY" ,"ESP" ,"EST", "ETH", "FIN", "FJI", "FRA"
#            , "GAB" ,"GBR" ,"GEO" ,"GHA", "GIN", "GMB" ,"GNB" ,"GNQ","GRC", "GRD" ,"GTM", "HKG" ,"HND" ,"HRV" ,"HTI"
#            ,"HUN", "IDN" ,"IND" ,"IRL", "IRN", "IRQ" ,"ISL" ,"ISR" ,"ITA", "JAM", "JOR" ,"JPN" ,"KAZ", "KEN", "KGZ"
#            ,"KHM" ,"KNA", "KOR" ,"KWT", "LAO" ,"LBN", "LBR" ,"LCA" ,"LKA" ,"LSO" ,"LTU" ,"LUX", "LVA" ,"MAC" ,"MAR"
#            ,"MDA" ,"MDG" ,"MDV", "MEX" ,"MKD" ,"MLI" ,"MLT", "MMR", "MNE" ,"MNG", "MOZ", "MRT", "MSR", "MUS" ,"MWI"
#            ,"MYS" ,"NAM" ,"NER" ,"NGA","NIC" ,"NLD", "NOR", "NPL", "NZL", "OMN", "PAK" ,"PAN", "PER" ,"PHL" ,"POL"
#            ,"PRT" ,"PRY" ,"PSE" ,"QAT" ,"ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP" ,"SLE", "SLV" ,"SRB", "STP"
#            ,"SUR" ,"SVK", "SVN" ,"SWE", "SWZ", "SXM", "SYC", "SYR" ,"TCA" ,"TCD", "TGO" ,"THA" ,"TJK" ,"TKM", "TTO"
#            , "TUN" ,"TUR" ,"TWN", "TZA" ,"UGA", "UKR" ,"URY" ,"USA" ,"UZB" ,"VCT" ,"VEN", "VGB", "VNM", "YEM" ,"ZAF"
#            ,"ZMB", "ZWE")

dataset = data.frame(countrycode = c(	"MAR",	"ABW",	"AGO",	"AIA",	"ALB",	"ARE",	"ARG",	"ARM",	"ATG",	"AUS",	"AUT",	"AZE",	"BDI",	"BEL",	"BEN",	"BFA",	"BGD",	"BGR",	"BHR",	"BHS",	"BIH",	"BLR",	"BLZ",	"BMU",	"BOL",	"BRA",	"BRB",	"BRN",	"BTN",	"BWA",	"CAF",	"CAN",	"CHE",	"CHL",	"CHN",	"CIV",	"CMR",	"COD",	"COG",	"COL",	"COM",	"CPV",	"CRI",	"CUW",	"CYM",	"CYP",	"CZE",	"DEU",	"DJI",	"DMA",	"DNK",	"DOM",	"DZA",	"ECU",	"EGY",	"ESP",	"EST",	"ETH",	"FIN",	"FJI",	"FRA",	"GAB",	"GBR",	"GEO",	"GHA",	"GIN",	"GMB",	"GNB",	"GNQ",	"GRC",	"GRD",	"GTM",	"HKG",	"HND",	"HRV",	"HTI",	"HUN",	"IDN",	"IND",	"IRL",	"IRN",	"IRQ",	"ISL",	"ISR",	"ITA",	"JAM",	"JOR",	"JPN",	"KAZ",	"KEN",	"KGZ",	"KHM",	"KNA",	"KOR",	"KWT",	"LAO",	"LBN",	"LBR",	"LCA",	"LKA",	"LSO",	"LTU",	"LUX",	"LVA",	"MAC",	"MDA",	"MDG",	"MDV",	"MEX",	"MKD",	"MLI",	"MLT",	"MMR",	"MNE",	"MNG",	"MOZ",	"MRT",	"MSR",	"MUS",	"MWI",	"MYS",	"NAM",	"NER",	"NGA",	"NIC",	"NLD",	"NOR",	"NPL",	"NZL",	"OMN",	"PAK",	"PAN",	"PER",	"PHL",	"POL",	"PRT",	"PRY",	"PSE",	"QAT",	"ROU",	"RUS",	"RWA",	"SAU",	"SDN",	"SEN",	"SGP",	"SLE",	"SLV",	"SRB",	"STP",	"SUR",	"SVK",	"SVN",	"SWE",	"SWZ",	"SXM",	"SYC",	"SYR",	"TCA",	"TCD",	"TGO",	"THA",	"TJK",	"TKM",	"TTO",	"TUN",	"TUR",	"TWN",	"TZA",	"UGA",	"UKR",	"URY",	"USA",	"UZB",	"VCT",	"VEN",	"VGB",	"VNM",	"YEM",	"ZAF",	"ZMB",	"ZWE"),
countryName = c(	"Morocco",	"Aruba",	"Angola",	"Anguilla",	"Albania",	"United Arab Emirates",	"Argentina",	"Armenia",	"Antigua and Barbuda",	"Australia",	"Austria",	"Azerbaijan",	"Burundi",	"Belgium",	"Benin",	"Burkina Faso",	"Bangladesh",	"Bulgaria",	"Bahrain",	"Bahamas",	"Bosnia and Herzegovina",	"Belarus",	"Belize",	"Bermuda",	"Bolivia (Plurinational State of)",	"Brazil",	"Barbados",	"Brunei Darussalam",	"Bhutan",	"Botswana",	"Central African Republic",	"Canada",	"Switzerland",	"Chile",	"China",	"C?te d'Ivoire",	"Cameroon",	"D.R. of the Congo",	"Congo",	"Colombia",	"Comoros",	"Cabo Verde",	"Costa Rica",	"Curazao",	"Cayman Islands",	"Cyprus",	"Czech Republic",	"Germany",	"Djibouti",	"Dominica",	"Denmark",	"Dominican Republic",	"Algeria",	"Ecuador",	"Egypt",	"Spain",	"Estonia",	"Ethiopia",	"Finland",	"Fiji",	"France",	"Gabon",	"United Kingdom",	"Georgia",	"Ghana",	"Guinea",	"Gambia",	"Guinea-Bissau",	"Equatorial Guinea",	"Greece",	"Grenada",	"Guatemala",	"China, Hong Kong SAR",	"Honduras",	"Croatia",	"Haiti",	"Hungary",	"Indonesia",	"India",	"Ireland",	"Iran (Islamic Republic of)",	"Iraq",	"Iceland",	"Israel",	"Italy",	"Jamaica",	"Jordan",	"Japan",	"Kazakhstan",	"Kenya",	"Kyrgyzstan",	"Cambodia",	"Saint Kitts and Nevis",	"Republic of Korea",	"Kuwait",	"Lao People's DR",	"Lebanon",	"Liberia",	"Saint Lucia",	"Sri Lanka",	"Lesotho",	"Lithuania",	"Luxembourg",	"Latvia",	"China, Macao SAR",	"Republic of Moldova",	"Madagascar",	"Maldives",	"Mexico",	"North Macedonia",	"Mali",	"Malta",	"Myanmar",	"Montenegro",	"Mongolia",	"Mozambique",	"Mauritania",	"Montserrat",	"Mauritius",	"Malawi",	"Malaysia",	"Namibia",	"Niger",	"Nigeria",	"Nicaragua",	"Netherlands",	"Norway",	"Nepal",	"New Zealand",	"Oman",	"Pakistan",	"Panama",	"Peru",	"Philippines",	"Poland",	"Portugal",	"Paraguay",	"State of Palestine",	"Qatar",	"Romania",	"Russian Federation",	"Rwanda",	"Saudi Arabia",	"Sudan",	"Senegal",	"Singapore",	"Sierra Leone",	"El Salvador",	"Serbia",	"Sao Tome and Principe",	"Suriname",	"Slovakia",	"Slovenia",	"Sweden",	"Eswatini",	"Sint Maarten (Dutch part)",	"Seychelles",	"Syrian Arab Republic",	"Turks and Caicos Islands",	"Chad",	"Togo",	"Thailand",	"Tajikistan",	"Turkmenistan",	"Trinidad and Tobago",	"Tunisia",	"Turkey",	"Taiwan",	"U.R. of Tanzania: Mainland",	"Uganda",	"Ukraine",	"Uruguay",	"United States",	"Uzbekistan",	"St. Vincent and the Grenadines",	"Venezuela (Bolivarian Republic of)",	"British Virgin Islands",	"Viet Nam",	"Yemen",	"South Africa",	"Zambia",	"Zimbabwe"),
country2 = c(	"MA",	"AW",	"AO",	"AI",	"AL",	"AE",	"AR",	"AM",	"AG",	"AU",	"AT",	"AZ",	"BI",	"BE",	"BJ",	"BF",	"BD",	"BG",	"BH",	"BS",	"BA",	"BY",	"BZ",	"BM",	"BO",	"BR",	"BB",	"BN",	"BT",	"BW",	"CF",	"CA",	"CH",	"CL",	"CN",	"CI",	"CM",	"CD",	"CG",	"CO",	"KM",	"CV",	"CR",	"CW",	"KY",	"CY",	"CZ",	"DE",	"DJ",	"DM",	"DK",	"DO",	"DZ",	"EC",	"EG",	"ES",	"EE",	"ET",	"FI",	"FJ",	"FR",	"GA",	"GB",	"GE",	"GH",	"GN",	"GM",	"GW",	"GQ",	"GR",	"GD",	"GT",	"HK",	"HN",	"HR",	"HT",	"HU",	"ID",	"IN",	"IE",	"IR",	"IQ",	"IS",	"IL",	"IT",	"JM",	"JO",	"JP",	"KZ",	"KE",	"KG",	"KH",	"KN",	"KR",	"KW",	"LA",	"LB",	"LR",	"LC",	"LK",	"LS",	"LT",	"LU",	"LV",	"MO",	"MD",	"MG",	"MV",	"MX",	"MK",	"ML",	"MT",	"MM",	"ME",	"MN",	"MZ",	"MR",	"MS",	"MU",	"MW",	"MY",	"NA",	"NE",	"NG",	"NI",	"NL",	"NO",	"NP",	"NZ",	"OM",	"PK",	"PA",	"PE",	"PH",	"PL",	"PT",	"PY",	"PS",	"QA",	"RO",	"RU",	"RW",	"SA",	"SD",	"SN",	"SG",	"SL",	"SV",	"RS",	"ST",	"SR",	"SK",	"SI",	"SE",	"SZ",	"SX",	"SC",	"SY",	"TC",	"TD",	"TG",	"TH",	"TJ",	"TM",	"TT",	"TN",	"TR",	"TW",	"TZ",	"UG",	"UA",	"UY",	"US",	"UZ",	"VC",	"VE",	"VG",	"VN",	"YE",	"ZA",	"ZM",	"ZW"))




ctcode = dataset$countrycode

yr = 1990:2019

indIndiv <- c("rgdpe","rgdpo","pop","emp","avh","hc",
                    "ccon","cda","cgdpe","cgdpo","cn","ck","ctfp","cwtfp",
                    "rgdpna","rconna","rdana","rnna","rkna","rtfpna","rwtfpna","labsh","irr","delta",
                    "xr","pl_con","pl_da","pl_gdpo",
                    "i_cig","i_xm","i_xr","i_outlier","i_irr","cor_exp","statcap",
                    "csh_c","csh_i","csh_g","csh_x","csh_m","csh_r",
                    "pl_c","pl_i","pl_g","pl_x","pl_m","pl_n","pl_k")

