

# List of high-income country iso2 codes
hic_countries <- c(
  # Western Europe
  "AT", "BE", "CH", "CY", "DE", "DK", "ES", "FI", "FR", "GB", 
  "GR", "IE", "IS", "IT", "LI", "LU", "MT", "NL", "NO", "PT", "SE",
  
  # Eastern Europe
  "CZ", "EE", "LT", "LV", "PL", "SI", "SK",
  
  # North America
  "CA", "US",
  
  # Asia-Pacific
  "AU", "BN", "HK", "JP", "KR", "NZ", "SG", "TW",
  
  # Middle East
  "AE", "BH", "IL", "KW", "OM", "QA", "SA",
  
  # Caribbean & Latin America
  "AW", "BB", "BS", "CL", "CW", "KN", "PA", "PR", "TC", "TT", "UY", "VG",
  
  # Other
  "AD", "GI", "MC", "SM"
)


hiceurope=c(  "AT", "BE", "CH", "CY", "DE", "DK", "ES", "FI", "FR", "GB", 
             "GR", "IE", "IS", "IT", "LI", "LU", "MT", "NL", "NO", "PT", "SE",
             
             # Eastern Europe
             "CZ", "EE", "LT", "LV", "PL", "SI", "SK")


################# from KEVCE
all_countries <- sort(unique(na.omit(countrycode::codelist$iso2c)))
lmics <- c("AF","AL","DZ","AO","AR","AM","AZ","BD","BJ","BO","BA","BW","BR","BG",
           "BF","BI","KH","CM","CV","CF","TD","CL","CN","CO","KM","CG","CR","CI",
           "CU","DJ","DM","DO","EC","EG","SV","GQ","ER","ET","FJ","GA","GM","GE",
           "GH","GT","GN","GW","GY","HT","HN","IN","ID","IR","IQ","JM","JO","KZ",
           "KE","KI","KP","KG","LA","LB","LS","LR","LY","MG","MW","MY","MV","ML",
           "MR","MU","MX","MD","MN","ME","MA","MZ","MM","NA","NP","NI","NE","NG",
           "MK","PK","PW","PA","PG","PY","PE","PH","RW","WS","ST","SN","RS","SC",
           "SL","SB","SO","ZA","LK","SD","SR","SY","TJ","TZ","TH","TL","TG","TO",
           "TN","TR","TM","TV","UG","UA","UZ","VU","VE","VN","YE","ZM","ZW")

lmics_excl_china <- setdiff(lmics, "CN")
eu_countries <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR",
                  "HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK",
                  "SI","ES","SE")
hic <- setdiff(all_countries, lmics)



hic=high_income_iso2 <- c(
  "AD", "AG", "AU", "AT", "BS", "BH", "BB", "BE", "BN", "BG", "CA", "CL",
  "CR", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "GY", "HU",
  "IS", "IE", "IL", "IT", "JP", "KW", "LV", "LI", "LT", "LU", "MT", "MC",
  "NR", "NL", "NZ", "NO", "OM", "PW", "PA", "PL", "PT", "QA", "RO", "RU",
  "KN", "SM", "SA", "SC", "SG", "SK", "SI", "KR", "ES", "SE", "CH", "TT",
  "AE", "GB", "US", "UY",
  # Territories and non-UN members
  "AS", "AW", "BM", "VG", "KY", "JE", "CW", "FO", "PF", "GI", "GL", "GU",
  "HK", "IM", "MO", "NC", "MP", "PR", "MF", "SX", "TW", "TC", "VI"
)


# Create dataframe
country_income <- data.frame(
  iso2 = all_countries,
  HIC = as.integer(all_countries %in% hic)
)





