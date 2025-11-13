

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

# Get all unique iso2 codes (you can adjust this list as needed)
all_countries <- c(
  "AD", "AE", "AF", "AG", "AI", "AL", "AM", "AO", "AQ", "AR", "AS", "AT",
  "AU", "AW", "AX", "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI",
  "BJ", "BL", "BM", "BN", "BO", "BQ", "BR", "BS", "BT", "BV", "BW", "BY",
  "BZ", "CA", "CC", "CD", "CF", "CG", "CH", "CI", "CK", "CL", "CM", "CN",
  "CO", "CR", "CU", "CV", "CW", "CX", "CY", "CZ", "DE", "DJ", "DK", "DM",
  "DO", "DZ", "EC", "EE", "EG", "EH", "ER", "ES", "ET", "FI", "FJ", "FK",
  "FM", "FO", "FR", "GA", "GB", "GD", "GE", "GF", "GG", "GH", "GI", "GL",
  "GM", "GN", "GP", "GQ", "GR", "GS", "GT", "GU", "GW", "GY", "HK", "HM",
  "HN", "HR", "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IO", "IQ", "IR",
  "IS", "IT", "JE", "JM", "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN",
  "KP", "KR", "KW", "KY", "KZ", "LA", "LB", "LC", "LI", "LK", "LR", "LS",
  "LT", "LU", "LV", "LY", "MA", "MC", "MD", "ME", "MF", "MG", "MH", "MK",
  "ML", "MM", "MN", "MO", "MP", "MQ", "MR", "MS", "MT", "MU", "MV", "MW",
  "MX", "MY", "MZ", "NA", "NC", "NE", "NF", "NG", "NI", "NL", "NO", "NP",
  "NR", "NU", "NZ", "OM", "PA", "PE", "PF", "PG", "PH", "PK", "PL", "PM",
  "PN", "PR", "PS", "PT", "PW", "PY", "QA", "RE", "RO", "RS", "RU", "RW",
  "SA", "SB", "SC", "SD", "SE", "SG", "SH", "SI", "SJ", "SK", "SL", "SM",
  "SN", "SO", "SR", "SS", "ST", "SV", "SX", "SY", "SZ", "TC", "TD", "TF",
  "TG", "TH", "TJ", "TK", "TL", "TM", "TN", "TO", "TR", "TT", "TV", "TW",
  "TZ", "UA", "UG", "UM", "US", "UY", "UZ", "VA", "VC", "VE", "VG", "VI",
  "VN", "VU", "WF", "WS", "YE", "YT", "ZA", "ZM", "ZW"
)

# Create dataframe
country_income <- data.frame(
  iso2 = all_countries,
  HIC = as.integer(all_countries %in% hic_countries)
)



