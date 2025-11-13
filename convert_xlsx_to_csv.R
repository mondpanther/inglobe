#!/usr/bin/env Rscript
# Script to load examples.xlsx, add capital city coordinates for country code columns,
# and save as CSV

# Load required libraries
library(readxl)
library(dplyr)

# Capital city coordinates database (ISO2 code -> longitude, latitude)
# Comprehensive list of world capitals
capital_coords <- list(
  # Americas
  AG = c(-61.8456, 17.1274),    # Saint John's
  AR = c(-58.3816, -34.6037),   # Buenos Aires
  AW = c(-70.0167, 12.5167),    # Oranjestad
  BB = c(-59.6167, 13.1),       # Bridgetown
  BO = c(-68.1193, -16.4897),   # La Paz
  BR = c(-47.8825, -15.7942),   # Brasília
  BS = c(-77.3963, 25.0443),    # Nassau
  BZ = c(-88.1962, 17.2510),    # Belmopan
  CA = c(-75.6972, 45.4215),    # Ottawa
  CL = c(-70.6693, -33.4489),   # Santiago
  CO = c(-74.0721, 4.7110),     # Bogotá
  CR = c(-84.0907, 9.9281),     # San José
  CU = c(-82.3666, 23.1136),    # Havana
  CW = c(-68.9335, 12.1224),    # Willemstad
  DM = c(-61.3883, 15.3092),    # Roseau
  DO = c(-69.9312, 18.4861),    # Santo Domingo
  EC = c(-78.4678, -0.1807),    # Quito
  GD = c(-61.7490, 12.0561),    # St. George's
  GT = c(-90.5069, 14.6349),    # Guatemala City
  GY = c(-58.1551, 6.8013),     # Georgetown
  HN = c(-87.2068, 14.0723),    # Tegucigalpa
  HT = c(-72.3074, 18.5944),    # Port-au-Prince
  JM = c(-76.7935, 17.9714),    # Kingston
  KN = c(-62.7175, 17.3578),    # Basseterre
  KY = c(-81.3857, 19.2866),    # George Town
  LC = c(-60.9833, 14.0167),    # Castries
  MX = c(-99.1332, 19.4326),    # Mexico City
  NI = c(-86.2362, 12.1150),    # Managua
  PA = c(-79.5199, 8.9824),     # Panama City
  PE = c(-77.0428, -12.0464),   # Lima
  PR = c(-66.1057, 18.4655),    # San Juan
  PY = c(-57.5759, -25.2637),   # Asunción
  SR = c(-55.2038, 5.8520),     # Paramaribo
  SV = c(-89.2182, 13.6929),    # San Salvador
  TC = c(-71.1423, 21.4612),    # Cockburn Town
  TT = c(-61.5089, 10.6549),    # Port of Spain
  US = c(-77.0369, 38.9072),    # Washington DC
  UY = c(-56.1645, -34.9011),   # Montevideo
  VC = c(-61.2248, 13.1600),    # Kingstown
  VE = c(-66.8792, 10.4806),    # Caracas
  VG = c(-64.6208, 18.4207),    # Road Town
  VI = c(-64.8963, 18.3358),    # Charlotte Amalie

  # Europe
  AD = c(1.5218, 42.5063),      # Andorra la Vella
  AL = c(19.8187, 41.3275),     # Tirana
  AT = c(16.3738, 48.2082),     # Vienna
  BA = c(18.4131, 43.8563),     # Sarajevo
  BE = c(4.3517, 50.8503),      # Brussels
  BG = c(23.3219, 42.6977),     # Sofia
  BY = c(27.5615, 53.9045),     # Minsk
  CH = c(7.4474, 46.9480),      # Bern
  CY = c(33.3823, 35.1856),     # Nicosia
  CZ = c(14.4378, 50.0755),     # Prague
  DE = c(13.4050, 52.5200),     # Berlin
  DK = c(12.5683, 55.6761),     # Copenhagen
  EE = c(24.7535, 59.4370),     # Tallinn
  ES = c(-3.7038, 40.4168),     # Madrid
  FI = c(24.9384, 60.1695),     # Helsinki
  FR = c(2.3522, 48.8566),      # Paris
  GB = c(-0.1276, 51.5074),     # London
  GE = c(44.8271, 41.7151),     # Tbilisi
  GI = c(-5.3599, 36.1408),     # Gibraltar
  GR = c(23.7275, 37.9838),     # Athens
  HR = c(15.9819, 45.8150),     # Zagreb
  HU = c(19.0402, 47.4979),     # Budapest
  IE = c(-6.2603, 53.3498),     # Dublin
  IS = c(-21.8174, 64.1265),    # Reykjavik
  IT = c(12.4964, 41.9028),     # Rome
  LI = c(9.5215, 47.1410),      # Vaduz
  LT = c(25.2797, 54.6872),     # Vilnius
  LU = c(6.1296, 49.6116),      # Luxembourg
  LV = c(24.1052, 56.9496),     # Riga
  MC = c(7.4197, 43.7384),      # Monaco
  MD = c(28.8497, 47.0105),     # Chisinau
  ME = c(19.2636, 42.4304),     # Podgorica
  MK = c(21.4314, 41.9973),     # Skopje
  MT = c(14.5146, 35.8989),     # Valletta
  NL = c(4.8952, 52.3702),      # Amsterdam
  NO = c(10.7522, 59.9139),     # Oslo
  PL = c(21.0122, 52.2297),     # Warsaw
  PT = c(-9.1393, 38.7223),     # Lisbon
  RO = c(26.1025, 44.4268),     # Bucharest
  RS = c(20.4489, 44.7866),     # Belgrade
  RU = c(37.6173, 55.7558),     # Moscow
  SE = c(18.0686, 59.3293),     # Stockholm
  SI = c(14.5058, 46.0569),     # Ljubljana
  SK = c(17.1077, 48.1486),     # Bratislava
  SM = c(12.4578, 43.9424),     # San Marino
  UA = c(30.5234, 50.4501),     # Kyiv
  VA = c(12.4534, 41.9029),     # Vatican City

  # Asia
  AE = c(54.3773, 24.4539),     # Abu Dhabi
  AF = c(69.2075, 34.5553),     # Kabul
  AM = c(44.5152, 40.1872),     # Yerevan
  AZ = c(49.8671, 40.4093),     # Baku
  BD = c(90.4125, 23.8103),     # Dhaka
  BH = c(50.5577, 26.0667),     # Manama
  BN = c(114.9480, 4.9031),     # Bandar Seri Begawan
  BT = c(89.6339, 27.4728),     # Thimphu
  CN = c(116.4074, 39.9042),    # Beijing
  CY = c(33.3823, 35.1856),     # Nicosia
  GE = c(44.8271, 41.7151),     # Tbilisi
  HK = c(114.1694, 22.3193),    # Hong Kong
  ID = c(106.8650, -6.2088),    # Jakarta
  IL = c(35.2137, 31.7683),     # Jerusalem
  IN = c(77.2090, 28.6139),     # New Delhi
  IQ = c(44.3661, 33.3152),     # Baghdad
  IR = c(51.3890, 35.6892),     # Tehran
  JO = c(35.9239, 31.9454),     # Amman
  JP = c(139.6503, 35.6762),    # Tokyo
  KG = c(74.5698, 42.8746),     # Bishkek
  KH = c(104.9160, 11.5564),    # Phnom Penh
  KP = c(125.7625, 39.0392),    # Pyongyang
  KR = c(126.9780, 37.5665),    # Seoul
  KW = c(47.9774, 29.3759),     # Kuwait City
  KZ = c(71.4704, 51.1694),     # Nur-Sultan (Astana)
  LA = c(102.6133, 17.9757),    # Vientiane
  LB = c(35.4951, 33.8886),     # Beirut
  LK = c(79.8612, 6.9271),      # Colombo
  MM = c(96.1561, 16.8661),     # Naypyidaw
  MN = c(106.9057, 47.8864),    # Ulaanbaatar
  MO = c(113.5439, 22.1987),    # Macau
  MV = c(73.5093, 4.1755),      # Malé
  MY = c(101.6869, 3.1390),     # Kuala Lumpur
  NP = c(85.3240, 27.7172),     # Kathmandu
  OM = c(58.4059, 23.5880),     # Muscat
  PH = c(120.9842, 14.5995),    # Manila
  PK = c(73.0479, 33.6844),     # Islamabad
  PS = c(35.2033, 31.9522),     # East Jerusalem
  QA = c(51.5310, 25.2854),     # Doha
  SA = c(46.7219, 24.7136),     # Riyadh
  SG = c(103.8198, 1.3521),     # Singapore
  SY = c(36.2765, 33.5138),     # Damascus
  TH = c(100.5018, 13.7563),    # Bangkok
  TJ = c(68.7738, 38.5598),     # Dushanbe
  TL = c(125.7275, -8.5569),    # Dili
  TM = c(58.3261, 37.9601),     # Ashgabat
  TR = c(32.8597, 39.9334),     # Ankara
  TW = c(121.5654, 25.0330),    # Taipei
  UZ = c(69.2401, 41.2995),     # Tashkent
  VN = c(105.8342, 21.0278),    # Hanoi
  YE = c(44.2075, 15.5527),     # Sana'a

  # Oceania
  AU = c(149.1300, -35.2809),   # Canberra
  FJ = c(178.4419, -18.1248),   # Suva
  KI = c(173.0176, 1.3382),     # Tarawa
  MH = c(171.1845, 7.1315),     # Majuro
  NR = c(166.9315, -0.5477),    # Yaren
  NZ = c(174.7633, -41.2865),   # Wellington
  PG = c(147.1803, -9.4438),    # Port Moresby
  PW = c(134.6289, 7.5150),     # Ngerulmud
  SB = c(159.9729, -9.4456),    # Honiara
  TO = c(-175.2018, -21.1393),  # Nuku'alofa
  TV = c(179.1942, -8.5211),    # Funafuti
  VU = c(168.3273, -17.7333),   # Port Vila
  WS = c(-171.7513, -13.8506),  # Apia

  # Africa
  AO = c(13.2343, -8.8383),     # Luanda
  BF = c(-1.5247, 12.3714),     # Ouagadougou
  BI = c(29.3599, -3.3731),     # Gitega
  BJ = c(2.3158, 6.4969),       # Porto-Novo
  BW = c(25.9231, -24.6282),    # Gaborone
  CD = c(15.2663, -4.4419),     # Kinshasa
  CF = c(18.5582, 4.3947),      # Bangui
  CG = c(15.2662, -4.2634),     # Brazzaville
  CI = c(-4.0305, 5.3600),      # Yamoussoukro
  CM = c(11.5021, 3.8480),      # Yaoundé
  CV = c(-23.6051, 14.9330),    # Praia
  DJ = c(43.1450, 11.8251),     # Djibouti
  DZ = c(3.0588, 36.7538),      # Algiers
  EG = c(31.2357, 30.0444),     # Cairo
  ER = c(38.9251, 15.3229),     # Asmara
  ET = c(38.7469, 9.1450),      # Addis Ababa
  GA = c(9.4673, 0.4162),       # Libreville
  GH = c(-0.1870, 5.6037),      # Accra
  GM = c(-16.5917, 13.4549),    # Banjul
  GN = c(-13.7120, 9.6412),     # Conakry
  GQ = c(8.7832, 3.7523),       # Malabo
  GW = c(-15.5932, 11.8037),    # Bissau
  KE = c(36.8219, -1.2921),     # Nairobi
  KM = c(43.2418, -11.7172),    # Moroni
  LR = c(-10.8074, 6.2907),     # Monrovia
  LS = c(27.4833, -29.3167),    # Maseru
  LY = c(13.1913, 32.8872),     # Tripoli
  MA = c(-6.8498, 33.9716),     # Rabat
  MG = c(47.5079, -18.8792),    # Antananarivo
  ML = c(-7.9465, 12.6392),     # Bamako
  MR = c(-15.9785, 18.0735),    # Nouakchott
  MU = c(57.4977, -20.1609),    # Port Louis
  MW = c(33.7874, -13.9626),    # Lilongwe
  MZ = c(32.5713, -25.9655),    # Maputo
  NA = c(17.0658, -22.5597),    # Windhoek
  NE = c(2.1154, 13.5127),      # Niamey
  NG = c(7.4951, 9.0765),       # Abuja
  RW = c(30.0619, -1.9403),     # Kigali
  SC = c(55.4540, -4.6796),     # Victoria
  SD = c(32.5599, 15.5007),     # Khartoum
  SL = c(-13.2317, 8.4657),     # Freetown
  SN = c(-17.4677, 14.7167),    # Dakar
  SO = c(45.3182, 2.0469),      # Mogadishu
  SS = c(31.5825, 4.8594),      # Juba
  ST = c(6.7313, 0.3365),       # São Tomé
  SZ = c(31.1367, -26.3054),    # Mbabane
  TD = c(15.0445, 12.1348),     # N'Djamena
  TG = c(1.2255, 6.1256),       # Lomé
  TN = c(10.1815, 36.8065),     # Tunis
  TZ = c(35.7382, -6.3690),     # Dodoma
  UG = c(32.5825, 0.3476),      # Kampala
  ZA = c(28.2293, -25.7479),    # Pretoria
  ZM = c(28.2871, -15.3875),    # Lusaka
  ZW = c(31.0492, -17.8252)     # Harare
)

# Function to get longitude for a country code
get_longitude <- function(iso2_code) {
  if (is.na(iso2_code) || iso2_code == "" || !(iso2_code %in% names(capital_coords))) {
    return(NA)
  }
  return(capital_coords[[iso2_code]][1])
}

# Function to get latitude for a country code
get_latitude <- function(iso2_code) {
  if (is.na(iso2_code) || iso2_code == "" || !(iso2_code %in% names(capital_coords))) {
    return(NA)
  }
  return(capital_coords[[iso2_code]][2])
}

# Function to check if a column likely contains ISO2 country codes
is_country_code_column <- function(col_values) {
  # Remove NA values
  non_na_values <- na.omit(col_values)

  if (length(non_na_values) == 0) {
    return(FALSE)
  }

  # Check if values are character type
  if (!is.character(non_na_values)) {
    return(FALSE)
  }

  # Check if most values are 2-character uppercase strings
  two_char_upper <- sum(grepl("^[A-Z]{2}$", non_na_values))
  proportion <- two_char_upper / length(non_na_values)

  # If more than 50% of values match ISO2 pattern, consider it a country code column
  return(proportion > 0.5)
}

# Main processing function
convert_xlsx_to_csv_with_coords <- function(input_file = "examples.xlsx",
                                             output_file = "examples_with_coords.csv") {

  cat("Loading", input_file, "...\n")

  # Read the Excel file
  data <- read_excel(input_file)

  cat("Original data has", nrow(data), "rows and", ncol(data), "columns\n")

  # Identify country code columns
  country_code_cols <- c()
  for (col_name in names(data)) {
    if (is_country_code_column(data[[col_name]])) {
      country_code_cols <- c(country_code_cols, col_name)
      cat("Detected country code column:", col_name, "\n")
    }
  }

  if (length(country_code_cols) == 0) {
    cat("Warning: No country code columns detected!\n")
    cat("Saving data without coordinate columns...\n")
  } else {
    # Add longitude and latitude columns for each country code column
    for (col_name in country_code_cols) {
      lon_col_name <- paste0(col_name, "_longitude")
      lat_col_name <- paste0(col_name, "_latitude")

      cat("Adding", lon_col_name, "and", lat_col_name, "\n")

      # Create new columns with coordinates
      data[[lon_col_name]] <- sapply(data[[col_name]], get_longitude)
      data[[lat_col_name]] <- sapply(data[[col_name]], get_latitude)
    }
  }

  # Save to CSV
  cat("Saving to", output_file, "...\n")
  write.csv(data, output_file, row.names = FALSE)

  cat("Done! Output has", nrow(data), "rows and", ncol(data), "columns\n")
  cat("\nFirst few rows:\n")
  print(head(data))

  return(data)
}

# Run the conversion
if (!interactive()) {
  # If running as a script
  result <- convert_xlsx_to_csv_with_coords()
} else {
  # If sourced interactively
  cat("Functions loaded. Call convert_xlsx_to_csv_with_coords() to run.\n")
}
