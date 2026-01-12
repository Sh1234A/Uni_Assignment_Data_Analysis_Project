# ASSIGNMENT DATA CLEANING

#Conterbuters:

# BISMA ALTAF 
# CHOMBA SERENA MBEWE 
# MARIYAM MAYAN SHAMEEZ 
# SHADHA ALI ALAWI AL-SALAMI 

library(dplyr)
library(stringr)

fileUrl = "C:\\Users\\user\\Desktop\\uni\\Y2S1\\Programming for Data Analysis\\hackingData.csv"
df = read.csv(fileUrl)
df

-------------------------------------------------------------------
# LOSS
-------------------------------------------------------------------

str(df)
summary(df$Loss)
colSums(is.na(df)) # NA's in loss: 33739

# impute using median
df$Loss = replace(df$Loss, is.na(df$Loss), median(df$Loss, na.rm = TRUE))


-------------------------------------------------------------------
# RANSOM
-------------------------------------------------------------------

#Assess the data type of all columns
str(df)  #structure function, #212095 observations(rows), 12 variables

#Assess summary
summary(df) #to replace all Not Applicable values that are available

#Asses how many values are Not Applicable in the data set
colSums(is.na(df))
View(df)

View(df)
summary(df$ransom)
str(df)
colSums(is.na(df))

#impute the 'Not Applicable' in the ransom column
colSums(is.na(df))

#impute it using median
df$Ransom = replace(df$Ransom, is.na(df$Ransom), median(df$Ransom, na.rm = TRUE))
options(scipen = 999)
summary(df$Ransom)

#Replace illogical values
# Assume values less than 100 and more than 3000 are illogical, hence replace
df$Ransom = replace(df$Ransom, df$Ransom < 100,100) 
df$Ransom = replace(df$Ransom,df$Ransom > 3000, 3000)
summary(df$Ransom)

----------------------------------------------------------------
# COUNTRY
----------------------------------------------------------------
  
# to check the data type of all columns
str(df)

# check summary
summary(df)

# check how many nas in dataset
colSums(is.na(df))
View(df)

# to view the df
View(df)

# to lowercase the entries inside the columns
df$Country = tolower(df$Country)
df$Country

# convert spaces and blanks to unknown
df$Country = replace(df$Country, df$Country == "", "unknown")
colSums(is.na(df))

# to check unique entries
unique(df$Country)

# to check frequency of each one
as.data.frame(table(df$Country))

# standardize names
df$Country = replace(df$Country, df$Country == "united state", "united states")

df$Country = replace(df$Country, df$Country == "united kingd", "united kingdom")

df$Country = replace(df$Country, df$Country == "bruneidarussalam", "brunei darussalam")

df$Country = replace(df$Country, df$Country == "burkinafaso", "burkina faso")

df$Country = replace(df$Country, df$Country == "capeverde", "cape verde")

df$Country = replace(df$Country, df$Country == "costarica", "costa rica")

df$Country = replace(df$Country, df$Country == "czech republ", "czech republic")

df$Country = replace(df$Country, df$Country == "elsalvador", "el salvador")

df$Country = replace(df$Country, df$Country == "iran, islami", "iran")

df$Country = replace(df$Country, df$Country == "korea, repub", "korea")

df$Country = replace(df$Country, df$Country == "libyanarabjamahiriya", "libyan arab jamahiriya")

df$Country = replace(df$Country, df$Country == "macedonia, t", "macedonia")

df$Country = replace(df$Country, df$Country == "moldova, rep", "moldova")

df$Country = replace(df$Country, df$Country == "netherlands”", "netherlands")

df$Country = replace(df$Country, df$Country == "new zealand", "new zealand")

df$Country = replace(df$Country, df$Country == "newzealand", "new zealand")

df$Country = replace(df$Country, df$Country == "nigeria", "nigeria")

df$Country = replace(df$Country, df$Country == "russian fede", "russian federation")

df$Country = replace(df$Country, df$Country == "saudiarabia", "saudi arabia")

df$Country = replace(df$Country, df$Country == "sri lanka", "srilanka")

df$Country = replace(df$Country, df$Country == "syrian arab", "syrian arab republic")

df$Country = replace(df$Country, df$Country == "taiwan, prov", "taiwan")

df$Country = replace(df$Country, df$Country == "united arabemirates", "united arab emirates")

df$Country = replace(df$Country, df$Country == "viet nam", "vietnam")

df$Country = gsub("\"", "", df$Country)

# remove incorrect data
df = df %>%
  filter(!(Country %in% c("africa", "americansamoa", "anonymous proxy",
                          "ascensionisland", "asia", "asia/pacific region",     
                          "bermuda", "caledonia", "cayman islan",
                          "cayman islands", "cook islands", "easteuro",    
                          "europe", "european uni", "european union",    
                          "faroe islands", "fiji", "french polyn",     
                          "french polynesia", "greenland", "guernsey",
                          "macau", "middleeast", "montserrat", 
                          "netherlandsantilles", "new caledoni", 
                          "new caledonia", "niue", "norfolkisland",     
                          "northern mariana islands", "oseania", 
                          "puerto rico", "puertorico", "saintkittsnevis",     
                          "satellite provider", "southamerica", 
                          "turksandcaicosislands", "vaticancitystate", 
                          "virgin islan", "virgin islands",    
                          "virginislands(british)", "virginislands(u.s.)",
                          "westeuro ")))

# convert to titlecase
df$Country = str_to_title(df$Country)

# group countries with low frequencies as "Other"
df$Country[!df$Country %in% c("USA", "Canada", "UK", "Germany", "France", "Japan", "Australia", 
                              "Brazil", "China", "Argentina", "Chile", 
                              "Netherlands", "South Korea", "Spain", "Italy", "Poland", 
                              "United States", "United Kingdom", "Indonesia", "Unknown")] <- "Other"



---------------------------------------------------------------------------
# OS
---------------------------------------------------------------------------

# view data
View(df)

# convert string to lowercase
df$OS = tolower(df$OS)
df$OS
  
# conversion

colSums(is.na(df)) # outputs 9795

# check unique entries
unique(df$OS)
as.data.frame(table(df$OS))

# convert "unknown" and spaces to NA
df$OS = replace (df$OS, df$OS == "unknown", NA)
df$OS = replace (df$OS, df$OS == "", NA)
# assume "unkno" is a misspelling of "unknown" and replace it with NA
df$OS = gsub("\\bunkno\\b", NA, df$OS)

# remove incorrect data
df = df %>%
  filter(!grepl("adapter|embedded|wap|switch|gateway|modem|laserjet|router|device|crestron", OS))
  
# standardization
  
# replace entries containing variations of "win" or "windows" in any form with "windows"
df$OS <- ifelse(grepl("win", df$OS), "windows", df$OS)

# replace entries containing variations of "solar" in an form with "solaris"
df$OS <- ifelse(grepl("solar", df$OS), "solaris", df$OS)

# replace entries containing "netbs", "freebsd", "openbsd" in any form with "bsd"
df$OS <- ifelse(grepl("netbs|freebsd|openbsd|bsd|freeb", df$OS), "bsd", df$OS)

# replace entries containing "juniper" in any form with "juniper"
df$OS <- ifelse(grepl("juniper", df$OS), "juniper", df$OS)

# replace entries containing "macos" in any form with "macos"
df$OS <- ifelse(grepl("macos", df$OS), "macos", df$OS)

# replace entries containing "cisco" in any form with "cicso"
df$OS <- ifelse(grepl("cisco", df$OS), "cisco", df$OS)

# replace entries containing "f5" in any form with "f5"
df$OS <- ifelse(grepl("f5", df$OS), "f5", df$OS)

# replace entries containing "unix" in any form with "unix"
df$OS <- ifelse(grepl("unix", df$OS), "unix", df$OS)

  
# group entries with Freq < 30 into "other"
low_freq_OS = c("blue coat sgos 6.x", "cisco", "citri", "compaq tru64", "google android 4.x",
                  "ibm aix 6.x|5.x", "ipxe 1.x", "juniper", "novellnetware", "os2", "ovh/shared",
                  "tru64", "vm")

df$OS = ifelse(df$OS %in% low_freq_OS, "other", df$OS)

as.data.frame(table(df$OS))

# convert to titlecase  
df$OS = str_to_title(df$OS)

# impute na's with mode according to country
df = df %>%
  group_by(Country) %>%
  mutate(OS = ifelse(is.na(OS), names(which.max(table(OS))), OS)) %>%
  ungroup()

--------------------------------------------------------------------------------
# ENCODING
--------------------------------------------------------------------------------

# Check unique values in Encoding column before cleaning
print(unique(df$Encoding))

# Standardizing Encoding
df$Encoding <- trimws(df$Encoding)

df$Encoding = tolower(df$Encoding)

df$Encoding <- str_replace_all(df$Encoding, "^windows-\\d+", "Windows") # Standardize Windows
df$Encoding <- str_replace_all(df$Encoding, "^iso-8859-\\d+", "ISO")    # Standardize ISO
df$Encoding <- str_replace_all(df$Encoding, "^euc-\\w+", "EUC")         # Standardize EUC
df$Encoding <- str_replace_all(df$Encoding, "^utf-\\d+[a-z]*", "UTF")   # Standardize UTF
df$Encoding <- str_replace_all(df$Encoding, "^(ascii|us-ascii)", "ASCII")

as.data.frame(table(df$Encoding))

# Replacing variations and ensuring proper capitalization
df$Encoding <- dplyr::case_when(
  df$Encoding %in% c("us-ascii", "usascii") ~ "US-ASCII", 
  df$Encoding %in% c("gb2312","GB2312") ~ "GB2312", 
  df$Encoding %in% c("tis-620", "tis620") ~ "TIS-620", 
  df$Encoding %in% c("asmo-708", "asmo708") ~ "ASMO-708",
  df$Encoding %in% c("liteSpeed") ~ "LiteSpeed",
  df$Encoding %in% c("ascii") ~ "ASCII",
  df$Encoding %in% c("shift_jis", "shiftjis") ~ "Shift_jis",  
  df$Encoding %in% c("big5", "Big5") ~ "Big5",
  df$Encoding %in% c("null","n", "") ~ "NULL",
  df$Encoding %in% c("koi8-r", "koi8r") ~ "KOI8-R",
  df$Encoding %in% c("ibm855") ~ "IBM855",
  df$Encoding %in% c("litespeed") ~ "Litespeed",
  TRUE ~ stringr::str_replace(df$Encoding, "^LiteSpeed", "LiteSpeed")
)

# Group entries with low freq as "Other"
df$Encoding[df$Encoding %in% c("ASMO-708", "Big5", "EUC", "IBM855", "KOI8-R", "Shift_jis", "TIS-620", "GB2312")] <- "Other"

-----------------------------------------------------------------------
# write to csv file
write.csv(df, file = "hackingData_cleaned.csv")