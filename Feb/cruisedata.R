#STEP 1
dir<-"D:/Postdoc/Dilution Data/Extracted data_KB format/"

library(tidyverse)

# Read all the files and create a FileName column to store filenames

files <- list.files(dir, pattern="\\.xlsx$", full.names = TRUE)
cruisedata <- files %>%
  set_names(.) %>%
  map_df(read_excel, .id = "FileName")

library(tools)
cruisedata$FileName <- basename(file_path_sans_ext(cruisedata$FileName))

pattern <- "(\\d+)(_)(\\d+)(_)(\\w+)(_)(\\w+)"

cruisedata$date <- gsub(pattern,'\\1',cruisedata$FileName)
cruisedata$time <- gsub(pattern,'\\3',cruisedata$FileName)
cruisedata$lightcond <- gsub(pattern,'\\5',cruisedata$FileName)
cruisedata$labnumber <- gsub(pattern,'\\7',cruisedata$FileName)

cruisedata <- cruisedata %>%
  mutate(nutrients = case_when(substr(Well, 1, 1) %in% c("A", "B", "C", "D") ~ "(+)", 
                        substr(Well, 1, 1) %in% c("E", "F", "G", "H") ~ "(-)")) %>%
  mutate(dfactor = case_when(substr(Well, 1, 1) %in% c("A", "E") ~ 1, substr(Well, 1, 1) %in% c("B", "F") ~ 0.5,
                             substr(Well, 1, 1) %in% c("C", "G") ~ 0.25, 
                             substr(Well, 1, 1) %in% c("D", "H") ~ 0.125))

part1 <- cruisedata %>%
  filter (labnumber %in% c("IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII")) %>%
  mutate(dtype = case_when(substr(Well, 2, 3) %in% c("01", "02", "03") ~ "0.02", 
                           substr(Well, 2, 3) %in% c("04", "05", "06") ~ "0.45",
                           substr(Well, 2, 3) %in% c("07", "08", "09") ~ "TFF", 
                           substr(Well, 2, 3) %in% c("10", "11", "12") ~ "HK"))
part2 <- cruisedata %>%
  filter (labnumber %in% c("LDI", "LDII")) %>%
  mutate(dtype = case_when(substr(Well, 2, 3) %in% c("01", "02", "03", "04", "05", "06") ~ 0.02, 
                           substr(Well, 2, 3) %in% c("07", "08", "09", "10", "11", "12") ~ 0.45)) 

part3 <- cruisedata %>%
  filter (labnumber %in% c("LDIII")) %>%
  mutate(dtype = case_when(substr(Well, 2, 3) %in% c("01", "02", "03", "04", "05" ) ~ "0.02", 
                           substr(Well, 2, 3) %in% c("06", "07") ~ "Dunaliella",
                           substr(Well, 2, 3) %in% c("08", "09", "10", "11", "12") ~ "0.45"))

cruisedata <- rbind (part1, part2, part3)
