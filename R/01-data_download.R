library(googlesheets4)
library(dplyr)
library(stringi)


raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/19pxS9OrfRdWGVIPB8Etn7szXGOoo4iy7LoabLPGtfo4/edit?resourcekey=&gid=2034190727#gid=2034190727")
dictionary <- read_sheet("https://docs.google.com/spreadsheets/d/1UbK2hAySdmX1vdB1swdLskAjy6iQvsDeC1vm8yltXro/edit?gid=0#gid=0")

glimpse(raw_data)

list_cols <- sapply(raw_data, is.list)
print(names(raw_data)[list_cols])

raw_data <- raw_data[ , !list_cols]

write.csv(raw_data, "R/data/raw/raw.csv")
write.csv(dictionary, "R/data/processed/dictionary.csv")


