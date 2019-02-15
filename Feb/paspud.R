
#import data, columns were renamed into one words
library(readxl)
paspud <- read_excel("PassionPuddle.xlsx", 
                     sheet = "Sheet1")
View(paspud)

paspud$date <- rep(c(180208, 180215, 180222, 180301, 180308), each=96)
paspud$sitetemp <- rep(c(3.5, 7, 11.5, 10.5, 6), each=96)
paspud$tempinc <- rep(c(4, 4, 15, 15, 4), each=96)

