read.csv('C:\\Users\\guoly\\Desktop\\123.csv',header = F)

install.packages("readr")
library(readr)


data <- read_csv("c:\\Users\\guoly\\Desktop\\Poisson_group\\grants.csv")


############################################## clean data ##############################################

library(dplyr)
library(tidyr)
library(car)

## choose where to get csv

setwd("c:\\Users\\guoly\\Desktop\\Candev")

## reading data in current directory

raw <- data

## filter only Canada

raw_canada = raw %>% filter(recipient_country == 'CA', recipient_type != '')

## Select relevant columns

raw_columns = raw_canada %>% select(agreement_start_date, agreement_end_date,
                                    recipient_province, recipient_city,
                                    recipient_postal_code, owner_org_title, agreement_value, recipient_type
)

## Rename relevant columns

rename = raw_columns %>% rename(Start = agreement_start_date, End = agreement_end_date, 
                                Province = recipient_province, City = recipient_city,
                                Postal = recipient_postal_code, Department = owner_org_title,
                                Value = agreement_value, Type = recipient_type)

## Clean province codes

province_clean = rename %>% mutate(Province = car::recode(Province, "c('QC', 'QB', 'Quebec') = 'Quebec';
                                                     c('ON', 'Ontario', 'ON ') = 'Ontario';
                                                     c('BC', 'British Columbia') = 'British Columbia';
                                                     c('MB', 'MA') = 'Manitoba';
                                                     c('NL', 'NF') = 'Newfoundland and Labrador';
                                                     c('SK','SK ') = 'Saskatchewan';
                                                     c('AB', 'AB ', 'Alberta') = 'Alberta';
                                                     'NS' = 'Nova Scotia';
                                                     'PE' = 'Prince Edward Island';
                                                     'NB' = 'New Brunswick';
                                                     'NT' = 'Northwest Territories';
                                                     'YT' = 'Yukon';
                                                     'NU' = 'Nunavut'
                                                     "))

## Rename recipient type names via dictionary

Type_dic = province_clean %>% mutate(Type = car::recode(Type, "'A' = 'Aboriginal';
                                                        'F' = 'For-Profit';
                                                        'G' = 'Government';
                                                        'I' = 'International';
                                                        'N' = 'Not-for-Profit/Charities';
                                                        'O' = 'Other';
                                                        'P' = 'Individual';
                                                        'S' = 'Academia'
                                                        "))

## Remove missing provinces

province_filter = Type_dic %>% filter(Province != '')

## Creating new columns 

date_years = province_filter %>% mutate(StartYear = substring(Start,1,4), EndYear = substring(End,1,4))

date_duration = date_years %>% mutate(Length = as.Date(End) - as.Date(Start))

postal_zone = date_duration %>% mutate(Zone = substring(Postal,1,1))

#### Cleaning new columns

## Remove non-alphabetic postal zones

clean_zones = postal_zone %>% filter(grepl('^[A-Za-z]+$', Zone))

## Coerce all zones to uppercase

cleaner_zones = clean_zones %>% mutate(Zone = toupper(Zone))

### naming final data

data = cleaner_zones



#########################################################################################################


unique(data$City)
Largest.cities$name



unique(data$Province)
View(data)

# Largest.cities$name
# [1] "Abbotsford BC"               "Barrie ON"                   "Calgary AB"                 
# [4] "Chicoutimi-Jonquiere QC"     "Edmonton AB"                 "Guelph ON"                  
# [7] "Halifax NS"                  "Hamilton ON"                 "Kanata ON"                  
# [10] "Kelowna BC"                  "Kingston ON"                 "Kitchener ON"               
# [13] "London ON"                   "Montreal QC"                 "Oshawa ON"                  
# [16] "OTTAWA ON"                   "Quebec QC"                   "Regina SK"                  
# [19] "Saint Catharines-Niagara ON" "Saint John's NL"             "Saskatoon SK"               
# [22] "Sherbrooke QC"               "Sudbury ON"                  "Toronto ON"                 
# [25] "Trois-Rivieres QC"           "Vancouver BC"                "Victoria BC"                
# [28] "Windsor ON"                  "Winnipeg MB"   

Largest.cities$name[1]

city_clean = data %>% mutate(City = car::recode(City, 
"c('Abbotsford', 'Abbotsford|Abbotsford') = 'Abbotsford BC';
c('Barrie|Barrie','Barrie') = 'Barrie ON';
c('Calgary','	CALGARY', '	Calgary | Calgary') = 'Calgary AB';
c('Chicoutimi', 'CHICOUTIMI | CHICOUTIMI', 'Chicoutimi|Chicoutimi' ) = 'Chicoutimi-Jonquiere QC';
c('edmonton', 'Edmonton', 'EDMONTON', 'Edmonton | Edmonton', 'EDMONTON | EDMONTON', 
'Edmonton|Edmonton', 'Edmonton International Airport') = 'Edmonton AB';
c('guelph', 'Guelph', '	GUELPH', 'GUELPH | GUELPH', 'Guelph|Guelph') = 'Guelph ON';
c('Halifax', 'HALIFAX | HALIFAX', 'Halifax|Halifax') = 'Halifax NS';
c('Hamilton|Hamilton', 'HAMILTON | HAMILTON', 'Hamilton | Hamilton', 'Hamilton') = 'Hamilton ON';
c('Kanata', 'Kanata|Kanata') = 'Kanata ON';
c('West Kelowna|West Kelowna', 'West Kelowna','Kelowna|Kelowna','KELOWNA | KELOWNA', 'KELOWNA', 'Kelowna') = 'Kelowna BC';
c('Kingston|Kingston', 'Kingston' ) = 'Kingston ON'; 
c('Kitchener', 'Kitchener|Kitchener') = 'Kitchener ON';
c('London|London', 'LONDON | LONDON', 'London') = 'London ON'; 
c('Montreal','MONTREAL','Montreal-Ouest|Montréal-Ouest','Montreal-Riv-Harbr|Montreal-Riv-Harbr','MONTREAL | MONTREAL','Montreal|Montreal', 'MONTREAL|MONTRÉAL') = 'Montreal QC';
c('Oshawa', 'Oshawa|Oshawa') = 'Oshawa ON';
c('ottawa','Ottawa','Ottawa | Ottawa', 'OTTAWA | OTTAWA', 'Ottawa|Ottawa') = 'OTTAWA ON';
c('QUEBEC | QUEBEC', 'Quebec|Québec') = 'Quebec QC';
c('Regina|Regina','REGINA', 'Regina') = 'Regina SK';
c('ST CATHARINES','St Catharines|St Catharines','St. Catharines', 'St. Catharines|St. Catharines', 
'Niagara-on-the-Lake', 'Niagara Falls','Niagara Falls|Niagara Falls', 'Niagara On The Lake|Niagara On The Lake') = 'Saint Catharines-Niagara ON';
c('saskatoon', 'Saskatoon', 'SASKATOON', 'SASKATOON | SASKATOON', 'SASKATOON SK', 'Saskatoon|Saskatoon') = 'Saskatoon SK';
c('Sherbrooke|Sherbrooke', 'SHERBROOKE | SHERBROOKE', 'Sherbrooke | Sherbrooke', 'Sherbrooke') = 'Sherbrooke QC';
c('Sudbury|Sudbury', 'SUDBURY', 'Sudbury') = 'Sudbury ON';
c('North York (Toronto)', 'toronto', 'Toronto', 'TORONTO', 'Toronto|Toronto', 'Toronto | Toronto', 
'Toronto|Toronto','TORONTO | TORONTO' ) = 'Toronto ON';
c('Trois-Rivières', 'TROIS-RIVIÈRES', 'TROIS-RIVIÈRES | TROIS-RIVIÈRES', 'Trois-Rivieres|Trois-Rivières') = 'Trois-Rivieres QC';
c('north vancouver', 'North Vancouver', 'NORTH VANCOUVER', 'NORTH VANCOUVER | NORTH VANCOUVER', 'North Vancouver|North Vancouver',
'Vancouver', 'Vancouver ,BC', 'Vancouver | Vancouver', 'VANCOUVER | VANCOUVER', 'Vancouver|Vancouver', 'West Vancouver', 
'West Vancouver|West Vancouver') = 'Vancouver BC';
c('VICTORIA|VICTORIA', 'Victoria|Victoria', 'Victoria BC', 'VICTORIA | VICTORIA', 'VICTORIA', 'Victoria') = 'Victoria BC';
c('Windsor|Windsor', 'Windsor')  = 'Windsor ON';
c('WINNIPEG|WINNIPEG', 'Winnipeg|Winnipeg' , 'Winnipeg Mb', 'Winnipeg Manitoba', 'Winnipeg Beach', 'Winnipeg | Winnipeg', 'WINNIPEG', 'Winnipeg') = 'Winnipeg MB'             "))


#c('St John's', 'St John's | St-Jean', 'ST. JOHN'S', 'ST. JOHN'S | SAINT JEAN DE TERRE-NEUVE', 'St. John's|St. John's') = 'Saint John\'s NL';

# merge these 28 largest cities

library(maps)
canada.cities
Largest.cities <- canada.cities[canada.cities$pop > 100000 , ]
head(Largest.cities)
Largest.cities$City <- Largest.cities$name
head(city_clean)
Largestcity_comb <- merge(Largest.cities, city_clean, by='City')
head(Largestcity_comb)
View(Largestcity_comb)
write.csv(Largestcity_comb, "C:\\Users\\guoly\\Desktop\\Candev\\largest_city.csv")

value <- tapply(Largestcity_comb$Value, Largestcity_comb$City, sum)
class(value)
value <- as.data.frame(value)
value$City <- rownames(value)
rownames(value) <- c()
top28 <- unique(merge(value, Largestcity_comb[,c("City", "pop", "lat", "long")], by="City"))
rownames(top28) <- c()
head(top28)
write.csv(top28, "C:\\Users\\guoly\\Desktop\\Candev\\top28.csv")

# build the dataset that we need for bubble plot

unique(Largestcity_comb$City)

Largest_new <- Largestcity_comb[,c("City","Province", "StartYear", "Value", "pop","lat", "long" ) ]

View(Largest_new)

summary_city = Largest_new %>% group_by(City, StartYear, .drop=F) %>% summarize(Total = sum(Value))


summary_city$Year <- summary_city$StartYear 

summary_city <- merge(summary_city, unique(Largest_new[,c(1,2,5,6,7)]), by= c("City"))

summary_city <- summary_city[,-2]

View(summary_city)

unique(summary_city$Province)


summary_city$Value <- summary_city$Total
summary_city$Year <- summary_city$StartYear
summary_city$Value <- summary_city$Total

class(summary_city$Value)

write.csv(summary_city, "C:\\Users\\guoly\\Desktop\\pic3\\summary_city.csv")

