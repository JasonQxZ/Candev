library(dplyr)
library(tidyr)
library(car)

## choose where to get csv

setwd(choose.dir())

## reading data in current directory

raw = read.csv('grants.csv')

## filter only Canada

raw_canada = raw %>% filter(recipient_country == 'CA')

## Select relevant columns

raw_columns = raw_canada %>% select(agreement_start_date, agreement_end_date,
                                    recipient_province, recipient_city,
                                    recipient_postal_code, owner_org_title, agreement_value
)

## Rename relevant columns

rename = raw_columns %>% rename(Start = agreement_start_date, End = agreement_end_date, 
                                Province = recipient_province, City = recipient_city,
                                Postal = recipient_postal_code, Department = owner_org_title,
                                Value = agreement_value)

## Clean province codes

province_clean = rename %>% mutate(Province = car::recode(Province, "c('QC', 'QB', 'Quebec') = 'QC';
                                                     c('ON', 'Ontario', 'ON ') = 'ON';
                                                     c('BC', 'British Columbia') = 'BC';
                                                     c('MB', 'MA') = 'MB';
                                                     c('NL', 'NF') = 'NL';
                                                     c('SK','SK ') = 'SK';
                                                     c('AB', 'AB ', 'Alberta') = 'AB';
                                                     "))

## Remove missing provinces

province_filter = province_clean %>% filter(Province != '')

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

