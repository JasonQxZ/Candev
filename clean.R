library(dplyr)
library(tidyr)
library(car)

## choose where to get csv

setwd(choose.dir())

raw = read.csv('grants.csv')

raw_canada = raw %>% filter(recipient_country == 'CA')

raw_columns = raw_canada %>% select(agreement_start_date, agreement_end_date,
                                     recipient_province, recipient_city,
                                     recipient_postal_code, owner_org_title, agreement_value
                                     )

rename = raw_columns %>% rename(Start = agreement_start_date, End = agreement_end_date, 
                                Province = recipient_province, City = recipient_city,
                                Postal = recipient_postal_code, Department = owner_org_title,
                                Value = agreement_value)

province_clean = rename %>% mutate(Province = car::recode(Province, "c('QC', 'QB', 'Quebec') = 'QC';
                                                     c('ON', 'Ontario', 'ON ') = 'ON';
                                                     c('BC', 'British Columbia') = 'BC';
                                                     c('MB', 'MA') = 'MB';
                                                     c('NL', 'NF') = 'NL';
                                                     c('SK','SK ') = 'SK';
                                                     c('AB', 'AB ', 'Alberta') = 'AB';
                                                     "))

province_filter = province_clean %>% filter(Province != '')

date_years = province_filter %>% mutate(StartYear = substring(Start,1,4), EndYear = substring(End,1,4))

date_duration = date_years %>% mutate(Length = as.Date(End) - as.Date(Start))

postal_zone = date_duration %>% mutate(Zone = substring(Postal,1,1))

data = postal_zone


