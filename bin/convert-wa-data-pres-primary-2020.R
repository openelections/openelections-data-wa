library(tidyverse)
library(sf)

OUTPUT_DATA_DIR = '../2020/'
SOURCE_DATA_DIR = '../../openelections-sources-wa/2020/'

countyCodes <- read_sf('/opt/data/washington/elections/statewide-precincts/2017/Statewide_Prec_2017.shp') %>% st_set_geometry(NULL) %>%
  select(County=COUNTY, CountyCode=COUNTYCODE) %>%
  distinct()

suppressMessages(read_csv(file.path(SOURCE_DATA_DIR, '20200310_AllCounties.csv'))) %>%
  mutate(county=County,
         office='US President',
         district=NA_character_,
         party=gsub(x=Race, pattern='.*President (.+) Party', replacement='\\1 Party')
  ) %>%
  select(county, office, district, party, candidate=Candidate, votes=Votes) -> countyResults

write_csv(countyResults, file.path(OUTPUT_DATA_DIR, '20200310__wa__primary__county.csv'), na='')

nonKingPrecincts <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, '20200310_AllStatePrecincts.csv'))) %>%
  mutate(office='US President',
         district=NA_character_,
         party=gsub(x=Race, pattern='.*President (.+) Party', replacement='\\1 Party')
  ) %>%
  filter(PrecinctCode != -1) %>%
  left_join(countyCodes, by='CountyCode') %>%
  select(county=County, precinct_code=PrecinctCode, precinct=PrecinctName, office, district, candidate=Candidate, votes=Votes, party) %>%
  mutate(precinct_code=as.character(precinct_code)) %>%
  select(county,precinct_code,precinct,office,district,party,candidate,votes)

kingPrecincts <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, '20200310_KingPrecincts.csv'), col_types='cc----cc')) %>%
  select(Precinct, Race, candidate=CounterType, votes=SumOfCount) %>%
  mutate(votes=as.integer(gsub(x=votes, pattern=',', replacement=''))) %>%
  mutate(office='US President',
         district=NA_character_,
         party=gsub(x=Race, pattern='.*President .+States (.+) Party', replacement='\\1 Party'),
         county='King'
  ) %>%
  filter(!grepl(x=candidate, pattern='^Times ')) %>%
  filter(!grepl(x=candidate, pattern='^Registered ')) %>%
  select(county, precinct=Precinct, office, district, party, candidate, votes) %>%
  mutate(precinct_code=precinct) %>%
  mutate(precinct_code=case_when(precinct=='ELECTIONS OFFICE' ~ precinct, TRUE ~ as.character(precinct_code))) %>%
  select(county,precinct_code,precinct,office,district,party,candidate,votes)

bind_rows(nonKingPrecincts, kingPrecincts) -> precinctResults

write_csv(precinctResults, file.path(OUTPUT_DATA_DIR, '20200310__wa__primary__precinct.csv'), na='')

