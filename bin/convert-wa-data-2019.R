library(tidyverse)
library(sf)

# configure script here

SOURCE_DATA_DIR = '../../openelections-sources-wa/2019/'
OUTPUT_DATA_DIR = '../2019/'

countyFiles <- c(
  '20190806_AllCounties.csv',
  '20191105_AllCounties.csv'
)

statePrecinctFiles <- c(
  '20190806_AllStatePrecincts.csv',
  '20191105_AllStatePrecincts.csv'
)

kingCountyPrecinctFiles <- c(
  '20190806_KingPrecincts.csv',
  '20191105_KingPrecincts.csv'
)

electionTypes <- c(
  'primary', 'general'
)

countyCodes <- read_sf('/opt/data/washington/elections/statewide-precincts/2017/Statewide_Prec_2017.shp') %>% st_set_geometry(NULL) %>%
  select(County=COUNTY, CountyCode=COUNTYCODE) %>%
  distinct()

cleanOffice <- function(Race) {
  gsub(x=Race, pattern='Legislative District [0-9]+(?: -)? State (.+)(?: Pos.*)?', replacement='State \\1', perl=TRUE) %>%
    gsub(x=., pattern='&amp;', replacement='&') %>%
    trimws(.)
}

cleanParty <- function(Party) {
  gsub(x=Party, pattern='\\(Prefers (.+) Party\\)', replacement='\\1') %>%
    gsub(x=., pattern='\\(?States No.+Prefer.+', replacement='States No Party Preference') %>%
    trimws(.)
}

cleanDistrict <- function(Race) {
  case_when(
    grepl(x=Race, pattern='^Legislative District No') ~ gsub(x=Race, pattern='Legislative District No. ([0-9]+) .+', replacement='\\1'),
    grepl(x=Race, pattern='^Legislative District') ~ gsub(x=Race, pattern='Legislative District ([0-9]+)(?: -)? State.+', replacement='\\1'),
    grepl(x=Race, pattern='^Congressional District No') ~ gsub(x=Race, pattern='Congressional District No. ([0-9]+).+', replacement='\\1'),
    grepl(x=Race, pattern='^Congressional District') ~ gsub(x=Race, pattern='Congressional District ([0-9]+).+', replacement='\\1'),
    TRUE ~ NA_character_
  )
}

extractElectionDate <- function(inputFileName) {
  gsub(x=inputFileName, pattern='([0-9]+)_.+', replacement='\\1')
}

processCountyFile <- function(inputFileName, electionType=c('special__general', 'special__general', 'primary', 'general')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    mutate(county=County,
           office=cleanOffice(Race),
           district=cleanDistrict(Race),
           party=cleanParty(Party)
    ) %>%
    select(county, office, district, party, candidate=Candidate, votes=Votes, Race) %>% select(-Race) # comment last to work on cleanups
  
  write_csv(cdf, paste0(OUTPUT_DATA_DIR, electionDate, '__wa__', electionType, '__county.csv'), na='')
  
  cdf
  
}

processPrecinctFile <- function(inputFileName, electionType=c('special__general', 'special__general', 'primary', 'general')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    mutate(office=cleanOffice(Race),
           district=cleanDistrict(Race)
    ) %>%
    filter(PrecinctCode != -1) %>%
    left_join(countyCodes, by='CountyCode') %>%
    select(county=County, precinct_code=PrecinctCode, precinct=PrecinctName, office, district, candidate=Candidate, votes=Votes) %>%
    mutate(precinct_code=as.character(precinct_code))
  
  cdf
  
}

countyDfs <- map2(countyFiles, electionTypes, processCountyFile)
names(countyDfs) <- extractElectionDate(countyFiles)

precinctDfs <- map2(statePrecinctFiles, electionTypes, processPrecinctFile)
names(precinctDfs) <- extractElectionDate(statePrecinctFiles)

precinctDfs <- map2(precinctDfs, names(precinctDfs), function(pdf, name) {
  pdf %>%
    left_join(countyDfs[[name]] %>% select(candidate, office, party) %>% distinct(), by=c('candidate', 'office')) %>%
    select(county, precinct_code, precinct, office, district, party, candidate, votes)
})
names(precinctDfs) <- extractElectionDate(statePrecinctFiles)

processPrecinctFile <- function(inputFileName, electionType=c('special__general', 'special__general', 'primary', 'general')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName), col_types='cciiicccc')) %>%
    select(Precinct, Race, party=Party, candidate=CounterType, votes=SumOfCount) %>%
    mutate(votes=as.integer(gsub(x=votes, pattern=',', replacement=''))) %>%
    mutate(office=cleanOffice(Race),
           district=cleanDistrict(Race),
           county='King'
    ) %>%
    mutate(party=case_when(grepl(x=party, pattern='NP') ~ NA_character_, TRUE ~ party)) %>%
    mutate(party=case_when(
      party=='Dem' ~ 'Democratic',
      party=='Rep' ~ 'Republican',
      party=='Lib' ~ 'Libertarian',
      is.na(party) ~ NA_character_,
      TRUE ~ party
    )) %>%
    select(county, precinct=Precinct, office, district, party, candidate, votes) %>%
    mutate(precinct_code=precinct) %>%
    filter(!grepl(x=candidate, pattern='^Times ')) %>%
    mutate(candidate=gsub(x=candidate, pattern='\xf1', replacement='\u00F1')) %>%
    mutate(precinct_code=case_when(precinct=='ELECTIONS OFFICE' ~ precinct, TRUE ~ as.character(precinct_code))) %>%
    bind_rows(precinctDfs[[electionDate]]) %>%
    select(county,precinct_code,precinct,office,district,party,candidate,votes)
  
  write_csv(cdf, paste0(OUTPUT_DATA_DIR, electionDate, '__wa__', electionType, '__precinct.csv'), na='')
  
  cdf
  
}

precinctDfs <- map2(kingCountyPrecinctFiles, electionTypes, processPrecinctFile)
names(precinctDfs) <- extractElectionDate(kingCountyPrecinctFiles)
