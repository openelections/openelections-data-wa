library(tidyverse)
library(openxlsx)

# configure script here

SOURCE_DATA_DIR = '../../openelections-sources-wa/2017/'
OUTPUT_DATA_DIR = '../2017/'

countyFiles <- c(
  '20170214_AllCounties.csv',
  '20170425_AllCounties.csv',
  '20170801_AllCounties.csv',
  '20171107_AllCounties.csv'
)

statePrecinctFiles <- c(
  '20170214_AllStatePrecincts.csv',
  '20170425_AllStatePrecincts.csv',
  '20170801_AllStatePrecincts.csv',
  '20171107_AllStatePrecincts.csv'
)

kingCountyPrecinctFiles <- c(
  '20170214_King-precinct-results.csv',
  '20170425_King-precinct-results.csv',
  '20170801_King-precinct-results.csv',
  '20171107_King-precinct-results.csv'
)

electionTypes <- c(
  'special__general', 'special__general', 'primary', 'general'
)

# end configure

if (!exists('precincts')) {
  precincts <- read.xlsx('https://www.sos.wa.gov/_assets/elections/research/2016.12.07-Districts_Precincts.xlsx') %>% as_tibble()
}

countyCodes <- precincts %>%
  select(County, CountyCode) %>%
  distinct()

precinctCodes <- precincts %>%
  select(CountyCode, PrecinctCode, PrecinctName) %>%
  mutate(PrecinctCode=as.integer(PrecinctCode)) %>%
  distinct() %>%
  bind_rows(
    # who knows why this is missing from the SOS file...
    tibble(CountyCode='SP', PrecinctCode=7037, PrecinctName='PCT 7037')
  )

cleanOffice <- function(Race) {
  gsub(x=Race, pattern='Legislative District [0-9]+(?: -)? State (.+)(?: Pos.*)?', replacement='State \\1', perl=TRUE) %>%
    gsub(x=., pattern='(Pos. [0-9]+)', replacement='(\\1)')
}

cleanParty <- function(Party) {
  gsub(x=Party, pattern='\\(Prefers (.+) Party\\)', replacement='\\1') %>%
    gsub(x=., pattern='\\(States No Party.+', replacement='States No Party Preference')
}

cleanDistrict <- function(Race) {
  case_when(
    grepl(x=Race, pattern='^Legislative District') ~ gsub(x=Race, pattern='Legislative District ([0-9]+)(?: -)? State.+', replacement='\\1', perl=TRUE),
    TRUE ~ NA_character_
  )
}

extractElectionDate <- function(inputFileName) {
  gsub(x=inputFileName, pattern='([0-9]+)_.+', replacement='\\1')
}

processCountyFile <- function(inputFileName, electionType=c('general', 'primary', 'special__general', 'special__primary')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    mutate(county=County,
           office=cleanOffice(Race),
           district=cleanDistrict(Race),
           party=cleanParty(Party)
    ) %>%
    select(county, office, district, party, candidate=Candidate, votes=Votes)
  
  write_csv(cdf, paste0(OUTPUT_DATA_DIR, electionDate, '__wa__', electionType, '__county.csv'), na='')
  
  cdf
  
}

processPrecinctFile <- function(inputFileName, electionType=c('general', 'primary', 'special__general', 'special__primary')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    mutate(office=cleanOffice(Race),
           district=cleanDistrict(Race)
    ) %>%
    select(-PrecinctName) %>%
    inner_join(countyCodes, by='CountyCode') %>%
    inner_join(precinctCodes, by=c('CountyCode', 'PrecinctCode')) %>%
    select(county=County, precinct=PrecinctName, office, district, candidate=Candidate, votes=Votes)
  
  cdf
  
}

countyDfs <- map2(countyFiles, electionTypes, processCountyFile)
names(countyDfs) <- extractElectionDate(countyFiles)

precinctDfs <- map2(statePrecinctFiles, electionTypes, processPrecinctFile)
names(precinctDfs) <- extractElectionDate(statePrecinctFiles)

precinctDfs <- map2(precinctDfs, names(precinctDfs), function(pdf, name) {
  pdf %>%
    left_join(countyDfs[[name]] %>% select(candidate, office, party) %>% distinct(), by=c('candidate', 'office')) %>%
    select(county, precinct, office, district, party, candidate, votes)
})
names(precinctDfs) <- extractElectionDate(statePrecinctFiles)

processPrecinctFile <- function(inputFileName, electionType=c('general', 'primary', 'special__general', 'special__primary')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    select(precinct=Precinct, Race, party=Party, candidate=CounterType, votes=SumOfCount) %>%
    mutate(office=gsub(x=Race, pattern='Legislative District No\\. ([0-9]+) Representative Position No. ([12])', replacement='State Representative (Pos. \\2)'),
           office=gsub(x=office, pattern='Legislative District No\\. ([0-9]+) State Senator', replacement='State Senator'),
           district=case_when(
             grepl(x=Race, pattern='^Legislative District') ~ gsub(x=Race, pattern='Legislative District No\\. ([0-9]+).+', replacement='\\1'),
             TRUE ~ NA_character_
           ),
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
    select(county, precinct, office, district, party, candidate, votes) %>%
    filter(!grepl(x=candidate, pattern='^Times ')) %>%
    mutate(candidate=gsub(x=candidate, pattern='\xf1', replacement='\u00F1')) %>%
    bind_rows(precinctDfs[[electionDate]])
  
  write_csv(cdf, paste0(OUTPUT_DATA_DIR, electionDate, '__wa__', electionType, '__precinct.csv'), na='')
  
  cdf
  
}

precinctDfs <- map2(kingCountyPrecinctFiles, electionTypes, processPrecinctFile)
names(precinctDfs) <- extractElectionDate(kingCountyPrecinctFiles)
