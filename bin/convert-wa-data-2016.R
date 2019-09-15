library(tidyverse)
library(openxlsx)

# configure script here

SOURCE_DATA_DIR = '../../openelections-sources-wa/2016/'
OUTPUT_DATA_DIR = '../2016/'

countyFiles <- c(
  '20160209_AllCounties.csv',
  '20160426_AllCounties.csv',
  '20160524_AllCounties.csv',
  '20160802_AllCounties.csv',
  '20161108_AllCounties.csv'
)

statePrecinctFiles <- c(
  '20160209_AllStatePrecincts.csv',
  '20160426_AllStatePrecincts.csv',
  '20160524_AllStatePrecincts.csv',
  '20160802_AllStatePrecincts.csv',
  '20161108_AllStatePrecincts.csv'
)

kingCountyPrecinctFiles <- c(
  '20160209_King-precinct-results.csv',
  '20160426_King-precinct-results.csv',
  '20160524_King-precinct-results.csv',
  '20160802_King-precinct-results.csv',
  '20161108_King-precinct-results.csv'
)

electionTypes <- c(
  'special__general', 'special__general', 'primary', 'primary', 'general'
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
  distinct()

cleanOffice <- function(Race) {
  gsub(x=Race, pattern='Legislative District [0-9]+(?: -)? State (.+)(?: Pos.*)?', replacement='State \\1', perl=TRUE) %>%
    gsub(x=., pattern='(Pos. [0-9]+)', replacement='(\\1)') %>%
    gsub(x=., pattern='Lt. Governor', replacement='Lieutenant Governor') %>%
    gsub(x=., pattern='^Washington State (.+)', replacement='\\1') %>%
    gsub(x=., pattern='.*President/Vice President', replacement='U.S. President/Vice President') %>%
    gsub(x=., pattern='United.+Senator', replacement='U.S. Senator') %>%
    gsub(x=., pattern='Congressional District ([0-9]+).*', replacement='U.S. Representative')
}

cleanParty <- function(Party) {
  gsub(x=Party, pattern='\\(Prefers (.+) Party\\)', replacement='\\1') %>%
    gsub(x=., pattern='\\(?States No.+Prefer.+', replacement='States No Party Preference') %>%
    gsub(x=., pattern='(.+) Party Nomine.+', replacement='\\1') %>%
    gsub(x=., pattern='&amp;', replacement='&') %>%
    gsub(x=., pattern=' Party$', replacement='') %>%
    gsub(x=., pattern='Indep.+ Dem.*', replacement='Independent Democrat') %>%
    gsub(x=., pattern='Independent Rep(?: Party)?', replacement='Independent Republican', perl=TRUE) %>%
    gsub(x=., pattern='G\\.O\\.P', replacement='GOP') %>%
    gsub(x=., pattern='GOP/Independent', replacement='Independent GOP') %>%
    gsub(x=., pattern='DEM', replacement='Democratic') %>%
    gsub(x=., pattern='^Democrat$', replacement='Democratic') %>%
    gsub(x=., pattern='REPUBLICAN', replacement='Republican') %>%
    gsub(x=., pattern='INDEPENDENT', replacement='Independent') %>%
    gsub(x=., pattern='Dem/Working Fmly.+', replacement='Democratic / Working Family')
}

cleanDistrict <- function(Race) {
  case_when(
    grepl(x=Race, pattern='^Legislative District') ~ gsub(x=Race, pattern='Legislative District ([0-9]+)(?: -)? State.+', replacement='\\1'),
    grepl(x=Race, pattern='^Congressional District') ~ gsub(x=Race, pattern='Congressional District ([0-9]+).+', replacement='\\1'),
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
    select(county=County, precinct_code=PrecinctCode, precinct=PrecinctName, office, district, candidate=Candidate, votes=Votes)
  
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

processPrecinctFile <- function(inputFileName, electionType=c('general', 'primary', 'special__general', 'special__primary')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    select(precinct=Precinct, Race, party=Party, candidate=CounterType, votes=SumOfCount) %>%
    mutate(office=gsub(x=Race, pattern='Legislative District ([0-9]+) Representative Position ([12])', replacement='State Representative (Pos. \\2)'),
           office=gsub(x=office, pattern='Legislative District ([0-9]+) State Senator', replacement='State Senator'),
           office=gsub(x=office, pattern='US President & Vice President', replacement='U.S. President/Vice President'),
           office=gsub(x=office, pattern='US Senator', replacement='U.S. Senator'),
           office=gsub(x=office, pattern='Congressional District.+', replacement='U.S. Representative'),
           district=case_when(
             grepl(x=Race, pattern='^Legislative District') ~ gsub(x=Race, pattern='Legislative District ([0-9]+).+', replacement='\\1'),
             grepl(x=Race, pattern='^Congressional District') ~ gsub(x=Race, pattern='Congressional District ([0-9]+)', replacement='\\1'),
             TRUE ~ NA_character_
           ),
           county='King'
    ) %>%
    mutate(party=case_when(grepl(x=party, pattern='NP') ~ NA_character_, TRUE ~ party)) %>%
    mutate(party=case_when(
      party=='Dem' ~ 'Democratic',
      party=='Rep' ~ 'Republican',
      party=='Lib' ~ 'Libertarian',
      party=='GP' ~ 'Green',
      party=='CPN' ~ 'Constitution',
      party=='DPN' ~ 'Democratic',
      party=='GPN' ~ 'Green',
      party=='LPN' ~ 'Libertarian',
      party=='RPN' ~ 'Republican',
      party=='SPN' ~ 'Socialism & Liberation',
      party=='SWN' ~ 'Socialist Workers',
      party=='DFW' ~ 'Democratic / Working Family',
      party=='NOP' ~ 'States No Party Preference',
      party=='PAR' ~ 'Non-Partisan',
      party=='ID' ~ 'Independent Democrat',
      party=='Dcr' ~ 'Democratic',
      is.na(party) ~ NA_character_,
      TRUE ~ party
    )) %>%
    left_join(precinctCodes %>% filter(CountyCode=='KI') %>% select(precinct_code=PrecinctCode, PrecinctName), by=c('precinct'='PrecinctName')) %>%
    select(county, precinct_code, precinct, office, district, party, candidate, votes) %>%
    filter(!grepl(x=candidate, pattern='^Times ')) %>%
    mutate(candidate=gsub(x=candidate, pattern='\xf1', replacement='\u00F1')) %>%
    bind_rows(precinctDfs[[electionDate]])
  
  write_csv(cdf, paste0(OUTPUT_DATA_DIR, electionDate, '__wa__', electionType, '__precinct.csv'), na='')
  
  cdf
  
}

precinctDfs <- map2(kingCountyPrecinctFiles, electionTypes, processPrecinctFile)
names(precinctDfs) <- extractElectionDate(kingCountyPrecinctFiles)
