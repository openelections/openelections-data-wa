library(tidyverse)
library(sf)

# configure script here

SOURCE_DATA_DIR = '../../openelections-sources-wa/2018/'
OUTPUT_DATA_DIR = '../2018/'

countyFiles <- c(
  '20180213_AllCounties.csv',
  '20180424_AllCounties.csv',
  '20180807_AllCounties.csv',
  '20181106_AllCounties.csv'
)

statePrecinctFiles <- c(
  '20180213_AllStatePrecincts.csv',
  '20180424_AllStatePrecincts.csv',
  '20180807_AllStatePrecincts.csv',
  '20181106_AllStatePrecincts.csv'
)

kingCountyPrecinctFiles <- c(
  '20180213_KingPrecincts.csv',
  '20180424_KingPrecincts.csv',
  '20180807_KingPrecincts.csv',
  '20181106_KingPrecincts.csv'
)

electionTypes <- c(
  'special__general', 'special__general', 'primary', 'general'
)

# end configure

precincts <- read_sf('/opt/data/washington/elections/statewide-precincts/2017/Statewide_Prec_2017.shp') %>% st_set_geometry(NULL) %>%
  select(COUNTY, COUNTYCODE, PRECCODE, PRECNAME) %>%
  bind_rows(
    read_sf('/opt/data/washington/elections/king-precincts-shp/Voting_Districts_of_King_County__votdst_area.shp') %>% st_set_geometry(NULL) %>%
      mutate(PRECCODE=as.double(votdst)) %>%
      select(PRECCODE, PRECNAME=NAME) %>% mutate(COUNTY='King', COUNTYCODE='KI')
  ) %>% distinct()

countyCodes <- precincts %>%
  select(County=COUNTY, CountyCode=COUNTYCODE) %>%
  distinct()

precincts <- filter(precincts, COUNTYCODE=='KI') # we only need to merge by precinct name for King County

if (nrow(precincts %>% group_by(COUNTY, PRECNAME) %>% filter(n() > 1))) {
  stop('King Precinct dups found')
}

cleanOffice <- function(Race) {
  gsub(x=Race, pattern='Legislative District [0-9]+(?: -)? State (.+)(?: Pos.*)?', replacement='State \\1', perl=TRUE) %>%
    gsub(x=., pattern='Legislative District No. [0-9]+ Representative Position No. (1|2)', replacement='State Representative Pos. \\1') %>%
    gsub(x=., pattern='Legislative District No. [0-9]+ .+Senator', replacement='State Senator') %>%
    gsub(x=., pattern='(Pos. [0-9]+)', replacement='(\\1)') %>%
    gsub(x=., pattern='^Washington State (.+)', replacement='\\1') %>%
    gsub(x=., pattern='United.+Senator', replacement='U.S. Senator') %>%
    gsub(x=., pattern='.*U\\.?S.+Senator', replacement='U.S. Senator') %>%
    gsub(x=., pattern='Congressional District ([0-9]+).*', replacement='U.S. Representative') %>%
    gsub(x=., pattern='Congressional District No. ([0-9]+).*', replacement='U.S. Representative') %>%
    gsub(x=., pattern='Court of Appeals, Division No. 1, District No. 1, Judge Position No. ([0-9])', replacement='Court of Appeals, Division 1, District 1 - Judge Position \\1') %>%
    gsub(x=., pattern='State Supreme Court, Justice Position No. ([0-9])', replacement='Supreme Court - Justice Position \\1') %>%
    gsub(x=., pattern='State of Washington Initiative Measure No. 940', replacement='Initiative to the Legislature 940 Initiative Measure No. 940 concerns law enforcement.') %>%
    gsub(x=., pattern='State of Washington Initiative Measure No. 1639', replacement='Initiative Measure No. 1639 Initiative Measure No. 1639 concerns firearms.') %>%
    gsub(x=., pattern='State of Washington Initiative Measure No. 1631', replacement='Initiative Measure No. 1631 Initiative Measure No. 1631 concerns pollution.') %>%
    gsub(x=., pattern='State of Washington Initiative Measure No. 1634', replacement='Initiative Measure No. 1634 concerns taxation of certain items intended for human consumption.')
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
    select(county, office, district, party, candidate=Candidate, votes=Votes)
  
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
    mutate(precinct_code=as.character(precinct))
  
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
    left_join(precincts %>% filter(COUNTYCODE=='KI') %>% select(precinct_code=PRECCODE, PRECNAME), by=c('Precinct'='PRECNAME')) %>%
    select(county, precinct_code, precinct=Precinct, office, district, party, candidate, votes) %>%
    filter(!grepl(x=candidate, pattern='^Times ')) %>%
    mutate(candidate=gsub(x=candidate, pattern='\xf1', replacement='\u00F1')) %>%
    mutate(precinct_code=case_when(precinct=='ELECTIONS OFFICE' ~ precinct, TRUE ~ as.character(precinct_code))) %>%
    bind_rows(precinctDfs[[electionDate]])
  
  write_csv(cdf, paste0(OUTPUT_DATA_DIR, electionDate, '__wa__', electionType, '__precinct.csv'), na='')
  
  cdf
  
}

precinctDfs <- map2(kingCountyPrecinctFiles, electionTypes, processPrecinctFile)
names(precinctDfs) <- extractElectionDate(kingCountyPrecinctFiles)
