library(tidyverse)
library(sf)

# configure script here

SOURCE_DATA_DIR = '../../openelections-sources-wa/2020/'
OUTPUT_DATA_DIR = '../2020/'

countyFiles <- c(
  '20200804_AllCounties.csv', '20201103_AllCounties.csv'
)

statePrecinctFiles <- c(
  '20200804_AllStatePrecincts.csv', '20201103_AllStatePrecincts.csv'
)

kingCountyPrecinctFiles <- c(
  '20200804_KingPrecincts.csv', '20201103_KingPrecincts.csv'
)

electionTypes <- c(
  'primary', 'general'
)

countyCodes <- read_sf('/opt/data/washington/elections/statewide-precincts/2017/Statewide_Prec_2017.shp') %>% st_set_geometry(NULL) %>%
  select(County=COUNTY, CountyCode=COUNTYCODE) %>%
  distinct()

cleanOffice <- function(Race) {
  gsub(x=Race, pattern='Legislative District [0-9]+(?: -)? State (.+)(?: Pos.*)?', replacement='State \\1', perl=TRUE, ignore.case=TRUE) %>%
    gsub(x=., pattern='Legislative District No\\.[ ]+[0-9]+(?: -)? State (.+)(?: Pos.*)?', replacement='State \\1', perl=TRUE, ignore.case=TRUE) %>%
    gsub(x=., pattern='Legislative District 15.+Representative.+([12])', replacement='State Representative Pos. \\1', perl=TRUE, ignore.case=TRUE) %>%
    gsub(x=., pattern='Congressional District [0-9]+.+', replacement='U.S. Representative', perl=TRUE, ignore.case=TRUE) %>%
    gsub(x=., pattern='Congressional District No\\.[ ]+[0-9]+.+', replacement='U.S. Representative', perl=TRUE, ignore.case=TRUE) %>%
    gsub(x=., pattern='State State', replacement='State', perl=TRUE, ignore.case=TRUE) %>%
    gsub(x=., pattern='&amp;', replacement='&') %>%
    gsub(x=., pattern='Position No', replacement='Pos') %>%
    gsub(x=., pattern='^Governor$', replacement='Governor') %>%
    gsub(x=., pattern='.*State.+Lt.+Governor.*', replacement='Lt. Governor') %>%
    gsub(x=., pattern='.*State.+Governor.*', replacement='Governor') %>%
    gsub(x=., pattern='.*Lt\\. Governor.*', replacement='Lt. Governor') %>%
    gsub(x=., pattern='.*Commissioner of Public Lands.*', replacement='Commissioner of Public Lands') %>%
    gsub(x=., pattern='.*Superintendent of Public Instruction.*', replacement='Superintendent of Public Instruction') %>%
    gsub(x=., pattern='.*Attorney General.*', replacement='Attorney General') %>%
    gsub(x=., pattern='.*Secretary of State.*', replacement='Secretary of State') %>%
    gsub(x=., pattern='^Auditor$', replacement='State Auditor') %>%
    gsub(x=., pattern='.*State Auditor.*', replacement='State Auditor') %>%
    gsub(x=., pattern='^Treasurer$', replacement='State Treasurer') %>%
    gsub(x=., pattern='.*State Treasurer.*', replacement='State Treasurer') %>%
    gsub(x=., pattern='.*Insurance Commissioner.*', replacement='Insurance Commissioner') %>%
    gsub(x=., pattern='.*President.+President$', replacement='President / Vice President') %>%
    gsub(x=., pattern='.*Supreme.+Justice.+([1-9])', replacement='State Supreme Court Justice Pos \\1') %>%
    gsub(x=., pattern='.*Advisory Vote.+([1-9]+)', replacement='State Advisory Vote \\1') %>%
    gsub(x=., pattern='.*Referendum.+([1-9]+)', replacement='Referendum Measure \\1') %>%
    gsub(x=., pattern='.+Senate.+8212.*)', replacement='Engrossed Senate Joint Resolution No. 8212') %>%
    trimws(.)
}

cleanParty <- function(Party) {
  gsub(x=Party, pattern='\\(Prefers (.+) Party\\)', replacement='\\1') %>%
    gsub(x=., pattern='\\(?States No.+Prefer.+', replacement='States No Party Preference') %>%
    gsub(x=., pattern='&#39;', replacement="'") %>%
    gsub(x=., pattern='DEMOCRATIC', replacement="Democratic") %>%
    gsub(x=., pattern='REPUBLICAN', replacement="Republican") %>%
    gsub(x=., pattern='INDEPENDENT', replacement="Independent") %>%
    gsub(x=., pattern='\\(?(.+) Party Nominees\\)?', replacement="\\1") %>%
    trimws(.)
}

cleanDistrict <- function(Race) {
  case_when(
    grepl(x=Race, pattern='^Legislative District 15', ignore.case=TRUE) ~ '15',
    grepl(x=Race, pattern='^Legislative District No', ignore.case=TRUE) ~ gsub(x=Race, pattern='Legislative District No.[ ]+([0-9]+) .+', replacement='\\1', ignore.case=TRUE),
    grepl(x=Race, pattern='^Legislative District', ignore.case=TRUE) ~ gsub(x=Race, pattern='Legislative District ([0-9]+)(?: -)? State.+', replacement='\\1', ignore.case=TRUE),
    grepl(x=Race, pattern='^Congressional District No', ignore.case=TRUE) ~ gsub(x=Race, pattern='Congressional District No.[ ]+([0-9]+).+', replacement='\\1', ignore.case=TRUE),
    grepl(x=Race, pattern='^Congressional District', ignore.case=TRUE) ~ gsub(x=Race, pattern='Congressional District ([0-9]+).+', replacement='\\1', ignore.case=TRUE),
    TRUE ~ NA_character_
  )
}

cleanCandidate <- function(Candidate) {
  gsub(x=Candidate, pattern='&quot;', replacement='"') %>%
    gsub(x=., pattern='(.+) and (.+)', replacement='\\1 / \\2') %>%
    gsub(x=., pattern='WRITE-IN', replacement='Write-in') %>%
    trimws(.)
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
           candidate=cleanCandidate(Candidate),
           party=cleanParty(Party)
    ) %>%
    select(county, office, district, party, candidate, votes=Votes, Race) %>% select(-Race) # comment last to work on cleanups
  
  write_csv(cdf, paste0(OUTPUT_DATA_DIR, electionDate, '__wa__', electionType, '__county.csv'), na='')
  
  cdf
  
}

processPrecinctFile <- function(inputFileName, electionType=c('special__general', 'special__general', 'primary', 'general')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    mutate(office=cleanOffice(Race),
           candidate=cleanCandidate(Candidate),
           district=cleanDistrict(Race)
    ) %>%
    filter(PrecinctCode != -1) %>%
    left_join(countyCodes, by='CountyCode') %>%
    select(county=County, precinct_code=PrecinctCode, precinct=PrecinctName, office, district, candidate, votes=Votes) %>%
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
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName), col_types='cciiicccc', locale = locale(encoding = 'ISO-8859-1'))) %>%
    select(Precinct, Race, party=Party, candidate=CounterType, votes=SumOfCount) %>%
    mutate(votes=as.integer(gsub(x=votes, pattern=',', replacement=''))) %>%
    mutate(office=cleanOffice(Race),
           party=cleanParty(party),
           candidate=cleanCandidate(candidate),
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
