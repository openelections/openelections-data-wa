library(tidyverse)
library(stringr)
library(sf)

SOURCE_DATA_DIR = '../../openelections-sources-wa/2014/'
OUTPUT_DATA_DIR = '../2014/'

countyFiles <- c(
  '20141104_AllCounties.csv',
  '20140805_AllCounties.csv'
)

statePrecinctFiles <- c(
  '20141104_AllStatePrecincts.csv',
  '20140805_AllStatePrecincts.csv'
)

kingCountyPrecinctFiles <- c(
  '20141104_King-precinct-results.csv',
  '20140805_King-precinct-results.csv'
)

electionTypes <- c(
  'general',
  'primary'
)

# the Washington Secretary of State's 2016 precinct shapefile is a convenient place to grab the county abbreviation-name lookup
countyCodes <- read_sf('/opt/data/washington/elections/statewide-precincts/2016/Statewide_Prec_2016.shp') %>%
  st_set_geometry(NULL) %>%
  select(County=COUNTY, CountyCode=COUNTYCODE) %>% distinct()

# this shapefile had to be assembled (for 2014) from individual county files
# see create-2014-precinct-shapefile.R in this directory
precinctCodes <- read_sf('/opt/data/washington/elections/statewide-precincts/2014-constructed/Statewide_Prec_2014.shp') %>%
  st_set_geometry(NULL) %>%
  mutate(PrecinctCode=as.integer(Code)) %>%
  select(CountyCode=County, PrecinctCode, PrecinctName=Name)

cleanOffice <- function(Race) {
  gsub(x=Race, pattern='Legislative District [0-9]+ \\- State (.+)(?: Pos.*)?', replacement='State \\1', perl=TRUE) %>%
    gsub(x=., pattern='(Pos. [12])', replacement='(\\1)') %>%
    gsub(x=., pattern='State Representative 2', replacement='State Representative (Pos. 2)') %>%
    gsub(x=., pattern='.*Congressional District ([0-9]+).*', replacement='U.S. Representative') %>%
    trimws()
}

cleanParty <- function(Party) {
  gsub(x=Party, pattern='\\(Prefers (.+) Party\\)', replacement='\\1') %>%
    gsub(x=., pattern='\\(?States No.+Prefer.+', replacement='States No Party Preference') %>%
    gsub(x=., pattern='(.+) Party Nomine.+', replacement='\\1') %>%
    gsub(x=., pattern='&amp;', replacement='&') %>%
    gsub(x=., pattern='Indep.+ Dem.*', replacement='Independent Democrat') %>%
    gsub(x=., pattern='Independent R|Ind\\. Repub.+|Independent Rep(?: Party)?', replacement='Independent Republican', perl=TRUE) %>%
    gsub(x=., pattern='G\\.O\\.P\\.?', replacement='GOP') %>%
    gsub(x=., pattern='GOP/Independent', replacement='Independent GOP') %>%
    gsub(x=., pattern='DEM', replacement='Democratic') %>%
    gsub(x=., pattern='^Democrat$', replacement='Democratic') %>%
    gsub(x=., pattern='REPUBLICAN', replacement='Republican') %>%
    gsub(x=., pattern='INDEPENDENT', replacement='Independent') %>%
    gsub(x=., pattern='Dem/Working Fmly.+', replacement='Democratic / Working Family') %>%
    gsub(x=., pattern='Nonpartisan', replacement='Non Partisan') %>%
    trimws()
}

cleanDistrict <- function(Race) {
  case_when(
    grepl(x=Race, pattern='^Legislative District') ~ gsub(x=Race, pattern='Legislative District ([0-9]+) .+', replacement='\\1'),
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

countyDfs <- map2(countyFiles, electionTypes, processCountyFile)
names(countyDfs) <- extractElectionDate(countyFiles)

processPrecinctFile <- function(inputFileName, electionType=c('general', 'primary', 'special__general', 'special__primary')) {
  
  electionType <- match.arg(electionType)
  electionDate <- extractElectionDate(inputFileName)
  
  cdf <- suppressMessages(read_csv(paste0(SOURCE_DATA_DIR, inputFileName))) %>%
    mutate(office=cleanOffice(Race),
           district=cleanDistrict(Race)
    ) %>%
    select(-PrecinctName) %>%
    inner_join(countyCodes, by='CountyCode') %>%
    filter(PrecinctCode != -1) %>%
    filter(Votes != 0) %>%
    inner_join(precinctCodes, by=c('CountyCode', 'PrecinctCode')) %>%
    select(county=County, precinct_code=PrecinctCode, precinct=PrecinctName, office, district, candidate=Candidate, votes=Votes)
  
  cdf
  
}

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
    mutate(office=gsub(x=Race, pattern='Legislative District No\\. ([0-9]+) Representative Position No\\. ([12])', replacement='State Representative (Pos. \\2)'),
           office=gsub(x=office, pattern='Legislative District No\\. ([0-9]+) State Senator', replacement='State Senator'),
           office=gsub(x=office, pattern='US.+Congressional District.+', replacement='U.S. Representative'),
           district=case_when(
             grepl(x=Race, pattern='^Legislative District') ~ gsub(x=Race, pattern='Legislative District No\\. ([0-9]+).+', replacement='\\1'),
             grepl(x=Race, pattern='US Rep.+Congressional District') ~ gsub(x=Race, pattern='.+Congressional District No\\. ([0-9]+)', replacement='\\1'),
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
      party=='IDM' ~ 'Independent Dem',
      party=='Dcr' ~ 'Democratic',
      party=='INR' ~ 'Independent Republican',
      party=='NON' ~ 'Independent',
      party=='I' ~ 'Independent',
      party=='RSP' ~ 'Republican Spirit',
      party=='SA' ~ 'Socialist Altern',
      party=='NU' ~ 'National Union',
      party=='CIT' ~ 'Citizens',
      party=='WAW' ~ 'Work and Wealth',
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

