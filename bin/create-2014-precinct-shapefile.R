library(tidyverse)
library(sf)
library(stringr)

# Assembles a single statewide precinct shapefile for precincts in effect for the 2014 Federal (and state) elections in Washington

# For most years, the Washington Secretary of State assembles (and publishes) a single shapefile
# For whatever reason, in 2014, the SoS published a shapefile for each county

# The shapefiles can be downloaded here: https://www.sos.wa.gov/_assets/elections/research/2014.zip

# As is evident below, some county shapefiles need a little editing. All manual edits here were verified against results and county sources (eg, human-readable maps)

baseDir <- '/opt/data/washington/elections/statewide-precincts/2014/'

precincts2012 <- read_sf('/opt/data/washington/elections/statewide-precincts/2012/Statewide_Prec_2012.shp') %>%
  select(CountyCode, Code, Name)

counties <- list()

counties$AD <- read_sf(file.path(baseDir, 'AD_Prec_2013.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  mutate(PrecinctCode=gsub(x=VTDST10, pattern='01([0-9]+)', replacement='\\1')) %>%
  select(PrecinctCode, PrecinctName=NAME10)

counties$AS <- read_sf(file.path(baseDir, 'AS_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  mutate(PrecinctName=gsub(x=Name, pattern='^A([12])$', replacement='Asotin \\1') %>%
           gsub(x=., pattern='^C([1-8])$', replacement='Clarkston \\1') %>%
           gsub(x=., pattern='^H([1-7])$', replacement='Clarkston Heights \\1') %>%
           gsub(x=., pattern='^SC([12])$', replacement='South Clarkston \\1') %>%
           gsub(x=., pattern='^SN$', replacement='Swallows Nest') %>%
           gsub(x=., pattern='^WA$', replacement='West Asotin') %>%
           gsub(x=., pattern='^WC([1-3])$', replacement='West Clarkston \\1') %>%
           trimws()) %>%
  select(PrecinctName) %>%
  left_join(precincts2012 %>% filter(CountyCode=='AS') %>% st_set_geometry(NULL) %>% select(-CountyCode), by=c('PrecinctName'='Name')) %>%
  rename(PrecinctCode=Code)

counties$BE <- read_sf(file.path(baseDir, 'BE_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=DistrictNa) %>%
  mutate(PrecinctName=PrecinctCode)

counties$CH <- read_sf(file.path(baseDir, 'CH_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PRECINCT) %>%
  mutate(PrecinctName=PrecinctCode)

counties$CM <- read_sf(file.path(baseDir, 'CM_Prec_2013.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PREC_NO, PrecinctName=DISTRICT)

counties$CR <- read_sf(file.path(baseDir, 'CR_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PRECINCT) %>%
  mutate(PrecinctName=PrecinctCode)
  
counties$CU <- read_sf(file.path(baseDir, 'CU_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=CODE, PrecinctName=NAME)

counties$CZ <- read_sf(file.path(baseDir, 'CZ_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=Code, PrecinctName=Name)

counties$DG <- read_sf(file.path(baseDir, 'DG_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PRECINCT) %>%
  mutate(PrecinctName=PrecinctCode)

counties$FE <- precincts2012 %>% filter(CountyCode=='FE') %>%
  select(PrecinctCode=Code, PrecinctName=Name)

counties$FR <- read_sf(file.path(baseDir, 'FR_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=LABEL) %>%
  mutate(PrecinctCode=gsub(x=PrecinctName, pattern='Precinct ([0-9]+)', replacement='\\1'))

counties$GA <- read_sf(file.path(baseDir, 'GA_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PrecCode, PrecinctName=PrecName)

counties$GR <- read_sf(file.path(baseDir, 'GR_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=Code, PrecinctName=PrecinctNm)

counties$GY <- read_sf(file.path(baseDir, 'GY_Prec_2013.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PREC_NUM, PrecinctName=PREC_NAME) %>%
  mutate(
    PrecinctName=case_when(
      PrecinctCode=='801' ~ 'Ocean Shores Ward 1',
      PrecinctCode=='802' ~ 'Ocean Shores Ward 2',
      PrecinctCode=='803' ~ 'Ocean Shores Ward 3',
      PrecinctCode=='601' ~ 'Montesano Ward 1',
      PrecinctCode=='602' ~ 'Montesano Ward 2',
      PrecinctCode=='603' ~ 'Montesano Ward 3',
      PrecinctCode=='301' ~ 'Elma Ward 1',
      PrecinctCode=='302' ~ 'Elma Ward 2',
      PrecinctCode=='141' ~ 'Aberdeen Ward 4-1',
      PrecinctCode=='142' ~ 'Aberdeen Ward 4-2',
      TRUE ~ PrecinctName
    )
  )

counties$IS <- read_sf(file.path(baseDir, 'IS_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PrecNo, PrecinctName=PrecinctNa)

counties$JE <- read_sf(file.path(baseDir, 'JE_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PRECINCTS) %>%
  mutate(PrecinctCode=as.character(PrecinctCode)) %>%
  left_join(precincts2012 %>% filter(CountyCode=='JE') %>% st_set_geometry(NULL) %>% select(-CountyCode), by=c('PrecinctCode'='Code')) %>%
  rename(PrecinctName=Name)

counties$KI <- read_sf(file.path(baseDir, 'KI_Prec_2014Gen.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=votdst, PrecinctName=NAME)

counties$KP <- read_sf(file.path(baseDir, 'KP_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=DISTRICT, PrecinctName=DESCR) %>%
  mutate(PrecinctName=case_when(
    grepl(x=PrecinctName, pattern='^(BREMERTON)$') ~ paste0(PrecinctName, ' ', PrecinctCode),
    grepl(x=PrecinctName, pattern='^(POULSBO)$') ~ paste0(PrecinctName, ' ', gsub(x=PrecinctCode, pattern='40([0-9])', replacement='\\1')),
    grepl(x=PrecinctName, pattern='^(PORT ORCHARD)$') ~ paste0(PrecinctName, ' ', gsub(x=PrecinctCode, pattern='20*([1-9][0-9]*)', replacement='\\1')),
    TRUE ~ PrecinctName
  )) %>%
  mutate(PrecinctCode=paste0('100', str_pad(PrecinctCode, 3, 'left', '0')))

counties$KS <- read_sf(file.path(baseDir, 'KS_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=precinct_1, PrecinctName=precinct_n) %>%
  mutate(PrecinctCode=gsub(x=as.character(PrecinctCode), pattern='37([0-9]{2})', replacement='\\1'))

counties$KT <- read_sf(file.path(baseDir, 'KT_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=PRECINCT) %>%
  mutate(PrecinctName=gsub(x=PrecinctName, pattern='(.+) [0-9]{3}$', replacement='\\1')) %>%
  left_join(precincts2012 %>% filter(CountyCode=='KT') %>% st_set_geometry(NULL) %>% select(-CountyCode), by=c('PrecinctName'='Name')) %>%
  rename(PrecinctCode=Code)

counties$LE <- read_sf(file.path(baseDir, 'LE_Prec_2013Dec.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=PRECINCT) %>%
  mutate(PrecinctName=gsub(x=PrecinctName, pattern='PeEll', replacement='Pe Ell')) %>%
  left_join(precincts2012 %>% filter(CountyCode=='LE') %>% st_set_geometry(NULL) %>% select(-CountyCode), by=c('PrecinctName'='Name')) %>%
  rename(PrecinctCode=Code) %>%
  mutate(PrecinctCode=case_when(
    grepl(x=PrecinctName, pattern='^Napavine') ~ paste0('50', gsub(x=PrecinctName, pattern='^Napavine ([1-6])$', replacement='\\1')),
    TRUE ~ PrecinctCode
  ))

counties$LI <- read_sf(file.path(baseDir, 'LI_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=Label, PrecinctName=Name)

counties$MA <- read_sf(file.path(baseDir, 'MA_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=ELECPREC, PrecinctName=NAME)

counties$OK <- read_sf(file.path(baseDir, 'OK_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PREC, PrecinctName=PRECINCT)

counties$PA <- read_sf(file.path(baseDir, 'PA_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=P_NAME) %>%
  mutate(PrecinctCode=gsub(x=PrecinctName, pattern='.+ ([0-9]+)$', replacement='\\1'))

counties$PE <- read_sf(file.path(baseDir, 'PE_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=PRECINCT_I) %>%
  mutate(PrecinctName=gsub(x=PrecinctName, pattern='IoneWest', replacement='Ione West') %>%
           gsub(x=., pattern='Tiger', replacement='Tiger Dry Canyon')) %>%
  left_join(precincts2012 %>% filter(CountyCode=='PE') %>% st_set_geometry(NULL) %>% select(-CountyCode), by=c('PrecinctName'='Name')) %>%
  rename(PrecinctCode=Code)

counties$PI <- suppressWarnings(read_sf(file.path(baseDir, 'PI_Prec_2014.shp'))) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PRECINCT, PrecinctName=NAME) %>%
  # these are malformed polygons that, for whatever reason, are in the pierce county file
  # there are second multipolygon sets in the file for these precincts that have the "real" properly formed polygon
  # so it's safe just to delete them
  filter(!(row_number() %in% c(368,437,477)))

counties$SJ <- read_sf(file.path(baseDir, 'SJ_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=CODE, PrecinctName=NAME)

counties$SK <- read_sf(file.path(baseDir, 'SK_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PREC_NO, PrecinctName=PRECINCT)

counties$SM <- read_sf(file.path(baseDir, 'SM_Prec_2013.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PRECNCT_NO, PrecinctName=PRECNCT_NM)

counties$SN <- read_sf(file.path(baseDir, 'SN_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PRECINCT, PrecinctName=PRECINCT_N)

counties$SP <- read_sf(file.path(baseDir, 'SP_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=PrecNum) %>% mutate(PrecinctName=PrecinctCode)

counties$ST <- read_sf(file.path(baseDir, 'ST_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  mutate(PrecinctCode=gsub(x=St_Code, pattern='ST(.+)', replacement='\\1')) %>%
  select(PrecinctName=PRECINCT, PrecinctCode)

counties$TH <- read_sf(file.path(baseDir, 'TH_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=PR_NAME, PrecinctCode=PR_NUM)

counties$WK <- read_sf(file.path(baseDir, 'WK_Prec_2012.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=Precint, PrecinctCode=Pre_Num)

counties$WL <- read_sf(file.path(baseDir, 'WL_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=PRECINCT) %>%
  left_join(precincts2012 %>% filter(CountyCode=='WL') %>% st_set_geometry(NULL) %>% select(-CountyCode), by=c('PrecinctName'='Name')) %>%
  rename(PrecinctCode=Code)

counties$WM <- read_sf(file.path(baseDir, 'WM_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctCode=NAME) %>%
  left_join(precincts2012 %>% filter(CountyCode=='WM') %>% st_set_geometry(NULL) %>% select(-CountyCode), by=c('PrecinctCode'='Code')) %>%
  rename(PrecinctName=Name) %>%
  mutate(PrecinctName=case_when(PrecinctCode=='268' ~ 'BELLINGHAM 268', TRUE ~ PrecinctName))

counties$WT <- precincts2012 %>% filter(CountyCode=='WT') %>%
  select(PrecinctCode=Code, PrecinctName=Name)

counties$YA <- read_sf(file.path(baseDir, 'YA_Prec_2014.shp')) %>%
  st_transform(st_crs(precincts2012)) %>%
  select(PrecinctName=CODE) %>%
  mutate(PrecinctCode=PrecinctName)

counties <- imap(counties, function(tdf, nm) {
  tdf %>% mutate(CountyCode=nm)
})

statewide_precincts <- purrr::reduce(counties, sf:::rbind.sf) %>%
  rename(Code=PrecinctCode, Name=PrecinctName, County=CountyCode)

statewide_precincts %>%
  write_sf('/opt/data/washington/elections/statewide-precincts/2014-constructed/Statewide_Prec_2014.shp')
