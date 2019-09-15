library(tidyverse)
library(readxl)
library(rvest)

SOURCE_DATA_DIR = '../../openelections-sources-wa/2012/'
OUTPUT_DATA_DIR = '../2012/'

nonKing <- list.files(SOURCE_DATA_DIR, pattern='.+2012Gen.+\\.xlsx$', full.names=TRUE) %>%
  #head(10) %>%
  keep(function(f) {
    #grepl(x=f, pattern='Wahk')
    TRUE
  }) %>%
  map_dfr(function(x) {
    county <- gsub(x=basename(x), pattern='(.+) 2012Gen.+', replacement='\\1')
    writeLines(paste0('Processing: ', x))
    tdf <- read_excel(x, col_types='text', .name_repair='minimal')
    if ('Precinct Name' %in% colnames(tdf)) {
      tdf %>%
        mutate(county=county) %>%
        mutate(precinct=case_when(county=='Spokane' ~ `Prec Cd`, TRUE ~ `Precinct Name`)) %>%
        select(county,
               precinct,
               office=`Contest Title`,
               candidate=`Candidate Name`,
               cd=Cong_dist,
               ld=Leg_dist,
               votes=Votes)
    } else if (any(grepl('Precinct_[nN]ame', colnames(tdf)))) {
      cn <- colnames(tdf)
      dups <- which(grepl(x=cn, pattern='Precinct_[nN]ame'))
      dups <- tail(dups, -1)
      if (length(dups) > 0) {
        newCn <- cn[-dups]
        tdf <- tdf %>% set_names(paste0('x', seq_along(cn)))
        tdf <- tdf %>%
          select(-dups) %>%
          set_names(newCn)
      }
      tdf <- tdf %>%
        set_names(tolower(names(tdf))) %>%
        mutate(county=county) %>%
        select(county,
               precinct=precinct_name,
               office=contest_title,
               candidate=candidate_name,
               cd=cong_dist,
               ld=leg_dist,
               votes=total_votes, reg_voters) %>%
        mutate(precinct=as.character(precinct))
      tdf %>%
        select(-votes) %>%
        rename(votes=reg_voters) %>%
        group_by(county, precinct) %>%
        filter(row_number()==1) %>%
        mutate(office='REGISTERED VOTERS - TOTAL', candidate=office) %>%
        ungroup() %>%
        bind_rows(
          tdf %>% select(-reg_voters)
        )
    } else if ('PRECINCT_NAME' %in% colnames(tdf)) {
      tdf %>%
        mutate(county=county) %>%
        select(county,
               precinct=PRECINCT_NAME,
               office=CONTEST_FULL_NAME,
               candidate=CANDIDATE_FULL_NAME,
               cd=Cong_dist,
               ld=Leg_dist,
               votes=TOTAL) %>%
        mutate(precinct=as.character(precinct))
    } else {
      writeLines('Unrecognized format')
      NULL
    }
  })

king <- read_tsv(file.path(SOURCE_DATA_DIR, 'King 2012general-ecanvass.txt'), col_types=cols(.default=col_character())) %>%
  mutate(county='King') %>%
  select(county,
         precinct=Precinct,
         office=Race,
         candidate=CounterType,
         cd=CG,
         ld=LEG,
         votes=SumOfCount)

king <- king %>% filter(candidate != 'Registered Voters') %>%
  bind_rows(
    king %>%
      filter(candidate=='Registered Voters') %>%
      mutate(office='Registered Voters') %>%
      group_by(precinct) %>%
      filter(row_number()==1) %>%
      ungroup()
  ) %>%
  mutate(
    office=gsub(x=office, pattern='[ ]+nonpartisan office', replacement=''),
    office=gsub(x=office, pattern='[ ]+partisan office', replacement=''),
  )

statewideClean <- nonKing %>%
  bind_rows(king) %>%
  mutate(
    office=case_when(
      grepl(x=office, pattern='president|Pres & VP', ignore.case=TRUE) ~ 'U.S. President/Vice President',
      grepl(x=office, pattern='U.+Senator', ignore.case=TRUE) ~ 'U.S. Senator',
      grepl(x=office, pattern='U.+Rep|Congress', ignore.case=TRUE) ~ 'U.S. Representative',
      grepl(x=office, pattern='L.*t.+governor|lieut\\. gov', ignore.case=TRUE) ~ 'Lieutenant Governor',
      grepl(x=office, pattern='governor', ignore.case=TRUE) ~ 'Governor',
      grepl(x=office, pattern='ins.+comm(issioner)?', ignore.case=TRUE) ~ 'Insurance Commissioner',
      grepl(x=office, pattern='comm.+lands', ignore.case=TRUE) ~ 'Commissioner of Public Lands',
      grepl(x=office, pattern='treasurer', ignore.case=TRUE) & county=='Snohomish' ~ 'State Treasurer',
      grepl(x=office, pattern='st.+treasurer', ignore.case=TRUE) ~ 'State Treasurer',
      grepl(x=office, pattern='sec.+state', ignore.case=TRUE) ~ 'Secretary of State',
      grepl(x=office, pattern='superintendent|Public Instr', ignore.case=TRUE) ~ 'Superintendent of Public Instruction',
      grepl(x=office, pattern='attorn.+general|atty.+gen', ignore.case=TRUE) ~ 'Attorney General',
      grepl(x=office, pattern='auditor', ignore.case=TRUE) ~ 'State Auditor',
      grepl(x=office, pattern='Initiative', ignore.case=TRUE) & county=='San Juan' ~ paste0('Initiative ', gsub(x=office, pattern='Initiative[^0-9]+([0-9]+)$', replacement='\\1', ignore.case=TRUE)),
      grepl(x=office, pattern='Initiative', ignore.case=TRUE) ~ paste0('Initiative ', gsub(x=office, pattern='.*Initiative[^0-9]+([0-9]+)[^0-9]*', replacement='\\1', ignore.case=TRUE)),
      grepl(x=office, pattern='1185', ignore.case=TRUE) ~ 'Initiative 1185',
      grepl(x=office, pattern='1240', ignore.case=TRUE) ~ 'Initiative 1240',
      grepl(x=office, pattern='502', ignore.case=TRUE) ~ 'Initiative 502',
      grepl(x=office, pattern='ref.+meas|74', ignore.case=TRUE) ~ 'Referendum Measure 74',
      grepl(x=office, pattern='(supr|justice).+pos.+[0-9]', ignore.case=TRUE) ~ paste0('Supreme Court Justice Position ', gsub(x=office, pattern='.+pos.+([0-9]).*', replacement='\\1', ignore.case=TRUE)),
      grepl(x=office, pattern='supr|justice', ignore.case=TRUE) ~ 'Supreme Court Justice',
      grepl(x=office, pattern='st.+senator|senator.+legis', ignore.case=TRUE) ~ 'State Senator',
      grepl(x=office, pattern='Rep.+Pos(itio)? LEG') ~ 'State Representative',
      grepl(x=office, pattern='DIST.+REP.+pos.+[12]', ignore.case=TRUE) ~ paste0('State Representative Position ', gsub(x=office, pattern='.+pos.+([12]).*', replacement='\\1', ignore.case=TRUE)),
      grepl(x=office, pattern='rep.+pos.+[12]', ignore.case=TRUE) ~ paste0('State Representative Position ', gsub(x=office, pattern='.+pos.+([12]).*', replacement='\\1', ignore.case=TRUE)),
      grepl(x=office, pattern='sup.+judge', ignore.case=TRUE) ~ 'Superior Court Judge',
      grepl(x=office, pattern='822?3', ignore.case=TRUE) ~ 'Advisory Vote: SJR 8223',
      grepl(x=office, pattern='8221', ignore.case=TRUE) | grepl(x=office, pattern='Amendment to the State Constitution') ~ 'Advisory Vote: SJR 8221 (Constitutional Amendment)',
      grepl(x=office, pattern='2590', ignore.case=TRUE) ~ 'Advisory Vote: SHB 2590',
      grepl(x=office, pattern='6635', ignore.case=TRUE) ~ 'Advisory Vote: ESB 6635',
      grepl(x=office, pattern='advisory.+1', ignore.case=TRUE) ~ 'Advisory Vote 1: B&O Tax Deduction',
      grepl(x=office, pattern='advisory.+2', ignore.case=TRUE) ~ 'Advisory Vote 2: Petroleum Tax',
      grepl(x=office, pattern='PUD|Public Util.+Dist.+[123A]$', ignore.case=TRUE) ~ paste0('PUD Commissioner ', gsub(x=office, pattern='.+([123A])$', replacement='\\1', ignore.case=TRUE)),
      office=='State Representative' & grepl(x=candidate, pattern='Fagan') ~ 'State Representative Position 1',
      office=='State Representative' & grepl(x=candidate, pattern='Schmick') ~ 'State Representative Position 2',
      TRUE ~ office
    ),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Alexander') ~ 'State Representative Position 1', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Hartman') ~ 'State Representative Position 1', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='DeBolt') ~ 'State Representative Position 1', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Reykdal') ~ 'State Representative Position 1', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Griffey') ~ 'State Representative Position 1', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Haigh') ~ 'State Representative Position 1', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Wilcox') ~ 'State Representative Position 2', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Orcutt') ~ 'State Representative Position 2', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Morgan') ~ 'State Representative Position 2', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Hunt') ~ 'State Representative Position 2', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='MacEwen') ~ 'State Representative Position 2', TRUE ~ office),
    office=case_when(office=='State Representative' & grepl(x=candidate, pattern='Ring\\-Erickson') ~ 'State Representative Position 2', TRUE ~ office),
    office=case_when(grepl(x=office, pattern='Supreme Court Justice') & grepl(x=candidate, pattern='Gonzalez') ~ 'Supreme Court Justice Position 8', TRUE ~ office),
    office=case_when(grepl(x=office, pattern='Supreme Court Justice') & grepl(x=candidate, pattern='McCloud') ~ 'Supreme Court Justice Position 9', TRUE ~ office),
    office=case_when(grepl(x=office, pattern='Supreme Court Justice') & grepl(x=candidate, pattern='Owens') ~ 'Supreme Court Justice Position 2', TRUE ~ office),
    office=case_when(grepl(x=office, pattern='Supreme Court Justice') & grepl(x=candidate, pattern='Sanders') ~ 'Supreme Court Justice Position 2', TRUE ~ office),
    district=case_when(
      office=='U.S. Representative' ~ cd,
      grepl(x=office, pattern='State Rep|State Senator') ~ ld,
      TRUE ~ NA_character_
    ),
    candidate=gsub(x=candidate, pattern='^\\- (.+)', replacement='\\1'),
    candidate=trimws(gsub(x=candidate, pattern='(.+)\\((Prefers|States) .+', replacement='\\1')),
    candidate=case_when(grepl(x=candidate, pattern='Mckenna') ~ 'Rob McKenna', TRUE ~ candidate),
    candidate=case_when(grepl(x=candidate, pattern='randy.+dorn', ignore.case=TRUE) ~ 'Randy I. Dorn', TRUE ~ candidate),
    candidate=case_when(candidate=='Jim Mclntire' ~ 'Jim McIntire', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Linds[ae]y') ~ 'Peta Lindsay / Yari Osorio', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Johnson') ~ 'Gary Johnson / James P. Gray', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Harris') ~ 'James Harris / Alyson Kennedy', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Anderson') ~ 'Ross C. (Rocky) Anderson / Luis J. Rodriguez', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Stein') ~ 'Jill Stein / Cheri Honkala', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Goode') ~ 'Virgil Goode / James N. Clymer', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Obama') ~ 'Barack Obama / Joe Biden', TRUE ~ candidate),
    candidate=case_when(grepl(x=office, pattern='President/') & grepl(x=candidate, pattern='Romney') ~ 'Mitt Romney / Paul Ryan', TRUE ~ candidate),
  ) %>% select(-cd, -ld)

candidates <- read_csv(file.path(SOURCE_DATA_DIR, 'FiledCandidates.csv'), col_types=cols(.default=col_character())) %>%
  select(RaceName, BallotName, PartyName, IsPartisanOffice) %>%
  mutate(IsPartisanOffice=as.logical(IsPartisanOffice)) %>%
  filter(IsPartisanOffice)

candidatePartyLookup <- candidates %>%
  select(candidate=BallotName, party=PartyName) %>%
  mutate(party=gsub(x=party, pattern='.+Prefers (.+) Party.+', replacement='\\1'),
         party=gsub(x=party, pattern='(.+) Party Nominees', replacement='\\1'),
         party=gsub(x=party, pattern='\\(|\\)', replacement=''),
         party=trimws(party)
  ) %>%
  bind_rows(
    tribble(
      ~candidate, ~party,
      'Mark T. Davies', 'Republican',
      'Kevin Morrison', 'Republican',
      'Martin Metz', 'Republican',
      'Brandon Robinson', 'States No Party Preference'
    )
  ) %>% distinct()

# used to verify manually candidates/parties that don't match up
# statewideClean %>%
#  filter(grepl(x=office, pattern='Governor|Representative|Senator|Secretary of State|Public Land|State Audit|State Treas|President|Attorney General')) %>%
#  filter(!grepl(x=candidate, pattern='Write-[Ii]n|Uncertified|Times ')) %>%
#  select(candidate, office) %>% distinct() %>% anti_join(candidatePartyLookup, by='candidate') %>% View()

statewideClean <- left_join(statewideClean, candidatePartyLookup, by='candidate')

# clean up precinct codes etc.

precincts2012 <- read_sf('/opt/data/washington/elections/statewide-precincts/2012/Statewide_Prec_2012.shp') %>%
  mutate(Name=case_when(
    CountyCode=='PA' & Code=='022' ~ 'PIONEER 2',
    CountyCode=='PA' ~ gsub(x=Name, pattern='(.+) [0-9]+$', replacement='\\1') %>%
      trimws() %>%
      gsub(x=., pattern='(.+)\\-$', replacement='\\1') %>%
      trimws() %>%
      gsub(x=., pattern='/', replacement='-') %>%
      gsub(x=., pattern='#', replacement='') %>%
      toupper(),
    CountyCode=='PE' ~ toupper(Name),
    CountyCode=='SN' ~ toupper(Name),
    TRUE ~ Name
  ))

statewideClean <- statewideClean %>%
  mutate(precinctM=case_when(
    county=='Klickitat' ~ gsub(x=precinct, pattern='N ', replacement='North ') %>%
      gsub(x=., pattern='E ', replacement='East ') %>%
      gsub(x=., pattern='W ', replacement='West ') %>%
      gsub(x=., pattern='Mt ', replacement='MTN ') %>%
      gsub(x=., pattern='Hts', replacement='Heights') %>%
      gsub(x=., pattern='Aldercreek', replacement='Alder Creek') %>%
      toupper(),
    county=='Pacific' ~ gsub(x=precinct, pattern='South Bend ([123])[ ]?R$', replacement='South Bend \\1 Rural') %>%
      gsub(x=., pattern='Raymond W([23]) R', replacement='Raymond \\1 Rural') %>%
      gsub(x=., pattern='Raymond W([23])', replacement='Raymond Ward \\1 City') %>%
      gsub(x=., pattern='Raymond W1p([123])', replacement='Raymond Ward 1 Precinct \\1 City') %>%
      gsub(x=., pattern='South Bend ([123])$', replacement='South Bend Ward \\1 City') %>%
      gsub(x=., pattern='^Long Beach$', replacement='Long Beach City') %>%
      toupper(),
    county=='Pend Oreille' ~ gsub(x=precinct, pattern='/', replacement=' - '),
    county=='Pierce' ~ gsub(x=precinct, pattern='-', replacement=''),
    county=='Thurston' ~ gsub(x=precinct, pattern='(.+) [0-9\\.]+$', replacement='\\1') %>%
      gsub(x=., pattern='#|\'', replacement='') %>%
      gsub(x=., pattern='BUTLER COVER', replacement='BUTLER COVE') %>%
      gsub(x=., pattern='ST. CLAIR', replacement='ST CLAIR') %>%
      gsub(x=., pattern='(SPURGEON|BEAVER) CREEK', replacement='\\1 CRK'),
    TRUE ~ precinct
  )) %>%
  left_join(precincts2012 %>% select(County, Name, ShpCode=Code) %>% st_set_geometry(NULL), by=c('county'='County', 'precinctM'='Name')) %>%
  select(-precinctM) %>%
  mutate(precinct_code=case_when(
    county %in% c('Adams','Columbia','Garfield','Grant','Jefferson','Lewis') ~ gsub(x=precinct, pattern='^([0-9]+) .+', replacement='\\1'),
    county %in% c('Ferry','King','Klickitat','Pacific','Pend Oreille','Snohomish','Thurston') ~ ShpCode,
    county %in% c('Franklin') ~ gsub(x=precinct, pattern='^Pct ([0-9]+)$', replacement='\\1'),
    county=='Kitsap' ~ paste0('100', gsub(x=precinct, pattern='.+ ([0-9]{3})$', replacement='\\1')),
    county=='Okanogan' ~ gsub(x=precinct, pattern='^0+([1-9][0-9]+)[ \\-].+', replacement='\\1'),
    county=='Wahkiakum' ~ gsub(x=precinct, pattern='^([0-9]+) \\-.+', replacement='\\1'),
    county=='Walla Walla' ~ gsub(x=precinct, pattern='.+ ([0-9]{2})$', replacement='\\1'),
    county=='Whitman' ~ gsub(x=precinct, pattern='^([0-9]{4}) .+', replacement='\\1'),
    TRUE ~ precinct
  )) %>% select(-ShpCode)

statewideClean %>%
  select(county,precinct_code,precinct,office,district,party,candidate,votes) %>%
  write_csv(file.path(OUTPUT_DATA_DIR, '20121106__wa__general__precinct.csv'))

