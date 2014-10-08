# openelections-data-wa

This data comes from a database dump of the database that drives the pre-2007 election results search at http://www.sos.wa.gov/elections/results\_search.aspx.

It was provided by email in a ZIP archive named 2014.04.07-Elections Dbase.zip

A representative of the Washington Office of the Secretary of State Elections Division provided this description:
 
> It looks like Results\_Candidate includes the total votes cast for that candidate in that election, while Results\_Vote breaks the votes out by individual county.
> 
> For a single-county contest (countywide or a district located entirely within a single county), the two tables would give the same numbers.  For a multi-county office (statewide or a district that crosses county lines), there will be several records in Results\_Vote for each record in Results\_Candidate, and the votes in Results\_Vote should sum to the total in Results\_Candidate.
> 
> Results\_VoteTotals gives the total number of votes cast for all the candidates listed (ignoring write-ins), for use in calculating the percentage vote for each candidate.

## Manual cleaning 

Some files needed adjustments in order to be parsed as delimited text and loaded into the temporary database.  In these cases, the uncleaned file is stored with the extension *orig.txt*.

### Results\_Office.txt

The encoding had to be converted from latin-1 to utf-8.

```
iconv -f iso-8859-1 -t utf-8 --output raw/Results_Office.txt raw/
Results_Office.orig.txt

```

Second, a few lines in Results\_Office.txt had an an incorrectly placed line break.

* Line 301 was appended to line 300
* Line 303 was appended to line 302
* Lines 339 and 340 were appended to line 338
* Lines 342 and 343 were appended to line 341
* Line 440 was appended to line 439
* Line 442 was appended to line 441
* Line 444 was appended to line 443
* Line 446 was appended to line 445
* Line 448 was appended to line 447
* Line 589 was appended to line 588

### Results\_Candidate.txt

* Line 15840 (record with CandidateID 15842) had an improperly quoted and delimited FirstName field.
* Line 16315 (record with CandidateID 16317) had an improperly quoted and delimited FirstName field.

### Results\_Election.txt

An updated file, sent to us on 2014-10-06, had the string "NULL" for the election type ID of two presidential primary elections. I replaced this with the ID value 2, to be consistent with the original file and the other records for presidential primary elections.

```
sed s/NULL/2/ raw/Results_Election.orig.txt > raw/Results_Election.txt
```

See https://github.com/openelections/openelections-data-wa/issues/2 for background on why we got an updated file.

## Processing

As the raw files appear to be a dump from a relational databse, I loaded the data into a SQLite database in order to denormalize it.

Running this command loads the data into the SQLite database ``openelexdata.db``:

```
./bin/processwa.py loaddata
```

To generate the CSVs, run the ``processwa.py createcsvs`` command:

```
./bin/processwa.py createcsvs
```

The data dump contains results for as far back as November 8, 1898.  However, in the interest of keeping this repository small, I have only created CSVs for elections starting in the year 2000.  To generate CSVs for all elections, use the ``--startyear`` option to the ``processwa.py createcsvs`` command:

```
./bin/processwa.py createcsvs --startyear 1898
```

## Fields

In general, I tried to keep the field names in the result CSVs consistent with the field namess in the data dump.  In the CSVs, however, the field names are all lowercase.

The result CSVs contain the following fields:

* firstname - The candidate's first name. This can also contain the middle name, initial or nickname of the candidate.
* lastname - The candidate's last name.
* ballotname - The candidate's full name as it appears on the ballot.
* partyname - The candidate's party.  In many cases these values appear to be codes or abbreviations and will likely need some research to interpret.
* partycode - The candidate's party code.  Like partyname, these values are difficult to interpret.
* officename - The name of the office for the election result.
* officeposition - The "position" of the office, often used to distinguish between judicial offices in the same district.  This field can also include specifications about ballot measures.
* measuretext - The text of a ballot measure.  This is empty for office results.
* reporting_level - Either "state" for contest-wide results or "county" for county-level results.
* jurisdiction - Either "Washington" for contest-wide results or the county name for county-level results.
* votes - Number of votes for the jurisdiction.  
