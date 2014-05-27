# openelections-data-wa

This data comes from a database dump of the database that drives the pre-2007 election results search at http://www.sos.wa.gov/elections/results\_search.aspx.

It was provided by email in a ZIP archive named 2014.04.07-Elections Dbase.zip

A representative of the Washington Office of the Secretary of State Elections Division provided this description:
 
> It looks like Results\_Candidate includes the total votes cast for that candidate in that election, while Results\_Vote breaks the votes out by individual county.
> 
> For a single-county contest (countywide or a district located entirely within a single county), the two tables would give the same numbers.  For a multi-county office (statewide or a district that crosses county lines), there will be several records in Results\_Vote for each record in Results\_Candidate, and the votes in Results\_Vote should sum to the total in Results\_Candidate.
> 
> Results\_VoteTotals gives the total number of votes cast for all the candidates listed (ignoring write-ins), for use in calculating the percentage vote for each candidate.

## Processing

As the raw files appear to be a dump from a relational databse, I loaded the data into a SQLite database in order to denormalize it.

Running this command loads the data into the SQLite database ``openelexdata.db``:

```
./bin/processwa loaddata
```

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
