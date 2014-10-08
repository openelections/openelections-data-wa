import datetime

from peewee import (SqliteDatabase, Model, CharField,
    ForeignKeyField, BooleanField, IntegerField, DateField, 
    TextField)

db = SqliteDatabase('openelexdatawa.db')

class BaseModel(Model):

    class Meta:
        database = db


class IntegerIdModel(BaseModel):
    id = IntegerField(primary_key=True)


class Party(IntegerIdModel):
    partyname = CharField(null=True)
    partycode = CharField(null=True)


class County(IntegerIdModel):
    countyname = CharField()
    countycode = CharField()


class ElectionType(IntegerIdModel):
    electiontype = CharField()


class Election(IntegerIdModel):
    electiondate = DateField()
    description = CharField()
    electiontype = ForeignKeyField(ElectionType, null=True)

    @property
    def election_type(self):
        """
        The OpenElections election type.
        
        Either 'primary' or 'general'.
        """
        if self.electiontype is None:
            return self._election_type_from_description()

        if self.electiontype.electiontype == "Special":
            # Assume elections with type "Special" are general elections.
            return "general"

        return self.electiontype.electiontype.lower()

    def _election_type_from_description(self):
        if "Primary" in self.description:
            return "primary"
        else:
            return "general"

    @property
    def special(self):
        if (self.electiontype is not None and 
                self.electiontype.electiontype == "Special"):
            return True

        return "Special" in self.description

    @property
    def standardized_filename(self):
        bits = [
            self.election_datestamp(),
            'wa'
        ]

        if self.special:
            bits.append('special')

        bits.append(self.election_type)

        assert self.election_type is not None

        return "__".join(bits) + ".csv"

    def election_datestamp(self, sep=""):
        bits = [
            "{}".format(self.electiondate.year),
            "{:02d}".format(self.electiondate.month),
            "{:02d}".format(self.electiondate.day),
        ]
        return sep.join(bits)

    def results(self):
        results = []
        for candidate in self.candidates.select():
            results.extend(candidate.results())
        return results

    def parse_electiondate(self):
        """Parse date string into a date object and save the model"""
        formats = [
            '%m/%d/%Y',
            '%Y-%m-%d %H:%M:%S.%f',
        ]
        for fmt in formats:
            try:
                d = datetime.datetime.strptime(self.electiondate, fmt)
                self.electiondate = datetime.date(d.year, d.month, d.day)
                self.save()
                return self
            except ValueError:
                pass

        raise ValueError

    @classmethod
    def parse_electiondates(cls):
        for m in cls.select():
            try:
                year = m.electiondate.year
            except AttributeError:
                m.parse_electiondate()


class OfficeType(IntegerIdModel):
    officetype = CharField()
    displayorder = IntegerField()


class Office(IntegerIdModel):
    officename = CharField()
    officeposition = CharField(null=True)
    # Values are integer-like
    officekey = CharField(null=True)
    officetype = ForeignKeyField(OfficeType)
    displayorder = IntegerField(null=True)
    measuretext = TextField(null=True)


class Candidate(IntegerIdModel):
    firstname = CharField(null=True, help_text="Can also be empty in the case "
        "of Yes/No votes on ballot measures.")
            
    lastname = CharField(help_text="May also be set to 'Yes' or 'No' "
        "in the case of ballot measures.")
    party = ForeignKeyField(Party, related_name='candidates')
    election = ForeignKeyField(Election, related_name='candidates')
    office = ForeignKeyField(Office, related_name='candidates')
    ballotname = CharField()
    statement = BooleanField()
    ballotorder = IntegerField(null=True)
    candidatekey = CharField(null=True)
    votes = IntegerField(null=True)

    def result_dict(self):
        return {
            'firstname': self.firstname,
            'lastname': self.lastname,
            'ballotname': self.ballotname,
            'partyname': self.party.partyname,
            'partycode': self.party.partycode,
            'officename': self.office.officename,
            'officeposition': self.office.officeposition,
            'measuretext': self.office.measuretext,
            'reporting_level': 'state',
            'jurisdiction': "Washington",
            'votes': self.votes,
        }

    def results(self):
        results = [v.result_dict() for v in self.votes_by_county.select()]
        results.append(self.result_dict())
        return results


class Vote(BaseModel):
    candidate = ForeignKeyField(Candidate, related_name='votes_by_county')
    county = ForeignKeyField(County, related_name='county_votes')
    votes = IntegerField()

    def result_dict(self):
        result = self.candidate.result_dict()
        result.update({
            'reporting_level': 'county',
            'jurisdiction': self.county.countyname,
            'votes': self.votes,
        })
        return result


class VoteTotal(BaseModel):
    office = ForeignKeyField(Office)
    election = ForeignKeyField(Election)
    totalvotes = IntegerField()


def create_tables():
    """
    Create database tables for models.

    This will fail silently if tables already exist.
    """
    for cls in [Party, County, ElectionType, Election, OfficeType, Office,
            Candidate, Vote, VoteTotal]:
        cls.create_table(True)
    

result_fieldnames = [
    'firstname',
    'lastname',
    'ballotname',
    'partyname',
    'partycode',
    'officename',
    'officeposition',
    'measuretext',
    'reporting_level',
    'jurisdiction',
    'votes',
]
