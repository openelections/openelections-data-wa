from peewee import (SqliteDatabase, Model, CharField,
    ForeignKeyField, BooleanField, IntegerField, DateTimeField, 
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
    electiondate = DateTimeField()
    description = CharField()
    electiontype = ForeignKeyField(ElectionType, null=True)


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


class Vote(BaseModel):
    candidate = ForeignKeyField(Candidate, related_name='candidate_votes')
    county = ForeignKeyField(County, related_name='county_votes')
    votes = IntegerField()


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
    
