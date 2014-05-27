import csv
from datetime import datetime
import os.path

from playhouse.csv_loader import load_csv

from openelexdata.us.wa.models import (Party, County, ElectionType, Election,
    OfficeType, Office, Candidate, Vote, VoteTotal, create_tables,
    result_fieldnames)


def raw_filename(model_cls):
    model_name = model_cls.__name__
    if model_name == "VoteTotal":
        model_name += "s"

    return "Results_{}.txt".format(model_name)

def loaddata(datadir):
    create_tables()

    model_classes = [Party, County, ElectionType, Election, OfficeType, Office,
        Candidate, Vote, VoteTotal]
    reader_kwargs = {
        'delimiter': '\t',
    }
    for model_cls in model_classes:
        infilename = os.path.join(datadir, raw_filename(model_cls))
        load_csv(model_cls, os.path.join(datadir, infilename),
            has_header=True, **reader_kwargs)

def createcsvs(start_year):
    elections = Election.select().where(Election.electiondate >= datetime(start_year, 1, 1)).order_by(Election.electiondate)
           
    for elec in elections:
        create_csv_for_election(elec)

def create_csv_for_election(elec):
    with open(elec.standardized_filename, "wb") as f:
        writer = csv.DictWriter(f, result_fieldnames)
        writer.writeheader()
        for result in elec.results():
            writer.writerow(result)
