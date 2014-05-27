import os.path

from playhouse.csv_loader import load_csv

from openelexdata.us.wa.models import (Party, County, ElectionType, Election,
    OfficeType, Office, Candidate, Vote, VoteTotal, create_tables)


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
#        'doublequote': True,
    }
    for model_cls in model_classes:
        infilename = os.path.join(datadir, raw_filename(model_cls))
        load_csv(model_cls, os.path.join(datadir, infilename),
            has_header=True, **reader_kwargs)
