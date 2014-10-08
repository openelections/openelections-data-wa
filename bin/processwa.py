#!/usr/bin/env python

import argparse
import logging
import os
import os.path

from openelexdata.us.wa import loaddata, createcsvs

logger = logging.getLogger()
logger.setLevel(logging.INFO)
handler = logging.StreamHandler()
handler.setLevel(logging.INFO)
logger.addHandler(handler)

thisdir = os.path.dirname(os.path.realpath(__file__))
project_root = os.path.abspath(os.path.join(thisdir, os.pardir)) 

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process raw Washington "
           "election result data")
    subparsers = parser.add_subparsers(dest='subparser_name')
    parser_loaddata = subparsers.add_parser('loaddata', help="Load raw data into a database")
    parser_loaddata.add_argument("--datadir", help="Directory containing raw data",
        default=os.path.join(project_root, "raw"))
    parser_createcsvs = subparsers.add_parser('createcsvs', help="Write CSV files "
        "from data")
    parser_createcsvs.add_argument('--startyear', help="Year to start writing result CSVs.",
        type=int, default=2000)
           
    args = parser.parse_args()

    if args.subparser_name == 'loaddata':
        loaddata(args.datadir)
    elif args.subparser_name == 'createcsvs':
        createcsvs(args.startyear)
