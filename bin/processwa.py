#!/usr/bin/env python

import argparse
import os
import os.path

from openelexdata.us.wa import loaddata

thisdir = os.path.dirname(os.path.realpath(__file__))
project_root = os.path.abspath(os.path.join(thisdir, os.pardir)) 

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process raw Washington "
           "election result data")
    subparsers = parser.add_subparsers(dest='subparser_name')
    parser_loaddata = subparsers.add_parser('loaddata', help="Load raw data into a database")
    parser_loaddata.add_argument("--datadir", help="Directory containing raw data",
        default=os.path.join(project_root, "raw"))
    args = parser.parse_args()

    if args.subparser_name == 'loaddata':
        loaddata(args.datadir)
