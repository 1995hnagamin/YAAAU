#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse

from html_message import *
from mail import *
from aoj_submit import *


def aoj_help(args):
    print """\
YAAU v0.0 - Yet Another AOJ Utilities
usage: aizu <subcommands>

available subcommands are:
    help\tshow help
    submit\tsubmit solution file to AOJ
"""


parser = argparse.ArgumentParser(description='Yet Another AOJ Utilities')
subparsers = parser.add_subparsers(help='help', title='subcommands')

parser_submit = subparsers.add_parser('submit', help='submit solution file to AOJ')
parser_submit.add_argument('filename', help='filename of solution')
parser_submit.add_argument('-p', '--problem', required=True, help='problem ID (required)')
parser_submit.add_argument('-l', '--language', help='programming language')
parser_submit.set_defaults(func=aoj_submit)

parser_help = subparsers.add_parser('help', help='show help and exit')
parser_help.set_defaults(func=aoj_help)

parser.add_argument('-v', '--version', action='version', version='YAAU v0.0')

args = parser.parse_args()
args.func(args)

