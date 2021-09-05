#!/usr/bin/env python3

# note: there most likely exist unused imports which haven't been removed as they have no effects on the generated output

from __future__ import print_function
import argparse
import os
import traceback
import tempfile
import shutil
import json
from glob import glob
from collections import namedtuple, defaultdict
from pprint import pformat, pprint
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import numbers
import math
from typing import List, Dict, Any
# from adjustText import adjust_text

def parse_args(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input",
        help="input file with the data",
        required=True)

    args = parser.parse_args(argv)

    return args


def run(args):

    input_file = args.input

    values = []

    with open(input_file, 'r') as reader:
        for line in reader.readlines():
            tokens = line.split()
            if len(tokens) != 3:
                continue
            values.append(str(float(tokens[2])/float(tokens[1])))

    print(', '.join(values))

    return True


def main(argv):
    args = parse_args(argv)
    success = run(args)

    # print('[*] intermediate results: {}'.format(build_dir))
    print('[{}] Done, {}'.format(
        success and '+' or '-',
        success and 'success' or 'failed'))
    return True

if __name__ == '__main__':
    if main(os.sys.argv[1:]) is not True:
        exit(1)
