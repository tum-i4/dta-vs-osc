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
    parser.add_argument("-a", "--old",
        help="input directory of the old programs",
        required=True)
    parser.add_argument("-b", "--new",
        help="input directory of the new programs",
        required=True)

    args = parser.parse_args(argv)

    return args


def run(args):

    old_dir = args.old
    new_dir = args.new

    values = []

    for data_file in sorted(os.listdir(old_dir)):
        name_parts = data_file.split('.')
        if len(name_parts) == 2 and name_parts[1] == 'out':
            old_size = os.path.getsize(os.path.join(old_dir, data_file))
            new_size = os.path.getsize(os.path.join(new_dir, data_file))
            values.append(str(new_size/old_size))

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
