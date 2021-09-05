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
        help="input directory of the data",
        required=True)

    args = parser.parse_args(argv)

    return args


def run(args):

    input_dir = args.input

    data_list = []
    only_failures = True

    for data_file in os.listdir(input_dir):
        with open(os.path.join(input_dir, data_file), 'r') as reader:
            current_dict = {'attack_result': 'success'}
            for line in reader.readlines():
                data = line.replace(':', '').split()
                if data[0] == 'milliseconds':
                    data[0] = 'seconds'
                    data[1] = str(int(data[1]) / 1000)
                current_dict[data[0]] = data[1]
            data_list.append(current_dict)

    print('time: ', ', '.join([d['seconds'] for d in data_list]))
    print('instructions: ', ', '.join([d['instructions'] for d in data_list]))
    print('skipped: ', ', '.join([str(100*int(d['skipped'])/int(d['instructions'])) for d in data_list]))

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
