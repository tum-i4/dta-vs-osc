#!/usr/bin/env python3

# note: there most likely exist unused imports which haven't been removed as they have no effects on the collected data

from __future__ import print_function
import argparse
import os
import subprocess
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
import time
from typing import List, Dict, Any
# from adjustText import adjust_text

# disable warning about too many figures open
plt.rcParams.update({'figure.max_open_warning': 0})

def parse_args(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("-a", "--old",
        help="input directory of the old programs",
        required=True)
    parser.add_argument("-b", "--new",
        help="input directory of the new programs",
        required=True)
    parser.add_argument("-o", "--output",
        help="output path of the collected data",
        required=True)

    args = parser.parse_args(argv)

    return args


def run(args):

    old_dir = args.old
    new_dir = args.new

    program_names = []
    old_times = []
    new_times = []

    for data_file in sorted(os.listdir(old_dir)):
        name_parts = data_file.split('.')
        if len(name_parts) == 2 and name_parts[1] == 'out' and name_parts[0] not in ('2048', 'snake', 'tetris'):
            cmdline_path = '/home/sip/attacking_anti_tamper/evaluation/dataset/cmdline-args/' + name_parts[0]
            cmdline_args = []
            if os.path.exists(cmdline_path):
                with open(cmdline_path) as f:
                    cmdline_args = f.readline().split()
                    if '<' in cmdline_args:
                        continue

            print('[*] timing execution of {:s}'.format(name_parts[0]))
            program_names.append(name_parts[0])
            start_time = time.time()
            for i in range(10):
                subprocess.run([os.path.join(old_dir, data_file)] + cmdline_args, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
            end_time = time.time()
            old_times.append(round((end_time-start_time)/10, 6))
            start_time = time.time()
            for i in range(10):
                subprocess.run([os.path.join(new_dir, data_file)] + cmdline_args, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
            end_time = time.time()
            new_times.append(round((end_time-start_time)/10, 6))

    print('[*] saving data at {:s}'.format(args.output))
    with open(args.output, "w") as myfile:
        myfile.writelines([program_names[i] + ' ' + str(old_times[i]) + ' ' + str(new_times[i]) + '\n' for i in range(len(program_names))])

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
