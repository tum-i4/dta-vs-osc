#!/usr/bin/env python3

# note: there most likely exist unused imports which haven't been removed as they have no effects on the generated chart

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

# disable warning about too many figures open
plt.rcParams.update({'figure.max_open_warning': 0})

def parse_args(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input",
        help="input file with the data",
        required=True)
    parser.add_argument("-o", "--output",
        help="output directory of the chart",
        required=True)

    args = parser.parse_args(argv)

    return args

# adapted from https://matplotlib.org/3.1.1/gallery/lines_bars_and_markers/barchart.html
def generate_chart(program_names, old_times, new_times):
    figures = {}

    x = np.arange(len(program_names))
    width = 0.35

    fig, ax = plt.subplots()
    rects1 = ax.bar(x - width/2, old_times, width, label='original VirtSC')
    rects2 = ax.bar(x + width/2, new_times, width, label='updated VirtSC')

    plt.xticks(rotation=90)
    ax.set_ylabel('average execution time in seconds')
    ax.set_yscale('log')
    ax.set_xticks(x)
    ax.set_xticklabels(program_names)
    ax.legend()

    def autolabel(rects):
        for rect in rects:
            height = rect.get_height()

    autolabel(rects1)
    autolabel(rects2)

    fig.tight_layout()

    return fig


def run(args):

    input_file = args.input

    program_names = []
    old_times = []
    new_times = []

    with open(input_file, 'r') as reader:
        for line in reader.readlines():
            tokens = line.split()
            if len(tokens) != 3:
                continue
            program_names.append(tokens[0])
            old_times.append(float(tokens[1]))
            new_times.append(float(tokens[2]))

    print('[*] generate_chart')
    chart = generate_chart(program_names, old_times, new_times)
    if not chart:
        print('[-] generate_chart')
        return False

    output_dir = args.output

    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    print('[*] saving chart at {:s}'.format(output_dir))
    figure_path = os.path.join(output_dir, 'time_comparison.pdf')
    chart.savefig(figure_path)

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
