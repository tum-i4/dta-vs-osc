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

# disable warning about too many figures open
plt.rcParams.update({'figure.max_open_warning': 0})

def parse_args(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input",
        help="input directory of the data",
        required=True)

    args = parser.parse_args(argv)

    return args


def run(args):

    input_dir = args.input

    print(r'\begin{table}[htpb]')
    print(r'  \centering')
    print(r'  \resizebox{\textwidth}{!}{')
    print(r'  \begin{tabular}{|c|c|c|c|c|c|c|c|}')
    print(r'    \hline')
    top_row_words = ['program', 'exception', 'bypassed', 'seconds', 'instructions', 'skipped', 'xsavec', 'diverged']
    top_row_formatted = [r'\textbf{'+word+'}' for word in top_row_words]
    print('      ', ' & '.join(top_row_formatted), r' \\')
    print(r'    \hline')

    for data_file in sorted(os.listdir(input_dir)):
        with open(os.path.join(input_dir, data_file), 'r') as reader:
            current_dict = {'attack_result': 'success'}
            for line in reader.readlines():
                data = line.replace(':', '').split()
                if data[0] in ('instructions', 'skipped', 'diverged'):
                    data[1] = format(int(data[1]), ',d')
                elif data[0] == 'milliseconds':
                    data[1] = '.'.join(format(int(data[1]), ',d').rsplit(',', 1)) # format as seconds using https://stackoverflow.com/a/59082116
                elif data[1] == 'yes':
                    data[1] = r'\cmark'
                elif data[1] == 'no':
                    data[1] = r'\xmark'
                current_dict[data[0]] = data[1]

            row = [data_file.split('.')[0].replace('_', r'\_'), current_dict['exception'], current_dict['bypassed'], current_dict['milliseconds'], current_dict['instructions'], current_dict['skipped'], current_dict['xsavec'], current_dict['diverged']]
            print('      ', ' & '.join(row), r' \\')
            print(r'    \hline')
    print(r'  \end{tabular}')
    print(r'  }')
    print(r'  \caption{All data collected when attacking the old version of VirtSC}\label{tab:all_data_attack}')
    print(r'\end{table}')

    return True


def main(argv):
    args = parse_args(argv)
    success = run(args)

    print('[{}] Done, {}'.format(
        success and '+' or '-',
        success and 'success' or 'failed'))
    return True

if __name__ == '__main__':
    if main(os.sys.argv[1:]) is not True:
        exit(1)
