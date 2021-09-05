#!/usr/bin/env python3

# note: there most likely exist unused imports which haven't been removed as they have no effects on the generated graph

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
    parser.add_argument("-o", "--output",
        help="output directory of the graph",
        required=True)

    args = parser.parse_args(argv)

    return args

def generate_graphs(report: Dict[str, List[Dict[str, Any]]], only_failures):
    figures = {}

    success_color = '#03fc3d'
    failure_color = '#fc6f03'
    colors = [success_color, failure_color] if not only_failures else [failure_color, success_color]
    cmap = mpl.colors.ListedColormap(colors)

    legend_descriptions = ['guard bypassed', 'no guard bypassed'] if not only_failures else ['no guard bypassed', 'guard bypassed']

    for config_name, config in report.items():
        x = []
        y = []
        z = []
        c = []


        fig = plt.figure(num=config_name, figsize=(20/2.54, 15/2.54))
        ax = fig.add_subplot(111,
            xscale='log',
            yscale='log',
            xlabel='number of instructions',
            ylabel='attack time in seconds')

        # collect maximum trace size in case we have a failed tracer/taint
        # entry that we need to put somewhere
        max_trace_size = -1
        max_attack_time = -1
        for result in config:
            trace_size = result.get('instructions') or -1
            if trace_size > max_trace_size:
                max_trace_size = trace_size
            attack_time = result.get('attack_time', 0)
            if attack_time > max_attack_time:
                max_attack_time = attack_time

        for result in config:
            x.append(result['instructions'])
            y.append(result['attack_time'])
            # z.append(result['checkers_patched'] / result['self_check_triggered'])
            c.append(colors.index(success_color) if result['attack_result'] == 'success'
                    else colors.index(failure_color))

        scatter = ax.scatter(x, y, c=c, cmap=cmap,
            alpha=0.5,
            edgecolors='black')
        handles = scatter.legend_elements()[0]
        ax.legend(handles=handles, labels=legend_descriptions)

        # add linear
        ax.plot([0,max_attack_time], [0,max_attack_time], linestyle='dotted', color='gray', label='linear scaling')

        figures[config_name + '_time'] = fig

    return figures


def run(args):

    input_dir = args.input

    data_list = []
    only_failures = True

    for data_file in os.listdir(input_dir):
        with open(os.path.join(input_dir, data_file), 'r') as reader:
            current_dict = {'attack_result': 'success'}
            for line in reader.readlines():
                data = line.replace(':', '').split()
                if data[0] in ('instructions', 'skipped', 'diverged'):
                    data[1] = int(data[1])
                elif data[0] == 'milliseconds':
                    data[0] = 'attack_time'
                    data[1] = int(data[1]) / 1000
                elif data[0] == 'exception' and data[1] == 'yes' or data[0] == 'bypassed' and data[1] == 'no':
                    current_dict['attack_result'] = 'failure'

                current_dict[data[0]] = data[1]
            if current_dict['attack_result'] == 'success':
                only_failures = False
            data_list.append(current_dict)

    report = {os.path.basename(args.input): data_list}

    print('[*] generate_graphs')
    graphs = generate_graphs(report, only_failures)
    if not graphs:
        print('[-] generate_graphs')
        return False

    output_dir = args.output # os.path.join(os.getcwd(), 'eval_figures')

    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    print('[*] saving figures at {:s}'.format(output_dir))
    for name, figure in graphs.items():
        figure_path = os.path.join(output_dir, name + '.pdf')
        figure.savefig(figure_path)

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
