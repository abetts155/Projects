#!/usr/bin/env python

import argparse
import re
from matplotlib import pyplot

int_regex   = re.compile(r"\d+")
float_regex = re.compile(r"\d+\.\d+")

def make_constraints_graph(data_points):
    x_axis                 = []
    y_axis_cfg             = []
    y_axis_super_block_cfg = []
    for data_point in data_points:
        x_axis.append(data_point.cfg_size)
        y_axis_cfg.append(data_point.cfg_constraints)
        y_axis_super_block_cfg.append(data_point.super_block_cfg_constraints)
     
    pyplot.plot(x_axis, y_axis_cfg, '-', label="ILP constructed from CFG")
    pyplot.plot(x_axis, y_axis_super_block_cfg, '-', label="ILP constructed from super block CFG")
    pyplot.xlabel("Number of vertices in CFG")
    pyplot.ylabel("Number of constraints in ILP")
    pyplot.legend(loc=2)
    pyplot.savefig("constraints.png")
    pyplot.clf()

def make_variables_graph(data_points):
    x_axis                 = []
    y_axis_cfg             = []
    y_axis_super_block_cfg = []
    for data_point in data_points:
        x_axis.append(data_point.cfg_size)
        y_axis_cfg.append(data_point.cfg_variables)
        y_axis_super_block_cfg.append(data_point.super_block_cfg_variables)
     
    pyplot.plot(x_axis, y_axis_cfg, '-', label="ILP constructed from CFG")
    pyplot.plot(x_axis, y_axis_super_block_cfg, '-', label="ILP constructed from super block CFG")
    pyplot.xlabel("Number of vertices in CFG")
    pyplot.ylabel("Number of variables in ILP")
    pyplot.legend(loc=2)
    pyplot.savefig("variables.png")
    pyplot.clf()

def make_min_time_graph(data_points):
    x_axis                 = []
    y_axis_cfg             = []
    y_axis_super_block_cfg = []
    for data_point in data_points:
        x_axis.append(data_point.cfg_size)
        y_axis_cfg.append(data_point.cfg_min_time)
        y_axis_super_block_cfg.append(data_point.super_block_cfg_min_time)
     
    pyplot.plot(x_axis, y_axis_cfg, '-', label="ILP constructed from CFG")
    pyplot.plot(x_axis, y_axis_super_block_cfg, '-', label="ILP constructed from super block CFG")
    pyplot.xlabel("Number of vertices in CFG")
    pyplot.ylabel("Minimum execution time to solve ILP (in seconds)")
    pyplot.legend(loc=2)
    pyplot.savefig("min_times.png")
    pyplot.clf()

def make_max_time_graph(data_points):
    x_axis                 = []
    y_axis_cfg             = []
    y_axis_super_block_cfg = []
    for data_point in data_points:
        x_axis.append(data_point.cfg_size)
        y_axis_cfg.append(data_point.cfg_max_time)
        y_axis_super_block_cfg.append(data_point.super_block_cfg_max_time)
     
    pyplot.plot(x_axis, y_axis_cfg, '-', label="ILP constructed from CFG")
    pyplot.plot(x_axis, y_axis_super_block_cfg, '-', label="ILP constructed from super block CFG")
    pyplot.xlabel("Number of vertices in CFG")
    pyplot.ylabel("Maximum execution time to solve ILP (in seconds)")
    pyplot.legend(loc=2)
    pyplot.savefig("max_times.png")
    pyplot.clf()

def make_average_time_graph(data_points):
    x_axis                 = []
    y_axis_cfg             = []
    y_axis_super_block_cfg = []
    for data_point in data_points:
        x_axis.append(data_point.cfg_size)
        y_axis_cfg.append(data_point.cfg_average_time)
        y_axis_super_block_cfg.append(data_point.super_block_cfg_average_time)
     
    pyplot.plot(x_axis, y_axis_cfg, '-', label="ILP constructed from CFG")
    pyplot.plot(x_axis, y_axis_super_block_cfg, '-', label="ILP constructed from super block CFG")
    pyplot.xlabel("Number of vertices in CFG")
    pyplot.ylabel("Average execution time to solve ILP (in seconds)")
    pyplot.legend(loc=2)
    pyplot.savefig("average_times.png")
    pyplot.clf()

class DataPoint:
    def __init__(self, filename, cfg_size):
        self.filename                     = filename
        self.cfg_size                     = cfg_size
        self.cfg_variables                = 0
        self.super_block_cfg_variables    = 0
        self.cfg_constraints              = 0
        self.super_block_cfg_constraints  = 0
        self.cfg_min_time                 = 0.0
        self.cfg_max_time                 = 0.0
        self.cfg_average_time             = 0.0
        self.super_block_cfg_min_time     = 0.0
        self.super_block_cfg_max_time     = 0.0
        self.super_block_cfg_average_time = 0.0
        
    def __str__(self):
        return """
CFG variables                = %d
CFG constraints              = %d
CFG min time                 = %f
CFG max time                 = %f
CFG average time             = %f
Super block CFG variables    = %d
Super block CFG constraints  = %d
Super block CFG min time     = %f
Super block CFG max time     = %f
Super block CFG average time = %f""" % \
(self.cfg_variables,
self.cfg_constraints,
self.cfg_min_time,
self.cfg_max_time,
self.cfg_average_time,
self.super_block_cfg_variables,
self.super_block_cfg_constraints,
self.super_block_cfg_min_time,
self.super_block_cfg_max_time,
self.super_block_cfg_average_time)        
        
def parse_file(data_point):
    with open(data_point.filename) as the_file:
        number_of_cfgs = 0
        for line in the_file:
            if re.match(r"Function", line):
                number_of_cfgs += 1
            if re.match(r"variables.*[0-9]+", line):
                variables = int_regex.findall(line)[0]
                if number_of_cfgs % 2 == 0:
                    data_point.super_block_cfg_variables += int(variables)
                else:
                    data_point.cfg_variables += int(variables)
            if re.match(r"constraints.*[0-9]+", line):
                constraints = int_regex.findall(line)[0]
                if number_of_cfgs % 2 == 0:
                    data_point.super_block_cfg_constraints += int(constraints)
                else:
                    data_point.cfg_constraints += int(constraints)
            if re.match(r"min.*", line):
                time = float_regex.findall(line)[0]
                if number_of_cfgs % 2 == 0:
                    data_point.super_block_cfg_min_time += float(time)
                else:
                    data_point.cfg_min_time += float(time)
            if re.match(r"max.*", line):
                time = float_regex.findall(line)[0]
                if number_of_cfgs % 2 == 0:
                    data_point.super_block_cfg_max_time += float(time)
                else:
                    data_point.cfg_max_time += float(time)
            if re.match(r"average.*", line):
                time = float_regex.findall(line)[0]
                if number_of_cfgs % 2 == 0:
                    data_point.super_block_cfg_average_time += float(time)
                else:
                    data_point.cfg_average_time += float(time)
        number_of_cfgs /= 2
        data_point.cfg_min_time /= number_of_cfgs
        data_point.cfg_max_time /= number_of_cfgs
        data_point.cfg_average_time /= number_of_cfgs
        data_point.super_block_cfg_min_time /= number_of_cfgs
        data_point.super_block_cfg_max_time /= number_of_cfgs
        data_point.super_block_cfg_average_time /= number_of_cfgs
        
def collect_data_points(files):
    data_points = []
    for filename in sorted(files):
        cfg_size = int_regex.findall(filename)[0]
        data_points.append(DataPoint(filename, cfg_size))
    return data_points

def the_command_line():
    parser = argparse.ArgumentParser(description="Plot the results of ILP runs")
    
    parser.add_argument("files",
                        nargs="+",
                        help="the result files")
    
    return parser.parse_args()

if __name__ == "__main__":
    args        = the_command_line()
    data_points = collect_data_points(args.files)
    for data_point in data_points:
        parse_file(data_point)
    make_constraints_graph(data_points)
    make_variables_graph(data_points)
    make_min_time_graph(data_points)
    make_max_time_graph(data_points)
    make_average_time_graph(data_points)    
    
        