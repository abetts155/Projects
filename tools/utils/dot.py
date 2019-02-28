import atexit
import os
import subprocess

from utils import messages


class Keywords:
    label = 'label'
    shape = 'shape'


class Shapes:
    record = 'record'


class Colors:
    black = 'black'
    blue = 'blue'
    cyan = 'cyan'
    lawn_green = 'lawngreen'
    light_grey = 'lightgrey'
    red = 'red'
    white = 'white'
    yellow = 'yellow'


class Styles:
    solid = 'solid'
    dotted = 'dotted'
    dashed = 'dashed'
    bold = 'bold'
    filled = 'filled'


class HTML:
    open_html = '<'
    open_table = '<TABLE BORDER="0">'
    open_row = '<TR>'

    @staticmethod
    def open_cell(color=Colors.white, border=0, column_span=1):
        return '<TD ALIGN="LEFT" BGCOLOR="{}" BORDER="{}" COLSPAN="{}">'.format(color, border, column_span)

    close_table = '</TABLE>'
    close_row = '</TR>'
    close_cell = '</TD>'
    close_html = '>'

    @staticmethod
    def dummy_row():
        return [HTML.open_row, HTML.open_cell(), HTML.close_cell, HTML.close_row]


child_processes = []


def generate(dot_filename, data):
    def launch_dot(ext):
        filename = os.path.splitext(dot_filename)[0] + '.' + ext
        messages.debug_message("Generating file '{}'".format(filename))
        try:
            with open(filename, 'w') as out_file:
                cmd = ["dot", "-T", ext, dot_filename]
                p = subprocess.Popen(cmd, stdout=out_file)
                child_processes.append(p)
                _, _ = p.communicate()
                if p.returncode != 0:
                    messages.error_message("Running '{}' failed".format(' '.join(cmd)))
                else:
                    messages.debug_message("Done with '{}'".format(' '.join(cmd)))
        except FileNotFoundError as e:
            messages.debug_message(e)

    if __debug__:
        with open(dot_filename, 'w') as dot_file:
            dot_file.write('digraph')
            dot_file.write('{\n')
            dot_file.write('nslimit=2;\n')
            dot_file.write('ordering=out;\n')
            dot_file.write('ranksep=0.3;\n')
            dot_file.write('nodesep=0.25;\n')
            dot_file.write('fontsize=8;\n')
            dot_file.write('fontname="Times new roman"\n')
            for d in data:
                dot_file.write(d)
            dot_file.write('}\n')

        launch_dot('png')
        os.remove(dot_filename)


def kill_child_processes():
    for p in child_processes:
        p.kill()


atexit.register(kill_child_processes)
