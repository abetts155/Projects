import enum
import os
import subprocess


class Colors(enum.StrEnum):
    BLACK = enum.auto()
    BLUE = enum.auto()
    CYAN = enum.auto()
    RED = enum.auto()
    WHITE = enum.auto()
    YELLOW = enum.auto()


class Styles(enum.StrEnum):
    SOLID = enum.auto()
    DOTTED = enum.auto()
    DASHED = enum.auto()
    BOLD = enum.auto()
    FILLED = enum.auto()


class HTML:
    open_html = '<'
    open_table = '<TABLE BORDER="0">'
    open_row = '<TR>'
    close_table = '</TABLE>'
    close_row = '</TR>'
    close_cell = '</TD>'
    close_html = '>'

    @staticmethod
    def open_cell(color=Colors.WHITE, border=0, column_span=1):
        return f'<TD ALIGN="LEFT" BGCOLOR="{color}" BORDER="{border}" COLSPAN="{column_span}">'


class DotError(Exception):
    pass


def _launch_dot(dot_filename, png_filename):
    with open(png_filename, 'w') as png_file:
        cmd = ['dot', '-T', 'png', dot_filename]
        process = subprocess.Popen(cmd, stdout=png_file)
        process.communicate()
        if process.returncode != 0:
            raise DotError(f"Failure when running '{cmd}'")


def generate(graph_data: list[str], filename: str):
    dot_filename = f'{filename}.dot'
    with open(dot_filename, 'w') as dot_file:
        dot_file.write('digraph\n')
        dot_file.write('{\n')
        dot_file.write('nslimit=2;\n')
        dot_file.write('ordering=out;\n')
        dot_file.write('ranksep=0.3;\n')
        dot_file.write('nodesep=0.25;\n')
        dot_file.write('fontsize=4;\n')
        dot_file.write('fontname="Arial"\n')
        for line in graph_data:
            dot_file.write(line)
        dot_file.write('}\n')

    png_filename = f'{filename}.png'
    _launch_dot(dot_filename, png_filename)
    os.remove(dot_filename)
