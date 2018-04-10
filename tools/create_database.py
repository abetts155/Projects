import sys
import os
import argparse
import sqlite3
import random

from programs import program
from graphs import graph


def create(database):
    connection = sqlite3.connect(database)
    cursor = connection.cursor()
    cursor.executescript('''DROP TABLE if exists wcet;''')
    cursor.executescript('''DROP TABLE if exists wfreq;''')
    cursor.execute('''CREATE TABLE wcet (vertex int, value int)''')
    cursor.execute('''CREATE TABLE wfreq (vertex int, value int)''')
    return connection, cursor


def fill_database(prog, connection, cursor):
    for subprogram in prog:
        ppg = graph.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
        lnt = graph.LoopNests(ppg)
        for v in ppg:
            if isinstance(v.program_point, graph.Vertex):
                values = [str(v.id_), str(random.randint(0, 20))]
                cursor.execute('''INSERT INTO wcet VALUES (?, ?)''', values)
            if lnt.is_header(v):
                values = [str(v.id_), str(random.randint(5, 50))]
                cursor.execute('''INSERT INTO wfreq VALUES (?, ?)''', values)


def main(**kwargs):
    prog = program.IO.read(kwargs['filename'])
    database = os.path.abspath(kwargs['database'])
    connection, cursor = create(database)
    fill_database(prog, connection, cursor)
    connection.commit()
    connection.close()


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Create a database of values needed in the WCET calculation')

    parser.add_argument('--filename',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--database',
                        help='write the data to this file',
                        required=True)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
