import sqlite3
import collections

from graphs import graph


class Table:
    def __init__(self, name, key, *args):
        self._name = name
        self._key = key
        self._columns = collections.OrderedDict()
        for column_name, data_type in args:
            self._columns[column_name] = data_type

    @property
    def name(self):
        return self._name

    def key(self, index):
        return self._key[index]

    def __iter__(self):
        for column_name, data_type in self._columns.items():
            yield column_name, data_type


class Database:
    def __init__(self, filename):
        self.__filename = filename
        self._wcet_table = Table('wcets',
                                 ['source_id', 'destination_id'],
                                 ('source_id', 'int'), ('destination_id', 'int'), ('value', 'int'))
        self._wfreq_table = Table('wfreqs',
                                  ['source_id', 'destination_id'],
                                  ('source_id', 'int'), ('destination_id', 'int'), ('value', 'int'))
        self._instrumentation_table = Table('instrumentation',
                                            ['source_id', 'destination_id'],
                                            ('source_id', 'int'), ('destination_id', 'int'), ('value', 'int'))
        self._tables = [self._wcet_table, self._wfreq_table, self._instrumentation_table]

    def __enter__(self):
        self.__connection = sqlite3.connect(self.__filename)
        self.__cursor = self.__connection.cursor()
        return self

    def __exit__(self, exception_type, exception_value, traceback):
        self.__connection.commit()
        self.__connection.close()

    def reset(self):
        for table in self._tables:
            self.__cursor.execute('DROP TABLE if exists {}'.format(table.name))
            self.__cursor.execute('CREATE TABLE {} ({})'.format(table.name,
                                                                ', '.join(['{} {}'.format(key, value) for key, value in table])))

    def add_wcet(self, program_point: graph.Vertex or graph.Edge, wcet: int):
        if isinstance(program_point, graph.Vertex):
            row = [program_point.id_, 0, wcet]
        else:
            assert isinstance(program_point, graph.Edge)
            row = [program_point.predecessor().id_, program_point.successor().id_, wcet]
        self.__cursor.execute('INSERT INTO {} VALUES (?, ?, ?)'.format(self._wcet_table.name), row)

    def add_wfreq(self, program_point: graph.Vertex or graph.Edge, wfreq: int):
        if isinstance(program_point, graph.Vertex):
            row = [program_point.id_, 0, wfreq]
        else:
            assert isinstance(program_point, graph.Edge)
            row = [program_point.predecessor().id_, program_point.successor().id_, wfreq]
        self.__cursor.execute('INSERT INTO {} VALUES (?, ?, ?)'.format(self._wfreq_table.name), row)

    def set_instrumentation(self, program_point: graph.Vertex or graph.Edge):
        if isinstance(program_point, graph.Vertex):
            row = [program_point.id_, 0, 1]
        else:
            assert isinstance(program_point, graph.Edge)
            row = [program_point.predecessor().id_, program_point.successor().id_, 1]
        self.__cursor.execute('INSERT INTO {} VALUES (?, ?, ?)'.format(self._instrumentation_table.name), row)

    def get_wcet(self, program_point: graph.Vertex or graph.Edge):
        if isinstance(program_point, graph.Vertex):
            key = [program_point.id_, 0]
        else:
            assert isinstance(program_point, graph.Edge)
            key = [program_point.predecessor().id_, program_point.successor().id_]
        query = "SELECT * from {} where {}='{}' and {}='{}'".format(self._wcet_table.name,
                                                                    self._wcet_table.key(0),
                                                                    key[0],
                                                                    self._wcet_table.key(1),
                                                                    key[1])
        self.__cursor.execute(query)
        (row,) = self.__cursor.fetchall()
        return row[2]

    def get_wfreq(self, program_point: graph.Vertex or graph.Edge):
        if isinstance(program_point, graph.Vertex):
            key = [program_point.id_, 0]
        else:
            assert isinstance(program_point, graph.Edge)
            key = [program_point.predecessor().id_, program_point.successor().id_]
        query = "SELECT * from {} where {}='{}' and {}='{}'".format(self._wfreq_table.name,
                                                                    self._wfreq_table.key(0),
                                                                    key[0],
                                                                    self._wfreq_table.key(1),
                                                                    key[1])
        self.__cursor.execute(query)
        (row,) = self.__cursor.fetchall()
        return row[2]

    def is_instrumentation(self, program_point: graph.Vertex or graph.Edge):
        if isinstance(program_point, graph.Vertex):
            key = [program_point.id_, 0]
        else:
            assert isinstance(program_point, graph.Edge)
            key = [program_point.predecessor().id_, program_point.successor().id_]
        query = "SELECT * from {} where {}='{}' and {}='{}'".format(self._instrumentation_table.name,
                                                                    self._instrumentation_table.key(0),
                                                                    key[0],
                                                                    self._instrumentation_table.key(1),
                                                                    key[1])
        self.__cursor.execute(query)
        rows = self.__cursor.fetchall()
        return len(rows) != 0

