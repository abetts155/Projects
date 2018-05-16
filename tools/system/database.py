import sqlite3

from graphs import vertices


class Table:
    def __init__(self, name):
        self._name = name
        self._key = ['source_id', 'destination_id']
        self._columns = {'source_id': 'int',
                         'destination_id': 'int',
                         'value': 'int'}

    @property
    def name(self):
        return self._name

    def key(self, index):
        return self._key[index]

    def __iter__(self):
        for column_name, data_type in self._columns.items():
            yield column_name, data_type


def get_key(v: vertices.ProgramPointVertex):
    if isinstance(v.program_point, vertices.Vertex):
        return [v.program_point.id_, 0]
    else:
        return [v.program_point.predecessor().id_, v.program_point.successor().id_]


class Database:
    def __init__(self, filename):
        self.__filename = filename
        self._wcet = Table('wcet')
        self._local_wfreq = Table('local_wfreq')
        self._global_wfreq = Table('global_wfreq')
        self._instrumentation = Table('instrumentation')
        self._tables = [self._wcet, self._local_wfreq, self._global_wfreq, self._instrumentation]

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

    def add_wcet(self, v: vertices.ProgramPointVertex, wcet: int):
        row = get_key(v)
        row.append(wcet)
        self.__cursor.execute('INSERT INTO {} VALUES (?, ?, ?)'.format(self._wcet.name), row)

    def add_local_wfreq(self, v: vertices.ProgramPointVertex, wfreq: int):
        row = get_key(v)
        row.append(wfreq)
        self.__cursor.execute('INSERT INTO {} VALUES (?, ?, ?)'.format(self._local_wfreq.name), row)

    def add_global_wfreq(self, v: vertices.ProgramPointVertex, wfreq: int):
        row = get_key(v)
        row.append(wfreq)
        self.__cursor.execute('INSERT INTO {} VALUES (?, ?, ?)'.format(self._global_wfreq.name), row)

    def set_instrumentation(self, v: vertices.ProgramPointVertex):
        row = get_key(v)
        row.append(1)
        self.__cursor.execute('INSERT INTO {} VALUES (?, ?, ?)'.format(self._instrumentation.name), row)

    def get_wcet(self, v: vertices.ProgramPointVertex):
        key = get_key(v)
        query = "SELECT * from {} where {}='{}' and {}='{}'".format(self._wcet.name,
                                                                    self._wcet.key(0),
                                                                    key[0],
                                                                    self._wcet.key(1),
                                                                    key[1])
        self.__cursor.execute(query)
        (row,) = self.__cursor.fetchall()
        return row[2]

    def get_local_wfreq(self, v: vertices.ProgramPointVertex):
        key = get_key(v)
        query = "SELECT * from {} where {}='{}' and {}='{}'".format(self._local_wfreq.name,
                                                                    self._local_wfreq.key(0),
                                                                    key[0],
                                                                    self._local_wfreq.key(1),
                                                                    key[1])
        self.__cursor.execute(query)
        (row,) = self.__cursor.fetchall()
        return row[2]

    def get_global_wfreq(self, v: vertices.ProgramPointVertex):
        key = get_key(v)
        query = "SELECT * from {} where {}='{}' and {}='{}'".format(self._global_wfreq.name,
                                                                    self._global_wfreq.key(0),
                                                                    key[0],
                                                                    self._global_wfreq.key(1),
                                                                    key[1])
        self.__cursor.execute(query)
        (row,) = self.__cursor.fetchall()
        return row[2]

    def is_instrumentation(self, v: vertices.ProgramPointVertex):
        key = get_key(v)
        query = "SELECT * from {} where {}='{}' and {}='{}'".format(self._instrumentation.name,
                                                                    self._instrumentation.key(0),
                                                                    key[0],
                                                                    self._instrumentation.key(1),
                                                                    key[1])
        self.__cursor.execute(query)
        rows = self.__cursor.fetchall()
        return len(rows) != 0

