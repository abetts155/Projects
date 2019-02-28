import sqlite3

from graphs import vertices


class Table:
    def __init__(self, name):
        self._name = name
        self._key = ['source_id', 'destination_id']
        self._columns = {'source_id': 'int',
                         'destination_id': 'int',
                         'value': 'int'}
        self._rows = []

    @property
    def name(self):
        return self._name

    @property
    def rows(self):
        return self._rows

    def key(self, index):
        return self._key[index]

    def __iter__(self):
        for column_name, data_type in self._columns.items():
            yield column_name, data_type


def get_key(v: vertices.ProgramPointVertex):
    if isinstance(v.program_point, vertices.Vertex):
        return [v.program_point.id_, None]
    else:
        return [v.program_point.predecessor().id_, v.program_point.successor().id_]


class Database:
    def __init__(self, filename):
        self.__filename = filename
        self._wcet = Table('wcet')
        self._local_wfreq = Table('local_wfreq')
        self._global_wfreq = Table('global_wfreq')
        self._tables = [self._wcet, self._local_wfreq, self._global_wfreq]

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

    def load_into_memory(self):
        for table in self._tables:
            query = "SELECT * from {}".format(table.name)
            self.__cursor.execute(query)
            table.rows.extend(self.__cursor.fetchall())

    def get_wcet(self, v: vertices.ProgramPointVertex):
        key = get_key(v)
        (row,) = [row for row in self._wcet.rows if row[0] == key[0] and row[1] == key[1]]
        return row[2]

    def get_local_wfreq(self, v: vertices.ProgramPointVertex):
        key = get_key(v)
        (row,) = [row for row in self._local_wfreq.rows if row[0] == key[0] and row[1] == key[1]]
        return row[2]

    def get_global_wfreq(self, v: vertices.ProgramPointVertex):
        key = get_key(v)
        (row,) = [row for row in self._global_wfreq.rows if row[0] == key[0] and row[1] == key[1]]
        return row[2]
