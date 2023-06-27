from model.fixtures import Event, Half
from sql.sql_columns import Affinity, Column, ColumnNames, id_column
from sql.sql_tables import Table


class Prediction:
    table = None
    inventory = {}

    def __init__(self, id_: int, country_code: str, fixture_id: int, odds: float, event: Event, half: Half):
        self.id_ = id_
        self.country_code = country_code
        self.fixture_id = fixture_id
        self.odds = odds
        self.event = event
        self.half = half

    def sql_values(self):
        values = [self.id_, self.country_code, self.fixture_id, self.odds, Event.encode(self.event), self.half.name]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> Table:
        if cls.table is None:
            cls.table = Table(cls.__name__,
                              id_column(),
                              [id_column(),
                               Column(ColumnNames.Country_Code.name, Affinity.TEXT),
                               Column(ColumnNames.Fixture_ID.name, Affinity.INTEGER),
                               Column(ColumnNames.Odds.name, Affinity.REAL),
                               Column(ColumnNames.Event.name, Affinity.TEXT),
                               Column(ColumnNames.Half.name, Affinity.TEXT)])
        return cls.table


def create_prediction_from_row(row):
    id_ = int(row[0])
    country_code = row[1]
    fixture_id = int(row[2])
    odds = float(row[3])
    event = Event.get(row[4])
    half = Half.from_string(row[5])
    prediction = Prediction(id_, country_code, fixture_id, odds, event, half)
    Prediction.inventory[id_] = prediction
    return prediction
