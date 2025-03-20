import enum
import typing

import football_api.structure
import lib.messages
import sql.sql
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames

ugly_separator = '-'
pretty_separator = ' '

demonym_dict = {
    "Afghanistan": "Afghan",
    "Albania": "Albanian",
    "Algeria": "Algerian",
    "Argentina": "Argentine",
    "Australia": "Australian",
    "Austria": "Austrian",
    "Bangladesh": "Bangladeshi",
    "Belgium": "Belgian",
    "Bosnia": "Bosnian",
    "Brazil": "Brazilian",
    "Bulgaria": "Bulgarian",
    "Canada": "Canadian",
    "Chile": "Chilean",
    "China": "Chinese",
    "Colombia": "Colombian",
    "Costa-Rica": "Costa Rican",
    "Czech-Republic": "Czech",
    "Denmark": "Danish",
    "Egypt": "Egyptian",
    "England": "English",
    "Estonia": "Estonian",
    "Ethiopia": "Ethiopian",
    "Finland": "Finnish",
    "France": "French",
    "Germany": "German",
    "Greece": "Greek",
    "Guatemala": "Guatemalan",
    "Hong-Kong": "Hong Kongese",
    "Hungary": "Hungarian",
    "India": "Indian",
    "Indonesia": "Indonesian",
    "Iran": "Iranian",
    "Iraq": "Iraqi",
    "Ireland": "Irish",
    "Israel": "Israeli",
    "Italy": "Italian",
    "Jamaica": "Jamaican",
    "Japan": "Japanese",
    "Kenya": "Kenyan",
    "Latvia": "Latvian",
    "Lithuania": "Lithuanian",
    "Malaysia": "Malaysian",
    "Malta": "Maltese",
    "Mexico": "Mexican",
    "Morocco": "Moroccan",
    "Netherlands": "Dutch",
    "New Zealand": "New Zealander",
    "Nigeria": "Nigerian",
    "Northern-Ireland": "Northern Irish",
    "Norway": "Norwegian",
    "Pakistan": "Pakistani",
    "Peru": "Peruvian",
    "Philippines": "Filipino",
    "Poland": "Polish",
    "Portugal": "Portuguese",
    "Romania": "Romanian",
    "Russia": "Russian",
    "Saudi-Arabia": "Saudi",
    "Scotland": "Scottish",
    "Serbia": "Serbian",
    "Slovakia": "Slovak",
    "Slovenia": "Slovenian",
    "South-Africa": "South African",
    "South-Korea": "South Korean",
    "Spain": "Spanish",
    "Sri Lanka": "Sri Lankan",
    "Sudan": "Sudanese",
    "Sweden": "Swedish",
    "Switzerland": "Swiss",
    "Syria": "Syrian",
    "Tajikistan": "Tajikistani",
    "Thailand": "Thai",
    "Tunisia": "Tunisian",
    "Turkey": "Turkish",
    "Uganda": "Ugandan",
    "Ukraine": "Ukrainian",
    "United-Arab-Emirates": "Emirati",
    "USA": "American",
    "Uzbekistan": "Uzbek",
    "Venezuela": "Venezuelan",
    "Vietnam": "Vietnamese",
    "World": "",
    "Yemen": "Yemeni",
    "Zambia": "Zambian",
    "Zimbabwe": "Zimbabwean"
}


def prettify(country_name: str) -> str:
    if ugly_separator in country_name:
        lexemes = country_name.split(ugly_separator)
        return pretty_separator.join(lex.capitalize() for lex in lexemes)
    else:
        return country_name


def uglify(country_name: str) -> str:
    if pretty_separator in country_name:
        lexemes = country_name.split(pretty_separator)
        return ugly_separator.join(lex.capitalize() for lex in lexemes)
    else:
        return country_name


class CompetitionType(enum.Enum):
    LEAGUE = enum.auto()
    CUP = enum.auto()


class Competition:
    def __init__(self, id_: int, country: str, name: str, flag: str, type_: CompetitionType):
        self.id: int = id_
        self.country: str = country
        self.name: str = name
        self.flag: str = flag
        self.type: CompetitionType = type_

    def get_competition_logo(self) -> str:
        return f"https://media.api-sports.io/football/leagues/{self.id}.png"

    def get_demonym(self) -> str:
        return demonym_dict[self.country]

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        id_col = Column(ColumnNames.ID.name, Affinity.INTEGER)
        table = sql.sql_tables.Table(
            cls.__name__,
            [
                id_col
            ],
            [
                id_col,
                Column(ColumnNames.Country.name, Affinity.TEXT),
                Column(ColumnNames.Name.name, Affinity.TEXT),
                Column(ColumnNames.Flag.name, Affinity.TEXT),
                Column(ColumnNames.Competition_Type.name, Affinity.TEXT)
            ]
        )
        return table

    def sql_values(self):
        values = [self.id, self.country, self.name, self.flag, self.type.name]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    def __eq__(self, other):
        if type(other) == type(self):
            return self.id == other.id
        return NotImplemented

    def __hash__(self):
        return self.id

    def __str__(self):
        return f'{prettify(self.country)}: {self.name}'


def create_competition_from_json(json_data: dict) -> Competition:
    return Competition(
        json_data['league']['id'],
        json_data['country']['name'],
        json_data['league']['name'],
        json_data['country']['flag'],
        CompetitionType[json_data['league']['type']]
    )


def create_competition_from_row(row: list[str]) -> Competition:
    return Competition(
        int(row[0]),
        row[1],
        row[2],
        row[3],
        CompetitionType[row[4]]
    )


def load_competition(competition_id: int) -> typing.Optional[Competition]:
    with sql.sql.Database(football_api.structure.database) as db:
        competition_id_constraint = f"{ColumnNames.ID.name}={competition_id}"
        competition_rows = db.fetch_all_rows(Competition.sql_table(), [competition_id_constraint])
        if not competition_rows:
            lib.messages.error_message(f"There is no competition with ID {competition_id} in the database")
        else:
            (row,) = competition_rows
            return create_competition_from_row(row)


def load_competitions(country: str = None, competition_type: CompetitionType = None) -> list[Competition]:
    competitions = []
    with sql.sql.Database(football_api.structure.database) as db:
        constraints = []
        if country is not None:
            country_constraint = f"{ColumnNames.Country.name}='{country}'"
            constraints.append(country_constraint)

        if competition_type is not None:
            competition_type_constraint = f"{ColumnNames.Competition_Type.name}='{competition_type.name.upper()}'"
            constraints.append(competition_type_constraint)

        competition_rows = db.fetch_all_rows(Competition.sql_table(), constraints)
        for row in competition_rows:
            competition = create_competition_from_row(row)
            competitions.append(competition)
    return competitions


def get_competition_whitelist(competition_type: CompetitionType) -> list[Competition]:
    if competition_type == CompetitionType.LEAGUE:
        filename = football_api.structure.get_leagues_whitelist()
    else:
        filename = football_api.structure.get_cups_whitelist()

    whitelist = []
    with open(filename, 'r') as in_file:
        for line in in_file:
            line = line.strip()
            whitelist.append(int(line))

    competitions = load_competitions(competition_type=competition_type)
    return [competition for competition in competitions if competition.id in whitelist]


def get_league(country: str, league_name: str) -> Competition:
    with sql.sql.Database(football_api.structure.database) as db:
        country_constraint = f"{ColumnNames.Country.name}='{country}'"
        name_constraint = f"{ColumnNames.Name.name}='{league_name}'"
        league_rows = db.fetch_all_rows(Competition.sql_table(), [country_constraint, name_constraint])
        (row,) = league_rows
        return create_competition_from_row(row)

