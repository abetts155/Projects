import enum
import pathlib
import typing

import lib.structure
import lib.messages
import sql.sql
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames

ugly_separator = '-'
pretty_separator = ' '


country_to_2_letter_iso_code = {
    "Afghanistan": "AF",
    "Albania": "AL",
    "Algeria": "DZ",
    "Argentina": "AR",
    "Armenia": "AM",
    "Australia": "AU",
    "Austria": "AT",
    "Azerbaijan": "AZ",
    "Bahrain": "BH",
    "Bangladesh": "BD",
    "Belgium": "BE",
    "Bolivia": "BO",
    "Bosnia": "BA",
    "Brazil": "BR",
    "Bulgaria": "BG",
    "Cambodia": "KH",
    "Canada": "CA",
    "Chile": "CL",
    "China": "CN",
    "Colombia": "CO",
    "Croatia": "HR",
    "Costa-Rica": "CR",
    "Cyprus": "CY",
    "Czech-Republic": "CZ",
    "Denmark": "DK",
    "Ecuador": "EC",
    "Egypt": "EG",
    "El-Salvador": "SV",
    "England": "GB",
    "Estonia": "EE",
    "Ethiopia": "ET",
    "Faroe-Islands": "FO",
    "Finland": "FI",
    "France": "FR",
    "Georgia": "GE",
    "Germany": "DE",
    "Ghana": "GH",
    "Greece": "GR",
    "Guatemala": "GT",
    "Honduras": "HN",
    "Hong-Kong": "HK",
    "Hungary": "HU",
    "Iceland": "IS",
    "India": "IN",
    "Indonesia": "ID",
    "Iran": "IR",
    "Iraq": "IQ",
    "Ireland": "IE",
    "Israel": "IL",
    "Italy": "IT",
    "Ivory-Coast": "CI",
    "Jamaica": "JM",
    "Japan": "JP",
    "Jordan": "JO",
    "Kazakhstan": "KZ",
    "Kenya": "KE",
    "Kosovo": "XK",
    "Kuwait": "KW",
    "Latvia": "LV",
    "Lebanon": "LB",
    "Lithuania": "LT",
    "Luxembourg": "LU",
    "Macedonia": "MK",
    "Malaysia": "MY",
    "Malta": "MT",
    "Mexico": "MX",
    "Moldova": "MD",
    "Montenegro": "ME",
    "Morocco": "MA",
    "Myanmar": "MM",
    "Netherlands": "NL",
    "New-Zealand": "NZ",
    "Nicaragua": "NI",
    "Nigeria": "NG",
    "Northern-Ireland": "GB",
    "Norway": "NO",
    "Oman": "OM",
    "Pakistan": "PK",
    "Panama": "PA",
    "Paraguay": "PY",
    "Peru": "PE",
    "Philippines": "PH",
    "Poland": "PL",
    "Portugal": "PT",
    "Qatar": "QA",
    "Romania": "RO",
    "Russia": "RU",
    "Rwanda": "RW",
    "Saudi-Arabia": "SA",
    "Scotland": "GB",
    "Senegal": "SN",
    "Serbia": "RS",
    "Singapore": "SG",
    "Slovakia": "SK",
    "Slovenia": "SI",
    "South-Africa": "ZA",
    "South-Korea": "KR",
    "Spain": "ES",
    "Sri Lanka": "LK",
    "Sudan": "SD",
    "Sweden": "SE",
    "Switzerland": "CH",
    "Syria": "SY",
    "Tajikistan": "TJ",
    "Tanzania": "TZ",
    "Thailand": "TH",
    "Tunisia": "TN",
    "Turkey": "TR",
    "Uganda": "UG",
    "Ukraine": "UA",
    "United-Arab-Emirates": "AE",
    "Uruguay": "UY",
    "USA": "US",
    "Uzbekistan": "UZ",
    "Venezuela": "VE",
    "Vietnam": "VN",
    "Wales": "GB",
    "World": None,
    "Yemen": "YE",
    "Zambia": "ZM",
    "Zimbabwe": "ZW"
}


country_to_demonym = {
    "Afghanistan": "Afghan",
    "Albania": "Albanian",
    "Algeria": "Algerian",
    "Andorra": "Andorran",
    "Angola": "Angolan",
    "Antigua-And-Barbuda": "Antiguan and Barbudan",
    "Argentina": "Argentine",
    "Armenia": "Armenian",
    "Aruba": "Aruban",
    "Australia": "Australian",
    "Austria": "Austrian",
    "Azerbaijan": "Azerbaijani",
    "Bahrain": "Bahraini",
    "Bangladesh": "Bangladeshi",
    "Barbados": "Barbadian",
    "Belarus": "Belarussian",
    "Belgium": "Belgian",
    "Belize": "Belizean",
    "Benin": "Beninese",
    "Bermuda": "Bermudian",
    "Bhutan": "Bhutanese",
    "Bolivia": "Bolivian",
    "Bosnia": "Bosnian",
    "Botswana": "Batswana",
    "Brazil": "Brazilian",
    "Bulgaria": "Bulgarian",
    "Burkina-Faso": "Burkinabé",
    "Burundi": "Burundian",
    "Cambodia": "Cambodian",
    "Cameroon": "Cameroonian",
    "Canada": "Canadian",
    "Chile": "Chilean",
    "China": "Chinese",
    "Chinese-Taipei": "Taiwanese",
    "Colombia": "Colombian",
    "Congo": "Congolese",
    "Congo-DR": "Congolese",
    "Crimea": "Crimean",
    "Croatia": "Croatian",
    "Costa-Rica": "Costa Rican",
    "Cuba": "Cuban",
    "Curacao": "Curaçaoan",
    "Cyprus": "Cypriot",
    "Czech-Republic": "Czech",
    "Denmark": "Danish",
    "Dominican-Republic": "Dominican",
    "Ecuador": "Ecuadorian",
    "Egypt": "Egyptian",
    "El-Salvador": "Salvadoran",
    "England": "English",
    "Estonia": "Estonian",
    "Eswatini": "Swazi",
    "Ethiopia": "Ethiopian",
    "Faroe-Islands": "Faroese",
    "Fiji": "Fijian",
    "Finland": "Finnish",
    "France": "French",
    "Gabon": "Gabonese",
    "Gambia": "Gambian",
    "Georgia": "Georgian",
    "Germany": "German",
    "Ghana": "Ghanaian",
    "Gibraltar": "Gibraltarian",
    "Greece": "Greek",
    "Grenada": "Grenadian",
    "Guadeloupe": "Guadeloupean",
    "Guatemala": "Guatemalan",
    "Guinea": "Guinean",
    "Haiti": "Haitian",
    "Honduras": "Honduran",
    "Hong-Kong": "Hong Kongese",
    "Hungary": "Hungarian",
    "Iceland": "Icelandic",
    "India": "Indian",
    "Indonesia": "Indonesian",
    "Iran": "Iranian",
    "Iraq": "Iraqi",
    "Ireland": "Irish",
    "Israel": "Israeli",
    "Italy": "Italian",
    "Ivory-Coast": "Ivorian",
    "Jamaica": "Jamaican",
    "Japan": "Japanese",
    "Jordan": "Jordanian",
    "Kazakhstan": "Kazakhstani",
    "Kosovo": "Kosovan",
    "Kyrgyzstan": "Kyrgyzstani",
    "Kenya": "Kenyan",
    "Kuwait": "Kuwaiti",
    "Laos": "Laotian",
    "Latvia": "Latvian",
    "Lebanon": "Lebanese",
    "Lesotho": "Mosotho",
    "Liberia": "Liberian",
    "Libya": "Libyan",
    "Liechtenstein": "Liechtenstein",
    "Lithuania": "Lithuanian",
    "Luxembourg": "Luxembourger",
    "Macao": "Macanese",
    "Macedonia": "Macedonian",
    "Malaysia": "Malaysian",
    "Malawi": "Malawian",
    "Maldives": "Maldivian",
    "Mali": "Malian",
    "Malta": "Maltese",
    "Mauritania": "Mauritanian",
    "Mauritius": "Mauritian",
    "Mexico": "Mexican",
    "Moldova": "Moldovan",
    "Mongolia": "Mongolian",
    "Montenegro": "Montenegrin",
    "Morocco": "Moroccan",
    "Myanmar": "Burmese",
    "Namibia": "Namibian",
    "Nepal": "Nepalese",
    "Netherlands": "Dutch",
    "New-Zealand": "New Zealander",
    "Nicaragua": "Nicaraguan",
    "Nigeria": "Nigerian",
    "Northern-Ireland": "Northern Irish",
    "Norway": "Norwegian",
    "Oman": "Omani",
    "Palestine": "Palestinian",
    "Pakistan": "Pakistani",
    "Panama": "Panamanian",
    "Paraguay": "Paraguayan",
    "Peru": "Peruvian",
    "Philippines": "Filipino",
    "Poland": "Polish",
    "Portugal": "Portuguese",
    "Qatar": "Qatari",
    "Romania": "Romanian",
    "Russia": "Russian",
    "Rwanda": "Rwandan",
    "San-Marino": "Sammarinese",
    "Saudi-Arabia": "Saudi",
    "Scotland": "Scottish",
    "Senegal": " Senegalese",
    "Serbia": "Serbian",
    "Singapore": "Singaporean",
    "Slovakia": "Slovak",
    "Slovenia": "Slovenian",
    "Somalia": "Somalian",
    "South-Africa": "South African",
    "South-Korea": "South Korean",
    "Spain": "Spanish",
    "Sri Lanka": "Sri Lankan",
    "Sudan": "Sudanese",
    "Suriname": "Surinamese",
    "Sweden": "Swedish",
    "Switzerland": "Swiss",
    "Syria": "Syrian",
    "Tajikistan": "Tajikistani",
    "Tanzania": "Tanzanian",
    "Thailand": "Thai",
    "Togo": "Togolese",
    "Trinidad-And-Tobago": "Trinbagonian",
    "Tunisia": "Tunisian",
    "Turkey": "Turkish",
    "Turkmenistan": "Turkmen",
    "Uganda": "Ugandan",
    "Ukraine": "Ukrainian",
    "United-Arab-Emirates": "Emirati",
    "Uruguay": "Uruguayan",
    "USA": "American",
    "Uzbekistan": "Uzbek",
    "Venezuela": "Venezuelan",
    "Vietnam": "Vietnamese",
    "Wales": "Welsh",
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
    def __init__(self, id_: int, country: str, name: str, flag: str, type_: CompetitionType, whitelisted: bool):
        self.id: int = id_
        self.country: str = country
        self.name: str = name
        self.flag: str = flag
        self.type: CompetitionType = type_
        self.whitelisted: bool = whitelisted

    def get_demonym(self) -> str:
        return country_to_demonym[self.country]

    def get_2_letter_iso_code(self) -> str:
        return country_to_2_letter_iso_code[self.country]

    def get_logo(self) -> str:
        return f"https://media.api-sports.io/football/leagues/{self.id}.png"

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
                Column(ColumnNames.Competition_Type.name, Affinity.TEXT),
                Column(ColumnNames.Whitelisted.name, Affinity.INTEGER)
            ]
        )
        return table

    def sql_values(self):
        values = [self.id, self.country, self.name, self.flag, self.type.name, self.whitelisted]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    def __eq__(self, other):
        if type(other) == type(self):
            return self.id == other.id
        return NotImplemented

    def __lt__(self, other):
        if type(other) == type(self):
            return self.id <= other.id
        return NotImplemented

    def __hash__(self):
        return self.id

    def __str__(self):
        return f'{self.get_demonym()} {self.name}'


def create_competition_from_json(json_data: dict) -> Competition:
    return Competition(
        json_data['league']['id'],
        json_data['country']['name'],
        json_data['league']['name'],
        json_data['country']['flag'],
        CompetitionType[json_data['league']['type'].upper()],
        False
    )


def create_competition_from_row(row: list[str]) -> Competition:
    return Competition(
        int(row[0]),
        row[1],
        row[2],
        row[3],
        CompetitionType[row[4]],
        bool(row[5])
    )


def load_competition(competition_id: int) -> typing.Optional[Competition]:
    with sql.sql.Database(lib.structure.get_base_database()) as db:
        competition_id_constraint = f"{ColumnNames.ID.name}={competition_id}"
        competition_rows = db.fetch_all_rows(Competition.sql_table(), [competition_id_constraint])
        if not competition_rows:
            lib.messages.error_message(f"There is no competition with ID {competition_id} in the database")
        else:
            (row,) = competition_rows
            return create_competition_from_row(row)


def load_competitions(
        database: pathlib.Path,
        country: str = None,
        competition_type: CompetitionType = None
) -> list[Competition]:
    competitions = []
    with sql.sql.Database(database) as db:
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


def get_whitelisted_competitions(competition_type: CompetitionType) -> list[Competition]:
    all_competitions = load_competitions(lib.structure.get_base_database())
    competitions = [
        competition for competition in all_competitions
        if competition.whitelisted and competition.type == competition_type
    ]
    competitions.sort(key=lambda competition: (competition.country, competition.id))
    return competitions


def get_league(country: str, league_name: str) -> Competition:
    with sql.sql.Database(lib.structure.get_base_database()) as db:
        country_constraint = f"{ColumnNames.Country.name}='{country}'"
        name_constraint = f"{ColumnNames.Name.name}='{league_name}'"
        league_rows = db.fetch_all_rows(Competition.sql_table(), [country_constraint, name_constraint])
        (row,) = league_rows
        return create_competition_from_row(row)


def get_competition(
    country: str,
    competition_name: str,
    competition_type: CompetitionType | None = None,
) -> Competition:
    with sql.sql.Database(lib.structure.get_base_database()) as db:
        constraints = [
            f"{ColumnNames.Country.name}='{country}'",
            f"{ColumnNames.Name.name}='{competition_name}'",
        ]
        if competition_type is not None:
            constraints.append(f"{ColumnNames.Competition_Type.name}='{competition_type.name}'")
        competition_rows = db.fetch_all_rows(Competition.sql_table(), constraints)
        if not competition_rows:
            lib.messages.warning_message(
                f"There is no competition with name {competition_name} in {country}."
            )
            return None
        (row,) = competition_rows
        return create_competition_from_row(row)


def country_flag(code: str) -> str:
    return ''.join(chr(0x1F1E6 + ord(c.upper()) - ord('A')) for c in code)
