from collections import OrderedDict
from lib import messages


ugly_separator = '-'
pretty_separator = ' '


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


class League:
    def __init__(self, country: str, name: str):
        self.country = country
        self.name = name

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self):
        return self.country.__hash__() + self.name.__hash__()

    def __str__(self):
        return '{} {}'.format(prettify(self.country), self.name)


country_register = [
    'Albania',
    'Algeria',
    'Argentina',
    'Australia',
    'Austria',
    'Azerbaidjan',
    'Bahrain',
    'Bangladesh',
    'Belarus',
    'Belgium',
    'Bolivia',
    'Bosnia',
    'Brazil',
    'Bulgaria',
    'Cambodia',
    'Canada',
    'Chile',
    'China',
    'Colombia',
    'Costa-Rica',
    'Croatia',
    'Cyprus',
    'Czech-Republic',
    'Denmark',
    'Ecuador',
    'Egypt',
    'England',
    'Estonia',
    'Ethiopia',
    'Faroe-Islands',
    'Finland',
    'France',
    'Georgia',
    'Germany',
    'Ghana',
    'Greece',
    'Guatemala',
    'Honduras',
    'Hungary',
    'Iceland',
    'India',
    'Indonesia',
    'Iran',
    'Ireland',
    'Israel',
    'Italy',
    'Jamaica',
    'Japan',
    'Jordan',
    'Kazakhstan',
    'Kenya',
    'Kosovo',
    'Kuwait',
    'Latvia',
    'Liechtenstein',
    'Lithuania',
    'Luxembourg',
    'Macedonia',
    'Malaysia',
    'Malta',
    'Mexico',
    'Moldova',
    'Montenegro',
    'Morocco',
    'Netherlands',
    'New-Zealand',
    'Nicaragua',
    'Nigeria',
    'Northern-Ireland',
    'Norway',
    'Oman',
    'Panama',
    'Paraguay',
    'Peru',
    'Poland',
    'Portugal',
    'Qatar',
    'Romania',
    'Russia',
    'Saudi-Arabia',
    'Scotland',
    'Serbia',
    'Singapore',
    'Slovakia',
    'Slovenia',
    'South-Africa',
    'South-Korea',
    'Spain',
    'Sweden',
    'Switzerland',
    'Syria',
    'Tanzania',
    'Thailand',
    'Tunisia',
    'Turkey',
    'Ukraine',
    'United-Arab-Emirates',
    'Uruguay',
    'USA',
    'Uganda',
    'Uzbekistan',
    'Venezuela',
    'Vietnam',
    'Wales']

league_register = OrderedDict({
    'ALB1': League('Albania', 'Superliga'),
    'ALG1': League('Algeria', 'Ligue 1'),
    #'ALG1U': League('Algeria', 'U21 League 1'),
    'ARG1': League('Argentina', 'Liga Profesional Argentina'),
    'ARG2A': League('Argentina', 'Primera Nacional'),
    'ARG2B': League('Argentina', 'Primera B Metropolitana'),
    'ARG3': League('Argentina', 'Primera C'),
    #'ARG4': League('Argentina', 'Torneo Federal A'),
    'AUS1': League('Australia', 'A-League'),
    #'AUS2A': League('Australia', 'Brisbane Premier League'),
    #'AUS2B': League('Australia', 'Victoria NPL'),
    #'AUS2C': League('Australia', 'Queensland NPL'),
    #'AUS2D': League('Australia', 'Tasmania NPL'),
    #'AUS2E': League('Australia', 'South Australia NPL'),
    #'AUS2F': League('Australia', 'Western Australia NPL'),
    #'AUS2G': League('Australia', 'Northern NSW NPL'),
    #'AUS2H': League('Australia', 'New South Wales NPL'),
    'AUS1W': League('Australia', 'W-League'),
    'AUT1': League('Austria', 'Bundesliga'),
    'AUT2': League('Austria', '2. Liga'),
    #'AUT3A': League('Austria', 'Regionalliga - Mitte'),
    #'AUT3B': League('Austria', 'Regionalliga - Ost'),
    #'AUT3C': League('Austria', 'Regionalliga - West'),
    #'AUT3D': League('Austria', 'Regionalliga - Salzburg'),
    #'AUT3E': League('Austria', 'Regionalliga - Tirol'),
    'AZE1': League('Azerbaidjan', 'Premyer Liqa'),
    'BEL1': League('Belgium', 'Jupiler Pro League'),
    'BEL2': League('Belgium', 'Challenger Pro League'),
    'BEL1A': League('Belgium', 'Reserve Pro League'),
    'BEL1W': League('Belgium', 'Super League Women'),
    'BGD1': League('Bangladesh', 'Premier League'),
    #'BHR1': League('Bahrain', 'Premier League'),
    'BRA1': League('Brazil', 'Serie A'),
    'BRA2': League('Brazil', 'Serie B'),
    'BRA3': League('Brazil', 'Serie C'),
    #'BRA4': League('Brazil', 'Serie D'),
    'BRA1W': League('Brazil', 'Brasileiro Women'),
    'BGR1': League('Bulgaria', 'First League'),
    #'BGR2': League('Bulgaria', 'Second League'),
    'BOL1': League('Bolivia', 'Primera División'),
    'BIH1': League('Bosnia', 'Premijer Liga'),
    #'BLR1': League('Belarus', 'Premier League'),
    #'BLR2': League('Belarus', '1. Division'),
    #'BLR1A': League('Belarus', 'Reserve League'),
    'CAM1': League('Cambodia', 'C-League'),
    'CHE1': League('Switzerland', 'Super League'),
    'CHE2': League('Switzerland', 'Challenge League'),
    'CHE1W': League('Switzerland', 'Nationalliga A Women'),
    'CHN1': League('China', 'Super League'),
    'CHN2': League('China', 'League One'),
    'CHL1': League('Chile', 'Primera División'),
    'CHL2': League('Chile', 'Primera B'),
    'COL1': League('Colombia', 'Primera A'),
    'COL2': League('Colombia', 'Primera B'),
    'CRI1': League('Costa-Rica', 'Primera División'),
    'CRI2': League('Costa-Rica', 'Liga de Ascenso'),
    'HRV1': League('Croatia', 'HNL'),
    'HRV2': League('Croatia', 'First NL'),
    'CYP1': League('Cyprus', '1. Division'),
    #'CYP2': League('Cyprus', '2. Division'),
    'CZE1': League('Czech-Republic', 'Czech Liga'),
    'CZE2': League('Czech-Republic', 'FNL'),
    'CZE1U': League('Czech-Republic', '1. Liga U19'),
    'CZE1W': League('Czech-Republic', '1. Liga Women'),
    'DE1': League('Germany', 'Bundesliga'),
    'DE2': League('Germany', '2. Bundesliga'),
    'DE3': League('Germany', '3. Liga'),
    'DE4A': League('Germany', 'Regionalliga - Nord'),
    'DE4B': League('Germany', 'Regionalliga - Nordost'),
    'DE4C': League('Germany', 'Regionalliga - SudWest'),
    'DE4D': League('Germany', 'Regionalliga - West'),
    'DE4E': League('Germany', 'Regionalliga - Bayern'),
    'DE1U': League('Germany', 'U19 Bundesliga'),
    'DE1W': League('Germany', 'Frauen Bundesliga'),
    'DNK1': League('Denmark', 'Superliga'),
    'DNK2': League('Denmark', '1. Division'),
    'DNK3A': League('Denmark', '2nd Division - Group 1'),
    'DNK3B': League('Denmark', '2nd Division - Group 2'),
    'DNK1W': League('Denmark', 'Kvindeliga'),
    'ECU1': League('Ecuador', 'Liga Pro'),
    'ECU2': League('Ecuador', 'Liga Pro Serie B'),
    'EGY1': League('Egypt', 'Premier League'),
    'ENG1': League('England', 'Premier League'),
    'ENG2': League('England', 'Championship'),
    'ENG3': League('England', 'League One'),
    'ENG4': League('England', 'League Two'),
    'ENG5': League('England', 'National League'),
    'ENG6A': League('England', 'National League - North'),
    'ENG6B': League('England', 'National League - South'),
    'ENG7A': League('England', 'Non League Premier - Isthmian'),
    'ENG7B': League('England', 'Non League Premier - Northern'),
    'ENG7C': League('England', 'Non League Premier - Southern South'),
    'ENG7D': League('England', 'Non League Premier - Southern Central'),
    'ENG1R': League('England', 'Premier League 2 Division One'),
    'ENG1AU': League('England', 'U18 Premier League - North'),
    'ENG1BU': League('England', 'U18 Premier League - South'),
    'ENG1W': League('England', 'FA WSL'),
    'ENG2W': League('England', "Women''s Championship"),
    'ES1': League('Spain', 'La Liga'),
    'ES2': League('Spain', 'Segunda División'),
    'ES3A': League('Spain', 'Segunda División RFEF - Group 1'),
    'ES3B': League('Spain', 'Segunda División RFEF - Group 2'),
    'ES3C': League('Spain', 'Segunda División RFEF - Group 3'),
    'ES3D': League('Spain', 'Segunda División RFEF - Group 4'),
    'ES3E': League('Spain', 'Segunda División RFEF - Group 5'),
    'ES1W': League('Spain', 'Primera División Femenina'),
    'EST1': League('Estonia', 'Meistriliiga'),
    #'EST2': League('Estonia', 'Esiliiga A'),
    'ETH1': League('Ethiopia', 'Premier League'),
    'FIN1': League('Finland', 'Veikkausliiga'),
    'FIN2': League('Finland', 'Ykkönen'),
    'FIN3A': League('Finland', 'Kakkonen - Lohko A'),
    'FIN3B': League('Finland', 'Kakkonen - Lohko B'),
    'FIN3C': League('Finland', 'Kakkonen - Lohko C'),
    'FIN1W': League('Finland', 'Kansallinen Liiga'),
    'FRA1': League('France', 'Ligue 1'),
    'FRA2': League('France', 'Ligue 2'),
    'FRA3': League('France', 'National 1'),
    #'FRA4A': League('France', 'National 2 - Group A'),
    #'FRA4B': League('France', 'National 2 - Group B'),
    #'FRA4C': League('France', 'National 2 - Group C'),
    #'FRA4D': League('France', 'National 2 - Group D'),
    'FRA1W': League('France', 'Feminine Division 1'),
    'FRO1': League('Faroe-Islands', 'Meistaradeildin'),
    #'FRO2': League('Faroe-Islands', '1. Deild'),
    'GEO1': League('Georgia', 'Erovnuli Liga'),
    #'GEO2': League('Georgia', 'Erovnuli Liga 2'),
    'GHA1': League('Ghana', 'Premier League'),
    'GRC1': League('Greece', 'Super League 1'),
    'GTM1': League('Guatemala', 'Liga Nacional'),
    'HND1': League('Honduras', 'Liga Nacional'),
    'HUN1': League('Hungary', 'NB I'),
    'HUN2': League('Hungary', 'NB II'),
    'IND1': League('India', 'Indian Super League'),
    #'IND2': League('India', 'I-League'),
    'IDN1': League('Indonesia', 'Liga 1'),
    #'IDN2': League('Indonesia', 'Liga 2'),
    'IRL1': League('Ireland', 'Premier Division'),
    'IRL2': League('Ireland', 'First Division'),
    'IRN1': League('Iran', 'Persian Gulf Pro League'),
    #'IRN2': League('Iran', 'Azadegan League'),
    'ISL1': League('Iceland', 'Úrvalsdeild'),
    'ISL2': League('Iceland', '1. Deild'),
    #'ISL3': League('Iceland', '2. Deild'),
    'ISL1W': League('Iceland', 'Úrvalsdeild Women'),
    'ISR1': League('Israel', "Ligat Ha''al"),
    'ISR2': League('Israel', 'Liga Leumit'),
    'ISR3': League('Israel', 'Liga Alef'),
    'ITA1': League('Italy', 'Serie A'),
    'ITA2': League('Italy', 'Serie B'),
    'ITA3': League('Italy', 'Serie C'),
    'ITA1W': League('Italy', 'Serie A Women'),
    'JAM1': League('Jamaica', 'Premier League'),
    'JAP1': League('Japan', 'J1 League'),
    'JAP2': League('Japan', 'J2 League'),
    'JAP3': League('Japan', 'J3 League'),
    'JOR1': League('Jordan', 'League'),
    'KAZ1': League('Kazakhstan', 'Premier League'),
    #'KAZ2': League('Kazakhstan', '1. Division'),
    'KEN1': League('Kenya', 'FKF Premier League'),
    'KOR1': League('South-Korea', 'K League 1'),
    'KOR2': League('South-Korea', 'K League 2'),
    'KOR3': League('South-Korea', 'K3 League'),
    'KOR1W': League('South-Korea', 'WK-League'),
    'KWT1': League('Kuwait', 'Premier League'),
    'LTU1': League('Lithuania', 'A Lyga'),
    #'LTU2': League('Lithuania', '1 Lyga'),
    'LUX1': League('Luxembourg', 'National Division'),
    'LVA1': League('Latvia', 'Virsliga'),
    #'LVA2': League('Latvia', '1. Liga'),
    'MDA1': League('Moldova', 'Super Liga'),
    'MEX1': League('Mexico', 'Liga MX'),
    'MEX2': League('Mexico', 'Liga de Expansión MX'),
    'MEX1W': League('Mexico', 'Liga MX Femenil'),
    'MOR1': League('Morocco', 'Botola Pro'),
    'MKD1': League('Macedonia', 'First League'),
    'MLT1': League('Malta', 'Premier League'),
    #'MLT2': League('Malta', 'Challenge League'),
    'MNE1': League('Montenegro', 'First League'),
    #'MNE2': League('Montenegro', 'Second League'),
    'MYS1': League('Malaysia', 'Super League'),
    'MYS2': League('Malaysia', 'Premier League'),
    'NIC1': League('Nicaragua', 'Primera Division'),
    'NIG1': League('Nigeria', 'NPFL'),
    'NIR1': League('Northern-Ireland', 'Premiership'),
    #'NIR2': League('Northern-Ireland', 'Championship'),
    'NLD1': League('Netherlands', 'Eredivisie'),
    'NLD2': League('Netherlands', 'Eerste Divisie'),
    #'NLD3': League('Netherlands', 'Tweede Divisie'),
    'NLD1W': League('Netherlands', 'Eredivisie Women'),
    'NOR1': League('Norway', 'Eliteserien'),
    'NOR2': League('Norway', '1. Division'),
    'NOR3A': League('Norway', '2. Division - Group 1'),
    'NOR3B': League('Norway', '2. Division - Group 2'),
    'NOR1W': League('Norway', 'Toppserien'),
    'OMN1': League('Oman', 'Professional League'),
    'PAN1': League('Panama', 'Liga Panameña de Fútbol'),
    'PAR1A': League('Paraguay', 'Division Profesional - Apertura'),
    'PAR1B': League('Paraguay', 'Division Profesional - Clausura'),
    'PAR2': League('Paraguay', 'Division Intermedia'),
    'PER1': League('Peru', 'Primera División'),
    'PER2': League('Peru', 'Segunda División'),
    'POL1': League('Poland', 'Ekstraklasa'),
    'POL2': League('Poland', 'I Liga'),
    'PRT1': League('Portugal', 'Primeira Liga'),
    'PRT2': League('Portugal', 'Segunda Liga'),
    'QAT1': League('Qatar', 'Stars League'),
    'ROU1': League('Romania', 'Liga I'),
    'ROU2': League('Romania', 'Liga II'),
    #'RUS1': League('Russia', 'Premier League'),
    #'RUS2': League('Russia', 'First League'),
    #'RUS3A': League('Russia', 'Second League - Group 1'),
    #'RUS3B': League('Russia', 'Second League - Group 2'),
    #'RUS3C': League('Russia', 'Second League - Group 3'),
    #'RUS3D': League('Russia', 'Second League - Group 4'),
    #'RUS1U': League('Russia', 'Youth Championship'),
    #'RUS1W': League('Russia', 'Supreme Division Women'),
    'SAU1': League('Saudi-Arabia', 'Pro League'),
    'SAU2': League('Saudi-Arabia', 'Division 1'),
    'SCO1': League('Scotland', 'Premiership'),
    'SCO2': League('Scotland', 'Championship'),
    'SCO3': League('Scotland', 'League One'),
    'SCO4': League('Scotland', 'League Two'),
    'SIN1': League('Singapore', 'Premier League'),
    'SRB1': League('Serbia', 'Super Liga'),
    'SRB2': League('Serbia', 'Prva Liga'),
    'SVK1': League('Slovakia', 'Super Liga'),
    'SVK2': League('Slovakia', '2. liga'),
    'SVN1': League('Slovenia', '1. SNL'),
    'SVN2': League('Slovenia', '2. SNL'),
    'SWE1': League('Sweden', 'Allsvenskan'),
    'SWE2': League('Sweden', 'Superettan'),
    'SWE3A': League('Sweden', 'Ettan - Norra'),
    'SWE3B': League('Sweden', 'Ettan - Södra'),
    #'SWE4A': League('Sweden', 'Division 2 - Östra Götaland'),
    #'SWE4B': League('Sweden', 'Division 2 - Västra Götaland'),
    #'SWE4C': League('Sweden', 'Division 2 - Södra Svealand'),
    #'SWE4D': League('Sweden', 'Division 2 - Norrland'),
    #'SWE4E': League('Sweden', 'Division 2 - Norra Svealand'),
    #'SWE4F': League('Sweden', 'Division 2 - Norra Götaland'),
    'SWE1W': League('Sweden', 'Damallsvenskan'),
    #'SWE2W': League('Sweden', 'Elitettan'),
    #'SYR1': League('Syria', 'Premier League'),
    'THA1': League('Thailand', 'Thai League 1'),
    'THA2': League('Thailand', 'Thai League 2'),
    'TUN1': League('Tunisia', 'Ligue 1'),
    'TUR1': League('Turkey', 'Süper Lig'),
    'TUR2': League('Turkey', '1. Lig'),
    'TUR3': League('Turkey', '2. Lig'),
    'TZA1': League('Tanzania', 'Ligi kuu Bara'),
    'UAE1': League('United-Arab-Emirates', 'Pro League'),
    #'UAE2': League('United-Arab-Emirates', 'Division 1'),
    'UGA1': League('Uganda', 'Premier League'),
    'UKR1': League('Ukraine', 'Premier League'),
    'UKR2': League('Ukraine', 'Persha Liga'),
    'URU1': League('Uruguay', 'Primera División - Apertura'),
    'URU2': League('Uruguay', 'Primera División - Clausura'),
    'USA1': League('USA', 'Major League Soccer'),
    'USA2': League('USA', 'USL Championship'),
    'USA3': League('USA', 'USL League One'),
    #'USA4': League('USA', 'USL League Two'),
    'USA1W': League('USA', 'NWSL Women'),
    #'UZB1': League('Uzbekistan', 'Super League'),
    'VEN1': League('Venezuela', 'Primera División'),
    #'VEN2': League('Venezuela', 'Segunda División'),
    'VIE1': League('Vietnam', 'V.League 1'),
    'WAL1': League('Wales', 'Premier League'),
    #'XXK1': League('Kosovo', 'Superliga'),
    'ZAF1': League('South-Africa', 'Premier Soccer League'),
    #'ZAF2': League('South-Africa', '1st Division')
})

reverse_league_register = {league: code for code, league in league_register.items()}


def get_league_code(league: League) -> str:
    return reverse_league_register[league]
