from enum import Enum, auto
from pathlib import Path
from re import compile
from requests import request
from typing import List

import arrow



country_to_iso_3166 = {'Albania': 'AL',
                       'Algeria': 'DZ',
                       'Argentina': 'AR',
                       'Armenia': 'AM',
                       'Australia': 'AU',
                       'Austria': 'AT',
                       'Azerbaijan': 'AZ',
                       'Belarus': 'BY',
                       'Belgium': 'BE',
                       'Bolivia': 'BO',
                       'Bosnia and Herzegovina': 'BA',
                       'Brazil': 'BR',
                       'Bulgaria': 'BG',
                       'Burundi': 'BI',
                       'Cambodia': 'KH',
                       'Cameroon': 'CM',
                       'Canada': 'CA',
                       'Chile': 'CL',
                       'China': 'CN',
                       'Colombia': 'CO',
                       'Costa Rica': 'CR',
                       'Croatia': 'HR',
                       'Cyprus': 'CY',
                       'Czech Republic': 'CZ',
                       'Denmark': 'DK',
                       'Djibouti': 'DJ',
                       'Dominica': 'DM',
                       'Dominican Republic': 'DO',
                       'Ecuador': 'EC',
                       'Egypt': 'EG',
                       'El Salvador': 'SV',
                       'Estonia': 'EE',
                       'Ethiopia': 'ET',
                       'Faroe Islands': 'FO',
                       'Finland': 'FI',
                       'France': 'FR',
                       'Georgia': 'GE',
                       'Germany': 'DE',
                       'Ghana': 'GH',
                       'Greece': 'GR',
                       'Guatemala': 'GT',
                       'Honduras': 'HN',
                       'Hungary': 'HU',
                       'Iceland': 'IS',
                       'India': 'IN',
                       'Indonesia': 'ID',
                       'Iran': 'IR',
                       'Iraq': 'IQ',
                       'Ireland': 'IE',
                       'Israel': 'IL',
                       'Italy': 'IT',
                       'Jamaica': 'JM',
                       'Japan': 'JP',
                       'Jordan': 'JO',
                       'Kazakhstan': 'KZ',
                       'Kenya': 'KE',
                       'Korea': 'KR',
                       'Kuwait': 'KW',
                       'Latvia': 'LV',
                       'Lithuania': 'LT',
                       'Luxembourg': 'LU',
                       'Macedonia': 'MK',
                       'Malaysia': 'MY',
                       'Malta': 'MT',
                       'Mexico': 'MX',
                       'Moldova': 'MD',
                       'Montenegro': 'ME',
                       'Morocco': 'MA',
                       'Netherlands': 'NL',
                       'New Zealand': 'NZ',
                       'Nicaragua': 'NI',
                       'Nigeria': 'NG',
                       'Norway': 'NO',
                       'Oman': 'OM',
                       'Palestine': 'PS',
                       'Panama': 'PA',
                       'Paraguay': 'PY',
                       'Peru': 'PE',
                       'Philippines': 'PH',
                       'Poland': 'PL',
                       'Portugal': 'PT',
                       'Qatar': 'QA',
                       'Romania': 'RO',
                       'Russia': 'RU',
                       'Saudi Arabia': 'SA',
                       'Senegal': 'SN',
                       'Serbia': 'RS',
                       'Singapore': 'SG',
                       'Slovakia': 'SK',
                       'Slovenia': 'SI',
                       'South Africa': 'ZA',
                       'Spain': 'ES',
                       'Sweden': 'SE',
                       'Switzerland': 'CH',
                       'Tanzania': 'TZ',
                       'Thailand': 'TH',
                       'Tunisia': 'TN',
                       'Turkey': 'TR',
                       'Ukraine': 'UA',
                       'United Arab Emirates': 'AE',
                       'United Kingdom': 'GB',
                       'United States': 'US',
                       'Uruguay': 'UY',
                       'Uzbekistan': 'UZ',
                       'Venezuela': 'VE',
                       'Vietnam': 'VN'}


class BetfairMarkets(Enum):
    BOTH_TEAMS_TO_SCORE = auto()
    DOUBLE_CHANCE = auto()
    FIRST_HALF_GOALS_05 = auto()
    FIRST_HALF_GOALS_15 = auto()
    FIRST_HALF_GOALS_25 = auto()
    MATCH_ODDS = auto()
    OVER_UNDER_05 = auto()
    OVER_UNDER_15 = auto()
    OVER_UNDER_25 = auto()


betfair_url = 'https://api.betfair.com/exchange/betting/json-rpc/v1'
betfair_api_prefix = 'SportsAPING/v1.0'


def read_hidden_contents(filename):
    hidden_file = Path.home().joinpath('.betfair').joinpath(filename)
    lines = []
    with open(hidden_file, 'r') as in_file:
        for line in in_file:
            if line:
                lines.append(line.strip())
    assert lines
    return ''.join(lines)


def get_headers():
    app_key = read_hidden_contents('AppKey')
    session_id = read_hidden_contents('SessionID')
    return {'X-Application': app_key, 'X-Authentication': session_id, 'content-type': 'application/json'}


def get_events(countries: List[str] = []):
    return """{{"jsonrpc": "2.0",
    "params": {{"filter": {{ "eventTypeIds": ["1"], "marketCountries": [{countries}], "inPlayOnly": "True" }} }},
    "method": "{api}/listEvents"}}""".format(countries=', '.join(['"{}"'.format(c) for c in countries]),
                                             api=betfair_api_prefix)


def get_market_types_payload(event_id: str):
    return """{{"jsonrpc": "2.0",
        "params": {{"filter": {{ "eventIds": [{event_id}] }} }},
        "method": "{api}/listMarketTypes"}}""".format(event_id=event_id, api=betfair_api_prefix)


def get_market_catalogue(event_id: str, market: BetfairMarkets):
    return """{{"jsonrpc": "2.0",
            "params": {{"filter": {{ "eventIds": [{event_id}], "marketTypeCodes": ["{market}"] }}, "maxResults": "1" }},
            "method": "{api}/listMarketCatalogue"}}""".format(event_id=event_id,
                                                              market=market.name,
                                                              api=betfair_api_prefix)


def get_market_book(market_id: str):
    return """{{"jsonrpc": "2.0",
            "params": {{"marketIds": ["{market_id}"], "priceProjection": {{ "priceData": ["EX_BEST_OFFERS", "EX_TRADED"], "virtualise": "true"}} }},
            "method": "{api}/listMarketBook"}}""".format(market_id=market_id, api=betfair_api_prefix)


def get_runner_book(market_id: str, selection_id: str):
    return """{{"jsonrpc": "2.0",
            "params": {{"marketId": "{market_id}", "selectionId": "{selection_id}" }},
            "method": "{api}/listRunnerBook"}}""".format(market_id=market_id,
                                                         selection_id=selection_id,
                                                         api=betfair_api_prefix)


def parse_market_book_json(json, market_id):
    results = json['result'][0]
    if results:
        for selection in results['runners']:
            print(selection)


def parse_market_results_json(json):
    results = json['result'][0]
    if results:
        market_id = results['marketId']
        response = request('GET', betfair_url, headers=get_headers(), data=get_market_book(market_id))
        parse_market_book_json(response.json(), market_id)


def parse_market_types_json(json, event_id):
    for event_data in json['result']:
        try:
            market = BetfairMarkets[event_data['marketType']]
            print('>', market.name)
            response = request('GET', betfair_url, headers=get_headers(), data=get_market_catalogue(event_id, market))
            parse_market_results_json(response.json())
        except KeyError:
            pass


def is_open_today(date: arrow.Arrow):
    now = arrow.now()
    return (date.datetime.day == now.datetime.day and
            date.datetime.month == now.datetime.month and
            date.datetime.year == now.datetime.year)


def parse_event_json(json):
    game_re = compile(r"([\w\-'\(\)\/ ]+) v ([\w\-'\(\)\/ ]+)")
    for event_data in json['result']:
        event_date = arrow.get(event_data['event']['openDate'])
        event_name = event_data['event']['name']
        match = game_re.match(event_name)
        if is_open_today(event_date):
            print(event_data)
            assert match
            event_id = event_data['event']['id']
            home, away = match.groups()
            print('-' * 10, home, 'vs', away, '-' * 10)
            response = request('GET', betfair_url, headers=get_headers(), data=get_market_types_payload(event_id))
            parse_market_types_json(response.json(), event_id)
            print()


def main():
    #response = request('GET', betfair_url, headers=get_headers(), data=get_events())
    #parse_event_json(response.json())


if __name__ == '__main__':
    main()
