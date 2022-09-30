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
    "id": "1", 
    "method": "{api}/listEvents"}}""".format(countries=', '.join(['"{}"'.format(c) for c in countries]),
                                             api=betfair_api_prefix)


def get_market_types_payload(event_id: int):
    return """{{"jsonrpc": "2.0",
        "params": {{"filter": {{ "eventIds": [{event_id}] }} }},
        "id": "1", 
        "method": "{api}/listMarketTypes"}}""".format(event_id=event_id, api=betfair_api_prefix)


def get_market_catalogue(event_id: int, bet_type):
    return """{{"jsonrpc": "2.0",
            "params": {{"filter": {{ "eventIds": [{event_id}], "marketTypeCodes": ["{bet}"] }}, "maxResults": "1" }},
            "id": "1", 
            "method": "{api}/listMarketCatalogue"}}""".format(event_id=event_id, bet=bet_type, api=betfair_api_prefix)


def parse_market_types_json(market_types_json, event_id):
    for event_data in market_types_json['result']:
        market = event_data['marketType']
        payload = get_market_catalogue(event_id, market)
        response = request('GET', betfair_url, headers=get_headers(), data=payload)
        market_json = response.json()
        results = market_json['result'][0]
        if results:
            print(market, ' ID={}'.format(results['marketId']), ' Matched={}'.format(results['totalMatched']))


def is_open_today(date: arrow.Arrow):
    now = arrow.now()
    return (date.datetime.day == now.datetime.day and
            date.datetime.month == now.datetime.month and
            date.datetime.year == now.datetime.year)


def parse_event_json(event_json):
    game_re = compile(r"([\w\-' ]+) v ([\w\-' ]+)")
    for event_data in event_json['result']:
        event_date = arrow.get(event_data['event']['openDate'])
        event_name = event_data['event']['name']
        match = game_re.match(event_name)
        if is_open_today(event_date):
            assert match
            event_id = event_data['event']['id']
            home, away = match.groups()
            print('-' * 10, home, 'vs', away, '-' * 10)
            payload = get_market_types_payload(int(event_id))
            response = request('GET', betfair_url, headers=get_headers(), data=payload)
            parse_market_types_json(response.json(), event_id)
            print()


def main():
    payload = get_events()
    response = request('GET', betfair_url, headers=get_headers(), data=payload)
    event_json = response.json()
    parse_event_json(event_json)


if __name__ == '__main__':
    main()
