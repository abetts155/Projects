import os
import random
import re
import sys
import time
from concurrent import futures

import bs4
import requests

from lib import utils
from lib.football import Fixture, Team


TRANSFERMARKT = 'http://www.transfermarkt.com'
STRAINER = bs4.SoupStrainer('div', {'class':'responsive-table'})


def get_betting_directory():
    path = os.path.expanduser('~') + os.sep + '.betting' + os.sep + 'football'
    if not os.path.exists(path):
        os.mkdir(path)
    return path


def have_connection():
    try:
        google_co_uk = 'http://216.58.206.3' 
        requests.get(google_co_uk, timeout=1)
        return True
    except requests.ConnectionError: 
        return False


def read_url(url):
    while True:
        try:
            sleep_duration = 10 * random.uniform(0.01, 0.1)
            time.sleep(sleep_duration)
            response = requests.get(url, 
                                    headers={'User-Agent': 'Mozilla/5.0'}, 
                                    timeout=5)
            if response.status_code == 200:
                return bs4.BeautifulSoup(response.text, 
                                         'lxml', 
                                         parse_only=STRAINER)
        except requests.ConnectionError:
            print('No internet connection...')
            sys.exit(1)
                

def __parse_page(season, filename, url):
    def create_from_web():
        soup = read_url(url)
        with open(filename, 'wb') as the_file:
            the_file.write(soup.prettify('utf-8'))
        return soup
    
    if season == utils.this_season() and have_connection():
        return create_from_web()
    else:
        if os.path.exists(filename):
            with open(filename, 'r') as the_file:
                return bs4.BeautifulSoup(the_file.read(), 
                                         'lxml', 
                                         parse_only=STRAINER)
        else:   
            return create_from_web()
        

def parse_league_season_page(league, season):
    filename = '{}{}{}_{}'.format(get_betting_directory(),
                                  os.sep,
                                  league,
                                  season)
    url = '{}/_/startseite/wettbewerb/{}/plus/?saison_id={}'.format(TRANSFERMARKT,
                                                                    league,
                                                                    season)
    return __parse_page(season, filename, url)
    

def parse_team_season_page(league, team, season):
    filename ='{}{}{}_{}_{}'.format(get_betting_directory(),
                                    os.sep,
                                    league,
                                    season,
                                    team.id_)
    url = '{}/_/spielplandatum/verein/{}/_/0?saison_id={}'.format(TRANSFERMARKT, 
                                                                  team.id_,
                                                                  season)
    return __parse_page(season, filename, url)


# How do we know if the game has been played yet?
played_regex = re.compile(r'\d+\:\d+')


def rip_out_fixtures(args):
    league, team, season, max_games = args
    soup = parse_team_season_page(league, team, season)
    tables = soup.findChildren('div', {'class':'responsive-table'})
    results_body = tables[0].findChild('tbody')
    table_rows = results_body.findChildren('tr', {'style': True})

    # Number of columns in a row
    expected_number_of_cells = 10

    matchday = 0
    for row in table_rows:
        cells = row.findChildren('td')
        if len(cells) != expected_number_of_cells:
            utils.exit_message('Table row must have exactly {} cells'.format(expected_number_of_cells),
                               'Found {} instead. See:\n {}'.format(len(cells), row))
        match_day_cell = cells[0]
        # Filter out non-league games
        match_day_attribute = match_day_cell.find('a')
        if match_day_attribute:
            if matchday < max_games:
                # Home or away?
                venue_cell = cells[3]
                venue = venue_cell.string.strip()
                # Who was the opposition?
                opponent_cell = cells[6]
                opponent = opponent_cell.find('a').string.strip()

                # What was the score?
                score_cell = cells[9]
                if score_cell.find('span') is not None:
                    scores = played_regex.findall(score_cell.find('span').text)
                    if scores:
                        assert len(scores) == 1
                        fixture = Fixture(scores[0],
                                          opponent,
                                          venue)
                        team.append(fixture)
                        matchday += 1


def collect_data(league, season):
    def sanitise_team_name():
        lexemes = team_name.split('-')
        return ' '.join([lex.lower() for lex in lexemes])

    soup = parse_league_season_page(league, season)
    tables = soup.findChildren('div', {'class':'responsive-table'})
    league_table = tables[-1]
    results_body = league_table.findChild('tbody')
    table_rows = results_body.findChildren('tr')
    max_games = 0
    teams = set()
    
    for row in table_rows:
        cells = row.findChildren('td')
        team_cell = cells[2]
        team_href = team_cell.find('a').get('href').strip()
        divider_indices = []
        for idx, c in enumerate(team_href):
            if c == '/':
                divider_indices.append(idx)
        assert len(divider_indices) == 6
        team_name = team_href[divider_indices[0]+1:divider_indices[1]]
        team_id = utils.parse_int(team_href[divider_indices[3]+1:divider_indices[4]])
        team = Team(sanitise_team_name(), team_id, season)
        teams.add(team)
        games_played = utils.parse_int(cells[3].string)
        if games_played is not None:
            max_games = max(max_games, games_played)

    max_workers = 10
    workers = min(max_workers, len(teams))
    with futures.ThreadPoolExecutor(workers) as executor:
        results = executor.map(rip_out_fixtures, 
                               [(league, team, season, max_games) for team in teams])
    list(results)
    return teams
