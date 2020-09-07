#!/usr/bin/env python3

import sys
assert sys.version_info >= (3,0), 'Script requires Python 3.0 or greater to run'

import os
import urllib.request
import shutil
import zipfile
import re
import sqlite3
import xlrd

from lib.betting import utils

        
def parse_sheet(year, xl_sheet):
    
    def get_set_score(row_idx, col_idx):
        cell = xl_sheet.cell(row_idx, col_idx)
        try:
            return int(cell.value)
        except ValueError:
            return 0
    
    def get_odds(row_idx, col_idx):
        cell = xl_sheet.cell(row_idx, col_idx)
        try:
            cell.value = str(cell.value).replace(',', '')
            return float(cell.value)
        except ValueError:
            return 0.0

    
    winner_sets = []
    loser_sets = []
    winner_odds_idxs = []
    loser_odds_idxs = []
    for col_idx in range(0, xl_sheet.ncols):
        cell = xl_sheet.cell(0, col_idx)
        name = cell.value.lower().strip()
        if name == 'tournament':
            tournament_idx = col_idx
        if name == 'court':
            court_idx = col_idx
        if name == 'surface':
            surface_idx = col_idx
        if name == 'round':
            round_idx = col_idx
        if name == 'best of':
            best_of_idx = col_idx
        if name == 'winner':
            winner_idx = col_idx
        if name == 'loser':
            loser_idx = col_idx
        if name == 'comment':
            comment_idx = col_idx
        if re.match(r'w\d', name):
            winner_sets.append(col_idx)
        if re.match(r'l\d', name):
            loser_sets.append(col_idx)
        if re.match(r'\w+w', name):
            winner_odds_idxs.append(col_idx)
        if re.match(r'\w+l', name):
            loser_odds_idxs.append(col_idx)
    
    matches = []
    for row_idx in range(1, xl_sheet.nrows):
        cell = xl_sheet.cell(row_idx, tournament_idx)
        tournament = cell.value.lower().strip()
        
        cell = xl_sheet.cell(row_idx, court_idx)
        court = cell.value.lower().strip()
        
        cell = xl_sheet.cell(row_idx, surface_idx)
        surface = cell.value.lower().strip()
        
        cell = xl_sheet.cell(row_idx, round_idx)
        the_round = cell.value.lower().strip()
        
        cell = xl_sheet.cell(row_idx, best_of_idx)
        best_of = int(cell.value)
        
        cell = xl_sheet.cell(row_idx, winner_idx)
        winner = cell.value.strip().lower()
        tokens = winner.split(' ')
        winner = ' '.join(t for t in tokens[:-1])
        
        cell = xl_sheet.cell(row_idx, loser_idx)
        loser = cell.value.strip().lower()
        tokens = loser.split(' ')
        loser = ' '.join(t for t in tokens[:-1])
        
        sets = []
        for idx in winner_sets:
            sets.append(get_set_score(row_idx, idx))
        for idx in loser_sets:
            sets.append(get_set_score(row_idx, idx))
        
        winner_odds = []
        for col_idx in winner_odds_idxs:
            winner_odds.append(get_odds(row_idx, col_idx))
            
        loser_odds = []
        for col_idx in loser_odds_idxs:
            loser_odds.append(get_odds(row_idx, col_idx))
        
        cell = xl_sheet.cell(row_idx, comment_idx)
        outcome = cell.value.lower().strip()
        if outcome == 'completed':
            matches.append((year,
                            tournament, 
                            court, 
                            surface, 
                            the_round, 
                            best_of, 
                            winner, 
                            loser,
                            round(sum(winner_odds)/len(winner_odds), 2), 
                            round(sum(loser_odds)/len(loser_odds), 2), 
                            *sets))
    return matches
        

def download_spreadsheets():
    filenames = []
    base_url = 'http://www.tennis-data.co.uk'
    for year in [2001 + i for i in range(0,17)]:
        url = '{0}/{1}/{1}.zip'.format(base_url, year)
        filename = '{}{}{}.zip'.format(utils.get_betting_directory(),
                                       os.sep,
                                       year)
        if not os.path.exists(filename):
            with urllib.request.urlopen(url) as response, open(filename, 'wb') as outfile:
                shutil.copyfileobj(response, outfile)
        with zipfile.ZipFile(filename, 'r') as zip_ref:
            zip_ref.extractall(utils.get_betting_directory())
            filenames.append((year, '{}{}{}'.format(utils.get_betting_directory(),
                                                     os.sep,
                                                     zip_ref.namelist()[0])))
    return filenames


def populate_table(cursor):
    for year, fname in download_spreadsheets():
        xl_workbook = xlrd.open_workbook(fname)
        sheet_names = list(filter(lambda x: x == '{}'.format(year), xl_workbook.sheet_names()))
        if len(sheet_names) == 1:
            matches = parse_sheet(year, xl_workbook.sheet_by_name(sheet_names[0]))
        else:
            matches = []
            for sname in xl_workbook.sheet_names():
                matches.extend(parse_sheet(year, xl_workbook.sheet_by_name(sname)))
        cursor.executemany('''INSERT INTO matches VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)''', 
                           matches)


def create_table(filename):
    conn = sqlite3.connect(filename)
    cursor = conn.cursor()
    cursor.execute('''CREATE TABLE matches 
                 (year int, tournament text, 
                 court text, surface text, 
                 round text, sets int, 
                 winner text, loser text,
                 w_odds float, l_odds float,
                 w_set_1 int, w_set_2 int, w_set_3 int, w_set_4 int, w_set_5 int, 
                 l_set_1 int, l_set_2 int, l_set_3 int, l_set_4 int, l_set_5 int)''')
    return conn, cursor    

    
def main():
    filename = '{}{}{}'.format(utils.get_betting_directory(), os.sep, 'tennis.db')
    if os.path.exists(filename):
        os.remove(filename)
    connection, cursor = create_table(filename)
    populate_table(cursor)
    connection.commit()
    connection.close()
    return 0
       

if __name__ == '__main__': 
    sys.exit(main())
    
    
                
            