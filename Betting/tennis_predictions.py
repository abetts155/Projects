#!/usr/bin/env python3

import itertools
import argparse
import sqlite3
import sys
import os
import re


class args:
    cmd = None
    tournament = None
    years = None
    court = None
    surface = None
    rounds = None
    players = None
    odds = None
    sets = None
    head_to_head = None
    

FAVOURITES = 'favourites'
PLAYERS = 'players'
    

def parse_command_line():
    parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter,
                                     description=
                                     'Tennis betting data analysis')
    
    parser.add_argument('--tournament',
                        type=str.lower,
                        help='only choose matches in this tournament')
    
    parser.add_argument('--years',
                        type=int,
                        nargs='+',
                        help='only consider matches in these years',
                        metavar='<INT>')
    
    parser.add_argument('--court',
                        choices=['indoor', 'outdoor'],
                        type=str.lower,
                        help='only choose matches on this court')
    
    parser.add_argument('--surface',
                        choices=['grass', 'clay', 'hard', 'carpet'],
                        type=str.lower,
                        help='only choose matches on this surface')
    
    parser.add_argument('--sets',
                        choices=[3, 5],
                        type=int,
                        help='only choose matches where this is the maximum number of sets')
    
    parser.add_argument('--rounds',
                        choices=['1', '2', '3', '4', 'q', 's', 'f'],
                        type=str.lower,
                        nargs='+',
                        help='only choose matches from these rounds')
    
    parser.add_argument('--odds',
                        type=float,
                        nargs=2,
                        metavar=('ODDS', 'PERCENTAGE'),
                        help='pick matches where player odds are within a percentage of this value'
                        ' (the first value is the odds and the second value is the percentage')
    
    subparsers = parser.add_subparsers(dest='cmd',
                                       help='choose one of {}'.
                                       format(' or '.join([FAVOURITES, PLAYERS])))
    subparsers.required = True
    
    subparsers.add_parser(FAVOURITES)
    
    parser_players = subparsers.add_parser(PLAYERS, 
                                           help='players help')
    
    parser_players.add_argument('players',
                                type=str.lower,
                                nargs='+',
                                help='only choose matches including these players')
    
    parser_players.add_argument('-H',
                                '--head-to-head',
                                action='store_true',
                                help='analyse head to head between listed players',
                                default=False)
    
    for arg, value in vars(parser.parse_args()).items():
        assert hasattr(args, arg), ('arg {} not found'.format(arg))
        if value is not None:
            setattr(args, arg, value)


def print_data(player, data, total_matches, scale):
    for key, value in data.items():
        print('With a handicap of {}, {} would have won {}% of {} matches'.
              format(scale * key,
                     player,
                     round(100 * (value/total_matches)),
                     total_matches))


def apply_handicap(player1_total, player2_total, handicap):
    if player1_total + handicap > player2_total:
        return 1
    return 0


HANDICAPS = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5]


def head_to_head_analysis(all_rows, pair):
    if all_rows:
        player1_won_data = {key:0 for key in HANDICAPS}
        player1_lost_data = {key: 0 for key in HANDICAPS}
        player2_won_data = {key:0 for key in HANDICAPS}
        player2_lost_data = {key: 0 for key in HANDICAPS}
        player1_won_games = []
        player2_won_games = []
        for r in all_rows:
            if pair[0] == r[6]:
                player1_won_games.append(r)
                player1_total = sum(r[i] for i in [-10, -9, -8, -7, -6])
                player2_total = sum(r[i] for i in [-5, -4, -3, -2, -1])
            else:
                player2_won_games.append(r)
                player2_total = sum(r[i] for i in [-10, -9, -8, -7, -6])
                player1_total = sum(r[i] for i in [-5, -4, -3, -2, -1])

            for h in HANDICAPS:
                if player1_total - h > player2_total:
                    player1_won_data[h] += 1
                if player1_total + h > player2_total:
                    player1_lost_data[h] += 1
                if player2_total + h > player1_total:
                    player2_lost_data[h] += 1
                if player2_total - h > player1_total:
                    player2_won_data[h] += 1

        print('>>>HEAD TO HEAD')
        print('{} won {} out of {} matches'.format(pair[0], len(player1_won_games), len(all_rows)))
        print('{} won {} out of {} matches'.format(pair[1], len(player2_won_games), len(all_rows)))
        print_data(pair[0], player1_won_data, len(all_rows), -1)
        print_data(pair[1], player2_won_data, len(all_rows), -1)

        print_data(pair[0], player1_lost_data, len(all_rows), 1)
        print_data(pair[1], player2_lost_data, len(all_rows), 1)
            

def player_analysis(all_rows, p):
    if all_rows:
        won_data = {key:0 for key in HANDICAPS}
        lost_data = {key:0 for key in HANDICAPS}
        won_games = []
        lost_games = []
        for r in all_rows:
            print(r)
            if p == r[6]:
                won_games.append(r)
                player_total = sum(r[i] for i in [-10, -9, -8, -7, -6])
                opponent_total = sum(r[i] for i in [-5, -4, -3, -2, -1])
            else:
                lost_games.append(r)
                opponent_total = sum(r[i] for i in [-10, -9, -8, -7, -6])
                player_total = sum(r[i] for i in [-5, -4, -3, -2, -1])
            for h in HANDICAPS:
                won_data[h] += apply_handicap(player_total, opponent_total, -h)
            for h in HANDICAPS:
                lost_data[h] += apply_handicap(player_total, opponent_total, h)

        print('>>> PER PLAYER ANALYSIS')
        print('{} won {} out of {} matches'.format(p, len(won_games), len(all_rows)))
        print('{} lost {} out of {} matches'.format(p, len(lost_games), len(all_rows)))
        print_data(p, won_data, len(all_rows), -1)
        print_data(p, lost_data, len(all_rows), 1)


def favourites_analysis(all_rows):
    if all_rows:
        won_data = {key:0 for key in HANDICAPS}
        lost_data = {key:0 for key in HANDICAPS}
        won_games = []
        lost_games = []
        for r in all_rows:
            if r[8] < r[9]:
                favourite_indices = -10, -9, -8, -7, -6
                non_favourite_indices = -5, -4, -3, -2, -1
                won_games.append(r)
            elif r[8] > r[9]:
                non_favourite_indices = -10, -9, -8, -7, -6
                favourite_indices = -5, -4, -3, -2, -1
                lost_games.append(r)
            else:
                continue

            favourite_total = sum(r[i] for i in favourite_indices)
            non_favourite_total = sum(r[i] for i in non_favourite_indices)
            for h in HANDICAPS:
                if favourite_total - h > non_favourite_total:
                    won_data[h] += 1
                if favourite_total + h > non_favourite_total:
                    lost_data[h] += 1

        print('>>> FAVOURITES ANALYSIS')
        print('The {} won {} out of {} matches'.format(FAVOURITES, len(won_games), len(all_rows)))
        print('The {} lost {} out of {} matches'.format(FAVOURITES, len(lost_games), len(all_rows)))
        print_data(FAVOURITES, won_data, len(all_rows), -1)
        print_data(FAVOURITES, lost_data, len(all_rows), 1)


def do_query(filename):
    predicates = []
    
    if args.years:
        predicates.append('({})'.format(' or '.join("year='%s'" % y for y in args.years)))
        
    if args.court:
        predicates.append("court='{}'".format(args.court))
    
    if args.surface:
        predicates.append("surface='{}'".format(args.surface))
        
    if args.rounds:
        rounds = []
        for r in args.rounds:
            if r == '1':
                rounds.append('1st round')
            elif r == '2':
                rounds.append('2nd round')
            elif r == '3':
                rounds.append('3rd round')
            elif r == '4':
                rounds.append('4th round')
            elif r == 'q':
                rounds.append('quarterfinals')
            elif r == 's':
                rounds.append('semifinals') 
            elif r == 'f':
                rounds.append('the final') 
        predicates.append('({})'.format(' or '.join("round='%s'" % r for r in rounds)))    
                    
    if args.odds:
        odds = args.odds[0]
        percentage = args.odds[1]
        slack = percentage*(odds/100.0)
        predicates.append("(({0}>='{2:.3}' and {0}<='{3:.3}') or ({1}>='{2:.3}' and {1}<='{3:.3}'))".
                          format('w_odds', 'l_odds', odds - slack, odds + slack))
            
    if args.sets:
        predicates.append('sets={}'.format(args.sets))
    
    conn = sqlite3.connect(filename)
    cursor = conn.cursor()
    if args.tournament:
        query = "SELECT DISTINCT tournament from matches"
        cursor.execute(query)
        all_tournaments = [t[0] for t in cursor.fetchall()]
        regex = re.compile(r'{}'.format(args.tournament))
        matches = [l for l in all_tournaments for m in [regex.match(l)] if m]
        if len(matches) != 1:
            print("Unable to disambiguate tournament '{}' among {}".
                  format(args.tournament, ','.join("'%s'" % m for m in matches)))
            sys.exit(1)
        predicates.append("tournament='{}'".format(matches[0]))

    if args.cmd == FAVOURITES:
        query = "SELECT * from matches where {}".format(' and '.join(p for p in predicates))
        cursor.execute(query)
        favourites_analysis(cursor.fetchall())
    elif args.cmd == PLAYERS:
        for p in args.players:
            predicates.append("(winner='{0}' or loser='{0}')".format(p))
            query = "SELECT * from matches where {}".format(' and '.join(p for p in predicates))
            cursor.execute(query)
            player_analysis(cursor.fetchall(), p)
            del predicates[-1]
        if args.head_to_head:
            for pair in itertools.combinations(args.players, 2):
                predicates.append("((winner='{0}' and loser='{1}') or "
                                  "(loser='{0}' and winner='{1}'))".format(*pair))
                query = "SELECT * from matches where {}".format(' and '.join(p for p in predicates))
                cursor.execute(query)
                head_to_head_analysis(cursor.fetchall(), pair)
                del predicates[-1]
    conn.close()
            

def main():
    filename = '{}{}{}'.format(get_betting_directory(), os.sep, 'tennis.db')
    if not os.path.exists(filename):
        print("Tennis database '{}' does not exist. First run Python script: create_tennis_database.py.".format(filename))
        sys.exit(1)
    parse_command_line()
    do_query(filename)
    return 0
       

if __name__ == '__main__': 
    sys.exit(main())