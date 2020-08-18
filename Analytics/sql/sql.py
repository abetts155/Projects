import sqlite3

from model import competitions, events, matches, teams


def create_tables(cursor: sqlite3.Cursor):
    classes = [competitions.Competition, teams.Coach, teams.Player, teams.Team, events.Event, matches.Match]
    for cls in classes:
        table = cls.sql_table()
        table.drop(cursor)
        table.create(cursor)


def create_database(database_name: str):
    connection = sqlite3.connect(database_name)
    cursor = connection.cursor()
    create_tables(cursor)
    connection.commit()
    connection.close()


def create_rows(database_name: str):
    connection = sqlite3.connect(database_name)
    cursor = connection.cursor()

    table = competitions.Competition.sql_table()
    for competition in competitions.Competition.inventory.values():
        table.add_row(competition.sql_values())
    table.insert_rows(cursor)

    table = teams.Coach.sql_table()
    for coach in teams.Coach.inventory.values():
        table.add_row(coach.sql_values())
    table.insert_rows(cursor)

    table = teams.Player.sql_table()
    for player in teams.Player.inventory.values():
        table.add_row(player.sql_values())
    table.insert_rows(cursor)

    table = teams.Team.sql_table()
    for team in teams.Team.inventory.values():
        table.add_row(team.sql_values())
    table.insert_rows(cursor)

    table = events.Event.sql_table()
    for event in events.Event.inventory.values():
        table.add_row(event.sql_values())
    table.insert_rows(cursor)

    table = matches.Match.sql_table()
    for match in matches.Match.inventory.values():
        table.add_row(match.sql_values())
    table.insert_rows(cursor)

    connection.commit()
    connection.close()
