
from lib.football import Fixture


def draw(fixture):
    """draw"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    return home_goals == away_goals


def win(fixture):
    """win"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    if fixture.venue == Fixture.HOME_GAME:
        return home_goals > away_goals
    else:
        return away_goals > home_goals
    

def loss(fixture):
    """loss"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    if fixture.venue == Fixture.AWAY_GAME:
        return home_goals > away_goals
    else:
        return away_goals > home_goals
    
    
def btts(fixture):
    """both teams to score"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    return home_goals > 0 and away_goals > 0


def gt_2_goals(fixture):
    """over 2.5 goals"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    return home_goals + away_goals > 2


def le_2_goals(fixture):
    """under 2.5 goals"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    return home_goals + away_goals <= 2


def clean_sheet(fixture):
    """clean sheet"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    if fixture.venue == Fixture.HOME_GAME:
        return away_goals == 0
    else:
        return home_goals == 0
    

def blank(fixture):
    """blank"""
    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
    if fixture.venue == Fixture.HOME_GAME:
        return home_goals == 0
    else:
        return away_goals == 0
