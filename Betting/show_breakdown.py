from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_half_option,
                     add_logging_options,
                     add_venue_option,
                     set_logging_options,
                     add_team_option,
                     add_block_option,
                     get_unique_league)
from matplotlib import pyplot as plt
from model.fixtures import Event, Half, Venue, win, loss
from model.leagues import league_register
from model.seasons import Season
from model.teams import Team
from sql.sql import extract_picked_team, load_league, load_teams
from typing import Callable, Dict


def parse_command_line():
    parser = ArgumentParser(description='Show breakdown of results')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_half_option(parser)
    add_venue_option(parser)
    add_logging_options(parser)
    add_team_option(parser, True)
    add_block_option(parser)
    return parser.parse_args()


def populate(season: Season, team: Team, venue: Venue, half: Half, values: Dict, func: Callable):
    fixtures = [fixture for fixture in season.fixtures() if fixture.home_team == team or fixture.away_team == team]

    if venue:
        if venue == Venue.home:
            fixtures = [fixture for fixture in fixtures if fixture.home_team == team]
        elif venue == Venue.away:
            fixtures = [fixture for fixture in fixtures if fixture.away_team == team]

    for fixture in fixtures:
        if half == Half.both:
            result = fixture.full_time()
        elif half == Half.first:
            result = fixture.first_half()
        elif half == Half.second:
            result = fixture.second_half()
        else:
            assert False

        result = fixture.canonicalise_result(team, result)
        if func(result):
            margin = result.left - result.right
            values.setdefault(margin, 0)
            values[margin] += 1


class BarContainer:
    __slots__ = ['data', 'color', 'title']

    def __init__(self, color: str, title: str):
        self.data = {}
        self.color = color
        self.title = title


def add_bar_chart(ax, container: BarContainer):
    x_values = []
    y_values = []
    x_labels = []
    for key in sorted(container.data.keys()):
        x_labels.append(key)
        x_values.append(key)
        y_values.append(container.data[key])

    ax.bar(x_values, y_values, color=container.color, edgecolor='black')

    total = sum(container.data.values())
    for k, v in container.data.items():
        ax.text(k, v, '{} ({:.1f}%)'.format(v, (v / total) * 100), ha='center', fontsize=8, fontweight='bold')

    ax.set_title(container.title)
    ax.set_yticks([])
    ax.set_xticks(x_labels)
    ax.set_xticklabels(x_labels)


def main(args: Namespace):
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if args.history:
        seasons = seasons[-args.history:]

    this_season = seasons.pop()
    (row,) = extract_picked_team(args.database, args.team, league)
    team = Team.inventory[row[0]]

    events = [win, loss]
    colors = ['dodgerblue', 'salmon']
    data = []
    for i, func in enumerate(events):
        title = '{} margin (Seasons:{}-{})'.format(Event.name(func, False), seasons[0].year, seasons[-1].year)
        container_a = BarContainer(colors[i], title)
        for season in seasons:
            populate(season, team, args.venue, args.half, container_a.data, func)

        title = '{} margin (Season:{})'.format(Event.name(func, False), this_season.year)
        container_b = BarContainer(colors[i], title)
        populate(this_season, team, args.venue, args.half, container_b.data, func)
        data.append([container_a, container_b])

    fig, axs = plt.subplots(nrows=2, ncols=2, figsize=(10, 10), squeeze=False, constrained_layout=True)
    for row in range(len(events)):
        containers = data[row]
        for col, container in enumerate(containers):
            ax = axs[row, col]
            add_bar_chart(ax, container)

    title = '{} {}: {}'.format(league.country, league.name, team.name)
    if args.venue == Venue.any:
        title = '{} ({} or {})'.format(title, Venue.home.name, Venue.away.name)
    else:
        title = '{} ({} only)'.format(title, args.venue.name)

    if args.half != Half.both:
        title = '{} ({} half)'.format(title, args.half.name)

    fig.suptitle(title, fontweight='bold', fontsize=14)
    plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)
