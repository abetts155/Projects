import argparse
import collections
import dataclasses
import datetime
import fractions
import functools
import operator
import os
import sys
import typing

import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker

import cli.cli
import lib.messages
import lib.structure
import model.competitions
import model.fixtures
import model.seasons
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='Analyse accumulator possibilities')
    cli.cli.add_logging_options(parser)

    parser.add_argument(
        '-C',
        '--countries',
        type=str,
        help='pick out countries that start with this sequence'
    )

    parser.add_argument(
        '-L',
        '--lower',
        type=int,
        help='the least number of fixtures to consider on a certain date',
        default=3
    )

    parser.add_argument(
        '-U',
        '--upper',
        type=int,
        help='the most number of fixtures to consider on a certain date',
        default=10
    )

    parser.add_argument(
        '-s',
        '--start-date',
        type=lambda s: datetime.datetime.strptime(s, "%d-%m-%y").date(),
        help="Start date in DD-MM-YY format (e.g. 01-04-24)",
        default=datetime.datetime.strptime("01-01-20", "%d-%m-%y").date()
    )

    return parser.parse_args()


@dataclasses.dataclass(frozen=True)
class MatchCondition:
    func: typing.Callable or functools.partial
    period: model.fixtures.Period

    def __hash__(self):
        if isinstance(self.func, functools.partial):
            func_hash = hash(
                (
                    self.func.func,
                    self.func.args,
                    frozenset(self.func.keywords.items()) if self.func.keywords else None
                )
            )
        else:
            func_hash = hash(self.func)

        return hash((func_hash, self.period))


CompetitionMatchCondition = tuple[model.competitions.Competition, MatchCondition]
DatedAchievedTotal = tuple[datetime.date, int, int]


match_conditions = [
    MatchCondition(model.fixtures.Event.get('gfa_le_3'), model.fixtures.Period.FULL),
    MatchCondition(model.fixtures.Event.get('gfa_gt_1'), model.fixtures.Period.FULL),
    MatchCondition(model.fixtures.Event.get('gfa_gt_2'), model.fixtures.Period.FULL),
    MatchCondition(model.fixtures.Event.get('gfa_le_1'), model.fixtures.Period.FIRST),
    MatchCondition(model.fixtures.Event.get('gfa_gt_0'), model.fixtures.Period.FIRST),
    MatchCondition(model.fixtures.Event.get('gfa_gt_0'), model.fixtures.Period.SECOND),
    MatchCondition(model.fixtures.Event.get('gfa_le_2'), model.fixtures.Period.SECOND),
    MatchCondition(model.fixtures.Event.get('bts'), model.fixtures.Period.FULL),
]


def analyse(condition: MatchCondition, fixtures: list[model.fixtures.Fixture]) -> int:
    total = 0
    for fixture in fixtures:
        if fixture.finished:
            score = fixture.result(condition.period)
            if score is not None and condition.func(score):
                total += 1
    return total


def analyse_competition(
        comp: model.competitions.Competition,
        date: datetime.date,
        lower_threshold: int,
        upper_threshold: int,
        results: dict[CompetitionMatchCondition, list[DatedAchievedTotal]]
):
    db = lib.structure.get_database(comp.country)
    fixtures = model.fixtures.load_fixtures_on_date(db, comp, date)
    if lower_threshold <= len(fixtures) <= upper_threshold:
        for condition in match_conditions:
            percentage = analyse(condition, fixtures)
            key = (comp, condition)
            data = (date, percentage, len(fixtures))
            results[key].append(data)


def create_line_chart(
        key: CompetitionMatchCondition,
        data: list[DatedAchievedTotal],
        lower_threshold: int,
        upper_threshold: int
):
    competition, condition = key
    data.sort()
    dates, totals, matches = zip(*data)
    percentages = [100 * t / m for t, m in zip(totals, matches)]

    fig, ax = plt.subplots(figsize=(10, 6))

    norm = mcolors.Normalize(vmin=0, vmax=100)
    cmap = plt.colormaps['coolwarm']
    _ = ax.scatter(
        dates,
        percentages,
        c=percentages,
        cmap=cmap,
        norm=norm,
        marker='s',
        s=30,
        alpha=0.9
    )

    for x, y, z in zip(dates, percentages, matches):
        color = 'white' if y > 80 else 'black'
        ax.text(
            x, y, z,
            ha='center', va='center',
            fontsize=4, color=color
        )

    if type(condition.func) is functools.partial:
        assert condition.func.func == model.fixtures.gfa
        op, arg = condition.func.args
        numerator = 2 * arg + 1
        frac = float(fractions.Fraction(numerator / 2))

        if op in [operator.le, operator.lt]:
            condition_str = f"Percentage of Games Under {frac}"
            file_prefix = f"Under {frac}"
        else:
            assert op in [operator.ge, operator.gt]
            condition_str = f"Percentage of Games Over {frac}"
            file_prefix = f"Over {frac}"
    else:
        assert condition.func == model.fixtures.bts
        condition_str = "Percentage of Games where Both Teams Scored"
        file_prefix = f"BTS"

    title = f"{competition}: {condition_str} ({condition.period.value})."
    fig.suptitle(title, fontsize=10, fontproperties=lib.structure.noto_bold_font)

    if lower_threshold == upper_threshold:
        sentence_1 = f"Only considers match days with exactly {lower_threshold} games"
    else:
        sentence_1 = f"Only considers match days with between {lower_threshold} and {upper_threshold} games"
    
    sentence_2 = "Each marker includes #games on that date"
    match_days_100_per_cent = sum([t == m for t, m in zip(totals, matches)])
    sentence_3 = f"100% was achieved in {match_days_100_per_cent} of {len(data)} match days"
    sub_title = f"{sentence_1}. {sentence_2}. {sentence_3}."
    ax.set_title(sub_title, fontsize=6, fontproperties=lib.structure.noto_regular_font, pad=10)

    ax.set_ylabel("% achieved", fontsize=6, fontproperties=lib.structure.noto_regular_font)
    ax.set_facecolor('#f9f9f9')
    fig.patch.set_facecolor('#ffffff')
    ax.set_ylim(-5, 105)
    ax.yaxis.set_major_locator(ticker.MultipleLocator(10))
    ax.yaxis.set_minor_locator(ticker.MultipleLocator(5))
    ax.tick_params(axis='y', which='minor', length=4, color='gray')
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%d %b %y'))
    fig.autofmt_xdate()

    for spine in ax.spines.values():
        spine.set_visible(False)

    filename = f"{file_prefix} {condition.period.value}.png"
    path = lib.structure.get_accumulator_dir(competition.country, competition.name) / filename
    plt.tight_layout()
    plt.savefig(path, dpi=300)
    plt.close(fig)


def main(countries_prefix: str, start_date: datetime.date, lower_threshold: int, upper_threshold: int):
    whitelisted = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)

    countries = {c.country for c in whitelisted}
    if countries_prefix:
        prefix = countries_prefix.casefold()
        countries = {c for c in countries if c.casefold().startswith(prefix)}

    competitions_by_country = collections.defaultdict(list)
    for country in countries:
        db = lib.structure.get_database(country)
        comps = model.competitions.load_competitions(db, country)
        competitions_by_country[country].extend(c for c in comps if c in whitelisted)

    current_date = start_date
    cutoff = datetime.date.today() - datetime.timedelta(days=1)
    results: dict[CompetitionMatchCondition, list[DatedAchievedTotal]] = collections.defaultdict(list)
    while current_date <= cutoff:
        for country, competitions in competitions_by_country.items():
            for comp in competitions:
                path = lib.structure.get_accumulator_dir(country, comp.name)
                lib.structure.purge_png_files(path)
                analyse_competition(comp, current_date, lower_threshold, upper_threshold, results)

        current_date += datetime.timedelta(days=1)

    for key, value in results.items():
        create_line_chart(key, value, lower_threshold, upper_threshold)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.countries, args.start_date, args.lower, args.upper)
    sys.exit(os.EX_OK)
