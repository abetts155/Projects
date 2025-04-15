import argparse
import collections
import dataclasses
import datetime
import fractions
import functools
import operator
import pathlib
import typing

import colorama
import matplotlib.pyplot as plt
import matplotlib.ticker
import zoneinfo

import cli.cli
import lib.messages
import lib.structure
import model.competitions
import model.fixtures
import model.seasons
import model.tables
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='Analyse events')
    cli.cli.add_logging_options(parser)

    parser.add_argument('-C',
                        '--countries',
                        nargs='+',
                        type=str,
                        help='pick out countries that start with this sequence')

    parser.add_argument('-l',
                        '--lower',
                        help='left-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=0)

    parser.add_argument('-u',
                        '--upper',
                        help='right-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=7 * 24)

    parser.add_argument('-T',
                        '--tolerance',
                        help='the tolerance',
                        metavar='<FLOAT>',
                        type=float,
                        default=0.8)

    parser.add_argument('-t',
                        '--tweets',
                        action='store_true',
                        help='tweet the predictions',
                        default=False)

    return parser.parse_args()


@dataclasses.dataclass(slots=True, frozen=True)
class Bet:
    func: collections.abc.Callable
    negate: bool
    venue: model.fixtures.Venue
    period: model.fixtures.Period


bets = [
    Bet(
        model.fixtures.Event.get('draw'),
        True,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('draw'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gf_eq_0'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('ga_eq_0'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('ga_eq_0'),
        False,
        model.fixtures.Venue.AWAY,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_le_1'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_le_1'),
        False,
        model.fixtures.Venue.HOME,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_le_1'),
        False,
        model.fixtures.Venue.AWAY,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_le_2'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_le_2'),
        False,
        model.fixtures.Venue.HOME,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_le_2'),
        False,
        model.fixtures.Venue.AWAY,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('win'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('bts'),
        True,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_eq_0'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_eq_0'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FIRST
    ),

    Bet(
        model.fixtures.Event.get('gfa_eq_0'),
        False,
        model.fixtures.Venue.HOME,
        model.fixtures.Period.FIRST
    ),

    Bet(
        model.fixtures.Event.get('gfa_eq_0'),
        False,
        model.fixtures.Venue.AWAY,
        model.fixtures.Period.FIRST
    ),

    Bet(
        model.fixtures.Event.get('gfa_eq_0'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.SECOND
    ),

    Bet(
        model.fixtures.Event.get('gfa_eq_0'),
        False,
        model.fixtures.Venue.HOME,
        model.fixtures.Period.SECOND
    ),

    Bet(
        model.fixtures.Event.get('gfa_eq_0'),
        False,
        model.fixtures.Venue.AWAY,
        model.fixtures.Period.SECOND
    ),

    Bet(
        model.fixtures.Event.get('gf_eq_0'),
        False,
        model.fixtures.Venue.HOME,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gf_eq_0'),
        False,
        model.fixtures.Venue.AWAY,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('draw'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FIRST
    ),

    Bet(
        model.fixtures.Event.get('draw'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.SECOND
    ),

    Bet(
        model.fixtures.Event.get('draw'),
        True,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FIRST
    ),

    Bet(
        model.fixtures.Event.get('gfa_ne_0'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FIRST
    ),

    Bet(
        model.fixtures.Event.get('bts'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_gt_2'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    ),

    Bet(
        model.fixtures.Event.get('gfa_gt_3'),
        False,
        model.fixtures.Venue.ANYWHERE,
        model.fixtures.Period.FULL
    )
]


def fixtures_within_the_window(
        fixtures: list[model.fixtures.Fixture],
        left_hand_side: datetime.datetime,
        right_hand_side: datetime.datetime
) -> list[model.fixtures.Fixture]:
    fixtures_to_analyse = []
    for fixture in fixtures:
        if not fixture.finished:
            fixture_datetime = datetime.datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
            if left_hand_side <= fixture_datetime <= right_hand_side:
                fixtures_to_analyse.append(fixture)
    return fixtures_to_analyse


@dataclasses.dataclass(slots=True, frozen=True)
class VenueAndPeriod:
    venue: model.fixtures.Venue
    period: model.fixtures.Period


@dataclasses.dataclass(slots=True)
class Scorelines:
    scorelines: dict[VenueAndPeriod, list[model.fixtures.Scoreline]]

    @staticmethod
    def create() -> "Scorelines":
        return Scorelines(
            {
                VenueAndPeriod(venue, period): []
                for venue in model.fixtures.Venue for period in model.fixtures.Period.regular()
            }
        )


def collect_data(
        league: model.competitions.Competition,
        season: model.seasons.Season,
        data: dict[model.teams.Team, Scorelines]
):
    database = lib.structure.get_database(league.country)
    fixtures = model.seasons.load_fixtures(database, league, season)
    teams = model.fixtures.teams(fixtures)
    for team in teams:
        if team not in data:
            data[team] = Scorelines.create()

    for fixture in fixtures:
        if fixture.finished:
            home_data = data[fixture.home_team]
            away_data = data[fixture.away_team]

            for period in model.fixtures.Period.regular():
                scoreline = fixture.result(period)
                if scoreline is not None:
                    home_canonical = model.fixtures.canonicalise_scoreline(fixture, fixture.home_team, scoreline)
                    away_canonical = model.fixtures.canonicalise_scoreline(fixture, fixture.away_team, scoreline)

                    home_key = VenueAndPeriod(model.fixtures.Venue.HOME, period)
                    away_key = VenueAndPeriod(model.fixtures.Venue.AWAY, period)
                    anywhere_key = VenueAndPeriod(model.fixtures.Venue.ANYWHERE, period)

                    home_data.scorelines[home_key].append(home_canonical)
                    home_data.scorelines[anywhere_key].append(home_canonical)
                    away_data.scorelines[away_key].append(away_canonical)
                    away_data.scorelines[anywhere_key].append(away_canonical)


def aggregate(bet: Bet, data: list[Scorelines]) -> collections.Counter:
    key = VenueAndPeriod(bet.venue, bet.period)
    counter = collections.Counter()
    for team_data in data:
        sequence = []
        for scoreline in team_data.scorelines[key]:
            if bet.negate:
                outcome = not bet.func(scoreline)
            else:
                outcome = bet.func(scoreline)

            if outcome:
                sequence.append(scoreline)
            else:
                counter[len(sequence)] += 1
                sequence = []

        counter[len(sequence)] += 1

    return counter


def country_flag(code: str) -> str:
    return ''.join(chr(0x1F1E6 + ord(c.upper()) - ord('A')) for c in code)


@dataclasses.dataclass(slots=True, frozen=True)
class Prediction:
    bet: Bet
    league: model.competitions.Competition
    team: model.teams.Team
    fixture: model.fixtures.Fixture
    run: int
    counter: collections.Counter
    history: int


def gather_predictions(
        league: model.competitions.Competition,
        fixtures_to_analyse: list[model.fixtures.Fixture],
        historical_data: dict[model.teams.Team, Scorelines],
        this_season_data: dict[model.teams.Team, Scorelines],
        tolerance: float
) -> list[Prediction]:
    home_teams_to_analyse = {}
    away_teams_to_analyse = {}
    for fixture in fixtures_to_analyse:
        home_teams_to_analyse[fixture.home_team] = fixture
        away_teams_to_analyse[fixture.away_team] = fixture

    database = lib.structure.get_database(league.country)
    seasons = model.seasons.load_seasons(database, league)
    predictions = []
    for bet in bets:
        counter = aggregate(bet, list(historical_data.values()))

        key = VenueAndPeriod(bet.venue, bet.period)
        for team, team_data in this_season_data.items():
            fixture = None
            if bet.venue == model.fixtures.Venue.ANYWHERE:
                if team in home_teams_to_analyse:
                    fixture = home_teams_to_analyse[team]
                elif team in away_teams_to_analyse:
                    fixture = away_teams_to_analyse[team]
            elif bet.venue == model.fixtures.Venue.HOME:
                if team in home_teams_to_analyse:
                    fixture = home_teams_to_analyse[team]
            elif bet.venue == model.fixtures.Venue.AWAY:
                if team in away_teams_to_analyse:
                    fixture = away_teams_to_analyse[team]

            if fixture is not None:
                sequence = []
                for scoreline in team_data.scorelines[key]:
                    if bet.negate:
                        outcome = not bet.func(scoreline)
                    else:
                        outcome = bet.func(scoreline)

                    if outcome:
                        sequence.append(scoreline)
                    else:
                        sequence = []

                longest = max(counter.keys())
                if len(sequence) >= tolerance * longest:
                    prediction = Prediction(bet, league, team, fixture, len(sequence), counter, len(seasons))
                    predictions.append(prediction)

    return predictions


def output_the_predictions(predictions: list[Prediction]):
    home_color = colorama.Fore.BLUE
    away_color = colorama.Fore.RED
    last_prediction = None
    for prediction in predictions:
        time_change = (
                last_prediction is None or
                last_prediction.fixture.date.day != prediction.fixture.date.day or
                last_prediction.fixture.date.hour != prediction.fixture.date.hour
        )

        if time_change:
            print()
            time_str = prediction.fixture.date.strftime('%-I %p')
            date_str = prediction.fixture.date.strftime('%d %B %Y')
            print('-' * 80)
            print(f"{time_str} on {date_str}")
            print('-' * 80, end='')

        league_change = last_prediction is None or last_prediction.league != prediction.league
        if time_change or league_change:
            print()
            flag = country_flag(prediction.league.get_2_letter_iso_code())
            league_str = f" {flag} {prediction.league} "
            print('*' * len(league_str))
            print(league_str)
            print('*' * len(league_str), end='')

        fixture_change = last_prediction is None or last_prediction.fixture != prediction.fixture
        if fixture_change:
            print()
            fixture_str = f"[{prediction.fixture.date.strftime('%H.%M: %d %b %Y')}]"
            home_str = f"{home_color}{prediction.fixture.home_team.name}{colorama.Style.RESET_ALL}"
            away_str = f"{away_color}{prediction.fixture.away_team.name}{colorama.Style.RESET_ALL}"
            print(f"{fixture_str} {home_str} vs {away_str}")

        if prediction.team == prediction.fixture.home_team:
            color = home_color
        else:
            color = away_color
        team_str = f"{color}{prediction.team.name}{colorama.Style.RESET_ALL}"
        event_info = model.fixtures.Event.name(prediction.bet.func, prediction.bet.negate)
        longest = max(prediction.counter.keys())
        event_str = f"{event_info} in the last {prediction.run} (longest={longest})"
        constraint_str = f"{prediction.bet.venue}) ({prediction.bet.period.value}"
        print(f"> {team_str}: {event_str} ({constraint_str})")

        last_prediction = prediction

    print()


def tweet_prediction(prediction: Prediction) -> str:
    period_str = {
        model.fixtures.Period.FULL: "in FT",
        model.fixtures.Period.FIRST: "in the 1st half",
        model.fixtures.Period.SECOND: "in the 2nd half"
    }.get(prediction.bet.period)

    def get_over_under_label(op: typing.Callable, negate: bool):
        if op in [operator.gt, operator.ge, operator.ne]:
            return "OVER" if negate else "UNDER"
        else:
            assert op in [operator.lt, operator.le, operator.eq]
            return "UNDER" if negate else "OVER"

    if isinstance(prediction.bet.func, functools.partial):
        op, arg = prediction.bet.func.args
        over_under_str = get_over_under_label(op, prediction.bet.negate)
        numerator = 2 * arg + 1
        frac = float(fractions.Fraction(numerator / 2))

        if prediction.bet.func.func == model.fixtures.gfa:
            return f"{over_under_str} {frac} {period_str}"
        elif prediction.bet.func.func == model.fixtures.gf:
            return f"{prediction.team.name} to score {over_under_str} {frac} {period_str}"
        else:
            assert prediction.bet.func.func == model.fixtures.ga
            return f"{prediction.team.name} to concede {over_under_str} {frac} {period_str}"
    else:
        if prediction.bet.func == model.fixtures.draw:
            if prediction.bet.negate:
                return f"Draw {period_str}"
            else:
                return f"{prediction.fixture.home_team.name} or {prediction.fixture.away_team.name} to win {period_str}"
        elif prediction.bet.func == model.fixtures.win:
            if prediction.bet.negate:
                return f"{prediction.team.name} to win {period_str}"
            else:
                return f"{prediction.team.name} to draw or lose {period_str}"
        elif prediction.bet.func == model.fixtures.loss:
            if prediction.bet.negate:
                return f"{prediction.team.name} to lose {period_str}"
            else:
                return f"{prediction.team.name} to draw or win {period_str}"
        else:
            assert prediction.bet.func == model.fixtures.bts
            if prediction.bet.negate:
                return f"BOTH teams to score {period_str}"
            else:
                return f"NO or just ONE team to score {period_str}"


def tweet_rationale(prediction: Prediction) -> str:
    period_str = {
        model.fixtures.Period.FULL: "games",
        model.fixtures.Period.FIRST: "1st halves",
        model.fixtures.Period.SECOND: "2nd halves"
    }.get(prediction.bet.period)

    def get_over_under_label(op: typing.Callable, negate: bool):
        if op in [operator.gt, operator.ge, operator.ne]:
            return "UNDER" if negate else "OVER"
        else:
            assert op in [operator.lt, operator.le, operator.eq]
            return "OVER" if negate else "UNDER"

    if isinstance(prediction.bet.func, functools.partial):
        op, arg = prediction.bet.func.args
        over_under_str = get_over_under_label(op, prediction.bet.negate)
        numerator = 2 * arg + 1
        frac = float(fractions.Fraction(numerator / 2))

        if prediction.bet.func.func == model.fixtures.gfa:
            event_str = f"have had {over_under_str} {frac} in"
        elif prediction.bet.func.func == model.fixtures.gf:
            event_str = f"have scored {over_under_str} {frac} in"
        else:
            assert prediction.bet.func.func == model.fixtures.ga
            event_str = f"have conceded {over_under_str} {frac} in"
    else:
        if prediction.bet.func == model.fixtures.draw:
            if prediction.bet.negate:
                event_str = f"have NOT drawn"
            else:
                event_str = f"have drawn"
        elif prediction.bet.func == model.fixtures.win:
            if prediction.bet.negate:
                event_str = f"have NOT won"
            else:
                event_str = f"have won"
        elif prediction.bet.func == model.fixtures.loss:
            if prediction.bet.negate:
                event_str = f"have NOT lost"
            else:
                event_str = f"have lost"
        else:
            assert prediction.bet.func == model.fixtures.bts
            if prediction.bet.negate:
                event_str = f"have had NO or just ONE team scoring in"
            else:
                event_str = f"have had BOTH teams scoring in"

    def pluralise(n: int) -> str:
        return "ONCE" if n == 1 else "TWICE" if n == 2 else f"{n} times"

    team_str = f"{prediction.team.name} {event_str} the last {prediction.run} {period_str} ({prediction.bet.venue.value})"

    longest = max(prediction.counter.keys())
    beyond = {
        key: prediction.counter[key]
        for key in sorted(prediction.counter.keys())
        if key >= prediction.run
    }

    if len(beyond) > 1:
        phrases = [f"{k} ({pluralise(v)})" for k, v in beyond.items()]
        joined = ", ".join(phrases)
        freq_str = f"Runs of length {joined} have occurred"
    else:
        if beyond[longest] == 1:
            freq_str = "This is THE longest run and is the FIRST time it has occurred"
        else:
            freq_str = f"This is THE longest run and has occurred {pluralise(beyond[longest])}"

    longest_str = f"{freq_str} in the past {prediction.history} seasons"
    return f"\u27A4 {team_str}. {longest_str}."


def create_tweet_bar_chart(
        prediction: Prediction,
        file: pathlib.Path
):
    labels, values = zip(*prediction.counter.most_common())
    highlight_color = '#E76F51'
    default_color = '#4B9CD3'
    bar_colors = [highlight_color if label == prediction.run else default_color for label in labels]

    fig, ax = plt.subplots(figsize=(10, 6))
    bars = ax.barh(labels, values, color=bar_colors, edgecolor="black", linewidth=0.5)

    ax.set_ylabel('Streak', fontsize=12)
    ax.set_xlabel('#', fontsize=12)
    ax.invert_yaxis()
    ax.xaxis.set_major_locator(matplotlib.ticker.MaxNLocator(integer=True))
    ax.yaxis.set_major_locator(matplotlib.ticker.MaxNLocator(integer=True))

    for spine in ax.spines.values():
        spine.set_visible(False)

    for bar, label in zip(bars, labels):
        count = int(bar.get_width())
        if label == prediction.run:
            annotation = f'{count} ({prediction.team.name})'
        else:
            annotation = f'{count}'
        ax.text(
            count + 0.5, bar.get_y() + bar.get_height() / 2,
            annotation,
            va='center',
            fontsize=10,
            fontproperties=lib.structure.noto_regular_font
        )

    def get_over_under(op: typing.Callable, negate: bool):
        if op in [operator.gt, operator.ge, operator.ne]:
            return "under" if negate else "over"
        elif op in [operator.lt, operator.le, operator.eq]:
            return "over" if negate else "under"
        else:
            assert False, "Unsupported operator"

    if isinstance(prediction.bet.func, functools.partial):
        op, arg = prediction.bet.func.args
        numerator = 2 * arg + 1
        frac = float(fractions.Fraction(numerator / 2))
        over_or_under_str = get_over_under(op, prediction.bet.negate)

        if prediction.bet.func.func == model.fixtures.gfa:
            title_prefix = f"Total goals {over_or_under_str} {frac}"
        elif prediction.bet.func.func == model.fixtures.gf:
            title_prefix = f"Scored {over_or_under_str} {frac}"
        elif prediction.bet.func.func == model.fixtures.ga:
            title_prefix = f"Conceded {over_or_under_str} {frac}"
    else:
        if prediction.bet.func == model.fixtures.draw:
            if prediction.bet.negate:
                title_prefix = f"Win or lose (never a draw)"
            else:
                title_prefix = f"Draw after draw after draw"
        elif prediction.bet.func == model.fixtures.win:
            if prediction.bet.negate:
                title_prefix = f"They cannot win"
            else:
                title_prefix = f"Consistently winning"
        elif prediction.bet.func == model.fixtures.loss:
            if prediction.bet.negate:
                title_prefix = f"Unbeaten"
            else:
                title_prefix = f"They just keep losing"
        else:
            assert prediction.bet.func == model.fixtures.bts
            if prediction.bet.negate:
                title_prefix = f"Only one or no team hits the net"
            else:
                title_prefix = f"Both teams keep scoring"

    title = f"{title_prefix} in the {prediction.league} ({prediction.bet.period.value}, {prediction.bet.venue.value})"
    fig.suptitle(title, fontsize=12, fontproperties=lib.structure.noto_bold_font)
    sub_title = f"Data from the last {prediction.history} seasons"
    ax.set_title(sub_title, fontsize=8, fontproperties=lib.structure.noto_regular_font, pad=10)

    plt.tight_layout()
    plt.savefig(file, dpi=300)
    plt.close()


def tweet_the_predictions(predictions: list[Prediction]):
    grouped_by_fixture: dict[model.fixtures.Fixture, list[Prediction]] = collections.defaultdict(list)
    for prediction in predictions:
        grouped_by_fixture[prediction.fixture].append(prediction)

    for fixture, fixture_predictions in grouped_by_fixture.items():
        localized = fixture.date.replace(tzinfo=zoneinfo.ZoneInfo("Europe/London"))
        time_str = localized.strftime('%H:%M %Z')
        print(f"{colorama.Style.BRIGHT}{colorama.Fore.BLUE}{time_str} {fixture.id}{colorama.Style.RESET_ALL}")
        first_prediction = fixture_predictions[0]
        flag = country_flag(first_prediction.league.get_2_letter_iso_code())
        print(f"{flag} {time_str}: {fixture.home_team.name} vs {fixture.away_team.name} ({first_prediction.league})")
        print()
        tweet_png_dir = lib.structure.get_tweet_dir(first_prediction.league.country, first_prediction.league.name)
        for prediction in fixture_predictions:
            print(f"{tweet_prediction(prediction)} because:")
            print(tweet_rationale(prediction))
            print()

            png_file = lib.structure.get_tweet_png_file(tweet_png_dir, prediction.fixture.id)
            png_file.touch()
            create_tweet_bar_chart(prediction, png_file)

        print()


def output_best_and_worst(
        table: model.tables.LeagueTable,
        team: model.teams.Team,
        venue: model.fixtures.Venue,
        columns: list[str],
        top_2: dict[model.fixtures.Venue, dict[str, float|int]],
        bottom_2: dict[model.fixtures.Venue, dict[str, float|int]]
):
    print(f"➤ {team.name} ({venue.value})")
    for col in columns:
        value = table.df.loc[table.df[model.tables.TEAM_COL] == team, col].values[0]
        first_top, second_top = top_2[venue][col]

        emoji = None
        if value == first_top:
            emoji = '🥇'
        elif value == second_top:
            emoji = '🥈'
        elif value in bottom_2[venue][col]:
            emoji = '❌'

        if emoji is not None:
            print(f"{emoji} {model.tables.translate(col)}: {value}{'%' if col in model.tables.PERCENTAGE_COLS else ''}")


def create_tweet_histogram(
        df,
        columns: list[str],
        league: model.competitions.Competition,
        team: model.teams.Team,
        fixture: model.fixtures.Fixture,
        venue: model.fixtures.Venue
):
    stats_df = df[df[model.tables.TEAM_COL] == team].squeeze()

    fig, axes = plt.subplots(nrows=3, ncols=3, figsize=(15, 9))
    axes = axes.flatten()

    bar_color = '#90CAF9'
    highlight_color = '#F44336'

    for i, col in enumerate(columns):
        ax = axes[i]
        values = df[col].dropna()
        team_val = stats_df[col]

        counts, bins, patches = ax.hist(values, bins=15, edgecolor='black')

        bin_index = None
        for j in range(len(bins) - 1):
            if bins[j] <= team_val < bins[j + 1]:
                bin_index = j
                break

        if team_val == bins[-1]:
            bin_index = len(bins) - 2

        for j, patch in enumerate(patches):
            patch.set_facecolor(bar_color)
            if j == bin_index:
                patch.set_facecolor(highlight_color)

        ax.set_title(model.tables.translate(col), fontsize=8, fontproperties=lib.structure.noto_regular_font)
        ax.set_ylabel("#Teams", fontsize=6, fontproperties=lib.structure.noto_regular_font)
        if col in model.tables.PERCENTAGE_COLS:
            ax.set_xlabel('% of Games', fontsize=6, fontproperties=lib.structure.noto_regular_font)
        else:
            ax.set_xlabel('per Game', fontsize=6, fontproperties=lib.structure.noto_regular_font)

        for spine in ax.spines.values():
            spine.set_visible(False)

    plt.suptitle(f"{team.name} {venue.value}", fontsize=12, fontproperties=lib.structure.noto_bold_font)
    tweet_png_dir = lib.structure.get_tweet_dir(league.country, league.name)
    file = lib.structure.get_tweet_png_file(tweet_png_dir, fixture.id)
    fig.tight_layout(pad=3.0, w_pad=2.0, h_pad=1.0)
    plt.savefig(file, dpi=300)
    plt.close()


def output_features(
        team: model.teams.Team,
        table: model.tables.LeagueTable,
        venue: model.fixtures.Venue,
        columns: list[str],
        top_2: dict[model.fixtures.Venue, dict[str, float|int]],
        bottom_2: dict[model.fixtures.Venue, dict[str, float|int]]
):
    print(f"➤ {team.name} ({venue.value})")
    for col in columns:
        value = table.df.loc[table.df[model.tables.TEAM_COL] == team, col].values[0]
        first_top, second_top = top_2[venue][col]
        first_bot, second_bot = bottom_2[venue][col]

        if value == first_top:
            emoji = '🥇'
        elif value == second_top:
            emoji = '🥈'
        else:
            assert value == first_bot or value == second_bot
            emoji = '❌'

        print(f"{emoji} {model.tables.translate(col)}: {value}{'%' if col in model.tables.PERCENTAGE_COLS else ''}")


def analyse_best_and_worst(
        fixtures_to_analyse: list[model.fixtures.Fixture],
        league: model.competitions.Competition,
        season: model.seasons.Season
):
    columns = [
        model.tables.S_COL,
        model.tables.C_COL,
        model.tables.BTS_COL,
        model.tables.OTS_COL,
        model.tables.OVER_1_5_COL,
        model.tables.OVER_2_5_COL,
        model.tables.F_COL,
        model.tables.A_COL,
        model.tables.GR_COL
    ]

    tables = {}
    top_2 = {}
    bottom_2 = {}
    for venue in model.fixtures.Venue:
        table = model.tables.LeagueTable(league, season, model.fixtures.Period.FULL, venue)
        tables[venue] = table

        top_2[venue] = {}
        bottom_2[venue] = {}
        for col in columns:
            sorted_values = table.df[col].sort_values(ascending=False)
            top_2[venue][col] = sorted_values.head(2).values
            bottom_2[venue][col] = sorted_values.tail(2).values

    for fixture in fixtures_to_analyse:
        venue = model.fixtures.Venue.HOME
        table = tables[venue]
        home_strong = []
        home_weak = []
        for col in columns:
            value = table.df.loc[table.df[model.tables.TEAM_COL] == fixture.home_team, col].values[0]
            if value > 0:
                if value in top_2[venue][col]:
                    home_strong.append(col)

                if value in bottom_2[venue][col]:
                    home_weak.append(col)

        venue = model.fixtures.Venue.AWAY
        table = tables[venue]
        away_strong = []
        away_weak = []
        for col in columns:
            value = table.df.loc[table.df[model.tables.TEAM_COL] == fixture.away_team, col].values[0]
            if value > 0:
                if value in top_2[venue][col]:
                    away_strong.append(col)

                if value in bottom_2[venue][col]:
                    away_weak.append(col)

        if len(home_strong) + len(away_strong) >= 5:
            localized = fixture.date.replace(tzinfo=zoneinfo.ZoneInfo("Europe/London"))
            time_str = localized.strftime('%H:%M %Z')
            flag = country_flag(league.get_2_letter_iso_code())
            print(f"{flag} {time_str}: {fixture.home_team.name} vs {fixture.away_team.name} ({league})")

            venue = model.fixtures.Venue.HOME
            table = tables[venue]
            output_features(fixture.home_team, table, venue, home_strong, top_2, bottom_2)
            create_tweet_histogram(table.df, columns, league, fixture.home_team, fixture, venue)

            venue = model.fixtures.Venue.AWAY
            table = tables[venue]
            output_features(fixture.away_team, table, venue, away_strong, top_2, bottom_2)
            create_tweet_histogram(table.df, columns, league, fixture.away_team, fixture, venue)

            print()

        if len(home_weak) + len(away_weak) >= 5:
            localized = fixture.date.replace(tzinfo=zoneinfo.ZoneInfo("Europe/London"))
            time_str = localized.strftime('%H:%M %Z')
            flag = country_flag(league.get_2_letter_iso_code())
            print(f"{flag} {time_str}: {fixture.home_team.name} vs {fixture.away_team.name} ({league})")

            venue = model.fixtures.Venue.HOME
            table = tables[venue]
            output_features(fixture.home_team, table, venue, home_weak, top_2, bottom_2)
            create_tweet_histogram(table.df, columns, league, fixture.home_team, fixture, venue)

            venue = model.fixtures.Venue.AWAY
            table = tables[venue]
            output_features(fixture.away_team, table, venue, away_weak, top_2, bottom_2)
            create_tweet_histogram(table.df, columns, league, fixture.away_team, fixture, venue)

            print()


def main(country_prefixes: list[str], hours_before: int, hours_after: int, tolerance: float, tweet: bool):
    lib.structure.purge_png_files(lib.structure.tweet_png_directory)

    left_window = datetime.datetime.now() + datetime.timedelta(hours=hours_before)
    right_window = datetime.datetime.now() + datetime.timedelta(hours=hours_after)

    whitelisted = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)
    if country_prefixes:
        whitelisted = [
            c for c in whitelisted for prefix in country_prefixes if c.country.casefold().startswith(prefix.casefold())
        ]

    predictions: list[Prediction] = []
    for league in whitelisted:
        database = lib.structure.get_database(league.country)
        season = model.seasons.load_current_season(database, league)
        fixtures = model.seasons.load_fixtures(database, league, season)
        fixtures_to_analyse = fixtures_within_the_window(fixtures, left_window, right_window)

        if fixtures_to_analyse:
            lib.messages.vanilla_message(f"Analsying {league} (id={league.id})")

            seasons = model.seasons.load_seasons(database, league)
            historical_data: dict[model.teams.Team, Scorelines] = {}
            for season in seasons:
                collect_data(league, season, historical_data)

            this_season = seasons[-1]
            assert this_season.current
            this_season_data: dict[model.teams.Team, Scorelines] = {}
            collect_data(league, this_season, this_season_data)

            league_predictions = gather_predictions(
                league,
                fixtures_to_analyse,
                historical_data,
                this_season_data,
                tolerance
            )
            predictions.extend(league_predictions)

            analyse_best_and_worst(fixtures_to_analyse, league, season)

    predictions.sort(
        key=lambda p: (p.fixture.date, p.league.country, p.league.id, p.fixture.id, p.team == p.fixture.away_team)
    )

    if tweet:
        tweet_the_predictions(predictions)
    else:
        output_the_predictions(predictions)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.countries, args.lower, args.upper, args.tolerance, args.tweets)
