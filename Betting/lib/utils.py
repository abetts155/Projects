import sys
import datetime


def verbose_message(*args):
    print(*args, file=sys.stderr)


def exit_message(*args):
    print(*args, file=sys.stderr)
    sys.exit(0)


def this_season():
    now = datetime.datetime.now()
    return now.year - 1


def parse_int(value):
    try:
        return int(value)
    except ValueError:
        pass
    except TypeError:
        pass

