import datetime
import inspect
import os
import sys


def vanilla_message(*args):
    print(*args, file=sys.stdout, flush=True)


def error_message(*args):
    print('ERROR:', *args, file=sys.stderr, flush=True)
    sys.exit(1)


warnings = True


def warning_message(*args):
    if warnings:
        print('WARNING:', *args, file=sys.stdout, flush=True)


verbose = False


def verbose_message(*args, new_lines=1):
    if verbose:
        print(*args, end='\n' * new_lines, flush=True)


debug = False


def debug_message(*args):
    if debug:
        caller_frame = inspect.stack()[1]
        info = inspect.getframeinfo(caller_frame[0])
        prefix = '[{}:{}.{}@{}]'.format(datetime.datetime.now().strftime('%H:%M'),
                                        os.path.basename(info.filename),
                                        info.function,
                                        info.lineno)
        print(prefix, *args, flush=True)
