import datetime
import sys
import inspect


def debug_message(*args):
    if __debug__:
        caller_frame = inspect.stack()[1]
        info = inspect.getframeinfo(caller_frame[0])
        print('[{:%H:%M:%S}: {}@{}]'.format(datetime.datetime.now(), info.function, info.lineno),
              *args,
              flush=True)


def verbose_message(*args, new_lines=1):
    print(*args, end='\n' * new_lines, flush=True)


def error_message(*args):
    print('ERROR:', *args, file=sys.stderr, flush=True)
    sys.exit(1)


def message(*args, new_lines=1):
    print(*args, end='\n' * new_lines, flush=True)
