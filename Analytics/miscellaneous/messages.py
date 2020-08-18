import sys


def error_message(*args):
    print('ERROR:', *args, file=sys.stderr, flush=True)
    sys.exit(1)


def warning_message(*args):
    print('WARNING:', *args, file=sys.stdout, flush=True)


def verbose_message(*args, new_lines=1):
    print(*args, end='\n' * new_lines, flush=True)
