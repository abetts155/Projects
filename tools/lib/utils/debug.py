
import sys

from lib.utils import config


def verbose_message(string, module):
    if config.Arguments.verbose:
        print('[{}] {}'.format(module, string), file=sys.stderr)

        
def debug_message (string, module):
    if config.Arguments.debug:
        print('[{}] {}'.format(module, string), file=sys.stderr)

        
def warning_message(string):
    print('[*****WARNING*****] {}'.format(string), file=sys.stderr)


def exit_message(string):
    print(string, file=sys.stderr)
    sys.exit(0)
