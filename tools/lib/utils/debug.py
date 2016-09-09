
import sys

from lib.utils import globals


def verbose_message(string, module):
    if globals.args['verbose']:
        print('[{}] {}'.format(module, string), file=sys.stderr)

        
def debug_message (string, module):
    if globals.args['debug']:
        print('[{}] {}'.format(module, string), file=sys.stderr)


def exit_message(string):
    print(string, file=sys.stderr)
    sys.exit(0)
