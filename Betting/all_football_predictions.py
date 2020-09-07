#!/usr/bin/env python3

import sys
assert sys.version_info >= (3,0), 'Script requires Python 3.0 or greater to run'

from lib.betting import football_config
import football_predictions

parser = football_config.add_common_command_line_options()

with open('leagues.txt', 'r') as the_file:
    for line in the_file.readlines(): 
        lexemes = line.split('=')
        code = lexemes[0].strip()
        description = lexemes[1].strip()
        
        football_config.args.reset()
        football_config.parse_command_line(parser)
        football_config.args.league = code
        
        try:
            print('*' * 80)
            print(description)
            print('*' * 80)
            football_predictions.main()
            print('\n' * 5)
        except KeyboardInterrupt:
            sys.exit(1)
        except:
            print('Unexpected error: {}'.format(sys.exc_info()[0]))
