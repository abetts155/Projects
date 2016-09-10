
import os

args = None

def set_filename_prefix(a_file):
    args['filename_prefix'] = os.path.abspath(os.path.dirname(a_file)) +\
                                os.sep +\
                                os.path.splitext(os.path.basename(a_file))[0]


def add_common_command_line_arguments(parser):
    parser.add_argument('-d',
                        '--debug',
                        action='store_true',
                        help='debug mode',
                        default=False)
    
    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='be verbose',
                        default=False)
    
    parser.add_argument('--graphviz',
                        action='store_true',
                        help='using Graphviz, produce PNG files of graphs'
                        ' produced during the analysis',
                        default=False)         
                
