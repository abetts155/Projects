import os

class Arguments:
    
    """
    Arguments set during parsing of the command line.
    """
    
    basepath       = None
    basename       = None
    verbose        = None
    debug          = None
    graphviz       = None
    program_file   = None
    purge_dot      = None
    instrument     = None
    repeat         = 1
    max_loop_bound = 10
    

def set_filename_prefix():
    Arguments.basepath =\
        os.path.abspath(os.path.dirname(Arguments.program_file))
    Arguments.basename =\
        os.path.splitext(os.path.basename(Arguments.program_file))[0]


def get_filename_prefix():
    return Arguments.basepath + os.sep + Arguments.basename


def purge_graphviz_files():
    """
    Remove anything and everything created for visual display through Graphviz.
    """
    if Arguments.purge_graphviz:
        for filename in os.listdir(Arguments.basepath):
            _, ext = os.path.splitext(filename) 
            if ext == '.png' or ext == '.dot':
                complete_path = os.path.join(Arguments.basepath, filename)
                os.remove(complete_path)