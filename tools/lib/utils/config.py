import os

class Arguments:
    
    """
    Arguments from both the command line and the configuration file.
    """
    
    basepath     = None
    basename     = None
    verbose      = None
    debug        = None
    dot          = None
    program_file = None
    instrument   = None
    

def set_filename_prefix():
    Arguments.basepath =\
        os.path.abspath(os.path.dirname(Arguments.program_file))
    Arguments.basename =\
        os.path.splitext(os.path.basename(Arguments.program_file))[0]


def get_filename_prefix():
    return Arguments.basepath + os.sep + Arguments.basename