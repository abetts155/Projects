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
    purge_dot    = None
    instrument   = None
    

def set_filename_prefix():
    Arguments.basepath =\
        os.path.abspath(os.path.dirname(Arguments.program_file))
    Arguments.basename =\
        os.path.splitext(os.path.basename(Arguments.program_file))[0]


def get_filename_prefix():
    return Arguments.basepath + os.sep + Arguments.basename


def purge_png_files():
    if Arguments.purge_dot:
        for filename in os.listdir(Arguments.basepath):
            _, ext = os.path.splitext(filename) 
            if ext == '.png' or ext == '.dot':
                complete_path = os.path.join(Arguments.basepath, filename)
                os.remove(complete_path)