
import os

args = None

def set_filename_prefix(a_file):
    args['filename_prefix'] = os.path.abspath(os.path.dirname(a_file)) +\
                                os.sep +\
                                os.path.splitext(os.path.basename(a_file))[0]


                
                
