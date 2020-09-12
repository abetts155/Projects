import os

from miscellaneous.messages import error_message


def check_file_or_directory_exists(name: str):
    if not os.path.exists(name):
        error_message("No such file or directory '{}'".format(name))
