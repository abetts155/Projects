import pathlib

hidden_directory = pathlib.Path.home().joinpath('.graphs')
hidden_directory.mkdir(exist_ok=True)

program_directory =  hidden_directory.joinpath('programs')
program_directory.mkdir(exist_ok=True)


def get_program_directory() -> pathlib.Path:
    existing_dirs = [d for d in program_directory.iterdir() if d.is_dir() and d.name.isdigit()]

    next_index = 1
    if existing_dirs:
        indices = [int(d.name) for d in existing_dirs]
        next_index = max(indices) + 1

    new_dir = program_directory / str(next_index)
    new_dir.mkdir()
    return new_dir
