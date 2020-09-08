from football_api.frontmatter import json_directory
from football_api.obtain import get_leagues

if __name__ == '__main__':
    json_directory.mkdir(parents=True, exist_ok=True)
    leagues_json = json_directory.joinpath('leagues.json')
    if not leagues_json.exists():
        response = get_leagues()
        with leagues_json.open('w') as out_file:
            out_file.write(response.text)
