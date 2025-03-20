import football_api.structure


def get_countries() -> list[str]:
    countries = []
    with open(football_api.structure.get_countries_whitelist(), 'r') as in_file:
        for line in in_file:
            country = line.strip()
            countries.append(country)
    countries.sort()
    return countries
