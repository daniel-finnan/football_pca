import os
import re
import pandas as pd

dir = 'scraping/tables_html'
save_dir = 'scraping/tables_pkl'
seasons = os.listdir(dir)

# Patterns for regex
tbody_pattern = "<tbody class=\"league-table__tbody isPL\">(.*?)</tbody>"
team_name_long_pattern = "<span class=\"league-table__team-name league-table__team-name--long long\">(.*?)</span>"
team_name_short_pattern = "<span class=\"league-table__team-name league-table__team-name--short short\">(.*?)</span>"
stats_pattern = "</a>   </td> <td>(.*?)</td> <td>(.*?)</td> <td>(.*?)</td> <td>(.*?)</td> <td class=\"hideSmall\">(.*?)</td> <td class=\"hideSmall\">(.*?)</td> <td>(.*?)</td> <td class=\"league-table__points points\">(.*?)</td>"

# Loop through the seasons
for season in seasons:
    with open(f'{dir}/{season}', encoding='UTF-8') as file:
        page = file.read()
    tbody = re.findall(tbody_pattern, page)
    team_name_long = re.findall(team_name_long_pattern, tbody[0])
    team_name_short = re.findall(team_name_short_pattern, tbody[0])
    stats = re.findall(stats_pattern, tbody[0])
    # Put data into a list of dicts and then into dataframe
    i = 0
    data = []
    while i <= 19:
        team_name = team_name_long[i]
        short_name = team_name_short[i]
        team_stats = stats[i]
        data.append({
            'Team': team_name_long[i],
            'Short_name': team_name_short[i],
            'Position': i + 1,
            'Played': stats[i][0],
            'Won': stats[i][1],
            'Drawn': stats[i][2],
            'Lost': stats[i][3],
            'Goals_For': stats[i][4],
            'Goals_Conceded': stats[i][5],
            'Goal_Difference': stats[i][6],
            'Points': stats[i][7],
        })
        i += 1
    df = pd.DataFrame(data)
    df = df.set_index('Team')
    df = df.astype({
        'Position': 'int',
        'Played': 'int',
        'Won': 'int',
        'Drawn': 'int',
        'Lost': 'int',
        'Goals_For': 'int',
        'Goals_Conceded': 'int',
        'Goal_Difference': 'int',
        'Points': 'int'
    })
    filename = season.split('.')[0]
    df.to_pickle(f'{save_dir}/{filename}.pkl')





