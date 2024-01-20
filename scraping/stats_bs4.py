import os
import pandas as pd
from bs4 import BeautifulSoup
from data_references import stats_attributes
import logging
import helpers

# Log all messages
logging.basicConfig(level=logging.INFO)

save_dir = 'scraping/stats_pkl/'
dir_seasons = os.listdir('scraping/stats_html/')

# Remove because these stats aren't available:
stats_attributes.remove('Caught Opponent Offside')
stats_attributes.remove('Substitutions On')
stats_attributes.remove('Fouls')

for season in dir_seasons:
    logging.info(f'Working on season: {season}')

    master_df = pd.DataFrame()

    for attribute in stats_attributes:
        logging.info(f'Working on statistic: {attribute}')
        stat_name = attribute.replace(' ', '_')
        teams_concat = []
        stats_concat = []
        # Going through pagination
        for page in range(1, 3):
            logging.info(f'Working on page: {page}')
            teams = []
            stats = []
            with open(f'scraping/stats_html/{season}/{attribute}_{page}.html', 'r', encoding='UTF-8') as input:
                soup = BeautifulSoup(input, 'html.parser')
            tbody = soup.find('tbody', class_='statsTableContainer')
            tr = tbody.find_all('tr', class_='table__row')
            for row in tr:
                stat = row.find('td', class_='stats-table__main-stat').string
                team_cell = row.find('td', class_='stats-table__name')
                team = team_cell.find('a', class_='stats-table__cell-icon-align').contents[2]
                teams.append(team.replace('\n', ''))
                stats.append(stat)
            if (teams != teams_concat):
                teams_concat.extend(teams)
                stats_concat.extend(stats)
        # Remove the space at start and end of teams
        teams_concat = [x.strip() for x in teams_concat]
        if (len(teams_concat) == 0) or (len(teams_concat) == 0):
            logging.warning(f"{stat_name} has no data")
        stat_df = pd.DataFrame({
            'Team': teams_concat,
            'stat': stats_concat
        })
        # Brighton is named differently in table and stats due to &
        stat_df['Team'] = stat_df['Team'].apply(lambda x: helpers.remove_amp(x))
        stat_df['stat'] = stat_df['stat'].apply(lambda x: helpers.remove_comma(x))
        stat_df = stat_df.set_index('Team')
        stat_df.rename(columns = {'stat':stat_name}, inplace = True)
        master_df = pd.concat([master_df, stat_df], axis=1)
    master_df = master_df.fillna(0)
    master_df = master_df.astype(int)
    # Drop duplicate columns
    master_df = master_df.drop(['Wins', 'Losses', 'Goals', 'Goals_Conceded'], axis=1)
    master_df.to_pickle(f'{save_dir}/{season}.pkl')


            





