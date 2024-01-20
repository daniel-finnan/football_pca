import os
import pandas as pd

tables = os.listdir('scraping/tables_pkl/')

master_df = pd.DataFrame()

for table in tables:
    table_df = pd.read_pickle(f'scraping/tables_pkl/{table}')
    season = table.split('.')[0]
    stats_df = pd.read_pickle(f'scraping/stats_pkl/{season}.pkl') 
    season_df = pd.concat([table_df, stats_df], axis=1, join='inner')
    season_df['Season'] = season
    season_df['Idx'] = season_df['Short_name'] + season_df['Season'].astype(str)
    season_df = season_df.set_index('Idx')
    master_df = pd.concat([master_df, season_df], axis=0)

# Not calculating these per game
cols = master_df.columns
cols = cols.drop(['Short_name', 'Position', 'Played', 'Drawn', 'Season', 'Points', 'Goal_Difference'])

# Calculate the stat per game
for col in cols:
    master_df[col] = master_df.apply(lambda x: x[col]/x['Played'], axis=1)

master_df.to_csv('pl_data.csv', sep=';')






