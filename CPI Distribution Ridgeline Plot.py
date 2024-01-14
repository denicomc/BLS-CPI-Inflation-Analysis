import pandas as pd
import requests
from io import StringIO
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime

# Download data
cpi_data_url = "https://download.bls.gov/pub/time.series/cu/cu.data.0.Current"
series_url = "https://download.bls.gov/pub/time.series/cu/cu.series"
items_url = "https://download.bls.gov/pub/time.series/cu/cu.item"

cpi_data_raw = requests.get(cpi_data_url, headers={'User-Agent': 'rortybomb@gmail.com'}).text
series_raw = requests.get(series_url, headers={'User-Agent': 'rortybomb@gmail.com'}).text
items_raw = requests.get(items_url, headers={'User-Agent': 'rortybomb@gmail.com'}).text

# Read data into pandas DataFrame
cpi_data = pd.read_csv(StringIO(cpi_data_raw), sep='\s+')
series = pd.read_csv(StringIO(series_raw), sep='\s+')
items = pd.read_csv(StringIO(items_raw), sep='\s+')

# Data cleaning and preprocessing
cpi_data = cpi_data.rename(columns=lambda x: x.lower())
cpi_data['value'] = pd.to_numeric(cpi_data['value'], errors='coerce')
cpi_data['series_id'] = cpi_data['series_id'].str.strip()
cpi_data['date'] = pd.to_datetime(cpi_data['period'].str[1:3] + "/01/" + cpi_data['year'].str[2:4], format="%m/%d/%y")

series = series.rename(columns=lambda x: x.lower())
series['series_id'] = series['series_id'].str.strip()

merged_series = pd.merge(series, items, left_on='item_code', right_on='item_code')
cpi_data = pd.merge(cpi_data, merged_series, left_on='series_id', right_on='series_id')

# Read inflation weights
cpi_weights = pd.read_csv("data/inflation_weights.csv").drop(columns=['year_weight'])
cpi_data = pd.merge(cpi_data, cpi_weights, left_on='item_name', right_on='item_name')

cpi_weights_2023 = pd.read_csv("data/inflation_weights_2023.csv").rename(columns={'weight': 'weight_2023'}).drop(columns=['year_weight'])
cpi_data = pd.merge(cpi_data, cpi_weights_2023, left_on=['item_name', 'year'], right_on=['item_name', 'year'], how='left')

cpi_data['weight'] = np.where(~pd.isna(cpi_data['weight_2023']), cpi_data['weight_2023'], cpi_data['weight'])

# Make the graphic
median_terms = pd.read_csv("data/mediancpi_component_table.csv").rename(columns={'Component': 'item_name'})

median = cpi_data[(cpi_data['item_name'].isin(median_terms['item_name']) | (cpi_data['item_name'] == "Owners' equivalent rent of residences")) &
                  (cpi_data['period'] != "M13") & (cpi_data['seasonal'] == "S")].sort_values(by='date')

median['pchange3'] = (median['value'] / median['value'].shift(3) - 1)
median['normalized'] = median.groupby('item_name')['weight'].transform('sum')
median['weightN'] = median['weight'] / median['normalized']
median['cumsum'] = (median.groupby('item_name')['weight'].cumsum() / 100)
median['cumsumN'] = median.groupby('item_name')['cumsum'].transform('last')
median['pchange3a'] = (1 + median['pchange3'])**4 - 1

start_month = median['date'].max().month
quarters = ((np.arange(start_month, start_month + 9, step=3) - 1) % 12) + 1

# Plot the graphic
plt.figure(figsize=(12, 8))
sns.kdeplot(data=median[(median['cumsumN'] <= 0.85) & (median['cumsum'] >= 0.15) &
                        (median['date'] >= "2017-06-01") & (median['date'] != "2020-06-01") &
                        (median['date'].dt.month.isin(quarters))],
            x='pchange3a', y='date', fill=True, cmap='viridis', levels=30)

plt.title("Distribution of Price Increases Moved Out\nBut Now Returning Back", size=20)
plt.xlabel("Three Month Percent Change")
plt.ylabel("")
plt.show()
