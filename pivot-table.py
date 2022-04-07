import pandas as pd

data = pd.read_csv(r'C:\Users\INNOVATIONLAB\Documents\co2-emissions.csv')

gas = data.pivot_table('gas', ['Entity', 'Code'], 'Year').reset_index()
gas['emission_type'] = 'gas'

oil = data.pivot_table('oil', ['Entity', 'Code'], 'Year').reset_index()
oil['emission_type'] = 'oil'

coal = data.pivot_table('coal', ['Entity', 'Code'], 'Year').reset_index()
coal['emission_type'] = 'coal'

emissions = pd.concat([gas,oil,coal])

emissions.fillna(0,inplace =True)

emissions[['Entity','Code','emission_type',2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017]].to_csv(r'C:\Users\akonowalchuk\Documents\emissions_output.csv')