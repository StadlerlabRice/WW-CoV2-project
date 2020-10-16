import numpy as np
import pandas as pd

conc_table = pd.read_csv('qPCR complete data - Concentration methods paper-2.csv')

#remove NTCs and DI
conc_table=conc_table[~conc_table['WWTP'].isin(['DI', '0'])]

#subset relevant columns for each N1 and N2
N1 = conc_table.loc[conc_table['Target Name'] == 'SARS CoV-2 N1', ['WWTP','Copies/l WW','Concentration_method']]
N2 = conc_table.loc[conc_table['Target Name'] == 'SARS CoV-2 N2', ['WWTP','Copies/l WW','Concentration_method']]

#prepare arrays
conc_method_names = ['Direct Extraction','HA Filtration','HA Filtration w/ Elution','PEG','Ultrafiltration']
N1_mean_perc_CV = np.zeros(5);
N2_mean_perc_CV = np.zeros(5);

for i in range(0,5) :
	#for each concentration method
	N1_sub = N1[N1['Concentration_method'] == conc_method_names[i]]
	N2_sub = N2[N2['Concentration_method'] == conc_method_names[i]]

	#get the mean and std
	N1_stats = N1_sub.groupby(['WWTP'])['Copies/l WW'].agg(['mean', 'std'])
	N2_stats = N2_sub.groupby(['WWTP'])['Copies/l WW'].agg(['mean', 'std'])

	#and calculate %CV as 100% * std/mean
	N1_perc_CV = 100*N1_stats['std']/N1_stats['mean']
	N2_perc_CV = 100*N2_stats['std']/N2_stats['mean']

	#store results here
	N1_mean_perc_CV[i] = N1_perc_CV.mean()
	N2_mean_perc_CV[i] = N2_perc_CV.mean()

#prepare output file and export
N1N2 = np.transpose([N1_mean_perc_CV, N2_mean_perc_CV])
perc_CVs = pd.DataFrame(N1N2, index = conc_method_names, columns = ['N1','N2'])
perc_CVs.to_csv(r'perc_CVs.csv', index = 1, header = 1)