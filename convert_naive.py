import pandas as pd
pd.set_option('display.max_columns', None)
import random
import numpy as np
file_name = "rs.fg.single.R1valid_density_5006016"
raw_data = pd.read_csv("data/data_new/" + file_name + ".csv")
speaker = 'UserName'
codes = ["Performance.Parameters_c","Client.and.Consultant.Requests_c", "Electric_c","Hydraulic_c","PAM_c","Pneumatic_c","SE_c","Technical.Constraints_c","null_c"]

#raw_data = pd.read_csv("data/toydata.csv")
#speaker = 'UserName'
#codes = ["A","B","C","Null"]

#raw_data = pd.read_csv("data/test_R1.csv")
#speaker = 'UserName'
#codes = ["Technical.Constraints_c","Performance.Parameters_c","null_c"]

#raw_data = pd.read_csv("data/rs.fg.single.R1valid.csv")
#speaker = 'UserName'
#codes = ["Performance.Parameters_c","Client.and.Consultant.Requests_c", "Electric_c", "Hydraulic_c","PAM_c","Pneumatic_c","SE_c","Technical.Constraints_c","null_c"]

#raw_data = pd.read_csv("data/rs.fg.sim.test_extra_cols.csv")
#speaker = 'UserName'
#codes = ["Technical.Constraints","Performance.Parameters_c","Client.and.Consultant.Requests_c","Electric_c","Hydraulic_c","PAM_c","Pneumatic_c","SE_c","Technical.Constraints.2_c","Technical.Constraints.3_c","Technical.Constraints.4_c","Technical.Constraints.5_c","Technical.Constraints.6_c","Technical.Constraints.7_c","Technical.Constraints.8_c","Technical.Constraints.9_c","null_c"]

rename_dict = {'UserName': 'speaker'}
for i, code in enumerate(codes):
    rename_dict[code] = str(i)

filtered_data = raw_data[(raw_data['GroupName'] == 'Electric')]

filtered_data = filtered_data.rename(columns=rename_dict)

filtered_data = filtered_data[['speaker'] + [str(i) for i in range(len(codes))]]

#print(filtered_data)

code_index = [str(i) for i in range(len(codes))]

#print(filtered_data.head(10))

unique_speakers = sorted(filtered_data['speaker'].unique())
print(unique_speakers)

binary_combinations = []
for decimal_number in range(2 ** (len(code_index))):  # Iterate through decimal numbers from 0 to 255
    binary_string = format(decimal_number,
                           '0' + str(len(code_index)) + 'b')  # Convert to 8-bit binary with leading zeros
    binary_combinations.append(binary_string)
print(binary_combinations)

raw_data[binary_combinations] = 0
raw_data['BinaryCode'] = raw_data[codes].astype(str).apply(''.join, axis=1)

for index, row in raw_data.iterrows():
    binary_code = row['BinaryCode']
    raw_data.loc[index, binary_code] += 1

raw_data = raw_data.drop(columns=['BinaryCode'])
raw_data = raw_data.drop(columns=codes)

print(raw_data)
raw_data.to_csv('data/data_new/' +file_name +'_binary.csv', index=False)
