import pandas as pd
pd.set_option('display.max_columns', None)
import random
import numpy as np

raw_data = pd.read_csv("data/rs.fg.sim.test.csv")
speaker = 'UserName'
codes = ["Technical.Constraints","Performance.Parameters_c","Client.and.Consultant.Requests_c","Electric_c","Hydraulic_c","PAM_c","Pneumatic_c","SE_c","Technical.Constraints.2_c","null_c"]

#raw_data = pd.read_csv("data/rs.fg.single.R1valid.csv")
#speaker = 'UserName'
#codes = ["Hydraulic_c","Technical.Constraints_c","Performance.Parameters_c","PAM_c","Pneumatic_c","Client.and.Consultant.Requests_c","Electric_c","SE_c","null_c"]

#raw_data = pd.read_csv("data/rs.fg.sim.test_extra_cols.csv")
#speaker = 'UserName'
#codes = ["Technical.Constraints","Performance.Parameters_c","Client.and.Consultant.Requests_c","Electric_c","Hydraulic_c","PAM_c","Pneumatic_c","SE_c","Technical.Constraints.2_c","Technical.Constraints.3_c","Technical.Constraints.4_c","Technical.Constraints.5_c","Technical.Constraints.6_c","Technical.Constraints.7_c","Technical.Constraints.8_c","Technical.Constraints.9_c","null_c"]

rename_dict = {'UserName': 'speaker'}
for i, code in enumerate(codes):
    rename_dict[code] = str(i)

filtered_data = raw_data[(raw_data['GroupName'] == 'Electric')]

filtered_data = filtered_data.rename(columns=rename_dict)

filtered_data = filtered_data[['speaker'] + [str(i) for i in range(len(codes))]]

print(filtered_data)

code_index = [str(i) for i in range(len(codes))]

print(filtered_data.head(10))

unique_speakers = sorted(filtered_data['speaker'].unique())
print(unique_speakers)

simple = True
naive = False

def convert_to_code(df):
    temp = []
    for i in range(1,len(df)):
        temp.append(df[i])
    return ''.join(map(str, temp))

if simple:
    dictionary_speaker_code = {}
    code_speaker_list = []
    for i in range(len(unique_speakers)):
        dictionary_speaker_code[unique_speakers[i]] = i
        code_speaker_list.append(pd.DataFrame(index=code_index, columns=code_index).fillna(0))
    speaker_df = pd.DataFrame(index=unique_speakers, columns=unique_speakers).fillna(0)

    for i in range(1, len(filtered_data)):
        previous = filtered_data.iloc[i - 1]
        current = filtered_data.iloc[i]
        speaker_df.at[previous["speaker"], current["speaker"]] += 1
        speaker_df_index = dictionary_speaker_code[current["speaker"]]

        for j in range(1,len(previous)):
            for k in range(1,len(current)):
                if previous[j] == 1 and current[k] == 1:
                    code_speaker_list[speaker_df_index].at[str(j-1), str(k-1)] += 1

    speaker_df = speaker_df.div(speaker_df.sum(axis=1), axis=0).fillna(0)

    for i in range(len(code_speaker_list)):
        code_speaker_list[i] = code_speaker_list[i].div(code_speaker_list[i].sum(axis=1), axis=0).fillna(0)
        print(code_speaker_list[i])
    print(speaker_df)

    #######################################################################################################################

    length_of_real_data = len(filtered_data)
    print(length_of_real_data)
    simulated_data = pd.DataFrame(columns=filtered_data.columns)
    print(simulated_data)

    speaker = random.choice(unique_speakers)
    speaker_matrix = code_speaker_list[dictionary_speaker_code[speaker]]
    print(speaker)
    print(speaker_matrix)

    non_zero_elements = np.argwhere(speaker_matrix.values != 0)
    random_indices = random.choice(non_zero_elements)
    random_row, random_column = random_indices
    print(random_row,random_column)

    simulated_data = simulated_data.append({'speaker': speaker}, ignore_index=True).fillna(0)
    simulated_data.loc[0,str(random_column)] = 1

    for i in range(length_of_real_data-1):
        previous_speaker = simulated_data['speaker'].iloc[i]
        previous_codes = [i for i, val in enumerate(simulated_data.iloc[i, 1:]) if val == 1]

        #print(previous_speaker)
        #print(previous_codes)

        rand = random.random()
        sum = 0
        for j in range(len(speaker_df.loc[previous_speaker])):
            sum += speaker_df.loc[previous_speaker].iloc[j]
            if rand < sum:
                new_speaker = speaker_df.loc[previous_speaker].index[j]
                break

        #print(new_speaker)

        simulated_data = simulated_data.append({'speaker': new_speaker}, ignore_index=True).fillna(0)
        new_code_table = code_speaker_list[dictionary_speaker_code[new_speaker]]

        rand = random.random()
        sum = 0
        #print(new_code_table)
        temp = new_code_table.loc[str(random.choice(previous_codes))]
        for j in range(len(temp)):
            sum += temp.iloc[j]
            if rand < sum:
                new_code = temp.index[j]
                break

        simulated_data.loc[i+1, str(new_code)] = 1
        #print(new_code)


    print(simulated_data)
    simulated_data.to_csv("output/simulated_new.csv",index=False)

if naive:

    binary_combinations = []
    for decimal_number in range(2**(len(code_index))):  # Iterate through decimal numbers from 0 to 255
        binary_string = format(decimal_number, '0' + str(len(code_index))+ 'b')  # Convert to 8-bit binary with leading zeros
        binary_combinations.append(binary_string)
    print(binary_combinations)

    dictionary_speaker_code = {}
    code_speaker_list = []
    for i in range(len(unique_speakers)):
        dictionary_speaker_code[unique_speakers[i]] = i
        code_speaker_list.append(pd.DataFrame(index=binary_combinations, columns=binary_combinations).fillna(0))
    speaker_df = pd.DataFrame(index=unique_speakers, columns=unique_speakers).fillna(0)

    for i in range(1, len(filtered_data)):
        previous = filtered_data.iloc[i - 1]
        current = filtered_data.iloc[i]
        speaker_df.at[previous["speaker"], current["speaker"]] += 1
        speaker_df_index = dictionary_speaker_code[current["speaker"]]

        previous_code = convert_to_code(previous)
        current_code = convert_to_code(current)
        code_speaker_list[speaker_df_index].at[previous_code, current_code] += 1

    speaker_df = speaker_df.div(speaker_df.sum(axis=1), axis=0).fillna(0)

    for i in range(len(code_speaker_list)):
        code_speaker_list[i] = code_speaker_list[i].div(code_speaker_list[i].sum(axis=1), axis=0).fillna(0)

    #######################################################################################################################

    length_of_real_data = len(filtered_data)
    print(length_of_real_data)
    simulated_data = pd.DataFrame(columns=filtered_data.columns)
    print(simulated_data)

    speaker = random.choice(unique_speakers)
    speaker_matrix = code_speaker_list[dictionary_speaker_code[speaker]]

    print(speaker)
    print(speaker_matrix)

    non_zero_elements = np.argwhere(speaker_matrix.values != 0)
    random_indices = random.choice(non_zero_elements)
    random_row, random_column = random_indices
    print(random_row,random_column)

    simulated_data = simulated_data.append({'speaker': speaker}, ignore_index=True).fillna(0)
    for i in range(len(speaker_matrix.columns[random_column])):
        if speaker_matrix.columns[random_column][i] == "1":
            simulated_data.loc[0, str(i)] = 1
    print(speaker_matrix.iloc[:, random_column].unique())

    previous_speaker = simulated_data['speaker'].iloc[0]
    previous_code = speaker_matrix.columns[random_column]

    for i in range(1,length_of_real_data):

        # print(previous_speaker)
        # print(previous_codes)

        rand = random.random()
        sum = 0
        for j in range(len(speaker_df.loc[previous_speaker])):
            sum += speaker_df.loc[previous_speaker].iloc[j]
            if rand < sum:
                new_speaker = speaker_df.loc[previous_speaker].index[j]
                break

        # print(new_speaker)

        simulated_data = simulated_data.append({'speaker': new_speaker}, ignore_index=True).fillna(0)
        new_code_table = code_speaker_list[dictionary_speaker_code[new_speaker]]

        rand = random.random()
        sum = 0
        # print(new_code_table)
        temp = new_code_table.loc[previous_code]
        print(temp)
        for j in range(len(temp)):
            sum += temp.iloc[j]
            if rand < sum:
                new_code = temp.index[j]
                break

        previous_speaker = new_speaker
        previous_codes = new_code

        for j in range(len(new_code)):
            if new_code[j] == "1":
                simulated_data.loc[i, str(j)] = 1
        # print(new_code)

    print(simulated_data)
    simulated_data.to_csv("output/simulated_naive_new_second.csv", index=False)
