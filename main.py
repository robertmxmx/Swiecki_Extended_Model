import pandas as pd
pd.set_option('display.max_columns', None)
import random
import numpy as np

groups = ["Electric","Hydraulic","PAM","Pneumatic","Series Elastic","1","2","3","4","5"]
letters = ["A","B","C","D","E","F","G","H","I","J","K","L","M"]
output_file_name = "iterative.csv"

import os

file_name = "simple_330_new"
naive = False
simple_window = False
use_basic = True
iterative_window = True
using_binary_compound_code = False

first_run = True

for iteration in range(1,1000):
    for group in groups:


        raw_data = pd.read_csv("data/data_new/rs.fg.single.R1valid_density_330.csv")
        speaker = 'UserName'
        codes = ["Performance.Parameters_c", "Client.and.Consultant.Requests_c", "Electric_c", "Hydraulic_c", "PAM_c",
                 "Pneumatic_c", "SE_c", "Technical.Constraints_c", "null_c"]
        null_code = 8

        #raw_data = pd.read_csv("data/data_new/rs.fg.single.R1valid_density_059893_binary.csv")
        #speaker = 'UserName'
        #codes = [format(i, '09b') for i in range(2**9)]
        #null_code = 1

        #raw_data = pd.read_csv("data/toydata.csv")
        #speaker = 'UserName'
        #codes = ["A","B","C","Null"]

        #raw_data = pd.read_csv("data/test_R1.csv")
        #speaker = 'UserName'
        #codes = ["Technical.Constraints_c","Performance.Parameters_c","null_c"]

       # raw_data = pd.read_csv("data/data_new/rs.fg.single.R1valid_density_059893.csv")
        #speaker = 'UserName'
        #codes = ["Performance.Parameters_c","Client.and.Consultant.Requests_c", "Electric_c", "Hydraulic_c","PAM_c","Pneumatic_c","SE_c","Technical.Constraints_c","null_c"]
        #null_code = 8

        #raw_data = pd.read_csv("data/rs.fg.sim.test_extra_cols.csv")
        #speaker = 'UserName'
        #codes = ["Technical.Constraints","Performance.Parameters_c","Client.and.Consultant.Requests_c","Electric_c","Hydraulic_c","PAM_c","Pneumatic_c","SE_c","Technical.Constraints.2_c","Technical.Constraints.3_c","Technical.Constraints.4_c","Technical.Constraints.5_c","Technical.Constraints.6_c","Technical.Constraints.7_c","Technical.Constraints.8_c","Technical.Constraints.9_c","null_c"]

        rename_dict = {'UserName': 'Speaker'}
        for i, code in enumerate(codes):
            rename_dict[code] = str(i)

        #print(rename_dict)

        filtered_data = raw_data[(raw_data['GroupName'] == group)]

        filtered_data = filtered_data.rename(columns=rename_dict)

        filtered_data = filtered_data[['Speaker'] + [str(i) for i in range(len(codes))]]

        if not os.path.exists('output/new/' + file_name+ "/" + str(len(filtered_data))):
            os.makedirs('output/new/' + file_name+ "/" + str(len(filtered_data)))

        #print(filtered_data)

        code_index = [str(i) for i in range(len(codes))]

        #print(filtered_data.head(10))

        unique_speakers = sorted(filtered_data['Speaker'].unique())
        rename_dict_speaker = {}
        for i, code in enumerate(unique_speakers):
            rename_dict_speaker[unique_speakers[i]] = i + 1

        #print(unique_speakers)
        #print(rename_dict_speaker)

        def convert_to_code(df):
            temp = []
            for i in range(1,len(df)):
                temp.append(df[i])
            return ''.join(map(str, temp))

        if simple_window:
            if first_run:
                dictionary_speaker_code = {}
                code_speaker_list = []
                for i in range(len(unique_speakers)):
                    dictionary_speaker_code[unique_speakers[i]] = i
                    code_speaker_list.append(pd.DataFrame(index=code_index, columns=code_index).fillna(0))
                speaker_df = pd.DataFrame(index=unique_speakers, columns=unique_speakers).fillna(0)

                connection_list = []

                if using_binary_compound_code:
                    new_filtered_data = filtered_data[['Speaker'] + [str(i) for i in range(9)]]
                    new_simulated_data = pd.DataFrame(columns=new_filtered_data.columns)
                    #print(new_simulated_data)

                for i in range(1, len(filtered_data)):
                    connection_list.append([])
                    previous = filtered_data.iloc[i - 1]
                    current = filtered_data.iloc[i]
                    speaker_df.at[previous["Speaker"], current["Speaker"]] += 1
                    speaker_df_index = dictionary_speaker_code[current["Speaker"]]
                    previous_speaker_df_index = dictionary_speaker_code[previous["Speaker"]]

                    #if sum(current[1:]) >= 3:

                    for j in range(1,len(previous)):
                        for k in range(1,len(current)):
                            if previous[j] == 1 and current[k] == 1:
                                if [speaker_df_index,str(j-1), str(k-1)] not in connection_list[i-1]:
                                    connection_list[i-1].append([speaker_df_index,str(j-1), str(k-1)])
                                if [speaker_df_index,str(k-1), str(j-1)] not in connection_list[i-1]:
                                    connection_list[i-1].append([speaker_df_index,str(k-1), str(j-1)])

                    for j in range(1, len(current)):
                        for k in range(1, len(current)):
                            if current[j] == 1 and current[k] == 1 and j != k:
                                if [speaker_df_index, str(j - 1), str(k - 1)] not in connection_list[i - 1]:
                                    connection_list[i-1].append([speaker_df_index,str(j-1), str(k-1)])

                    i = 0
                    for list in connection_list:
                        i+= 1
                        #print(i,list)
                        for line in list:
                            code_speaker_list[line[0]].at[line[1], line[2]] += 1


                    #print(connection_list)
                        #for j in range(1, len(previous)):
                       #     for k in range(1, len(previous)):
                        #        if current[j] == 1 and current[k] == 1 and j != k:
                        #            code_speaker_list[previous_speaker_df_index].at[str(j - 1), str(k - 1)] += 1
                        #            code_speaker_list[previous_speaker_df_index].at[str(k - 1), str(j - 1)] += 1

                speaker_df = speaker_df.div(speaker_df.sum(axis=1), axis=0).fillna(0)
                #print(speaker_df)
                for i in range(len(code_speaker_list)):
                    #print(code_speaker_list[i])
                    code_speaker_list[i] = code_speaker_list[i].div(code_speaker_list[i].sum(axis=1), axis=0).fillna(0)
                    #print(codes)
                   # print(code_speaker_list[i])

                print(speaker_df)
                first_run = False

            #######################################################################################################################

            length_of_real_data = len(filtered_data)
           # print(length_of_real_data)
            simulated_data = pd.DataFrame(columns=filtered_data.columns)

            speaker = random.choice(unique_speakers)
            #print(dictionary_speaker_code)
            speaker_matrix = code_speaker_list[dictionary_speaker_code[speaker]]
            #print(speaker)
            #print(speaker_matrix)

            non_zero_elements = np.argwhere(speaker_matrix.values != 0)
            random_indices = random.choice(non_zero_elements)
            random_row, random_column = random_indices
            #print(random_row,random_column)

            simulated_data = simulated_data.append({'Speaker': speaker}, ignore_index=True).fillna(0)
            simulated_data.loc[0,str(random_column)] = 1

            if using_binary_compound_code:
                binary_format = format(int(random_column), '09b')
                for j in range(len(binary_format)):
                    if binary_format[j] == "1":
                        new_simulated_data.loc[0, str(j)] = 1

            for i in range(length_of_real_data-1):
                #print(i)
                previous_speaker = simulated_data['Speaker'].iloc[i]

                previous_codes = [i for i, val in enumerate(simulated_data.iloc[i, 1:]) if val == 1]
                #print()

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

                simulated_data = simulated_data.append({'Speaker': new_speaker}, ignore_index=True).fillna(0)
                new_code_table = code_speaker_list[dictionary_speaker_code[new_speaker]]

                new_code = "unassigned"
                rand = random.random()
                sum = 0
                #print(new_code_table)
                temp = new_code_table.loc[str(random.choice(previous_codes))]
                for j in range(len(temp)):
                    sum += temp.iloc[j]
                    if rand < sum:
                        new_code = temp.index[j]
                        break

                if new_code == "unassigned":
                    new_code = null_code

                simulated_data.loc[i+1, str(new_code)] = 1

               # print(simulated_data.loc[i+1])
                #print(new_code)

                if using_binary_compound_code:
                    #print(new_code)
                    binary_format = format(int(new_code), '09b')
                    for j in range(len(binary_format)):
                        if binary_format[j] == "1":
                            new_simulated_data.loc[i+1, str(j)] = 1

            if using_binary_compound_code:
                new_simulated_data['Speaker'] = simulated_data['Speaker']
                new_simulated_data = new_simulated_data.fillna(0)

                for i in range(new_simulated_data.shape[1] - 2):
                    new_simulated_data.rename(columns={str(i): letters[i]}, inplace=True)

                new_simulated_data.rename(columns={str(new_simulated_data.shape[1] - 2): 'null_c'}, inplace=True)
                new_simulated_data["Run"] = 1
                new_simulated_data['Speaker'] = new_simulated_data['Speaker'].replace(rename_dict_speaker)

                column_name = 'Run'
                other_columns = [col for col in new_simulated_data.columns if col != column_name]
                new_simulated_data = new_simulated_data[other_columns + [column_name]]

                column_name = 'Speaker'
                other_columns = [col for col in new_simulated_data.columns if col != column_name]
                new_simulated_data = new_simulated_data[other_columns + [column_name]]

                new_simulated_data.to_csv("output/new/" + file_name+ "/" + str(len(filtered_data)) + "/" + str(iteration) + ".csv", index=False)

            else:
            #print(simulated_data)

                for i in range(simulated_data.shape[1] - 2):
                    simulated_data.rename(columns={str(i): letters[i]}, inplace=True)

                simulated_data.rename(columns={str(simulated_data.shape[1] - 2): 'null_c'}, inplace=True)
                simulated_data["Run"] = 1
                simulated_data['Speaker'] = simulated_data['Speaker'].replace(rename_dict_speaker)

                column_name = 'Run'
                other_columns = [col for col in simulated_data.columns if col != column_name]
                simulated_data = simulated_data[other_columns + [column_name]]

                column_name = 'Speaker'
                other_columns = [col for col in simulated_data.columns if col != column_name]
                simulated_data = simulated_data[other_columns + [column_name]]

                simulated_data.to_csv("output/new/" + file_name +"/" + str(len(filtered_data))  + "/" + str(iteration) + ".csv", index=False)

        if iterative_window:
            dictionary_speaker_code = {}
            code_speaker_list = []
            for i in range(len(unique_speakers)):
                dictionary_speaker_code[unique_speakers[i]] = i
                code_speaker_list.append([])
                for j in range(len(codes)):
                    code_speaker_list[i].append(pd.DataFrame(index=code_index, columns=code_index).fillna(0))

            speaker_df = pd.DataFrame(index=unique_speakers, columns=unique_speakers).fillna(0)

            speaker_frequency_list = pd.DataFrame(index=unique_speakers, columns=range(1,len(code_index)+1)).fillna(0)

            for i in range(0, len(filtered_data)):
                current = filtered_data.iloc[i]
                speaker_name = current[0]
                speaker_frequency_list.loc[speaker_name, current[1:].sum()] += 1

            speaker_frequency_list = speaker_frequency_list.div(speaker_frequency_list.sum(axis=1), axis=0)
            #print(speaker_frequency_list)


            connection_list = []
            for i in range(1, len(filtered_data)):
                connection_list.append([])
                previous = filtered_data.iloc[i - 1]
                current = filtered_data.iloc[i]
                speaker_df.at[previous["Speaker"], current["Speaker"]] += 1
                speaker_df_index = dictionary_speaker_code[current["Speaker"]]
                previous_speaker_df_index = dictionary_speaker_code[previous["Speaker"]]

                for j in range(1,len(previous)):
                    for k in range(1,len(current)):
                        if previous[j] == 1 and current[k] == 1:
                            if [speaker_df_index,str(j-1), str(k-1)] not in connection_list[i-1]:
                                connection_list[i-1].append([speaker_df_index,str(j-1), str(k-1),current[1:].sum()])
                            if [speaker_df_index,str(k-1), str(j-1)] not in connection_list[i-1]:
                                connection_list[i-1].append([speaker_df_index,str(j-1), str(k-1),current[1:].sum()])

                for j in range(1, len(current)):
                    for k in range(1, len(current)):
                        if current[j] == 1 and current[k] == 1 and j != k:
                            if [speaker_df_index, str(j - 1), str(k - 1)] not in connection_list[i - 1]:
                                connection_list[i-1].append([speaker_df_index,str(j-1), str(k-1),current[1:].sum()])

            i = 0
            for list in connection_list:
                i+= 1
                #print(i,list)
                for line in list:
                    for l in range(len(codes)+1):
                        if l <= line[3]:
                            code_speaker_list[line[0]][l].at[line[1], line[2]] += 1


           # print(connection_list)

            speaker_df = speaker_df.div(speaker_df.sum(axis=1), axis=0).fillna(0)
            #print(speaker_df)
            for i in range(len(code_speaker_list)):
                for j in range(len(codes)):
                    #print(code_speaker_list[i][j])
                    code_speaker_list[i][j] = code_speaker_list[i][j].div(code_speaker_list[i][j].sum(axis=1), axis=0).fillna(0)
                   # print(codes)
                    #print(code_speaker_list[i][j])

            #print(speaker_df)

            #######################################################################################################################

            length_of_real_data = len(filtered_data)
            #print("length_of_real_data")
           # print(length_of_real_data)
            simulated_data = pd.DataFrame(columns=filtered_data.columns)

            speaker = random.choice(unique_speakers)
            speaker_matrix = code_speaker_list[dictionary_speaker_code[speaker]][0]
            #print(speaker)
            #print(speaker_matrix)

            non_zero_elements = np.argwhere(speaker_matrix.values != 0)
            random_indices = random.choice(non_zero_elements)
            random_row, random_column = random_indices
            #print(random_row,random_column)

            simulated_data = simulated_data.append({'Speaker': speaker}, ignore_index=True).fillna(0)
            simulated_data.loc[0,str(random_column)] = 1

            for i in range(length_of_real_data):
                print(len(simulated_data))
                previous_speaker = simulated_data['Speaker'].iloc[i]
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

                rand = random.random()
                sum = 0
                for j in range(len(codes)):
                    sum += speaker_frequency_list.loc[new_speaker].iloc[j]
                    if rand < sum:
                        number_of_codes = speaker_frequency_list.loc[new_speaker].index[j]
                        break

                if use_basic:
                    number_of_codes = 1

                simulated_data = simulated_data.append({'Speaker': new_speaker}, ignore_index=True).fillna(0)
                for l in range(number_of_codes):

                    new_code_table = code_speaker_list[dictionary_speaker_code[new_speaker]][l]

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
                    previous_codes = new_code

            for i in range(simulated_data.shape[1]-2):
                simulated_data.rename(columns={str(i): letters[i]}, inplace=True)

            simulated_data.rename(columns={str(simulated_data.shape[1]-2): 'null_c'}, inplace=True)
            simulated_data["Run"] = 1
            simulated_data['Speaker'] = simulated_data['Speaker'].replace(rename_dict_speaker)

            column_name = 'Run'
            other_columns = [col for col in simulated_data.columns if col != column_name]
            simulated_data = simulated_data[other_columns + [column_name]]

            column_name = 'Speaker'
            other_columns = [col for col in simulated_data.columns if col != column_name]
            simulated_data = simulated_data[other_columns + [column_name]]

            simulated_data.to_csv("output/new/" +file_name +'/' +str(len(filtered_data)) + "/" + str(iteration) + ".csv", index=False)

        if naive:

            binary_combinations = []
            for decimal_number in range(2**(len(code_index))):  # Iterate through decimal numbers from 0 to 255
                binary_string = format(decimal_number, '0' + str(len(code_index))+ 'b')  # Convert to 8-bit binary with leading zeros
                binary_combinations.append(binary_string)
          #  print(binary_combinations)

            dictionary_speaker_code = {}
            code_speaker_list = []
            for i in range(len(unique_speakers)):
                dictionary_speaker_code[unique_speakers[i]] = i
                code_speaker_list.append(pd.DataFrame(index=binary_combinations, columns=binary_combinations).fillna(0))
            speaker_df = pd.DataFrame(index=unique_speakers, columns=unique_speakers).fillna(0)

            for i in range(1, len(filtered_data)):
                previous = filtered_data.iloc[i - 1]
                current = filtered_data.iloc[i]
                speaker_df.at[previous["Speaker"], current["Speaker"]] += 1
                speaker_df_index = dictionary_speaker_code[current["Speaker"]]

                previous_code = convert_to_code(previous)
                current_code = convert_to_code(current)
                code_speaker_list[speaker_df_index].at[previous_code, current_code] += 1
                code_speaker_list[speaker_df_index].at[current_code, previous_code] += 1

                if current_code == previous_code:
                    code_speaker_list[speaker_df_index].at[previous_code, current_code] -= 1

            speaker_df = speaker_df.div(speaker_df.sum(axis=1), axis=0).fillna(0)
          #  print(speaker_df)
            for i in range(len(code_speaker_list)):
                code_speaker_list[i] = code_speaker_list[i].div(code_speaker_list[i].sum(axis=1), axis=0).fillna(0)

            #######################################################################################################################

            length_of_real_data = len(filtered_data)
            #print(length_of_real_data)
            simulated_data = pd.DataFrame(columns=filtered_data.columns)
            #print(simulated_data)

            speaker = random.choice(unique_speakers)
            speaker_matrix = code_speaker_list[dictionary_speaker_code[speaker]]

           # print(speaker)
           # print(speaker_matrix)

            non_zero_elements = np.argwhere(speaker_matrix.values != 0)
            random_indices = random.choice(non_zero_elements)
            random_row, random_column = random_indices
            #print(random_row,random_column)

            simulated_data = simulated_data.append({'Speaker': speaker}, ignore_index=True).fillna(0)
            for i in range(len(speaker_matrix.columns[random_column])):
                if speaker_matrix.columns[random_column][i] == "1":
                    simulated_data.loc[0, str(i)] = 1
            #print(speaker_matrix.iloc[:, random_column].unique())

            previous_speaker = simulated_data['Speaker'].iloc[0]
            previous_code = speaker_matrix.columns[random_column]

            for i in range(1,length_of_real_data):
               # print(i)

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

                simulated_data = simulated_data.append({'Speaker': new_speaker}, ignore_index=True).fillna(0)
                new_code_table = code_speaker_list[dictionary_speaker_code[new_speaker]]

                rand = random.random()
                sum = 0
                # print(new_code_table)
                temp = new_code_table.loc[previous_code]
               # print(temp)
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

            #print(simulated_data)
            simulated_data.to_csv("output/simulated_new_original.csv", index=False)




