import os
import pandas as pd

def calculate_average(directory):
    # Initialize an empty list to store the averages
    averages = []

    # List of columns to calculate the average for
    columns_to_average = ["A", "B", "C",
                           "D", "E", "F", "G", "H"]
    columns_to_average_null=["A", "B", "C",
                           "D", "E", "F", "G", "H","null_c"]
    # Recursively traverse all subdirectories
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.csv'):
                file_path = os.path.join(root, file)

                # Read the CSV file into a pandas DataFrame
                df = pd.read_csv(file_path)

                # Filter out rows where all specified columns are 0
                df = df[~(df[columns_to_average_null] == 0).all(axis=1)]

                # Calculate the average for the specified columns and append to the list
                avg_values = df[columns_to_average].mean().mean()
                averages.append(avg_values)

    # Calculate the overall average of averages
    overall_average = sum(averages) / len(averages) if len(averages) > 0 else 0

    return overall_average

# Example usage
directory_path = r"C:\Users\Desktop\Swiecki_Extended_Model\output\new\iterative_500_simple"
result = calculate_average(directory_path)
print("Overall Average:", result)