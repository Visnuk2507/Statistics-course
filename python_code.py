def add(x,y):
    return x + y



import pandas as pd

# Load the dataset
df = pd.read_csv('Projects\dinans1_data.csv', delimiter=';')

# Check for missing values
missing_values = df.isnull().sum()

# Display total missing values in the dataset
total_missing = missing_values.sum()
print(f"Total missing values: {total_missing}")

# Display missing values per column
print(missing_values)
