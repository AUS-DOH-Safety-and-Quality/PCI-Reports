# Install required packages if not already installed
import sys
import subprocess

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

try:
    import pandas
except ImportError:
    install("pandas")

try:
    import matplotlib
except ImportError:
    install("matplotlib")

import pandas as pd
import matplotlib.pyplot as plt

# Load the data
df = pd.read_csv('pci_data.csv')

# Sex proportions
sex_map = {1: 'Male', 2: 'Female'}
df['sex_cat'] = df['sex'].map(sex_map)
sex_counts = df['sex_cat'].value_counts(normalize=True) * 100
sex_counts.plot(kind='bar', color=['#0047AB', '#d62728'])
plt.title('Proportion by Sex')
plt.ylabel('Percentage')
plt.show()

# Age categories
bins = [0, 40, 50, 60, 70, 80, df['age'].max()]
labels = ['<= 40 yrs', '41-50 yrs', '51-60 yrs', '61-70 yrs', '71-80 yrs', '>= 81 yrs']
df['age_cat'] = pd.cut(df['age'], bins=bins, labels=labels, right=True)
age_counts = df['age_cat'].value_counts(normalize=True).sort_index() * 100
age_counts.plot(kind='bar')
plt.title('Proportion by Age Category')
plt.ylabel('Percentage')
plt.show()

# Diabetes status proportions
if 'db' in df.columns:
    diabetes_map = {0: 'No Diabetes', 1: 'Diabetes'}
    df['diabetes_cat'] = df['db'].map(diabetes_map)
    diabetes_counts = df['diabetes_cat'].value_counts(normalize=True) * 100
    diabetes_counts.plot(kind='bar')
    plt.title('Proportion by Diabetes Status')
    plt.ylabel('Percentage')
    plt.show()
else:
    print("Diabetes column ('db') not found in the data.")
