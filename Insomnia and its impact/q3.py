"""
Mingyang Xue, Coco Cheng
CSE 163 AG, AF

This file plots out four plots on the same figure,
analyzing what is the impact of insomnia have on people
"""


import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


def relationISI(df):
    """
    Takes in the cleaned dataset
    Plots out the scatterplots for ISI and other four index:
    anxiety, depression, bmi and health problems,
    and plots out their best fit lines
    """
    fig, [[ax1, ax2], [ax3, ax4]] = plt.subplots(2, figsize=(20, 10), ncols=2)
    d1 = df[['HADS_Anxiety', 'ISI']]
    x = np.array(df['ISI'])
    y = np.array(df['HADS_Anxiety'])
    m, b = np.polyfit(x, y, 1)
    d1.plot(x='ISI', y='HADS_Anxiety', ax=ax1,
            kind='scatter', c='HADS_Anxiety', colormap='BuPu')
    ax1.plot(x, m*x + b)
    ax1.set_title('ISI V.S. Anxiety_index')
    d3 = df[['HADS_Depression', 'ISI']]
    x = np.array(df['ISI'])
    y = np.array(df['HADS_Depression'])
    m, b = np.polyfit(x, y, 1)
    d3.plot(x='ISI', y='HADS_Depression', ax=ax3,
            kind='scatter', c='HADS_Depression', colormap='BuPu')
    ax3.plot(x, m*x + b)
    ax3.set_title('ISI V.S. Depression_index')
    d2 = df[['KSQ_HealthProblem', 'ISI']]
    x = np.array(df['ISI'])
    y = np.array(df['KSQ_HealthProblem'])
    m, b = np.polyfit(x, y, 1)
    d2.plot(x='ISI', y='KSQ_HealthProblem', ax=ax2,
            kind='scatter', c='KSQ_HealthProblem', colormap='BuPu')
    ax2.plot(x, m*x + b)
    ax2.set_title('ISI V.S. HealthProblem_index')
    d4 = df[['BMI1', 'ISI']]
    x = np.array(df['ISI'])
    y = np.array(df['BMI1'])
    m, b = np.polyfit(x, y, 1)
    d4.plot(x='ISI', y='BMI1', ax=ax4,
            kind='scatter', c='BMI1', colormap='BuPu')
    ax4.plot(x, m*x + b)
    ax4.set_title('ISI V.S. BMI')
    fig.savefig('q3ISI.png')


def main():
    df = pd.read_csv('/home/data2.csv')
    relationISI(df)


if __name__ == '__main__':
    main()
