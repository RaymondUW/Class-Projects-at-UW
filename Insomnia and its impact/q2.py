"""
Mingyang Xue, Coco Cheng
CSE 163 AG, AF

This file computes some statistics for individuals with different
characteristics and plots three plots about the relationship between
sleep quality and different characteristics.
"""


import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import ttest_rel
import numpy as np


def sex_stat(df):
    """
    Take in a dataframe called df as parameter.
    Calculate mean of sleep quality for female and male and print the means
    out. Use the data to compute p value of null hypothesis as described in
    report.
    """
    female = df.loc[df['sex'] == 'Female']['KSQ_OverallSleepQuality'][0:42]
    male = df.loc[df['sex'] == 'Male']['KSQ_OverallSleepQuality']
    sleep_quality_mean = df.groupby('sex')['KSQ_OverallSleepQuality'].mean()
    female_mean = sleep_quality_mean[0]
    male_mean = sleep_quality_mean[1]
    print('average sleep quality for female: ' + str(female_mean))
    print('average sleep quality for male: ' + str(male_mean))
    ttest, pval = ttest_rel(female, male)
    print('p value: ' + str(pval))


def age_groups_stat(df):
    """
    Take in a dataframe called df as parameter.
    Calculate mean of sleep quality for old individuals and
    young individuals and print the means out.
    """
    sleep_quality_mean = \
        df.groupby('AgeGroup')['KSQ_OverallSleepQuality'].mean()
    old_mean = sleep_quality_mean[0]
    young_mean = sleep_quality_mean[1]
    print('average sleep quality for old people: ' + str(old_mean))
    print('average sleep quality for young people: ' + str(young_mean))


def educ_level_stat(df):
    """
    Take in a dataframe called df as parameter.
    Calculate mean of sleep quality for patients with different levels
    of education and print the means out.
    """
    sleep_quality_mean = \
        df.groupby('EducationLevel')['KSQ_OverallSleepQuality'].mean()
    primary_mean = sleep_quality_mean[0]
    secondary_mean = sleep_quality_mean[1]
    students_in_univ_mean = sleep_quality_mean[2]
    finished_univ = sleep_quality_mean[3]
    print('average sleep quality for people finished primary school: '
          + str(primary_mean))
    print('average sleep quality for people finished secondary school: '
          + str(secondary_mean))
    print('average sleep quality for students in university: '
          + str(students_in_univ_mean))
    print('average sleep quality for people finished university: '
          + str(finished_univ))


def plots(df):
    """
    Take in a dataframe called df as parameter.
    Plots out the scatterplots for KSQ_OverallSleepQuality and other
    three index: age, education level and BMI,
    then plots out their best fit lines
    """
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(20, 10))
    d1 = df[['age', 'KSQ_OverallSleepQuality']]
    x = np.array(df['age'])
    y = np.array(df['KSQ_OverallSleepQuality'])
    m, b = np.polyfit(x, y, 1)
    d1.plot(x='age', y='KSQ_OverallSleepQuality', ax=ax1, kind='scatter',
            c='KSQ_OverallSleepQuality', colormap='BuPu')
    ax1.plot(x, m*x + b)
    ax1.set_title('age vs KSQ_OverallSleepQuality')
    d2 = df[['EducationLevel', 'KSQ_OverallSleepQuality']]
    x = np.array(df['EducationLevel'])
    y = np.array(df['KSQ_OverallSleepQuality'])
    m, b = np.polyfit(x, y, 1)
    d2.plot(x='EducationLevel', y='KSQ_OverallSleepQuality', ax=ax2,
            kind='scatter', c='KSQ_OverallSleepQuality', colormap='BuPu')
    ax2.plot(x, m*x + b)
    ax2.set_title('EducationLevel vs KSQ_OverallSleepQuality')
    d3 = df[['BMI1', 'KSQ_OverallSleepQuality']]
    x = np.array(df['BMI1'])
    y = np.array(df['KSQ_OverallSleepQuality'])
    m, b = np.polyfit(x, y, 1)
    d3.plot(x='BMI1', y='KSQ_OverallSleepQuality', ax=ax3, kind='scatter',
            c='KSQ_OverallSleepQuality', colormap='BuPu')
    ax3.plot(x, m*x + b)
    ax3.set_title('BMI vs KSQ_OverallSleepQuality')
    fig.savefig('q2.png')


def main():
    df = pd.read_csv('/home/data2.csv')
    sex_stat(df)
    age_groups_stat(df)
    educ_level_stat(df)
    plots(df)


if __name__ == '__main__':
    main()
