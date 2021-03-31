"""
Mingyang Xue, Coco Cheng
CSE 163 AG, AF

This file implements test functions for the final project.
"""


import pandas as pd
import matplotlib.pyplot as plt
import q1CleanData
import q1
import q2


def testq1Num(dt, num):
    """
    Takes in the cleaned dataset dt and the num from q1
    Tests q1 by calculating the average sleeping time
    and tests the accuracy of the plot by calculating
    the min value of the number of the insomniac
    people
    """
    avg_sleeptime = dt['Avg hrs per day sleeping'].mean()
    print("The average sleeping time of the entire experiment is ",
          avg_sleeptime)
    min_num = num['Number of Insomniac People'].min()
    min_year = num[num['Number of Insomniac People'] == min_num]
    print(min_year)


def testq2mean(df):
    """
    Takes in the cleaned dataset data2
    Tests the accuracy of average sleep quality measurement for different
    groups of patients by calculating those values in a different way.
    """
    q2.sex_stat(df)
    print(df.loc[df['sex'] == 'Female']['KSQ_OverallSleepQuality'].mean())
    print(df.loc[df['sex'] == 'Male']['KSQ_OverallSleepQuality'].mean())
    q2.age_groups_stat(df)
    print(df.loc[df['AgeGroup'] == 'Old']['KSQ_OverallSleepQuality'].mean())
    print(df.loc[df['AgeGroup'] == 'Young']['KSQ_OverallSleepQuality'].mean())
    q2.educ_level_stat(df)
    print(df.loc[df['EducationLevel'] == 0]['KSQ_OverallSleepQuality'].mean())
    print(df.loc[df['EducationLevel'] == 1]['KSQ_OverallSleepQuality'].mean())
    print(df.loc[df['EducationLevel'] == 2]['KSQ_OverallSleepQuality'].mean())
    print(df.loc[df['EducationLevel'] == 3]['KSQ_OverallSleepQuality'].mean())


def testq3Relation(df):
    """
    Takes in the cleaned dataset data2
    Tests the accuracy of q3's plots by running similar
    codes which draw scatter plots on similar dataset
    """
    fig, [[ax1, ax2], [ax3, ax4]] = plt.subplots(2, figsize=(20, 10), ncols=2)
    d1 = df[['HADS_Anxiety', 'KSQ_OverallSleepQuality']]
    d1.plot(x='KSQ_OverallSleepQuality', y='HADS_Anxiety',
            ax=ax1, kind='scatter', c='HADS_Anxiety')
    d3 = df[['HADS_Depression', 'KSQ_OverallSleepQuality']]
    d3.plot(x='KSQ_OverallSleepQuality', y='HADS_Depression',
            ax=ax3, kind='scatter', c='HADS_Depression')
    d2 = df[['KSQ_HealthProblem', 'KSQ_OverallSleepQuality']]
    d2.plot(x='KSQ_OverallSleepQuality', y='KSQ_HealthProblem',
            ax=ax2, kind='scatter', c='KSQ_HealthProblem')
    d4 = df[['BMI1', 'KSQ_OverallSleepQuality']]
    d4.plot(x='KSQ_OverallSleepQuality', y='BMI1',
            ax=ax4, kind='scatter', c='BMI1')
    fig.savefig('q3Quality.png')


def main():
    data = pd.read_excel('/home/data1.xlsx')
    dt = q1CleanData.cleanData(data)
    num = q1.numberOfPeople(dt)
    testq1Num(dt, num)
    df = pd.read_csv('/home/data2.csv')
    testq2mean(df)
    testq3Relation(df)


if __name__ == '__main__':
    main()
