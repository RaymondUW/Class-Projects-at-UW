"""
Mingyang Xue, Coco Cheng
CSE 163 AG, AF

This file calculate the number of people with
short sleep duration each year and plot it out
with a line graph
"""


import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import q1CleanData


def numberOfPeople(dt):
    """
    Takes in the cleaned dataset
    Calculate the number of people with short
    sleep duration each year
    and return the result dataframe
    """
    age1 = dt['Age Group'] == '15 years and over'
    age2 = dt['Age Group'] == '15 to 24 years'
    age3 = dt['Age Group'] != '15 years and over'
    age4 = dt['Age Group'] != '15 to 24 years'
    lessThan8hrs = dt['Avg hrs per day sleeping'] < 8.6
    lessThan9hrs = dt['Avg hrs per day sleeping'] < 9.3
    num = dt[(age1 & age2 & lessThan9hrs) | (age3 & age4 & lessThan8hrs)]
    num = num[['Year', 'Avg hrs per day sleeping']]
    num = num.groupby('Year').count()
    num.columns.values[0] = 'Number of Insomniac People'
    return num


def plot(num):
    """
    Takes in the dataframe with the number of
    people with short sleep duration each year
    and plot it out with a line plot
    """
    sns.relplot(data=num, kind="line")
    plt.title('Number of Insomniac People Change over Time')
    plt.xlabel('Year')
    plt.ylabel('Number of Insomniac People')
    plt.savefig('q1line_plot', bbox_inches='tight')


def main():
    data = pd.read_excel('/home/data1.xlsx')
    dt = q1CleanData.cleanData(data)
    num = numberOfPeople(dt)
    plot(num)


if __name__ == '__main__':
    main()
