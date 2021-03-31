"""
Mingyang Xue, Coco Cheng
CSE 163 AG, AF

This file cleans up the data that is going to be
used for q1 by filtering old data and missing values
"""


import pandas as pd


def cleanData(data):
    """
    Takes in the dataset data1
    Filters old data and missing values
    and sorts by time
    Returns the cleaned dataframe
    """
    dt = data.dropna(subset=['Year', 'Avg hrs per day sleeping', 'Age Group'])
    min_y = data['Year'] >= 2004
    max_y = data['Year'] <= 2016
    dt = data[min_y & max_y]
    dt = dt.sort_values('Year')
    return dt


def main():
    data = pd.read_excel('/home/data1.xlsx')
    cleanData(data)


if __name__ == '__main__':
    main()
