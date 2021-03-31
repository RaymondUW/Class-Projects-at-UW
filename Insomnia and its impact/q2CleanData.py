"""
Mingyang Xue, Coco Cheng
CSE 163 AG, AF

This file cleans up the data that is going to be
used for q2 and q3 by filtering out unneccessary data
and making some elusive data clear.
"""


import pandas as pd


def clean_data(df):
    """
    Take in a dataframe called df as parameter.
    Edit one column of data that representing the id of
    patients.
    Return the cleaned dataframe.
    """
    df['id'] = df['id'].apply(substring)
    df['id'] = df['id'].astype(int)
    return df


def merge(df_1, df_2):
    """
    Take in two dataframes called df_1 and df_2 as parameters.
    Merge these two datasets into one new dataset based on the id
    of patients.
    Return the merged dataframe.
    """
    return df_1.merge(df_2,  how='left', left_on='id', right_on='id')


def clean_merged_data(df_old):
    """
    Take in a dataframe called df as parameter.
    Drop all data that won't be used in the analysis and make the rest
    data readable and concise by removing some redundent words.
    Return the cleaned dataframe.
    """
    df = df_old.copy()
    df = df[['id', 'sex', 'age', 'AgeGroup', 'BMI1', 'EducationLevel',
            'HADS_Anxiety', 'HADS_Depression', 'ISI', 'KSQ_HealthProblem',
             'KSQ_OverallSleepQuality']]
    df['BMI1'] = df['BMI1'].apply(substring_2)
    df['BMI1'] = df['BMI1'].astype(float)
    df['KSQ_HealthProblem'] = df['KSQ_HealthProblem'].apply(substring_3)
    df['KSQ_HealthProblem'] = df['KSQ_HealthProblem'].astype(int)
    OSQ_3 = df['KSQ_OverallSleepQuality'] == '3 Varken bra eller d̴ligt'
    OSQ_4 = df['KSQ_OverallSleepQuality'] == '4 Ganska bra '
    OSQ_5 = df['KSQ_OverallSleepQuality'] == '5 Mycket bra'
    df.loc[OSQ_3, 'KSQ_OverallSleepQuality'] = 3
    df.loc[OSQ_4, 'KSQ_OverallSleepQuality'] = 4
    df.loc[OSQ_5, 'KSQ_OverallSleepQuality'] = 5
    primary_school = df['EducationLevel'] == 'Har avslutat grundskolan'
    df.loc[primary_school, 'EducationLevel'] = 0
    secondary_school = df['EducationLevel'] == 'Har avslutat gymnasieskolan'
    df.loc[secondary_school, 'EducationLevel'] = 1
    student_at_univ = df['EducationLevel'] == \
        'Studerar f̦r n�_rvarande p̴ universitet/h̦gskola'
    df.loc[student_at_univ, 'EducationLevel'] = 2
    finished_univ = df['EducationLevel'] == \
        'Har examen fr̴n universitet/h̦gskola'
    df.loc[finished_univ, 'EducationLevel'] = 3
    return df


def substring(string):
    """
    Take in a string called string as parameter.
    Return part of term which only contains integer.
    """
    return string[4:]


def substring_2(string):
    """
    Take in a string called string as parameter.
    Edit and return part of string.
    """
    return string[0:2] + '.' + string[3:]


def substring_3(string):
    """
    Take in a string called string as parameter.
    Return part of string.
    """
    return string[0]


def main():
    df_1 = pd.read_csv('/home/data2.1.csv')
    df_2 = pd.read_csv('/home/data2.2.csv')
    df_1 = clean_data(df_1)
    df = merge(df_1, df_2)
    df = clean_merged_data(df)
    df.to_csv('/home/data2.csv')


if __name__ == '__main__':
    main()
