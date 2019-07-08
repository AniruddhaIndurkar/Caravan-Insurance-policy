# Caravan-Insurance-policy
Choosing and Explaining Likely Caravan Insurance Customers

## Introduction
Nowadays, data is everywhere and in large scale. However, data is of no use
if there is no one to analyse and extract knowledge from it. This assessment
focuses on extracting knowledge from the car insurance data. Assume that you
are a data analyst hired by a car insurance company, your task is to analyse
the customer data collected by the car insurance company and use the knowledge from your analysis to help the company implement a targeted marketing
strategy.
Analyse a car insurance data set, which
is based on real world business data. The problem you are going to solve is:
Can you
* predict who would be interested in buying a caravan insurance policy?
* explain why you make such a prediction? Describe the actual or potential
customers; and explain why these customers buy a caravan policy.
## Data set
The data set was previously used in a KDD data challenge and is freely available online. It contains about 10K customer records, each of which have 86
attributes. The last attribute indicates if a customer actually bought the caravan insurance. Those features have originally been discretised. It has been split
into training and testing sets. The training set contains 5822 records, and the
testing set contains 4000 records.
The training and testing files are stored in three different txt files below
(click the hyper-line to download each txt file)
### TICDATA2000.txt :
Dataset to train and validate prediction models and
build a description (5822 customer records). Each record consists of 86 attributes, containing socio-demographic data (attribute 1-43) and product
ownership (attributes 44-86).The socio-demographic data is derived from
zip codes. All customers living in areas with the same zip code have the
same socio-demographic attributes. Attribute 86,“CARAVAN: Number
of mobile home policies”, is the target variable, indicated by “V86” in the
txt file.
### TICEVAL2000.txt :
Dataset for predictions (4000 customer records). It has
the same format as TICDATA2000.txt, only the target is missing. Students are supposed to generate a list of predicted targets. All datasets
are in tab delimited format. The meaning of the attributes and attribute
values can be found in the data dictionary.
### TICTGTS2000.txt 
Targets for the evaluation set.
