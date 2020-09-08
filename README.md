# kaggle_ML_DS_Survey_project
University Statistical Data Science Lecture Team Project

* In this repository, till now, only uploaded files for classifying high salary or not.
* more files will be uploaded. 

-------------------------------------------------------------

## Dataset 
Kaggle 2019 ML & DS Survey dataset, 

You can download dataset from https://www.kaggle.com/c/kaggle-survey-2019/data

-------------------------------------------------------------

## Project Goal : "What is the Trait of Successful Data Scientists, and What makes them valuable?"
classifying which kaggler will be paid top 20% among other kagglers and find their skills in various fields.

-------------------------------------------------------------

## Folders 

### EDA & Preprocessing
preprocessing code written in R. After simple preprocessing, all other jobs were done by python. 


(drop confidence=NA variables with respect to duration distribution, re-encoded Role, droped observations with too much duration and too less duration. Additional preprocessing have been done in those files) 

### Modeling Trials

**Salary_include_disclose.ipynb** : Included "I do not wish to disclose my approximate yearly compensation", re-coded target variable "salary" to be 1 if the kaggler's get paid more than top 20% or otherwise, 0. Also, some uninformative variables and variables which have too many scales are included in classification process.

**Salary_multiclass.ipynb** : Classify all salaries into 3 categories. 0 ~ 70,000 : 0, 70,000 ~ 500,000+ : 1, "I do not wish to disclose my approximate yearly compensation" : 2 => 2 class classification was better. And Also decided to drop the disclose values in salary. 

**Salary_drop_disclose.ipynb** : Classify top 20% or not, and also, droped the Disclose values in the salary. Additionally, droped some variables that are non informative, or have too many scales, or too strong that hides other variables' effects. Model Selection, Parameter Tuning have been done. 

**Salary_re_encoded_Role.ipynb** : Final Report. Re-encoded some Role classes, with salary distribution at each role. (more information) Also, re-encoding and labeling have been done. Also, Because lasso logistic regression has been selected, we could examine coefficients of each variables and see which characteristic makes kagglers more valuable. 


### Final Results 
**Salary_re_encoded_Role.ipynb** : Final Modeling Results and analysis using the results 

#### Reference(updating) 
https://www.kaggle.com/andresionek/what-makes-a-kaggler-valuable

