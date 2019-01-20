#!/usr/bin/python3
# -*- coding: utf-8 -*-
import sys

import pandas as pd
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import LabelEncoder

import os
os.chdir('C:\\Users\\Philippe\\Documents\\CNAM\\STA 211')


filename = 'data.extend.csv'
filename = 'data.extend2.csv' # bin 4
filename = 'data.extend-bin-10.csv' # bin 10
filename = 'data.extend-imputeFAMD-EM-bin-10.csv' # bin 10
filename = "data.extend.ImputeFamd-No_S_SBP-bin-10.csv"
filename = "data.extend.ImputeFamd-No_S_SBP-bin-10-centre_country.csv"
filename = "2019-01-19-extend-ImputeFAMD-20-bin-10.csv"
filename = "2019-01-19-ImputeFAMD-15-EM-bin-10.csv"
delimiter = ';'
data = []

# all parameter extended with centre_country
cols_score=["lvefbin", "gender_score", "bmi_bin_score", "age_bin_score",  "sbp_bin_score", "dbp_bin_score", "hr_bin_score",
                         "copd_score", "hypertension_score", "previoushf_score", "afib_score", "cad_score","centre_country_score"]

dtype_score={"lvefbin":'category',
        "gender_score":'float', "bmi_bin_score":'float', "age_bin_score":'float',  "sbp_bin_score":'float', "dbp_bin_score":'float', "hr_bin_score":'float',
        "copd_score":'float', "hypertension_score":'float', "previoushf_score":'float', "afib_score":'float', "cad_score":'float', "centre_country_score": 'float'
        }


# all parameter
cols_score=["lvefbin", "gender_score", "bmi_bin_score", "age_bin_score",  "sbp_bin_score", "dbp_bin_score", "hr_bin_score",
                         "copd_score", "hypertension_score", "previoushf_score", "afib_score", "cad_score"]

dtype_score={"lvefbin":'category',
        "gender_score":'float', "bmi_bin_score":'float', "age_bin_score":'float',  "sbp_bin_score":'float', "dbp_bin_score":'float', "hr_bin_score":'float',
        "copd_score":'float', "hypertension_score":'float', "previoushf_score":'float', "afib_score":'float', "cad_score":'float'
        }

# without hypertension
cols_score=["lvefbin", "gender_score", "bmi_bin_score", "age_bin_score",  "sbp_bin_score", "dbp_bin_score", "hr_bin_score",
                         "copd_score",  "previoushf_score", "afib_score", "cad_score"]

dtype_score={"lvefbin":'category',
        "gender_score":'float', "bmi_bin_score":'float', "age_bin_score":'float',  "sbp_bin_score":'float', "dbp_bin_score":'float', "hr_bin_score":'float',
        "copd_score":'float', "previoushf_score":'float', "afib_score":'float', "cad_score":'float'
        }


# without  sbp and dbp
cols_score=["lvefbin", "gender_score", "bmi_bin_score", "age_bin_score",   "hr_bin_score",
                         "copd_score", "hypertension_score", "previoushf_score", "afib_score", "cad_score"]

dtype_score={"lvefbin":'category',
        "gender_score":'float', "bmi_bin_score":'float', "age_bin_score":'float',  "hr_bin_score":'float',
        "copd_score":'float', "hypertension_score":'float', "previoushf_score":'float', "afib_score":'float', "cad_score":'float'
        }


# without copd
cols_score=["lvefbin", "gender_score", "bmi_bin_score", "age_bin_score",  "sbp_bin_score", "dbp_bin_score", "hr_bin_score",
                         "hypertension_score", "previoushf_score", "afib_score", "cad_score"]

dtype_score={"lvefbin":'category',
        "gender_score":'float', "bmi_bin_score":'float', "age_bin_score":'float',  "sbp_bin_score":'float', "dbp_bin_score":'float', "hr_bin_score":'float',
         "hypertension_score":'float', "previoushf_score":'float', "afib_score":'float', "cad_score":'float'
        }

# previous feature set
cols_score=["lvefbin", "gender", "bmi", "age",  "sbp", "dbp", "hr",
                         "previoushf", "afib", "cad", "copd", "centre_country"]

dtype_score={"lvefbin":'category',
        "gender":'category', "bmi":'float', "age":'float',  "sbp":'float', "dbp":'float', "hr":'float',
         "previoushf":'category', "afib":'category', "cad":'category', "centre_country":'category'
        }

categories = {"gender": 'category', "copd": 'category',
              "previoushf": 'category', "afib": 'category', "cad": 'category',
              "centre_country": 'category'}
X = pd.read_csv(filename, header=0, sep=delimiter, error_bad_lines=False,decimal=",",
                usecols=cols_score,
                dtype=dtype_score)
y = X["lvefbin"].values.ravel()
X = X.drop("lvefbin", 1)

filename_test = 'test.2019-01-19-extend-ImputeFAMD-20-bin-10.csv'
filename_test = 'test.2019-01-19-ImputeFAMD-15-EM-bin-10.csv'
cols_test_score= cols_score.copy()
cols_test_score.remove("lvefbin")
data_test = pd.read_csv(filename_test, header=0, sep=delimiter, error_bad_lines=False,decimal=",",
                        usecols=cols_test_score,
                        dtype=dtype_score)

for i in categories.keys():
    label_encoder = LabelEncoder().fit(X[i])
    X[i] = label_encoder.transform(X[i])
#    data_test[i] = label_encoder.transform(data_test[i])


    
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)

# Basic extra tree
clf = ExtraTreesClassifier(min_samples_split=16, n_estimators=150, max_depth=11, min_samples_leaf=4)
clf.fit(X_train, y_train)
print("ExtraTreesClassifier")
print("Score apprentissage  = %f" % clf.score(X_train, y_train))
print("Score test = %f" % clf.score(X_test, y_test))

# Grid Search
tuned_parameters = {'n_estimators': range(50, 450, 50)
    , 'min_samples_leaf': range(4, 20, 2)
    , 'min_samples_split': range(4, 20, 2)
    , 'max_depth': range(1, 12, 2)
                    }

clf = GridSearchCV(ExtraTreesClassifier(),
                   tuned_parameters,
                   cv=5,
                   n_jobs=-1,  # Use all processors
                   verbose=True
                   )
clf.fit(X_train, y_train)

print("Optimise ExtraTreesClassifier")
print("Score apprentissage  = %f" % clf.score(X_train, y_train))
print("Score test = %f" % clf.score(X_test, y_test))

print("Params")
print(clf.best_params_)
print("Best score")
print(clf.best_score_)
print("Variable importance")
print(clf.best_estimator_.feature_importances_)

if len(data_test) != 987:
    sys.exit("Missing values")

#pred_test = clf.best_estimator_.predict(data_test)
pred_test = clf.predict(data_test)
df = pd.DataFrame(pred_test)

import datetime

today = datetime.datetime.now()
df.to_csv(today.strftime('%Y%m%d%H%M') + "-python_extratrees-imputeFAMD-15-EM-bin-10.csv", index=False, encoding='utf-8', header=False)