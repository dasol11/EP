# -*- coding: utf-8 -*-

"""
Created on Sun Jul 31 16:43:28 2022

@author: Junhyun
"""

import numpy as np
import pandas as pd

from sklearnex import patch_sklearn 
patch_sklearn()

# Ridge Regression
from sklearn.linear_model import Ridge
from sklearn.linear_model import RidgeCV

# Randomforest Regression
from sklearn.ensemble import RandomForestRegressor

# Support Vector Regression
from sklearn.svm import SVR

# hyper parameter tunning
from sklearn.model_selection import GridSearchCV

# ARIMA
from statsmodels.tsa.arima.model import ARIMA
from pmdarima.arima import auto_arima

# LSTM
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.layers import Dense, Dropout
from keras.layers import LSTM


class TimeSeriesRegression():
    
    def __init__(self):
        self.data = None
    
    def TimeSeriesDataTransform(self, data, lag):
        """
        # 참조 코드 : http://103.60.126.183:8150/gidatalab (LSTM)
        
        데이터를 변환하기 위해서는 Y값이 맨 왼쪽에 위치해있어야함 
        
        To transoform data to timeseries data, target data(Y) have to be located at leftmost
    
        Parameters
        ----------
        data : DataFrame
            data
        lag : int
            시계열 예측에서 데이터를 미는 시점 (= Time sequence)

        Returns
        -------
        agg : 시계열 에측이 가능하도록 변환된 데이터

        """
        if isinstance(self.data, np.ndarray):
            data = pd.DataFrame(self.data)
        elif isinstance(self.data, pd.core.series.Series):
            data = pd.DataFrame(self.data)
        
        n_vars = 1 if type(data) is list else data.shape[1]
        df = pd.DataFrame(data)

        cols, names = list(), list()

        # 입력값의 순서 (t-n, ... t-1)
        for i in range(lag, 0, -1):
            cols.append(df.shift(i))
            names += [('%s(t-%d)' % (data.columns[j], i)) for j in range(n_vars)]

        # 예측의 순서 (t, t+1, ... t+n)
        for i in range(0, 1):
            cols.append(df.shift(-i))
            if i == 0:
                names += [('%s(t)' % (data.columns[j])) for j in range(n_vars)]
            else:
                names += [('%s(t+%d)' % (data.columns[j], i)) for j in range(n_vars)]

        # 합치기
        agg = pd.concat(cols, axis=1)
        agg.columns = names

        # NaN 값의 row를 제거
        agg.dropna(inplace=True)
        
        # 인덱스 초기화
        agg = agg.reset_index(drop=True)
        
        agg = agg.iloc[:,0:(data.shape[1]*lag)+1]

        return agg

        
    # Ridge Regression
    def Ridge_regression(self, X_train, X_test, y_train, y_test):

        alphas = np.arange(0, 1, 0.03)
        ridgecv = RidgeCV(alphas = alphas, cv = 5) 
        ridgecv.fit(X_train, y_train)
        print("alpha : %.2f" % ridgecv.alpha_)

        ridge_train_pred = ridgecv.predict(X_train)
        ridge_test_pred = ridgecv.predict(X_test)

        return({'trainPrediction':ridge_train_pred, 'testPrediction':ridge_test_pred})
    
    # Random Forest Regression
    def Randomforest_regression(self, X_train, X_test, y_train, y_test):

        params = {
            'n_estimators' : [100],
            'max_depth' : [6,8,10,12],
            'min_samples_leaf' : [8,12,8],
            'min_samples_split' : [8,16,20]
        }

        rf = RandomForestRegressor()
        grid_cv = GridSearchCV(rf, param_grid=params, cv=5)
        grid_cv.fit(X_train, y_train)
        print("최적 하이퍼 파라미터:\n", grid_cv.best_params_)

        rf_train_pred = grid_cv.predict(X_train)
        rf_test_pred = grid_cv.predict(X_test)

        return({'trainPrediction':rf_train_pred, 'testPrediction':rf_test_pred})
    
    # Support Vector Regression
    def Supportvector_regression(self, X_train, X_test, y_train, y_test):

        param_grid = [
            {'kernel':['linear'], 'C': [0.0001, 0.001, 0.01, 0.1, 1.0, 10.0, 100.0, 1000.0]},
            {'kernel':['rbf'], 'C': [0.0001, 0.001, 0.01, 0.1, 1.0, 10.0, 100.0, 1000.0]},
            {'gamma':[0.0001, 0.001, 0.01, 0.1, 1.0]}
        ]
        svr = SVR()
        grid_cv = GridSearchCV(svr, param_grid, cv=5, scoring='neg_mean_squared_error', verbose=1)
        grid_cv.fit(X_train, y_train)
        print("최적 하이퍼 파라미터:\n", grid_cv.best_params_)

        svr_train_pred = grid_cv.predict(X_train)
        svr_test_pred = grid_cv.predict(X_test)

        return({'trainPrediction':svr_train_pred, 'testPrediction':svr_test_pred})

    # LSTM (수정해야함)
    def LSTM(self, data, X_train, X_test, y_train, y_test, epochs=100):

        data = pd.DataFrame(data)
        
        # scale data (딥러닝을 돌리기 위해서는 scale 필요)
        scaler = MinMaxScaler(feature_range=(0, 1)) # 0~1사이로 scale
        scaled = scaler.fit_transform(data.values)
        scaled_df = pd.DataFrame(scaled, columns=data.columns)


        # 2차원 array를 1차원으로 변형, 후에 3차원으로 변형
        X_trainD = DimensionTransform(X_train.values)
        X_train = X_trainD.reshape(X_train.shape[0], lag, data.shape[1])

        # 2차원 array를 1차원으로 변형, 후에 3차원으로 변형
        X_testD = DimensionTransform(X_test.values)
        X_test = X_testD.reshape(X_test.shape[0], lag, data.shape[1])

        # LSTM의 구조
        model = Sequential()
        model.add(LSTM(8, input_shape=(X_train.shape[1], X_train.shape[2]), return_sequences=True, activation='relu')) # 하나의 층 8개의 노드, return_sequences=True 필수
        model.add(LSTM(4, activation='relu', return_sequences=False)) # 하나의층, 4개의 노드, 마지막에는 return_sequences=False
        model.add(Dense(1)) # 노드가 하나인 구조를 만들었다 (하나의 예측값으로 표현하기 위해)

        # model compile
        model.compile(loss='mse', optimizer='adam')

        # fit network
        history = model.fit(X_train, y_train, epochs=epochs, verbose=1, shuffle=False) # epochs : 반복횟수

        lstm_train_pred = model.predict(X_train)
        lstm_test_pred = model.predict(X_test)

        lstm_train_pred = lstm_train_pred.reshape(len(lstm_train_pred),)
        lstm_test_pred = lstm_test_pred.reshape(len(lstm_test_pred),)

        return({'trainPrediction':lstm_train_pred, 'testPrediction':lstm_test_pred})
