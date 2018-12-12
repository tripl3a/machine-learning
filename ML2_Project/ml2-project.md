# Machine Learning 2 Project

## Time series forecasting

Would be okay to do ARIMA and LSTM; Dions group wants to do it as well

Tutorials:

* [Forecast (beginner) - Simple, Exponential, ARIMA](https://www.kaggle.com/goldens/forecast-beginner-simple-exponential-arima?utm_medium=email&utm_source=intercom&utm_campaign=datanotes-20181129) (R)
* [Time Series Analysis - Artificial Neural Networks](https://www.kaggle.com/abhishekmamidi/time-series-analysis-artificial-neural-networks?utm_medium=email&utm_source=intercom&utm_campaign=datanotes-20181129) (Python)

Datasets:

* [Bike Sharing in Washington D.C. Dataset](https://www.kaggle.com/marklvl/bike-sharing-dataset)
    * daily: 731 x 16
    * hourly: 17379 x 17
* [US Candy Production by Month](https://www.kaggle.com/rtatman/us-candy-production-by-month): 548 x 2
* [Precipitation Data of Pune from 1965 to 2002](https://www.kaggle.com/abhishekmamidi/precipitation-data-of-pune-from-1965-to-2002): 38 x 13 (wide format) // 456 x 2 (long format)

## Anomaly detection

Statistical Approaches:

* [Anomaly Detection with the Normal Distribution](https://anomaly.io/anomaly-detection-normal-distribution/)
    * I think it could be used in combination with something like ARIMA on the residuals after seasonality and trend were substracted
* [Simple algorithm for online outlier detection of a generic time series](https://stats.stackexchange.com/questions/1142/simple-algorithm-for-online-outlier-detection-of-a-generic-time-series)
* [Simple anomaly detection for metrics with a weekly pattern](https://machinelearnings.co/data-science-tricks-simple-anomaly-detection-for-metrics-with-a-weekly-pattern-2e236970d77)
    * a multi model solution using Exponential Moving Average and Exponential Moving Standard Deviation

Tutorials:

* [Unsupervised Anomaly Detection](https://www.kaggle.com/victorambonati/unsupervised-anomaly-detection)
* [Introduction to Anomaly Detection](https://www.datascience.com/blog/python-anomaly-detection) (Python)
    * introductory knowledge of anomaly detection, including how to use low-pass filter and simple moving average to detect abnormalities

Datasets:

* [Sunspots](http://www-personal.umich.edu/~mejn/cp/data/sunspots.txt) 
    * The file has 3,143 rows, which contain information about sunspots collected between the years 1749-1984. Sunspots are defined as dark spots on the surface of the sun. The study of sunspots helps scientists understand the sun's properties over a period of time; in particular, its magnetic properties.
* Any time series of stock values could be used, e.g. daily closing prices
* [Outlier Detection DataSets (ODDS)](http://odds.cs.stonybrook.edu/)
    * a collection of many data sets for different purposes, some of them are labeled
* [Numenta Anomaly Benchmark (NAB)](https://www.kaggle.com/boltzmannbrain/nab)

## Predictive maintenance

[Machine learning for predictive maintenance: where to start?](https://medium.com/bigdatarepublic/machine-learning-for-predictive-maintenance-where-to-start-5f3b7586acfb)
