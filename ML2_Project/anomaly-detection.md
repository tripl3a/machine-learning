# Anomaly Detection

The task would be unsupervised anomaly detection.

## Data Set

### Sunspots

The file has 3,143 rows, which contain information about sunspots collected between the years 1749-1984. Sunspots are defined as dark spots on the surface of the sun. The study of sunspots helps scientists understand the sun's properties over a period of time; in particular, its magnetic properties.

http://www-personal.umich.edu/~mejn/cp/data/sunspots.txt

### Stock Values

Another option would be to use any time series of stock values, e.g. daily closing prices.

## Algorithms

### 1. Statistical Approach

Building a simple detection solution using a low-pass filter, e.g. moving average using discrete linear convolution. We might need to decompose the data in order to remove seasonality and trend.

### 2. Machine Learning Approach

There are three options we are considering, of which we would try one or more:

1. Density-Based Anomaly Detection (K-nearest neighbour)
1. Clustering-Based Anomaly Detection (K-means)
1. Support Vector Machine-Based Anomaly Detection