# real-estate
 The data analyzed can be found: https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set
 
We will use the given variables “transaction date” (years), “age” (years), “distance to nearest mrt
station” (meters), “number of stores in area”, and “price” (10000 NWT / ping). For longitude and
latitude, we will divide the district into a suitable number of regions (which may involve external
research as well as outlier handling), and treat the region as a categorical variable. We will also
consider seasonality components if we find seasonal variation in the price. We will consider
log-transforming the price.

Our research question is to determine which factors are associated with an increase in price
over time. In other words, we seek to find which non-date variables, when multiplied by date,
are associated with an increase in price. This conclusion can be used to provide advice on
where in the region to invest in real estate for higher returns.
 
