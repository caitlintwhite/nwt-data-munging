Extended summer PCA sensitivity analysis (preliminary)
================
CTW
2019-06-17

### Sensitivity of extended summer scores and input metrics to source precip/temp dataset

Regardless of input dataset used, PC1 explains 50% of the variation in the summer climate data.

Fig 1. ![Extended Summer PC score over time, by data source](figs/PCAsensitivity_PC1overtime.png) Fig 2. ![Difference between NWT-derived extended summer score and Jennings-derived extended summer score](figs/PCAsensitivity_deltaPC1_overtime.png)

Fig 3. ![Extended summer year ordination](figs/PCAsensitivity_yrloadings.png) Fig 4. ![Extended summer variable loadings](figs/PCAsensitivity_sploadings.png)

Fig 5. ![Input summer metrics, by data source](figs/PCAsensitivity_summermetrics.png) Fig 6. ![Difference in summer metric derived from NWT NSF data and Jennings et al. infilled data](figs/PCAsensitivity_metricdelta.png)

### Compare PCA input data using Saddle chart vs. Saddle logger temperature

The following figure compares Extended Summer PCA input variables derived from different saddle temperature datasets. The NWT renewal panel shows data used in the NWT NSF renewal (1982-2014), with 2015-2017 saddle chart-derived values appeneded and 2018 saddle-chart projected from cr1000 (the Saddle chart temperature data was discontinued after 2017-12-31.) The CR1000 + CR1000 projected panel shows values derived from the CR1000 dat logger (2013-ongoing) + CR1000 projected backwards to 1982 from the saddle chart. The last two panels show PCA input variables derived from the saddle logger datasets (1986-ongoing), with 1982-1986 logger-projected using two different loggers: the current logger (cr1000) and the logger active closest-in-time the 1980s saddle chart data (cr21x). The grey shaded area denotes the period logger temperature was projected backwards from the saddle chart. The dark blue vertical lines denote years when new saddle data loggers were launched (we estimate the sensor used in the logger was the same since 1995/1996 regardless of logger changes). Although shown, values for 2015 and 2016 in the NWT renewal panel should be ignored because temperature data from Mar 2015-Aug 2016 in the saddle chart are bad (update to dataset not done yet). Because backwards-projected logger values using the saddle chart and cr21x logger are most similar to the values using the saddle chart, I recommend using the cr21x logger to backfill if we decide to use the Saddle logger data.

![](extsumpca_sensitivity_report_files/figure-markdown_github/prep%20and%20plot%20PCA%20input%20data-1.png)

### Compare raw source data

Some visuals of the raw data to highlight the origin of the any differences above..

Looking at the raw data, precipitation in the two source datasets overlaps well except for a handful of differences in later years. The extended summer analysis strongly considers just summer months' (JJA) precipitation, which overlaps well in all years (not shown). TMIN and TMAX tend to have warmer values in the Jennings infilled dataset. The NWT NSF data have a single max temp and min temp value per day, whereas the Jennings dataset provides hourly average air temperature. I took the max and min of those hourly values, per day, which gives the Jennings TMIN and TMAX values shown here. Daily TMEAN used in the extended summer analysis is the average of daily TMIN and TMAX.

![](extsumpca_sensitivity_report_files/figure-markdown_github/plot%20raw%20jennings%20vs%20nwt%20nsf-1.png)

Looking at temperature by season, the biggest differences are in the summer months (JJA):

![](extsumpca_sensitivity_report_files/figure-markdown_github/summer%20temperature%20plots-1.png)![](extsumpca_sensitivity_report_files/figure-markdown_github/summer%20temperature%20plots-2.png)

Spring and fall temperatures in both datasets overlap better compared to the summer months. For metrics used in the extended summer PCA, spring temperature values are used to calculate warming temperature thresholds ("fivedayrunning5C" and "fivedayrunnings12C"). Fall temperature values are used to calculate 3-day running minimum temperature thresholds ("GSLthreedayneg3C").

![](extsumpca_sensitivity_report_files/figure-markdown_github/spring%20and%20fall%20plots-1.png)![](extsumpca_sensitivity_report_files/figure-markdown_github/spring%20and%20fall%20plots-2.png)![](extsumpca_sensitivity_report_files/figure-markdown_github/spring%20and%20fall%20plots-3.png)![](extsumpca_sensitivity_report_files/figure-markdown_github/spring%20and%20fall%20plots-4.png)
