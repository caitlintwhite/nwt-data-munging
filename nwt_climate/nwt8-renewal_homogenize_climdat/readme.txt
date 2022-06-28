##Files in this directory are largely derived from Caitlin White's code together with code used for the NWT VII renewal.
Original files can be found in: https://github.com/NWTlter/long-term-trends/tree/master/extended_summer

SCE made slight modifications to allow them to work on more current data.
2020 data still not included due to:
1) Lack of resolution on Saddle temperature turnover.


==== Notes on scripts and data from CTW (ongoing) ====

** 2021-12-08 ****
2021-12-08: Draft versions of Saddle daily precipitation (infilled using same methods to infill D1 and C1 long term records), C1 chart temp 1952-2020, and D1 chart temp 1952-2020 on repo for preliminary use in preparing upcoming NWT renewal. There are still QA issues to treat in all of these datasets (for yrs 2019-2020 in C1 and D1 temp -- note some high tmins in 2019 at D1), and especially for the Saddle precipitation dataset. Notably, the Saddle precipitation dataset has a mean changepoint around Oct 1995 (winter ppt afterwards becomes much greater than winter ppt before), and trend not present at C1 or D1 so suspect change in data due to instrumentation or some artificial factor.

All of these draft datasets live on the nwt8-renewal repo in: c1_d1_sdl_clim/homogenize_climdate/data/

--------------------------------------------------
*CTW will write full metadata later, but currently crunched for time*
Variables of interest in each dataset:
--------------------------------------------------

Saddle ppt: 
"adjusted_precip" = daily precip with Williams et al. correction factor applied to daily values Oct-May. If you don't want to use winter-adjusted data, use the "precip" column, but using winter adjusted is strongly recommended to correct for/moderate winter overcatch with snow-blowing events.

C1 and D1 temp datasets: max, min, mean, and DTR (diurnal) are in their own columns. If the data point was infilled, regression equations are present. Please note both chart and reference data were *not* QA'd prior to infilling for years 2019-2020 (the most recent years treated). CTW has screened QA issues in both, removed a handful of the worst offenders (e.g. tmin > tmax for 1 date at C1), but for time proceeded with infilling. The QA issues present are not so prevalent or egregious that any analyses using these draft temp data should generate results substantially different than ones using future draft versions that will be QA'd more thoroughly prior to infilling.


**** 2021-11-25 ****
2021-11-25: Draft version of Saddle daily air temperature dataset ready for use in preparing upcoming NWT renewal.
See: c1_d1_sdl_clim/homogenize_climdat/data/sdl_temp_1981-2020_draft.csv

*Important*: The Saddle temp dataset from chart discontinued after 2018. The all-years draft sdl temp dataset is based mostly on daily CR21x, 23x and 1000 logger temp data from late 1986-2018, and daily means of three cr1000 HMPs for 2018-present. For the early part of the SDL temp record (pre-loggers, Jul 1981 - late 1986), the draft dataset uses projected cr21x daily temps based on cr21x temp ~ sdl chart temp linear regression.

This draft airtemp dataset will be subject to more rigorous QA and homogenization (correcting articial changes in temperature due to instrument or methods changes) in the coming months. As is, the draft version is good enough for proposal data (CTW ran through quick QA, coarse homogenization, and compared patterns in detrended data with detrended C1 and D1). Data were harmonized across SDL instrument changes using QA'd and gap-filled C1 Ameriflux and C1 chart (prepared by Tim Kittel + others for the last renewal) airtemp datasets as reference. CTW assessed the average difference btwn those C1 datasets, and between those sites and SDL for each instrument/infill method period at SDL. The recent HMP period is assumed as the most accurate/reliable temperature reading for SDL. Tmax, Tmin, and Tmean relationships were evaluated separately, but keeping diurnal temp dynamics in mind when making adjustments. Values for an entire instrument period were adjusted roughly to the degree that period's mean relationship with C1 differed from the HMP period relationship with C1. CTW compared mean differences using detrended temp (seasonal signal and random noise removed) as these were more conservative adjustments than using the raw data. C1 Ameriflux data were used as the primary reference station, but complete years in that dataset don't begin until 1999 Jan 1, and so C1 chart data were used to assess the change from SDL chart-based data in early 1980s to cr21x data. Adjustment amounts per period are noted in the draft dataset, and the unadjusted qa'd data as well as raw data are both present should the data user wish to compare differences or use unadjusted data. CTW's recommendation is to use adjusted_airtemp (especially for temporal trends).

Daily tmeans from cr 21x, 23x, and 1000 data loggers have not been QA'd. Until then, CTW is using the daily mean derived from Keith Jennings et al. QA'd and gap-filled hourly dataset (on EDI: knb-lter-nwt.168.2), which are roughly close to Tmeans from daily logger dataset, for 1990-2017. CTW used HMP period daily data for 2018-present (able to run through quick QA). Early period tmean data (1981-1989) are the mathematical mean of tmax and tmin. Maths means were also used when the derived daily mean from the Jennings et al. dataset exceeded daily tmax or were below daily tmin. When CTW able to run daily logger data through same QA procedure as tmin and tmax will update. Electronic loggers can capture microspikes in tmax or tmin that chart data cannot, which is why the math mean of electronic tmax and tmin doesn't necessarily reflect the actual average temp for the day (as recorded by the data logger; e.g., it could be within a few decimal places to several degrees).

----------
QA checks:
----------
Logic checks: min < max; mean < max; mean > min; all dates have value
Plausibility: values fall within -36 to 25C, if near or outside check against patterns at C1 and D1
Distribution: review values that fall outside 4 std. deviations (assessed globally and monthly), check against patterns at C1 and D1
Flatlines: temperature does not change for 4+ days (applicable more so to chart than logger data)
Rate changes: consecutive day temperature change falls outside 4-5 std. devs (assessed globally and monthly) or absolute change of 25 degrees; check against patterns at C1 and D1

*NWT all stations data (c1, sdl, d1, charts and loggers) through 2018 were QA-reviewed more thoroughly in 2019 for the site visit; recent HMP period data reviewed just on station-basis (so far just SDL because needed first for renewal analyses). Data through 2010 at c1 and d1 not QA reviewed for 2019 site visit, just the data not treated and infilled by Kittel et al. (years 2011-2018), following their methods (see metatadata on EDI). 

--------------------
Conditions for use: 
--------------------
These draft data should not be published, nor used in any analysis for publication until run through more rigorous QA and homogenization. Use for the NWT renewal and for any preliminary analyses is okay. For long term records, see the Kittel et al. D1 and C1 temp datasets. Please let CTW (caitlin.t.white@colorado.edu) know if you find any issues with the data (I have a running list and will add!).

--------------------------------------------------
SDL temperature metadata (columns left to right):
--------------------------------------------------
1. LTER_site: NWT;
2. local_site: sdl;
3. date: YYYY-MM-DD;
4. yr: YYYY;
5. mon: month, 1-12;
6. doy: day of year, 1-366;
7. source_instrument: primary instrument used to inform temp values (i.e., instrument period);
8. infilled: was the raw value removed during QA and infilled? T/F;
9. infill_method: method used for infilling (regression hierarchy follows Kittel et al. methods), or primary instrument if not infilled;
10. metric: air temperature metric (max, mean, or min);
11. qa_airtemp: air temperature value after qa + infill procedure;
12. hmps_used: which hmps were used to calculate the daily mean (sometimes hmp data were missing or flagged values removed after QA review);
13. adjustment_to_2018hmps: amount qa_airtemp adjusted to homogenize with HMP 2018-ongoing instrument period;
14. adjusted_airtemp: qa'd and homogenized air temperature (CTW recommends using this);
15. draft_qa_note: CTW qa notes about airtemp value in this draft dataset;
16. raw_airtemp: airtemp value as is in the current source EDI dataset;
17-19. hmp[1-3]_raw_airtemp: hmp airtemp values as they are in current source EDI dataset.

