# R sample script for:
#plotting d1


#-------------------------------------------------------------------------------
#SETUP
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
#use metajam package to nicely view the metadata
#devtools::install_github('NCEAS/metajam', build_vignettes = TRUE)


## first time will need to make sure all of these are installed
#install.packages(c("tidyverse", "ggplot2", "gridExtra")

#to do the eml workflow you'll need a few more
#install.packages(c("jqr", "json ", "devtools","xml2")


#clean your workspace
rm(list = ls(all = TRUE))
if (length (sessionInfo()$otherPkgs)>0){
  lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
         detach,character.only=TRUE,unload=TRUE)
}

library (EML)
#most functions referenced directly, but magrittr needed for piping
# and ggplot2 need a lot of internal functions to plot, and I got lazy typing them all
library (magrittr)
library (ggplot2)
library (dplyr) 

source ('utility_functions/utility_functions_all.R')
#from https://github.com/gavinsimpson/random_code/blob/master/derivFun.R

#-------------------------------------------------------------------------------
#SELECT DATA AND MECHANISM OF ACCESS
#get new data? T/F, TRUE = download, F = not necessary, saves time
getNewData=TRUE
#set local path to save data, this works on PC to just send them to your downloads
#adjust accordingly
datadir='../../../Downloads/'
#datadir='~/data/'

#-------------------------------------------------------------------------------
#DEFINE WRITE DIRECTORIES FOR PLOTTING
#& to dump metadata if it's helpful
plot_dir<-'clim_d1/plots/'
if (!file.exists(plot_dir)){
  dir.create(plot_dir)
}

#-------------------------------------------------------------------------------
#DECIDE HOW MANY CORES TO USE
ncores=2


#-------------------------------------------------------------------------------
#DOWNLOAD DATA AND METADATA
#pick a dataset to explore
edi_id='402'

packageid=getCurrentVersion(edi_id)

#you will want to note which version you are working from in case of updates
packageid

#grab the metadata
myeml=getEML(packageid)

#at this point the entirety of the eml is in a list
#However, EML isn't very human friendly to read, it's much easier to view in a browser
#or simply copy the url from meta_file below and paste it into a browser
# use format
#https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-nwt&identifier=number.version
#example: 
#https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-nwt&identifier=93.1

#you can also get the basics out in tabular from
myeml.table=tabularize_eml(myeml)
myeml.table

#if >1 object, get attributes, and download
if (is.null(names(myeml$dataset$dataTable))){
  attributeList=lapply(myeml$dataset$dataTable, function(x){
    EML::get_attributes(x$attributeList)
  })
  names(attributeList)=lapply(myeml$dataset$dataTable, function(x){
    x$physical$objectName})
  if(getNewData){
    #download all the datatables in the package
    lapply(myeml$dataset$dataTable, function(x){
      url_to_get=x$physical$distribution$online$url$url
      download.file(url_to_get, destfile=paste0(datadir, x$physical$objectName))
    })
  }
  
}else{
  #if only one data table
  attributeList=list(EML::get_attributes(myeml$dataset$dataTable$attributeList))
  names(attributeList)=myeml$dataset$dataTable$physical$objectName
  if(getNewData){
    url_to_get=myeml$dataset$dataTable$physical$distribution$online$url$url
    download.file(url_to_get, destfile=paste0(datadir, myeml$dataset$dataTable$physical$objectName))
  }
}

#read files - you can modify this
#source('https://portal.edirepository.org/nis/codegenerationdownload?filename=knb-lter-and.2719.6.tidyr')
source('https://portal.edirepository.org/nis/codegenerationdownload?filename=knb-lter-nwt.402.2.tidyr')

#map eml types to R col classes
# attributeList[[1]]$attributes=dplyr::mutate(
#   attributeList[[1]]$attributes, col_classes=dplyr::case_when(domain=='textDomain' ~'col_character()',
#                                                        domain=='dateTimeDomain' ~'col_date()',
#                                                        domain=='numericDomain' ~'col_number()',
#                                                        domain=='enumeratedDomain' ~'character()'))%>%
#   dplyr::mutate(
#     colclasses=dplyr::case_when(domain=='textDomain' ~'character',
#                          domain=='dateTimeDomain' ~'Date',
#                          domain=='numericDomain' ~'numeric',
#                          domain=='enumeratedDomain' ~'character'))


# datafile=read.csv(paste0(datadir, names(attributeList)[[1]]), na=c('NA', 'NaN', ''),
#                                    colClasses=attributeList[[1]]$attributes$colclasses)%>%tibble::as_tibble()

#or just let R sort out what the types are
datafile=readr::read_csv(paste0(datadir,names(attributeList)[[1]]), na=c('NA', 'NaN', ''), guess_max=100000)
#if you get warnings about type conversions it's likely because there
# are non-numeric missing value codes you need to deal with, try reading
#in before setting colClasses and viewing/deciding what to do, e.g.

#datafile=read.csv(paste0(datadir, names(attributeList)[[1]]), na=c('NA', 'NaN', ''),
#apply(datafile,2, findNonNumeric)
#attributeList[[1]]$attributes$missingValueCode
#attributeList[[1]]$attributes$missingValueCodeExplanation
#-------------------------------------------------------------------------------
#VIEW DATA AND DEFINE VARIABLES FOR SUMMARY AND PLOTTING
#read in file

#view a little of the data to understand contents/structure
tibble::glimpse(datafile)


#define vars to plot, skip flags, names
num_vars=names(datafile)%>%grep('LTER|local|flag|date|jday|logger|year', .,
                                value=TRUE, invert=TRUE)

use_theme <- function () { 
  theme_grey()+
    theme(
      #this is size of the font on the panels
      strip.text.y = element_text(size = 3),
      #yaxis font size
      axis.text.y = element_text(size = 4),
      #axis.title.y = element_blank(),
      axis.text.x=element_text(angle=-90)
    )
}

for (j in num_vars){
  datafile[[j]]<-as.numeric(as.character(datafile[[j]]))
  boxplot_year<-ggplot(datafile, aes_string(x='year', y=as.name(j), group='year'))+
    geom_boxplot()+
    use_theme()
  trend_year<-ggplot(datafile, aes_string(x='date', y=as.name(j)))+
    geom_point()+geom_smooth()+#facet_wrap(~local_site, nrow=2)+
    use_theme()
  
  within_year<-ggplot(datafile, aes_string(x='jday', y=as.name(j),
                                           group='year',
                                           color='year'))+
    viridis::scale_color_viridis()+
    geom_point()+geom_smooth()#+facet_wrap(~local_site)+
  use_theme()
  
  ggsave(trend_year, file=paste0(plot_dir, '/', 'trend_year_', as.name(j), '.jpg'))
  ggsave(boxplot_year, file=paste0(plot_dir, '/', 'boxplot_year',as.name(j), '.jpg'))
  ggsave(within_year, file=paste0(plot_dir, '/', 'within_year_', j, '.jpg'))
}


#sce add in removing the flagged values - then rerunning


#here's an example of how to look at just a few years
# within_year_2016<-ggplot(datafile[datafile$year%in%c(2015,2016, 2017),], aes_string(x='jday', y=as.name('bp_avg'),
#                                          group='year',
#                                          color='year'))+
#   viridis::scale_color_viridis()+
#   geom_point()+geom_smooth()


#when have looked at them all, delete
#file.remove(list.files(plot_dir)%>%grep('\\.jpg', ., value=TRUE))







#####




#define pkey for viewing/removing any duplicates before analysis
pKey=c('point_ID', 'date')


#note for the saddle site it is probably most sensical analyze spatially by
# by veg type. Veg type is not included in the snow survey, so we need to get it
# from the saddle npp data
myeml2<-getEML('knb-lter-nwt.16.2')
#usually there is just one data table per set
# so you can just read like this
datafile2<-readr::read_csv(myeml2$dataset$dataTable$physical$distribution$online$url$url,
                           na = c("", "NA", "NaN"))

#some reason 1992 has the wrong veg class for point5 - this will be
# fixed with the upload of the 2018 data
# data but not yet fixed here
veg_class=datafile2 %>%
  dplyr::filter(., year!=1992)%>%
  dplyr::select(., 'grid_pt', 'veg_class')%>%
  dplyr::distinct(.)


#set the point_ID to numeric for joining, then add veg classes
#remainder are all ROCK
datafile %<>% dplyr::mutate(., point_ID=as.numeric(point_ID))%>%
  dplyr::left_join(., veg_class, by=c('point_ID'='grid_pt'))%>%
  dplyr::mutate(., veg_class=replace(veg_class, is.na(veg_class), 'rock'))



#define variables that would be useful to plot
# for this example, I picked out things that are numeric
# not a flag, not a standard deviation

#-------------------------------------------------------------------------------
#CLEAN DATA AS NEEDED

#View duplicates, handle as you see appropriate
# if there's 0 rows here, you don't need to do anything
#otherwise, need to address
datafile %>% 
  dplyr::group_by(.dots=pKey) %>% 
  dplyr::filter(n()>1)

#One (simple) way for duplicates is just to resolve ties by keeping the row
# with more content (fewer NAs)

# datafile %<>% dplyr::mutate(colsWithData = rowSums(!is.na(.))) %>%
#   dplyr::arrange(., colsWithData) %>%
#   dplyr::distinct_(.dots=pKey, .keep_all=TRUE) %>%
#   dplyr::select(-colsWithData)

#-------------------------------------------------------------------------------
#ADD AUXILIARY AND DERIVED VARIABLES
#extract year, doy to help with plotting
# add decimal_date for gams
datafile %<>% dplyr::mutate(., year=lubridate::year(date),
                            doy = lubridate::yday(date),
                            month = lubridate::month(date),
                            dec_date= lubridate::decimal_date(date))

#add extended summer PCA, categorize as spring and fall
datafile%<>%dplyr::left_join(.,
                             dplyr::select(extsummer, eco_year, sumallPC1, sumallPC2),
                             by=c('year'='eco_year'))%>%dplyr::mutate(
                               season=ifelse(doy>240, "fall", "spring"))

#-------------------------------------------------------------------------------
#EXAMINE SAMPLING SEASON
#figure out if the survey length was the same in all years - this will affect seaonality
# estimates and possible require some infilling

#end of spring sampling
eos=datafile%>%dplyr::filter(., season=='spring')%>%
  dplyr::group_by(year)%>%
  dplyr::summarize(.,max_spring=max(doy))

#start of fall sampling
sof=datafile%>%dplyr::filter(., season=='fall')%>%
  dplyr::group_by(year)%>%
  dplyr::summarize(.,min_fall=min(doy)) %>%
  dplyr::inner_join(eos, .)

#-------------------------------------------------------------------------------
#PLOT
#patterns over time
datafile%>%dplyr::group_by(veg_class, date, year)%>%
  dplyr::summarize(mean_depth=mean(mean_depth))%>%
  ggplot(data=., aes(x=date, y=mean_depth,
                     group=veg_class,
                     color=veg_class))+geom_point()+geom_smooth()+
  facet_wrap(~veg_class)+use_theme()


#seasonality - patterns WITHIN year
datafile%>%group_by(veg_class, date, year, doy)%>%
  dplyr::summarize(mean_depth=mean(mean_depth))%>%
  ggplot(data=., aes(x=doy, y=mean_depth,
                     group=year,
                     color=year))+geom_smooth()+
  geom_point()+facet_wrap(~veg_class)+
  viridis::scale_color_viridis()+use_theme()


#-------------------------------------------------------------------------------
# ADDRESS OFF SEASON DATA
#unclear if the '0' depths are always recorded
# pers comm. Jen Morse they usually sample til snowfree in the spring
# but might miss the first snow in the fall
# for simplicity here I made all weeks inbetween spring and fall have 0 snow
# but probably should leave as missing for a few weeks before the start of fall
snowfree=list()
for (i in 1:nrow(sof)){
  snowfree[[i]]=data.frame(expand.grid(year=sof$year[i], doy=seq(sof$max_spring[i], sof$min_fall[i], by=7),
                                       point_ID=unique(datafile$point_ID)))%>%
    dplyr::left_join(., datafile%>%dplyr::select(., point_ID, veg_class))%>%distinct()%>%
    dplyr::mutate(date=as.Date(paste(doy, year), format="%j %Y"))%>%
    dplyr::mutate(dec_date=lubridate::decimal_date(date))
}


#add the infilled 0 values to the measured snow data
datafile=snowfree%>%bind_rows()%>%
  dplyr::mutate(mean_depth=0)%>%
  bind_rows(., datafile)

#replot
datafile%>%dplyr::group_by(veg_class, date, year, doy)%>%
  dplyr::summarize(mean_depth=mean(mean_depth))%>%
  ggplot(data=., aes(x=doy, y=mean_depth,
                     group=year,
                     color=year))+geom_smooth()+
  geom_point()+facet_wrap(~veg_class)+
  viridis::scale_color_viridis()+use_theme()

datafile%>%dplyr::group_by(veg_class, date, year)%>%
  dplyr::summarize(mean_depth=mean(mean_depth))%>%
  ggplot(data=., aes(x=date, y=mean_depth,
                     group=veg_class,
                     color=veg_class))+geom_point()+geom_smooth()+
  facet_wrap(~veg_class)+use_theme()


#-------------------------------------------------------------------------------
# ANALYSIS OPTION 1 = Seasonal and/or regional Kendall test
#RKT
#conduct regional/seasonal kendal test,
#taking the median values for each veg-class/month as the unit
#test for interannual trend (could do average instead by changing the "rep" parameter in rkt)
datafile%<>%dplyr::mutate(month=lubridate::month(date))%>%
  tidyr::unite(., 'veg_class_month', c('veg_class', 'month'))%>%
  dplyr::mutate(veg_class_month_integer=as.numeric(as.factor(veg_class_month)))



rkt_veg_month=rkt(date=datafile$year, y=datafile$mean_depth,
                  block=datafile$veg_class_month_integer, correct=TRUE, rep='m')

rkt_veg_month%>%as.list()

#tau
rkt_veg_month$tau

#2 sided p value
rkt_veg_month$sl

#if interested, could also for instance separate by season (fall vs spring)
# and analyse separately
byseason=datafile %>% dplyr::group_by(season)%>%
  dplyr::do(., out=unlist(rkt(date=.$year, y=.$mean_depth,
                              block=.$veg_class_month_integer,
                              correct=TRUE, rep='m')))

#suggestive that fall snow is increasing, but not spring
#this may be because summer months were included in spring - could play around with it.
byseason$out[byseason$season=='spring'][[2]][['tau']]
byseason$out[byseason$season=='spring'][[2]][['sl']]

byseason$out[byseason$season=='fall'][[2]][['tau']]
byseason$out[byseason$season=='fall'][[2]][['sl']]
#-------------------------------------------------------------------------------
# ANALYSIS OPTION 2 = GAM(M)s

#user specific, based on ncores you decided to use above
ctrl <- list(nthreads=ncores)

#gamm approach
#first median by veg_class (this is done inside the rkt function, here, we do it first
#note you can run on the raw data, just doing this to speed up as the raw fits take a long.... time)
meds=datafile %>% 
  tidyr::separate(., veg_class_month, into=c('veg_class', 'month'))%>%
  dplyr::group_by(year, doy, date, dec_date, veg_class, month)%>%
  dplyr::summarize(med_depth=median(mean_depth, na.rm=TRUE))%>%
  dplyr::mutate(month=as.numeric(month))


#-------------------------------------------------------------------------------
# First option - analyze each veg_class separately
# with trend simple function of month (seasonal), year (trend)
#note must be sorted correctly for the acf to make sense
gamm_by_veg_class=  meds%>%dplyr::group_by(veg_class)%>%
  dplyr::arrange(., dec_date)%>%
  do(gamm=mgcv::gamm(med_depth ~s(year)+s(month,bs="cc",k=12),
                     knots = list(month = c(0.5, 12.5)),
                     data = ., control=ctrl))




#quick plots show you definitely need to deal with the temporal autocorrelation
for (i in 1:nrow(gamm_by_veg_class)){
  acf(resid(gamm_by_veg_class$gamm[[i]]$lme, type='normalized'), main=gamm_by_veg_class$veg_class[[i]])
}

#but here's more or less what the fits look like for one example
gratia::draw(gamm_by_veg_class$gamm[[1]]$gam)

##########add in temporal autocorrelation

# for anything seasonal, want to use cyclic cubic(CC) splines
#cyclic cubic smoothers are another smoother that penalizes the squared second derivative
#of the smooth across the function. In these, though, start and end of the smoother are
#constrained to match in value and first derivative.

#The cyclic smoother requires that the start and end points of the cyclic variable are specified, via the
#knots argument; the smoother will have the exact same value at the start and end.

#can read more about them by ??mgcv::smoooth.terms
gamm_by_veg_class_corCAR1=  meds%>%dplyr::group_by(veg_class)%>%
  dplyr::arrange(., dec_date)%>%
  do(gamm=mgcv::gamm(med_depth~s(year)+s(month,bs="cc",k=12),
                knots = list(month = c(0.5, 12.5)),
                correlation = nlme::corCAR1(form = ~ dec_date),
                     data = ., control=ctrl))

#can see from the plot this effectively deals with the autocorrelation
for (i in 1:nrow(gamm_by_veg_class_corCAR1)){
  acf(resid(gamm_by_veg_class_corCAR1$gamm[[i]]$lme, type='normalized'), main=gamm_by_veg_class_corCAR1$veg_class[[i]])
}


#cycle through and plot each one (first is plot of fit, 2nd shows places where first deriv !=0, i.e. significant trend)
#should probably save these to files as currently can't set the title without digging into cowplot
#not scales vary across figs
for (i in 1:nrow(gamm_by_veg_class_corCAR1)){
  gratia::draw(gamm_by_veg_class_corCAR1$gamm[[i]]$gam)%>%ggsave(., file=paste0(plot_dir, 'mod_spline_', gamm_by_veg_class_corCAR1$veg_class[[i]], '.jpg'), width=8)
  gratia::derivatives(gamm_by_veg_class_corCAR1$gamm[[i]]$gam)%>%gratia::draw()%>%
    ggsave(., file=paste0(plot_dir, '/mod_spline_deriv', gamm_by_veg_class_corCAR1$veg_class[[i]], '.jpg'), width=8)
}

#You can also combine the two above to highlight periods of increasing/decreasing change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/

#It is also possible to model interactions between the intraannual- and inter-annual
# trends, for example, are some months getting snowier/less snowy than others

#example code
#https://www.fromthebottomoftheheap.net/2015/11/21/climate-change-and-spline-interactions/

#this includes a significance test for the nested models
# note to use anova on the nested models, one should first fix 
# the phi parameter and rerun using ML instead of REML for fitting

#here run just for one veg_class as it's kind of slow
rocks =datafile %>% 
  tidyr::separate(., veg_class_month, into=c('veg_class', 'month'))%>%
  dplyr::group_by(year, doy, date, dec_date, veg_class, month)%>%
  dplyr::summarise(med_depth=median(mean_depth, na.rm=TRUE))%>%
  dplyr::mutate(month=as.numeric(month))%>%
  dplyr::filter(veg_class=='rock')


# first fit the tensor product model in order to estimate the temporal correlation structure
m = mgcv::gamm(med_depth~te(year, month,bs = c("cr","cc"),k=c(10, 12)),data=rocks,
               knots = list(month = c(0.5, 12.5)),method='REML',
               correlation = nlme::corCAR1(form = ~ dec_date), control=ctrl)

#extract autocorrelation param
x=unname (m$lme$modelStruct$corStruct)
phi=coef(x,unconstrained=FALSE)


#then fix the phi (using estimate from above), refit using ML rather than reml
#additive model
ma = mgcv::gamm(med_depth~s(year, bs = "cr", k = 10) + s(month, bs = "cc", k = 12),data=rocks,
                knots = list(month = c(0.5, 12.5)),method = "ML",
                correlation = nlme::corCAR1(value = phi, fixed = TRUE, form = ~ dec_date),control=ctrl)

#nested tensor interaction model
mi = mgcv::gamm(med_depth~s(year, bs = "cr", k = 10) + s(month, bs = "cc", k = 12)+
                  ti(year, month, bs = c("cr","cc"), k = c(10, 12)),data=rocks,
                knots = list(month = c(0.5, 12.5)),method = "ML",
                correlation = nlme::corCAR1(value = phi, fixed = TRUE, form = ~ dec_date),control=ctrl)

#are some months snow changes more than others?
#nope, more support for additive model
#than interactions
anova(mi$lme, ma$lme)


#-------------------------------------------------------------------------------
# ANALYSIS OPTION 2a = GAMMs with - multiple locations analyzed together
# NOTE some of these will not converge with this dataset
# Separate analysis may be more fruitful

#Useful background here
# https://www.fromthebottomoftheheap.net/2017/10/10/difference-splines-i/
#and here
#https://peerj.com/preprints/27320.pdf
#https://raw.githubusercontent.com/eric-pedersen/mixed-effect-gams/master/compiled_paper/supplemental_code.R

#or model comparison faq's here, most of which do not address
# these with the required gamm correlation structure
#https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/mgcv-FAQ.html

#-------------------------------------------------------------------------------
#Mixed model 1
#simplest model, common spline for month, year, random intercepts for veg class

meds=meds %>%dplyr::ungroup()%>%dplyr::mutate(veg_class=factor(veg_class))
#meds%<>% dplyr::mutate(veg_class=factor(veg_class))

#without temporal autocorrelation structure
modG <- mgcv::gam(med_depth ~ s(month,bs="cc",k=12, m=2)+
                    s(year, m=2) +
                    s(veg_class, k=8, bs="re"), data = meds, 
                  knots = list(month = c(0.5, 12.5)),
                  method='REML',control=ctrl)

summary(modG)
gratia::draw(modG)

#with temporal autocorrelation structure
modG.1 <- mgcv::gamm(med_depth ~ s(month,bs="cc",k=12, m=2)+
                     s(year, m=2) +
                     s(veg_class, k=8, bs="re"), data = meds, 
                   knots = list(month = c(0.5, 12.5)),
                   correlation = nlme::corCAR1(form = ~ dec_date|factor(veg_class)),
                   method='REML',control=ctrl)
summary(modG.1)            
                   
gratia::draw(modG.1$gam)

# setup prediction data
modG_pred <- with(meds,
                      expand.grid(date=seq (min(date), max(date), by=30),
                                  veg_class=levels(veg_class)))
modG_pred$year=lubridate::year(modG_pred$date)
modG_pred$month=lubridate::month(modG_pred$date)

# make the prediction, add this and a column of standard errors to the prediction
# data.frame. Predictions are on the log scale.
modG_pred <- cbind(modG_pred,
                       predict(modG, 
                               modG_pred, 
                               se.fit=TRUE, 
                               type="response"))

#this does not look amazing, some places
#clearly fluctuate more than others
(ggplot(data=meds, aes(x=date, y=med_depth, group=veg_class)) +
  facet_wrap(~veg_class) +
  geom_ribbon(aes(ymin=fit - 2*se.fit, ymax=fit + 2*se.fit, x=date),
              data=modG_pred , 
              alpha=0.3, 
              inherit.aes=FALSE) +
  geom_line(aes(y=fit), data=modG_pred ) +
  geom_point())%>%ggsave(., file=paste0(plot_dir, 'modG_pred.jpg', width=8))


######################
#A  single common smoother plus group-level smoothers that have the same wig-
#gliness (Model 2)
#factor smoother interaction or "fs" basis
# include xt in the factor smooths to make sure the intra annual ones are cyclic

modGS <- mgcv::gam(med_depth ~ s(month,bs="cc",k=12, m=2)+
                    s(year, m=2) +
                    s(month, veg_class, k=8, bs="fs", m=2, xt=list(bs="cc"))+
                    s(year, veg_class, bs="fs", m=2), data = meds, 
                  knots = list(month = c(0.5, 12.5)), method='REML',control=ctrl)





#adding in the autocorrelation - cannot solve
# modGS.1 <- mgcv::gamm(med_depth ~ s(month,bs="cc",k=12, m=2)+
#                       s(year, m=2) +
#                       s(month, veg_class, k=8, bs="fs", m=2, xt=list(bs="cc"))+
#                       s(year, veg_class, bs="fs", m=2),
#                     data = meds, 
#                     knots = list(month = c(0.5, 12.5)),
#                     correlation = nlme::corCAR1(form = ~ dec_date|factor(veg_class)),
#                     method='REML')


gratia::draw(modGS)

modGS_pred <- with(meds,
                  expand.grid(date=seq (min(date), max(date), by=30),
                              veg_class=levels(veg_class)))
modGS_pred$year=lubridate::year(modGS_pred$date)
modGS_pred$month=lubridate::month(modGS_pred$date)

modGS_pred <- cbind(modGS_pred,
                   predict(modGS, 
                           modGS_pred, 
                           se.fit=TRUE, 
                           type="response"))

(ggplot(data=meds, aes(x=date, y=med_depth, group=veg_class)) +
    facet_wrap(~veg_class) +
    geom_ribbon(aes(ymin=fit - 2*se.fit, ymax=fit + 2*se.fit, x=date),
                data=modGS_pred , 
                alpha=0.3, 
                inherit.aes=FALSE) +
    geom_line(aes(y=fit), data=modGS_pred ) +
    geom_point())%>%ggsave(., file=paste0(plot_dir, 'modGS_pred.jpg', width=8))


######################
#A single common smoother plus group-level smoothers with differing wiggliness
# (Model 3)
# here with the "by" syntax need to include a ranef for the intercept

#
modGI <- mgcv::gam(med_depth ~ s(month,bs="cc",k=12, m=2)+
                     s(year, m=2) +
                     s(month, by=veg_class, k=8, bs="cc", m=2)+
                     s(year, by=veg_class, m=2)+
                     s(veg_class, bs="re", m=2, k=8),#random intercepts
                   data = meds, 
                   knots = list(month = c(0.5, 12.5)),
                   method='REML',control=ctrl)
#singularity, won't fit
# modGI.1 <- mgcv::gamm(med_depth ~ s(month,bs="cc",k=12, m=2)+
#                      s(year, m=2) +
#                      s(month, by=veg_class, k=8, bs="cc", m=2)+
#                      s(year, by=veg_class, m=2)+
#                      s(veg_class, bs="re", m=2, k=8),#random intercepts
#                    data = meds, 
#                    knots = list(month = c(0.5, 12.5)),
#                    correlation = nlme::corCAR1(form = ~ dec_date|factor(veg_class)),
#                    method='REML')



gratia::draw(modGI)
modGI_pred <- with(meds,
                   expand.grid(date=seq (min(date), max(date), by=30),
                               veg_class=levels(veg_class)))
modGI_pred$year=lubridate::year(modGI_pred$date)
modGI_pred$month=lubridate::month(modGI_pred$date)

modGI_pred <- cbind(modGI_pred,
                    predict(modGI, 
                            modGI_pred, 
                            se.fit=TRUE, 
                            type="response"))

(ggplot(data=meds, aes(x=date, y=med_depth, group=veg_class)) +
    facet_wrap(~veg_class) +
    geom_ribbon(aes(ymin=fit - 2*se.fit, ymax=fit + 2*se.fit, x=date),
                data=modGI_pred , 
                alpha=0.3, 
                inherit.aes=FALSE) +
    geom_line(aes(y=fit), data=modGI_pred ) +
    geom_point())%>%ggsave(., file=paste0(plot_dir, 'modGI_pred.jpg', width=8))


######################
#No global smoother version of model 2 (factor smooths)
# (Model 4), shapes of individual smooths are unrelated
modS= mgcv::gam(med_depth ~ 
            s(month, veg_class, k=8, bs="fs", m=2, xt=list(bs="cc"))+
            s(year, veg_class, bs="fs", m=2),
          data = meds, 
          knots = list(month = c(0.5, 12.5)),
          correlation = nlme::corCAR1(form = ~ dec_date|factor(veg_class)),
          method='REML',control=ctrl)

#can't fit
# modS.1= mgcv::gamm(med_depth ~ 
#                   s(month, veg_class, k=8, bs="fs", m=2, xt=list(bs="cc"))+
#                   s(year, veg_class, bs="fs", m=2),
#                 data = meds, 
#                 knots = list(month = c(0.5, 12.5)),
#                 correlation = nlme::corCAR1(form = ~ dec_date|factor(veg_class)),
#                 method='REML',control=ctrl))


gratia::draw(modS)

modS_pred <- with(meds,
                   expand.grid(date=seq (min(date), max(date), by=30),
                               veg_class=levels(veg_class)))
modS_pred$year=lubridate::year(modS_pred$date)
modS_pred$month=lubridate::month(modS_pred$date)

modS_pred <- cbind(modI_pred,
                    predict(modS, 
                            modS_pred, 
                            se.fit=TRUE, 
                            type="response"))

(ggplot(data=meds, aes(x=date, y=med_depth, group=veg_class)) +
    facet_wrap(~veg_class) +
    geom_ribbon(aes(ymin=fit - 2*se.fit, ymax=fit + 2*se.fit, x=date),
                data=modS_pred , 
                alpha=0.3, 
                inherit.aes=FALSE) +
    geom_line(aes(y=fit), data=modI_pred ) +
    geom_point())%>%ggsave(., file=paste0(plot_dir, 'modS_pred.jpg', width=8))



####
# (Model 5)[the independent wiggliness model 3 without the first term]
#this is most akin to what Will W. started with with the chemistry
#here, we give knot placement for gthe cubic splines
# and use ranef rather than fixed effect for intercept
modI <- mgcv::gam(med_depth ~ 
                     s(month, by=veg_class, k=8, bs="cc", m=2)+
                     s(year, by=veg_class, m=2)+
                     s(veg_class, bs="re", m=2, k=8),#random intercepts
                   data = meds, 
                   knots = list(month = c(0.5, 12.5)),
                   method='REML')


monthparams=row.names(summary(modI)$s.table)%>%grep('month', ., value=TRUE)
yearparams=row.names(summary(modI)$s.table)%>%grep('year', ., value=TRUE)
(gratia::draw(modI, select = monthparams))%>%ggsave(file=paste0(plot_dir, 'month_effects.jpg'))
(gratia::draw(modI, select = yearparams))%>%ggsave(file=paste0(plot_dir, 'year_effects.jpg'))

#this one super ridiculously slow to run
modI.1 <- mgcv::gamm(med_depth ~ 
                    s(month, by=veg_class, k=8, bs="cc", m=2)+
                    s(year, by=veg_class, m=2)+
                    s(veg_class, bs="re", m=2, k=8),#random intercepts
                  data = meds, 
                  knots = list(month = c(0.5, 12.5)),
                  correlation = nlme::corCAR1(form = ~ dec_date|factor(veg_class)),
                  method='REML')



modI_pred <- with(meds,
                  expand.grid(date=seq (min(date), max(date), by=30),
                              veg_class=levels(veg_class)))
modI_pred$year=lubridate::year(modI_pred$date)
modI_pred$month=lubridate::month(modI_pred$date)

modI_pred <- cbind(modI_pred,
                   predict(modI, 
                           modI_pred, 
                           se.fit=TRUE, 
                           type="response"))

(ggplot(data=meds, aes(x=date, y=med_depth, group=veg_class)) +
    facet_wrap(~veg_class) +
    geom_ribbon(aes(ymin=fit - 2*se.fit, ymax=fit + 2*se.fit, x=date),
                data=modI_pred , 
                alpha=0.3, 
                inherit.aes=FALSE) +
    geom_line(aes(y=fit), data=modI_pred ) +
    geom_point())%>%ggsave(., file=paste0(plot_dir, 'modI_pred.jpg', width=8))


monthparams=row.names(summary(modI.1$gam)$s.table)%>%grep('month', ., value=TRUE)
yearparams=row.names(summary(modI.1$gam)$s.table)%>%grep('year', ., value=TRUE)

gratia::draw(modI.1$gam, select=monthparams)%>%ggsave(., file=paste0(plot_dir, 'month_splines.jpg', width=8))
gratia::draw(modI.1$gam,, select=yearparams)%>%ggsave(., file=paste0(plot_dir, 'year_splines.jpg'), width=8)



#compare model fits
#without the autocorrelation , GI is the best
AIC_table <- AIC(modG,modGS, modGI, modS, modI)%>%
  tibble::rownames_to_column(var= "Model")%>%
  mutate(deltaAIC = AIC - min(AIC))%>%
  mutate_at(.vars = vars(df,AIC, deltaAIC), 
            .funs = funs(round,.args = list(digits=0)))

#note gam.checks not run on all models, etc

#-------------------------------------------------------------------------------
#LAST OPTION - FIT SEPARATELY BY YEAR, EXTRACT DERIVED THINGS FROM FITS
rocks%<>%ungroup()%>%dplyr::mutate(year=factor(year))
# independent_by_year=mgcv::gamm(med_depth ~ s(month,by=year, bs="cc",k=12, m=2) +
#                 s(year,  bs="re", k = 26, m=2),
#               data = rocks, method = "REML", knots=list(month = c(0.5, 12.5)))


independent_by_year=mgcv::gamm(med_depth ~ s(doy,by=year, bs="cc",k=12, m=2) +
                s(year,  bs="re", k = 26, m=2),
              data = rocks, method = "REML", knots=list(doy = c(0.5, 365.5)))


#won't fit with corCAR1 but residuals look fine
acf(resid(independent_by_year$lme, type='normalized'))

doyparams=row.names(summary(independent_by_year$gam)$s.table)%>%grep('doy', ., value=TRUE)%>%grep('1993|1994|1995|1996|1997|1998', ., value=TRUE)

#monthparams=row.names(summary(independent_by_year$gam)$s.table)%>%grep('month', ., value=TRUE)%>%grep('1993|1994|1995|1996|1997|1998', ., value=TRUE)
gratia::draw(independent_by_year$gam, select=doyparams)%>%ggsave(., file=paste0(plot_dir, 'by_year.jpg'), width=8)

#find predictions for each year
independent_by_year_pred <- with(rocks,
                  expand.grid(date=seq (min(date), max(date), by=1)))
independent_by_year_pred$year=lubridate::year(independent_by_year_pred$date)
independent_by_year_pred$doy=lubridate::month(independent_by_year_pred$date)

independent_by_year_pred <- cbind(independent_by_year_pred,
                   predict(independent_by_year$gam, 
                           independent_by_year_pred, 
                           se.fit=TRUE, 
                           type="response"))

#make summary table of whatever you want, total snow_cm days, max depth, snowpack in may, etc
summary=independent_by_year_pred%>%
  dplyr::group_by(year)%>%
  dplyr::summarize(snow_cm_day=sum(fit),
  max_depth=max(fit))

summary
