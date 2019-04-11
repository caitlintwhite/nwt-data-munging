# prep productivity

snowclass <- read.csv("~/Dropbox/NWT_data/NWT_SnowXProd.csv", stringsAsFactors = F) # from nwt renewal dropbox
# emily used class_3 for renewal
snowclass <- snowclass[c("sort_num","class_3")] #sort_num == saddle grid plot
snowclass <- unique(snowclass)
snowclass <- snowclass[order(snowclass$sort_num),]
# check only one class per plot
summary(duplicated(snowclass$sort_num)) # no dups
# write out for use in climate calculations script
write.csv(snowclass, "extended_summer/output_data/sdlprodsnowclass.csv", row.names = F, quote = F)


lakedat <- read.csv("http://niwot.colorado.edu/data_csvs/glakeice.nc.data.csv",
                    stringsAsFactors = F,
                    strip.white = TRUE,
                    na.strings = c("NaN", NA, "NA ", " ", ""))
