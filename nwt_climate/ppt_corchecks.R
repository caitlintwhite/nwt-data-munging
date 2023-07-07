rawpptcor <- dplyr::select(compare, date, local_site, measurement) %>%
  spread(local_site, measurement)

library(corrplot)
corrplot(cor(rawpptcor[,-1], use = "pairwise.complete.obs"), type = "upper")
corrplot(cor(rawpptcor[,!grepl("date|USC", names(rawpptcor))], use = "pairwise.complete.obs"), type = "upper")

corrplot(cor(rawpptcor[month(rawpptcor$date) %in% 6:9,!grepl("date|USC", names(rawpptcor))], use = "pairwise.complete.obs"), type = "upper")

corrplot(cor(rawpptcor[!month(rawpptcor$date) %in% 6:9,!grepl("date|USC", names(rawpptcor))], use = "pairwise.complete.obs"), type = "upper")

qcpptcor <- mutate(chartppt_out_qc2, qcppt = ifelse(qdays != 1, NA, measurement)) %>%
  dplyr::select(date, local_site, qcppt) %>%
  spread(local_site, qcppt)

corrplot(cor(qcpptcor[,-1], use = "pairwise.complete.obs"), type = "upper", addCoef.col = "black")
corrplot(cor(qcpptcor[month(qcpptcor$date) %in% 6:9,-1], use = "pairwise.complete.obs"), type = "upper", addCoef.col = "black")
corrplot(cor(qcpptcor[!month(qcpptcor$date) %in% 6:9,-1], use = "pairwise.complete.obs"), type = "upper", addCoef.col = "black")

pptcor <- mutate(chartppt_out_compare, measurement = ifelse(qdays != 1, NA, measurement)) %>%
  dplyr::select(date, local_site, measurement) %>%
  spread(local_site, measurement)

corrplot(cor(pptcor[,-1], use = "pairwise.complete.obs"), type = "upper", addCoef.col = "black")
corrplot(cor(pptcor[month(pptcor$date) %in% 6:9,-1], use = "pairwise.complete.obs"), type = "upper", addCoef.col = "black")
corrplot(cor(pptcor[!month(pptcor$date) %in% 6:9,-1], use = "pairwise.complete.obs"), type = "upper", addCoef.col = "black")
corrp

cor.test(~sdl + d1, data = pptcor)
cor.test(~sdl + d1, data = qcpptcor)
cor.test(~sdl + d1, data = subset(pptcor, month(date) %in% 6:9))
cor.test(~sdl + d1, data = subset(qcpptcor, month(date) %in% 6:9))
cor.test(~sdl + d1, data = subset(pptcor, !month(date) %in% 6:9))
cor.test(~sdl + d1, data = subset(qcpptcor, !month(date) %in% 6:9))
