
library(magrittr)
library(ggplot2)
options(stringsAsFactors = F)
ILexpression <- openxlsx::read.xlsx("ILexpression in pre- and 2-cycle treatment.xlsx")
ILexpression <- ILexpression[order(ILexpression$Exp),]
ILexpression <- ILexpression[!duplicated(ILexpression[,1:5]),]
ILexpS <- ILexpression %>% tidyr::spread(TimePoint,Exp,fill=0)
ILexpS$PFS_T <- as.numeric(gsub("\\+","",ILexpS$PFS))
ILexpS$ILDif <- (ILexpS$`3`-ILexpS$`1`)/(ILexpS$`1`+0.1)
ILexpS$ILDifW <- ifelse(ILexpS$ILDif > 0.5,1,ifelse(ILexpS$ILDif < -0.5,-1,0))
#ILexpS$ILDifW <- ifelse(ILexpS$ILDif > 0,1,ifelse(ILexpS$ILDif < 0,-1,0))

ILexpSM <- ILexpS %>% dplyr::select(-c(ILDif,`1`,`3`)) %>% tidyr::spread(ILs,ILDifW)

ILexpSM$ILscore <- apply(ILexpSM[,c("IL-2","IL-4","IL-6","IL-10")],1,sum)
ILexpSM$ImmAct <- ifelse(ILexpSM$ILscore > 0,"Activation","NoActivation")

