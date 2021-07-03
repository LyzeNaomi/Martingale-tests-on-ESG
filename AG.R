#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
system.time(
  EQ_indices <- openxlsx::read.xlsx('C:/Users/wamolyna/Documents/WORK/ESG/Data/2018M12_AIAG_ESF_Base.xlsx', colNames = TRUE,
                          cols = c(1:2,5:6, 11:12,17, 69))
)
attach(EQ_indices)

EQ_EUROSTOXX_50 <- subset(EQ_indices, select = c("Simulation" ,"Projection.year" , "ESG.Assets.EquityAssets.EUROSTOXX_50.TotalReturnIndex"))
EQ_SP_500 <- subset(EQ_indices, select = c("Simulation", "Projection.year" , "ESG.Assets.EquityAssets.SP_500.TotalReturnIndex"))
Euro_Deflator <- subset(EQ_indices, select = c("Simulation" , "Projection.year", "ESG.Economies.EUR.Deflator"))
US_Deflator <- subset(EQ_indices, select = c("Simulation" , "Projection.year", "ESG.Economies.USD.Deflator"))

dd <- subset(EQ_indices, select = c("Simulation" ,"Projection.year" , "ESG.Assets.EquityAssets.EUROSTOXX_50.TotalReturnIndex",
                                    "ESG.Assets.EquityAssets.SP_500.TotalReturnIndex","ESG.Assets.EquityAssets.REBE_Offices.TotalReturnIndex",
                                     "ESG.Assets.EquityAssets.REBE_Funds.TotalReturnIndex"))

subdata <- dd
n_cols_returns = ncol(subdata) ; disc <- c()
for (i in 3: n_cols_returns){
  disc[[i]] = subset(subdata, select=c(names(subdata[1]), names(subdata[2]), names(subdata[i])))
  disc[[i]] = Data_Cleaning(tidyr::spread(disc[[i]], names(subdata[2]), names(subdata[i])))
}

disc <- list.clean(disc)
names(disc) <- names(subdata)[3:n_cols_returns]
for (i in disc){
  t[i] = data.frame()
}

d<-dd %>% 
  gather(variable, value, -(Projection.year:Simulation)) %>%
  unite(temp, Simulation, variable) %>%
  spread(temp, value)

basic_stats <-c() ; Year<- colnames(EUR_disc_factors)
for (i in 1: 4){
  basic_stats[[i]] = as.data.frame(test0(EUR_disc_factors, disc[[i]]), col.names = c("mean", "quant_based_stdev","empe_quant"))
  basic_stats[[i]]  <- cbind(Year, basic_stats[[i]])  
  }
names(basic_stats) <- names(disc)

EQ_EUROSTOXX_50 <- spread(EQ_EUROSTOXX_50, 'Projection.year', ESG.Assets.EquityAssets.EUROSTOXX_50.TotalReturnIndex)
EQ_SP_500 <- spread(EQ_SP_500, 'Projection.year', 'ESG.Assets.EquityAssets.SP_500.TotalReturnIndex')
Euro_Deflator <- spread(Euro_Deflator, Projection.year, ESG.Economies.EUR.Deflator)
US_Deflator <- spread(US_Deflator, Projection.year, ESG.Economies.USD.Deflator)


dataset <-list(Euro_Deflator, EQ_EUROSTOXX_50, EQ_SP_500, US_Deflator)
datasets <- lapply(dataset, Data_Cleaning)
EUR_disc_factors <- datasets[[1]] ; EQ_EUROSTOXX_50 <- datasets[[2]] ; EQ_SP_500 <- datasets[[3]] ; USD_disc_factors <- datasets[[4]]

returns <- list(EQ_EUROSTOXX_50, EQ_SP_500)
cum_returns <- lapply(returns, Cum_Returns)
cr2 <- Cum_Returns(EQ_SP_500)
bs <- test0(discount_factors, cr2)

results <- lapply()
#Basic_Stats <- as.data.frame(test0(discount_rates, cum_returns), col.names = c("mean", "quant_based_stdev","empe_quant"))

Basic_Stats <- as.data.frame(test0(EUR_disc_factors, cum_returns[[1]]), col.names = c("mean", "quant_based_stdev","empe_quant")) #this will be changed by looping
Basic_Stats <- as.data.frame(test0(USD_disc_factors, cum_returns[[2]]), col.names = c("mean", "quant_based_stdev","empe_quant"))# sp_500 indices not very good--> seem not to pass the martingale test


#----------------------------------------------------------------------------------------------------------------------------------------------
REBE_Offices <- subset(EQ_indices, select = c("Simulation", "Projection.year" , "ESG.Assets.EquityAssets.REBE_Offices.TotalReturnIndex"))
REBE_Funds <- subset(EQ_indices, select = c("Simulation", "Projection.year" , "ESG.Assets.EquityAssets.REBE_Funds.TotalReturnIndex"))


REBE_Offices <- spread(REBE_Offices, Projection.year , ESG.Assets.EquityAssets.REBE_Offices.TotalReturnIndex)
REBE_Funds <- spread(REBE_Funds, Projection.year , ESG.Assets.EquityAssets.REBE_Funds.TotalReturnIndex)

REBE_Offices <- Data_Cleaning(REBE_Offices)
REBE_Funds <- Data_Cleaning(REBE_Funds)

real_estates <- list(REBE_Offices, REBE_Funds)
cum_returns_re <- lapply(real_estates, Cum_Returns)

Martingale_Test <- as.data.frame(testx(EUR_disc_factors, cum_returns_re[[1]], 15), col.names = c("mean", "quant_based_stdev","empe_quant")) #rebeoffices
Martingale_Test <- as.data.frame(test0(EUR_disc_factors, cum_returns_re[[2]]), col.names = c("mean", "quant_based_stdev","empe_quant")) #rebeofunds
#----------------------------------------------------------------------------------------------------------------------------------------------
# Plot 
Martingale_Test $Year<- rownames(Martingale_Test)
mart_test<- subset(Martingale_Test, select = c(mean, quant_based_stdev.1., quant_based_stdev.99., 
                                               empe_quant.1., empe_quant.99., Year))

test_data_long <- melt(mart_test , id="Year")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Year, y=value, colour=variable, group = 1)) +
  geom_point(aes(shape=variable))
#-----------------------------------------------------------------------------------------------------------------------------------------------
plot(Martingale_Test$quant_based_stdev.1., col='black', ylim=c(-80, 100))
lines(Martingale_Test$mean, type='b', col='red')
lines(Martingale_Test$quant_based_stdev.99., col='blue')
lines(Martingale_Test$empe_quant.1., col='green')
lines(Martingale_Test$empe_quant.99., col='purple')




test_data_long <- melt(Martingale_Test , id="Year") 
#---------------------------------------------------------------------------------------------------------------------------------------------

write.xlsx(EQ_EUROSTOXX_50, "C:/Users/wamolyna/Documents/eq_eurostoxx_50.xlsx")

write.xlsx(discount_factors, "C:/Users/wamolyna/Documents/discount_ag.xlsx")

write.csv2(EQ_indices, "C:/Users/wamolyna/Documents/EQ_indices.csv", row.names = FALSE)
library(readr)
EQ_indices <- read_delim("EQ_indices.csv", 
                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                     grouping_mark = "."), trim_ws = TRUE)

library(xlsx)
options(java.parameters = "-Xmx16000m")
myfile="EQ_indices.xlsx" #DO NOT USE SPACES, it will cause errors
wb = createWorkbook() #create a workbook object
mysheet = createSheet(wb, "Sheet1") #create a sheet and name it
addDataFrame(EQ_indices, sheet=mysheet, startColumn=1, row.names=FALSE) #add data to Sheet1
saveWorkbook(wb, myfile)

sdev<- apply(EUR_disc_factors*cum_returns[[1]], 2, sd)

#------------------------------------------------------------------------------------------------
renderPlots <- function(n, input, output, prefix="plot") {
  for (i in 1:4) {
    local({
      ii <- i  # need i evaluated here
      ## These would be your 10 plots instead
      output[[sprintf('%s_%g', prefix, ii)]] <- renderPlot({
        ggplot(data=basic_stats[[ii]],
               aes(x=Year, y=value, colour=variable, group = 1)) +
          geom_point(aes(shape=variable))
      })
    })
  }
}

d <- lapply(basic_stats, function (x) x[c('Year', 'mean','quant_based_stdev.1.')])

#-------------------------------------------------------------------------------------------------------------------------------------
path <- "WORK/ESG/Data/2019-01-14_Base_1000_Q4_2018.xlsx"
sheet_names <- excel_sheets(path = path)

LST <- c("Return_actions_1", "Return_actions_2","Return_actions_3")
# SheetList <- lapply(sheet_names[7:9], openxlsx::read.xlsx,xlsxFile = path)
# names(SheetList) <- sheet_names[7:9]
disc <- openxlsx::read.xlsx(path, sheet = 'CashAccount', colNames = TRUE) ; disc <- 1/disc
disc <- disc[-1,] ;
disc <- disc[,-1]
SheetList <- lapply(LST , openxlsx::read.xlsx, xlsxFile = path)
SheetList <- lapply(SheetList, function(x){x[-1,]}) ; SheetList <- lapply(SheetList, function(x){x[,-1]}) 

cum_returns<- Cum_Returns(net_returns) #gross returns and not cumulative returns
SheetList_mod <- lapply(SheetList, Cum_Returns)

names(SheetList) <- LST 

Martingale_test <- c() ; Period = 1:ncol(disc)
ncols = length(SheetList)
for (i in 1:(ncols)){
  Martingale_test[[i]] = as.data.frame(test0(disc, SheetList_mod[[i]]), col.names = c("Mean", "quant_based_stdev","empe_quant"))
  Martingale_test[[i]]  <- cbind(Period, Martingale_test[[i]])
}

names(Martingale_test) <- LST
for (i in 1:(ncols)){
  Martingale_test[[i]] = as.data.frame(test0(disc, SheetList_mod[[i]]), col.names = c("Mean", "quant_based_stdev","empe_quant"))
  Martingale_test[[i]]  <- cbind(Period, Martingale_test[[i]])
}