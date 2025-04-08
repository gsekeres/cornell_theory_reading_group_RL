# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table 1
# Summary statistics in the benchmark model
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# Clear memory

rm(list = ls())
gc()

# Set working directory and load packages

if (!require(rstudioapi)) {
    install.packages("rstudioapi")
    library(rstudioapi)
}

current.path = getActiveDocumentContext()$path 
setwd(dirname(current.path))

if (!require(data.table)) {
    install.packages("data.table")
    library(data.table)
}

# Loads data in txt format

# det file

det = fread(file = "A_det_1.txt", 
            header = TRUE,
            verbose = TRUE)
det = det[order(det$Session, 
                det$DevToPrice, 
                det$ShockAgent, 
                det$ObsAgent, 
                det$PreShockNumInCycle), ]

det$devAg = (det$ShockAgent == det$ObsAgent)
det$nondevAg = !det$devAg
det$PreShockPriceDevAg = ifelse(det$ShockAgent == 1, det$PreShockPrice1, det$PreShockPrice2)
det$PreShockPriceNonDevAg = ifelse(det$ShockAgent == 1, det$PreShockPrice2, det$PreShockPrice1)

# InfoExperiment file

numPrices = 15
numStates = numPrices^2
numAgents = 2
numGames = 1000

Converged = array(data = NA, dim = numGames)
TimeToConvergence = array(data = NA, dim = numGames)
CycleLength = array(data = NA, dim = numGames)
CycleStates = matrix(data = NA, nrow = numGames, ncol = 50)
CyclePrices = matrix(data = NA, nrow = numGames, ncol = numAgents*50)
CycleProfits = matrix(data = NA, nrow = numGames, ncol = numAgents*50)
Strategies = matrix(data = NA, nrow = numStates*numAgents, numGames)
strategy = matrix(data = NA, nrow = numStates, ncol = numAgents)
con = file("InfoExperiment_1.txt", open = "r")
for (iGame in 1:numGames) {
    if ((iGame %% 100) == 0) cat("iGame = ", iGame, "\n")
    q = readLines(con, n = 1) # Skip iGame line    
    Converged[iGame] = as.numeric(readLines(con, n = 1))    
    TimeToConvergence[iGame] = as.numeric(readLines(con, n = 1))
    CycleLength[iGame] = as.numeric(readLines(con, n = 1))
    CycleStates[iGame, 1:CycleLength[iGame]] = as.numeric(strsplit(readLines(con, n = 1), " +")[[1]][-1])
    q = readLines(con, n = 1)
    if (substr(q, 1, 1) != " ") {
        qq = c(substr(q, 1, 2), strsplit(substr(q, 3, nchar(q)), "\\D+")[[1]][-1])
    } else {
        qq = strsplit(q, "\\D+")[[1]][-1]
    }
    CyclePrices[iGame, 1:(numAgents*CycleLength[iGame])] = as.numeric(qq)
    CycleProfits[iGame, 1:(numAgents*CycleLength[iGame])] = as.numeric(strsplit(readLines(con, n = 1), " +")[[1]][-1])
    for (iState in 1:numStates) {
        q = readLines(con, n = 1)
        if (substr(q, 1, 1) != " ") {
            qq = c(substr(q, 1, 2), strsplit(substr(q, 3, nchar(q)), " +")[[1]][-1])
        } else {
            qq = strsplit(q, "\\D+")[[1]][-1]
        }
        strategy[iState, ] = as.numeric(qq)
    }
    for (iAgent in 1:numAgents) {
        Strategies[((iAgent-1)*numStates+1):(iAgent*numStates), iGame] = strategy[, iAgent]
    }
    q = readLines(con, n = 1) # Skip empty line    
}
close(con)

# Drop unused columns

maxCycleLength = max(CycleLength)
CycleStates = CycleStates[, 1:maxCycleLength]
CyclePrices = CyclePrices[, 1:(numAgents*maxCycleLength)]
CycleProfits = CycleProfits[, 1:(numAgents*maxCycleLength)]

# Save data in Rdata format

save(det, 
     Strategies, Converged, TimeToConvergence,
     CycleLength, CycleStates, CyclePrices, CycleProfits, maxCycleLength, 
     file = "det.Rdata")

# Clear memory

rm(list = ls())
gc()

# Loads data in Rdata format (should be faster)

load("det.Rdata")
numGames = 1000

# Length of cycle frequencies

FreqCycle = table(CycleLength)
FreqCycle

# Merge three or more

FreqCycle = c(FreqCycle[1:2], sum(FreqCycle[3:length(FreqCycle)]))
names(FreqCycle) = c("1", "2", "3+")
FreqCycle
cumsum(FreqCycle)
F = length(FreqCycle)

#############################################
# Table 1: summary statistics by cycle length
#############################################

tab = matrix(data = NA, nrow = 8, ncol = 2+F+1+1)
rownames(tab) = c("Frequency", "Avg. Profit Gain", "S.D. Profit Gain", 
                  "% Equilibrium on Path", 
                  "Avg. Q Loss On Path", "S.D. Q Loss On Path", 
                  "Avg. Q Loss All States", "S.D. Q Loss All States") 
colnames(tab) = c("1-Symmetric", "1-Asymmetric", names(FreqCycle), 
                  "All", "Full Eq. on Path")

# Symmetric, 1-period cycle replications

CondPreCycle1 = (det$PreShockCycleLength == 1) & 
    (det$PreShockNumInCycle == 1) &
    (det$PreShockPrice1 == det$PreShockPrice2) &
    (det$DevToPrice == 1) &
    (det$ShockAgent == 1) & (det$ObsAgent == 1)
sum(CondPreCycle1)

tab[1, 1] = sum(CondPreCycle1)/numGames*100
tmp = (det$ProfitGain1+det$ProfitGain2)/2
tmp = tmp[CondPreCycle1]
tab[2, 1] = mean(tmp)
tab[3, 1] = sd(tmp)
tmp = det$flagEQOnPath
tmp = tmp[CondPreCycle1]
tab[4, 1] = mean(tmp)
tmp = det$QGapOnPath
tmp = tmp[CondPreCycle1]
tab[5, 1] = mean(tmp)
tab[6, 1] = sd(tmp)
tmp = det$QGapNotOnPath
tmp = tmp[CondPreCycle1]
tab[7, 1] = mean(tmp)
tab[8, 1] = sd(tmp)

# Asymmetric, 1-period cycle replications

CondPreCycle1 = (det$PreShockCycleLength == 1) & 
    (det$PreShockNumInCycle == 1) &
    (det$PreShockPrice1 != det$PreShockPrice2) &
    (det$DevToPrice == 1) &
    (det$ShockAgent == 1) & (det$ObsAgent == 1)
sum(CondPreCycle1)

tab[1, 2] = sum(CondPreCycle1)/numGames*100
tmp = (det$ProfitGain1+det$ProfitGain2)/2
tmp = tmp[CondPreCycle1]
tab[2, 2] = mean(tmp)
tab[3, 2] = sd(tmp)
tmp = det$flagEQOnPath
tmp = tmp[CondPreCycle1]
tab[4, 2] = mean(tmp)
tmp = det$QGapOnPath
tmp = tmp[CondPreCycle1]
tab[5, 2] = mean(tmp)
tab[6, 2] = sd(tmp)
tmp = det$QGapNotOnPath
tmp = tmp[CondPreCycle1]
tab[7, 2] = mean(tmp)
tab[8, 2] = sd(tmp)

# All cycles

for (j in 1:F) {
    if (j < 3) {
        CondPreCycle1 = (det$PreShockCycleLength == j) & 
            (det$PreShockNumInCycle == 1) &
            (det$DevToPrice == 1) &
            (det$ShockAgent == 1) & (det$ObsAgent == 1)
    } else if (j == 3) {
        CondPreCycle1 = (det$PreShockCycleLength >= 3) & 
            (det$PreShockNumInCycle == 1) &
            (det$DevToPrice == 1) &
            (det$ShockAgent == 1) & (det$ObsAgent == 1)
    }
    
    tab[1, j+2] = sum(CondPreCycle1)/numGames*100
    tmp = (det$ProfitGain1+det$ProfitGain2)/2
    tmp = tmp[CondPreCycle1]
    tab[2, j+2] = mean(tmp)
    tab[3, j+2] = sd(tmp)
    tmp = det$flagEQOnPath
    tmp = tmp[CondPreCycle1]
    tab[4, j+2] = mean(tmp)
    tmp = det$QGapOnPath
    tmp = tmp[CondPreCycle1]
    tab[5, j+2] = mean(tmp)
    tab[6, j+2] = sd(tmp)
    tmp = det$QGapNotOnPath
    tmp = tmp[CondPreCycle1]
    tab[7, j+2] = mean(tmp)
    tab[8, j+2] = sd(tmp)
}

# All sessions

CondPreCycle1 = (det$DevToPrice == 1) &
    (det$PreShockNumInCycle == 1) &
    (det$ShockAgent == 1) & (det$ObsAgent == 1)
sum(CondPreCycle1)

tab[1, 2+F+1] = sum(CondPreCycle1)/numGames*100
tmp = (det$ProfitGain1+det$ProfitGain2)/2
tmp = tmp[CondPreCycle1]
tab[2, 2+F+1] = mean(tmp)
tab[3, 2+F+1] = sd(tmp)
tmp = det$flagEQOnPath
tmp = tmp[CondPreCycle1]
tab[4, 2+F+1] = mean(tmp)
tmp = det$QGapOnPath
tmp = tmp[CondPreCycle1]
tab[5, 2+F+1] = mean(tmp)
tab[6, 2+F+1] = sd(tmp)
tmp = det$QGapNotOnPath
tmp = tmp[CondPreCycle1]
tab[7, 2+F+1] = mean(tmp)
tab[8, 2+F+1] = sd(tmp)

# All sessions with full equilibrium on path

CondPreCycle1 = (det$DevToPrice == 1) &
    (det$PreShockNumInCycle == 1) &
    (det$ShockAgent == 1) & (det$ObsAgent == 1) &
    (det$flagEQOnPath == 1)
sum(CondPreCycle1)

tab[1, 2+F+1+1] = sum(CondPreCycle1)/numGames*100
tmp = (det$ProfitGain1+det$ProfitGain2)/2
tmp = tmp[CondPreCycle1]
tab[2, 2+F+1+1] = mean(tmp)
tab[3, 2+F+1+1] = sd(tmp)
tmp = det$flagEQOnPath
tmp = tmp[CondPreCycle1]
tab[4, 2+F+1+1] = mean(tmp)
tmp = det$QGapOnPath
tmp = tmp[CondPreCycle1]
tab[5, 2+F+1+1] = mean(tmp)
tab[6, 2+F+1+1] = sd(tmp)
tmp = det$QGapNotOnPath
tmp = tmp[CondPreCycle1]
tab[7, 2+F+1+1] = mean(tmp)
tab[8, 2+F+1+1] = sd(tmp)

# Final table

tab = round(tab, digits = 3)
max.print = getOption('max.print')
options(max.print = nrow(tab)*ncol(tab))
sink('table_I.txt')
tab
sink()
options(max.print = max.print)
