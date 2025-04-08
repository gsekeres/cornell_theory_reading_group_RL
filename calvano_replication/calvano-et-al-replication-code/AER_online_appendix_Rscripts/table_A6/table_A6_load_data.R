# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A6
# Summary statistics with 3 agents
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

numPrices = 15
numGames = 1000

# Load results for benchmark model

numAgents = 3
numStates = numPrices^numAgents
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
     file = "det_1.Rdata")

# Load results for the improved model

numAgents = 3
numStates = numPrices^numAgents
det = fread(file = "A_det_2.txt", 
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

Converged = array(data = NA, dim = numGames)
TimeToConvergence = array(data = NA, dim = numGames)
CycleLength = array(data = NA, dim = numGames)
CycleStates = matrix(data = NA, nrow = numGames, ncol = 50)
CyclePrices = matrix(data = NA, nrow = numGames, ncol = numAgents*50)
CycleProfits = matrix(data = NA, nrow = numGames, ncol = numAgents*50)
Strategies = matrix(data = NA, nrow = numStates*numAgents, numGames)
strategy = matrix(data = NA, nrow = numStates, ncol = numAgents)
con = file("InfoExperiment_2.txt", open = "r")
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
     file = "det_2.Rdata")

