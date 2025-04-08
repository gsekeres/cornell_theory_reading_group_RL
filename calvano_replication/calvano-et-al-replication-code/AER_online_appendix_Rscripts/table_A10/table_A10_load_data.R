# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A10
# Summary statistics with varying product substitutability
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

numAgents = 2
numPrices = 15
numStates = numPrices^numAgents
numGames = 1000

# Load results 

numModels = 12
for (iModel in 1:numModels) {
    
    filename = paste("A_det_", sprintf("%02d", iModel), ".txt", sep = "")
    det = fread(file = filename, header = TRUE, verbose = TRUE)
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
    
    filename = paste("InfoExperiment_", sprintf("%02d", iModel), ".txt", sep = "")
    con = file(filename, open = "r")
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
    
    filename = paste("det_", iModel, ".Rdata", sep = "")
    save(det, 
         Strategies, Converged, TimeToConvergence,
         CycleLength, CycleStates, CyclePrices, CycleProfits, maxCycleLength, 
         file = filename)
    
}

