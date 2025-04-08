# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Table A13
# Summary statistics 1- and 2- memory models
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
numAgents = 2
numGames = 1000
depthVec = c(1, 2)

# Load results 

numModels = 2
for (iModel in 1:numModels) {
    
    numStates = numPrices^(numAgents*depthVec[iModel])
    filename = paste("A_det_", iModel, ".txt", sep = "")
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

    # InfoExperiment file: first pass
    
    Converged = array(data = NA, dim = numGames)
    TimeToConvergence = array(data = NA, dim = numGames)
    CycleLength = array(data = NA, dim = numGames)
    filename = paste("InfoExperiment_", iModel, ".txt", sep = "")
    con = file(filename, open = "r")
    for (iGame in 1:numGames) {
        if ((iGame %% 100) == 0) cat("iGame = ", iGame, "\n")
        q = readLines(con, n = 1) # Skip iGame line    
        Converged[iGame] = as.numeric(readLines(con, n = 1))    
        TimeToConvergence[iGame] = as.numeric(readLines(con, n = 1))
        CycleLength[iGame] = as.numeric(readLines(con, n = 1))
        q = readLines(con, n = 3+numStates+1) # Skip remaining lines
    }
    close(con)
    maxCycleLength = max(CycleLength)
    
    # InfoExperiment file: second pass
    
    CycleStates = matrix(data = NA, nrow = numGames, ncol = maxCycleLength)
    CyclePrices = matrix(data = NA, nrow = numGames, ncol = numAgents*maxCycleLength)
    CycleProfits = matrix(data = NA, nrow = numGames, ncol = numAgents*maxCycleLength)
    Strategies = matrix(data = NA, nrow = numStates*numAgents, numGames)
    strategy = matrix(data = NA, nrow = numStates, ncol = numAgents)
    
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
    
    # Save data in Rdata format
    
    filename = paste("det_", iModel, ".Rdata", sep = "")
    save(det, 
         Strategies, Converged, TimeToConvergence,
         CycleLength, CycleStates, CyclePrices, CycleProfits, maxCycleLength, 
         file = filename)
    
}

