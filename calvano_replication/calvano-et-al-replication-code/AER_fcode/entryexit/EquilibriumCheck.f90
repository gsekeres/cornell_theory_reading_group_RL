MODULE EquilibriumCheck
!
USE globals
USE generic_routines
USE QL_routines
!
! Computes check for best response and equilibrium in all states and for all agents
!
IMPLICIT NONE
!
CONTAINS
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computeEqCheckSession ( OptimalStrategy, CycleLengthSession, CycleStatesSession, &
        freqBRAll, freqBROnPath, freqBROffPath, freqEQAll, freqEQOnPath, freqEQOffPath, &
        flagBRAll, flagBROnPath, flagBROffPath, flagEQAll, flagEQOnPath, flagEQOffPath )
    !
    ! Computes equilibrium check for an individual replication
    !
    ! INPUT:
    !
    ! - OptimalStrategy     : strategy for all agents
    ! - CycleLengthSession     : length of the replication's path (i.e., state cycle)
    ! - CycleStatesSession     : replication's path (i.e., state cycle)
    !
    ! OUTPUT:
    !
    ! - freqBRAll           : % of all states in which at least one agent is best responding
    ! - freqBROnPath        : % of on path states in which at least one agent is best responding
    ! - freqBROffPath       : % of off path states in which at least one agent is best responding
    ! - freqEQAll           : % of all states in which at all agents are best responding
    ! - freqEQOnPath        : % of on path states in which at all agents are best responding
    ! - freqEQOffPath       : % of off path states in which at all agents are best responding
    ! - flagBRAll           : = 1: in all states at least one agent is best responding
    ! - flagBROnPath        : = 1: in all on path states at least one agent is best responding
    ! - flagBROffPath       : = 1: in all off path states at least one agent is best responding
    ! - flagEQAll           : = 1: in all states both agents are best responding
    ! - flagEQOnPath        : = 1: in all on path states both agents are best responding
    ! - flagEQOffPath       : = 1: in all off path states both agents are best responding
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: OptimalStrategy(numStates,numAgents), CycleLengthSession, CycleStatesSession(CycleLengthSession)
    REAL(8), DIMENSION(numAgents), INTENT(OUT) :: freqBRAll, freqBROnPath, freqBROffPath
    REAL(8), INTENT(OUT) :: freqEQAll, freqEQOnPath, freqEQOffPath
    INTEGER, DIMENSION(numAgents), INTENT(OUT) :: flagBRAll, flagBROnPath, flagBROffPath
    INTEGER, INTENT(OUT) :: flagEQAll, flagEQOnPath, flagEQOffPath
    !
    ! Declaring local variables
    !
    INTEGER :: IsBestReply(numStates,numAgents), iAgent, iState, CycleStates(numStates), &
        pPrime(numAgents), StrategyPrice, iPrice, VisitedStates(numPeriods), &
        PreCycleLength, CycleLength, iPeriod, numImprovedPrices, ImprovedPrices(numStates), &
        numStatesBRAll(numAgents), numStatesBROnPath(numAgents), numStatesBROffPath(numAgents), &
        numStatesEQAll, numStatesEQOnPath, numStatesEQOffPath
    REAL(8) :: StateValueFunction(numPrices), MaxStateValueFunction, TestDiff
    !
    ! Beginning execution
    !
    ! 1. For each agent A and each state S, check whether A is best responding in state S
    !
    IsBestReply = 0
    DO iState = 1, numStates            ! Start of loop over states
        !
        ! Compute state value function for OptimalStrategy in iState, for all prices and agents
        !
        DO iAgent = 1, numAgents            ! Start of loop over agents
            !
            StateValueFunction = 0.d0
            DO iPrice = 1, numPrices            ! Start of loop over prices to compute a row of Q
                !
                CALL computeQCell(OptimalStrategy,iState,iPrice,iAgent,delta, &
                    StateValueFunction(iPrice),VisitedStates,PreCycleLength,CycleLength)
                !
            END DO                              ! End of loop over prices
            !
            MaxStateValueFunction = MAXVAL(StateValueFunction)
            StrategyPrice = OptimalStrategy(iState,iAgent)
            numImprovedPrices = 0
            ImprovedPrices = 0
            DO iPrice = 1, numPrices            ! Start of loop over prices to find optimal price(s)
                !
                TestDiff = ABS(StateValueFunction(iPrice)-MaxStateValueFunction)
                IF (TestDiff .LE. 0.d0) THEN
                    !
                    numImprovedPrices = numImprovedPrices+1                        
                    ImprovedPrices(numImprovedPrices) = iPrice
                    !
                END IF
                !
            END DO                              ! End of loop over prices
            IF (ANY(ImprovedPrices(:numImprovedPrices) .EQ. StrategyPrice)) IsBestReply(iState,iAgent) = 1
            !
        END DO                              ! End of loop over agents
        !
    END DO                              ! End of loop over states
    !
    ! 2. For each agent A, compute:
    ! - 1) the TOTAL number of states in which A is best responding (INTEGER)
    ! - 2) the number of states ON PATH in which A is best responding (INTEGER)
    ! - 3) the number of states OFF PATH in which A is best responding (INTEGER)
    ! - 4) whether A is best responding on all states (INTEGER 0/1)
    ! - 5) whether A is best responding on all states ON PATH (INTEGER 0/1)
    ! - 6) whether A is best responding on all states OFF PATH (INTEGER 0/1)
    !
    numStatesBRAll = 0
    numStatesBROnPath = 0
    numStatesBROffPath = 0
    flagBRAll = 0
    flagBROnPath = 0
    flagBROffPath = 0
    DO iAgent = 1, numAgents
        !
        numStatesBRAll(iAgent) = SUM(IsBestReply(:,iAgent))
        numStatesBROnPath(iAgent) = SUM(IsBestReply(CycleStatesSession,iAgent))
        numStatesBROffPath(iAgent) = numStatesBRAll(iAgent)-numStatesBROnPath(iAgent)
        IF (numStatesBRAll(iAgent) .EQ. numStates) flagBRAll(iAgent) = 1
        IF (numStatesBROnPath(iAgent) .EQ. CycleLengthSession) flagBROnPath(iAgent) = 1
        IF (numStatesBROffPath(iAgent) .EQ. (numStates-CycleLengthSession)) flagBROffPath(iAgent) = 1
        !
    END DO
    !
    ! 3. Simultaneously for all agents, compute:
    ! - 1) the TOTAL number of states in which the agents are best responding (INTEGER)
    ! - 2) the number of states ON PATH in which the agents are best responding (INTEGER)
    ! - 3) the number of states OFF PATH in which the agents are best responding (INTEGER)
    ! - 4) whether the agents are best responding on all states (INTEGER 0/1)
    ! - 5) whether the agents are best responding on all states ON PATH (INTEGER 0/1)
    ! - 6) whether the agents are best responding on all states OFF PATH (INTEGER 0/1)
    !
    numStatesEQAll = 0
    numStatesEQOnPath = 0
    numStatesEQOffPath = 0
    DO iState = 1, numStates
        !
        IF (ALL(IsBestReply(iState,:) .EQ. 1)) THEN
            !
            numStatesEQAll = numStatesEQAll+1
            !
            IF (ANY(CycleStatesSession .EQ. iState)) THEN
                !
                numStatesEQOnPath = numStatesEQOnPath+1
                !
            ELSE
                !
                numStatesEQOffPath = numStatesEQOffPath+1
                !
            END IF
            !
        END IF
        !
    END DO
    flagEQAll = 0
    flagEQOnPath = 0
    flagEQOffPath = 0
    IF (numStatesEQAll .EQ. numStates) flagEQAll = 1
    IF (numStatesEQOnPath .EQ. CycleLengthSession) flagEQOnPath = 1
    IF (numStatesEQOffPath .EQ. (numStates-CycleLengthSession)) flagEQOffPath = 1
    !
    ! 4. Convert number of states into frequencies
    !
    freqBRAll = DBLE(numStatesBRAll)/DBLE(numStates)
    freqBROnPath = DBLE(numStatesBROnPath)/DBLE(CycleLengthSession)
    freqBROffPath = DBLE(numStatesBROffPath)/DBLE(numStates-CycleLengthSession)
    freqEQAll = DBLE(numStatesEQAll)/DBLE(numStates)
    freqEQOnPath = DBLE(numStatesEQOnPath)/DBLE(CycleLengthSession)
    freqEQOffPath = DBLE(numStatesEQOffPath)/DBLE(numStates-CycleLengthSession)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computeEqCheckSession
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
END MODULE EquilibriumCheck
