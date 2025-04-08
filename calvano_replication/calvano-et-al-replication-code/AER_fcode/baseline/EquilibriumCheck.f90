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
    SUBROUTINE computeEqCheck ( iExperiment )
    !
    ! Computes statistics for one model
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: iExperiment
    !
    ! Declaring local variable
    !
    INTEGER :: iSession, iAgent, iThres, i, j, iState, &
        OptimalStrategyVec(lengthStrategies), OptimalStrategy(numStates,numAgents), &
        CycleStatesSession(numPeriods), &
        CycleLengthSession, numCycleLength(0:numThresCycleLength), &
        ThresCycleLength(numThresCycleLength)
    INTEGER, DIMENSION(numAgents) :: flagBRAllSession, flagBROnPathSession, flagBROffPathSession
    INTEGER :: flagEQAllSession, flagEQOnPathSession, flagEQOffPathSession
    INTEGER, DIMENSION(numAgents,numSessions) :: flagBRAll, flagBROnPath, flagBROffPath
    INTEGER, DIMENSION(numSessions) :: flagEQAll, flagEQOnPath, flagEQOffPath
    !
    REAL(8) :: r_num
    REAL(8), DIMENSION(numAgents) :: freqBRAllSession, freqBROnPathSession, freqBROffPathSession
    REAL(8) :: freqEQAllSession, freqEQOnPathSession, freqEQOffPathSession
    REAL(8), DIMENSION(numAgents,numSessions) :: freqBRAll, freqBROnPath, freqBROffPath
    REAL(8), DIMENSION(numSessions) :: freqEQAll, freqEQOnPath, freqEQOffPath
    REAL(8), DIMENSION(0:numAgents,0:numThresCycleLength) :: AvgFreqBRAll, AvgFreqBROnPath, AvgFreqBROffPath
    REAL(8), DIMENSION(0:numThresCycleLength) :: AvgFreqEQAll, AvgFreqEQOnPath, AvgFreqEQOffPath
    REAL(8), DIMENSION(0:numAgents,0:numThresCycleLength) :: AvgFlagBRAll, AvgFlagBROnPath, AvgFlagBROffPath
    REAL(8), DIMENSION(0:numThresCycleLength) :: AvgFlagEQAll, AvgFlagEQOnPath, AvgFlagEQOffPath
    !
    LOGICAL :: cond(numSessions), matcond(numAgents,numSessions)
    !
    ! Beginning execution
    !
    PRINT*, 'Computing equilibrium checks'
    ThresCycleLength = (/ (i, i = 1, numThresCycleLength) /)
    !
    ! Initializing variables
    !
    !$ CALL OMP_SET_NUM_THREADS(numCores)
    !
    ! Reading strategies and states at convergence from file
    !
    CALL ReadInfoExperiment()
    !
    ! Beginning loop over sessions
    !
    !$omp parallel do &
    !$omp private(OptimalStrategy,OptimalStrategyVec,CycleLengthSession,CycleStatesSession, &
    !$omp   freqBRAllSession,freqBROnPathSession,freqBROffPathSession,freqEQAllSession,freqEQOnPathSession,freqEQOffPathSession, &
    !$omp   flagBRAllSession,flagBROnPathSession,flagBROffPathSession,flagEQAllSession,flagEQOnPathSession,flagEQOffPathSession)
    DO iSession = 1, numSessions                  ! Start of loop aver sessions
        !
        PRINT*, 'iSession = ', iSession
        !
        !$omp critical
        OptimalStrategyVec = indexStrategies(:,iSession)
        CycleLengthSession = CycleLength(iSession)
        CycleStatesSession(:CycleLengthSession) = CycleStates(:CycleLengthSession,iSession)
        !$omp end critical
        !
        OptimalStrategy = RESHAPE(OptimalStrategyVec, (/ numStates,numAgents /) )
        !
        CALL computeEqCheckSession(OptimalStrategy,CycleLengthSession,CycleStatesSession(:CycleLengthSession), &
            freqBRAllSession,freqBROnPathSession,freqBROffPathSession,freqEQAllSession,freqEQOnPathSession,freqEQOffPathSession, &
            flagBRAllSession,flagBROnPathSession,flagBROffPathSession,flagEQAllSession,flagEQOnPathSession,flagEQOffPathSession)
        !
        !$omp critical
        freqBRAll(:,iSession) = freqBRAllSession
        freqBROnPath(:,iSession) = freqBROnPathSession
        freqBROffPath(:,iSession) = freqBROffPathSession
        freqEQAll(iSession) = freqEQAllSession
        freqEQOnPath(iSession) = freqEQOnPathSession
        freqEQOffPath(iSession) = freqEQOffPathSession
        flagBRAll(:,iSession) = flagBRAllSession
        flagBROnPath(:,iSession) = flagBROnPathSession
        flagBROffPath(:,iSession) = flagBROffPathSession
        flagEQAll(iSession) = flagEQAllSession
        flagEQOnPath(iSession) = flagEQOnPathSession
        flagEQOffPath(iSession) = flagEQOffPathSession
        !$omp end critical
        !
    END DO                                  ! End of loop over sessions
    !$omp end parallel do
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Computing averages and descriptive statistics
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    AvgFreqBRAll = 0.d0
    AvgFreqBROnPath = 0.d0
    AvgFreqBROffPath = 0.d0
    AvgFreqEQAll = 0.d0
    AvgFreqEQOnPath = 0.d0
    AvgFreqEQOffPath = 0.d0
    AvgFlagBRAll = 0
    AvgFlagBROnPath = 0
    AvgFlagBROffPath = 0
    AvgFlagEQAll = 0
    AvgFlagEQOnPath = 0
    AvgFlagEQOffPath = 0
    !
    ! Total averages
    !
    numCycleLength(0) = numSessions
    !
    r_num = DBLE(numAgents*numCycleLength(0))
    AvgFreqBRAll(0,0) = SUM(freqBRAll)/r_num
    AvgFreqBROnPath(0,0) = SUM(freqBROnPath)/r_num
    AvgFreqBROffPath(0,0) = SUM(freqBROffPath)/r_num
    AvgFlagBRAll(0,0) = SUM(FlagBRAll)/r_num
    AvgFlagBROnPath(0,0) = SUM(FlagBROnPath)/r_num
    AvgFlagBROffPath(0,0) = SUM(FlagBROffPath)/r_num
    !
    r_num = DBLE(numCycleLength(0))
    AvgFreqEQAll(0) = SUM(freqEQAll)/r_num
    AvgFreqEQOnPath(0) = SUM(freqEQOnPath)/r_num
    AvgFreqEQOffPath(0) = SUM(freqEQOffPath)/r_num
    AvgFlagEQAll(0) = SUM(FlagEQAll)/r_num
    AvgFlagEQOnPath(0) = SUM(FlagEQOnPath)/r_num
    AvgFlagEQOffPath(0) = SUM(FlagEQOffPath)/r_num
    !
    DO iAgent = 1, numAgents
        !
        r_num = DBLE(numCycleLength(0))
        AvgFreqBRAll(iAgent,0) = SUM(freqBRAll(iAgent,:))/r_num
        AvgFreqBROnPath(iAgent,0) = SUM(freqBROnPath(iAgent,:))/r_num
        AvgFreqBROffPath(iAgent,0) = SUM(freqBROffPath(iAgent,:))/r_num
        AvgFlagBRAll(iAgent,0) = SUM(FlagBRAll(iAgent,:))/r_num
        AvgFlagBROnPath(iAgent,0) = SUM(FlagBROnPath(iAgent,:))/r_num
        AvgFlagBROffPath(iAgent,0) = SUM(FlagBROffPath(iAgent,:))/r_num
        !
    END DO
    !
    ! Averages by cycle length
    !
    DO iThres = 1, numThresCycleLength
        !
        IF (iThres .LT. numThresCycleLength) THEN
            !
            cond = (CycleLength .EQ. ThresCycleLength(iThres))
            numCycleLength(iThres) = COUNT(cond)
            !
            IF (numCycleLength(iThres) .GT. 0) THEN
                !
                r_num = DBLE(numAgents*numCycleLength(iThres))
                matcond = SPREAD(cond,DIM = 1,NCOPIES = numAgents)
                !
                AvgFreqBRAll(0,iThres) = SUM(freqBRAll,MASK = matcond)/r_num
                AvgFreqBROnPath(0,iThres) = SUM(freqBROnPath,MASK = matcond)/r_num
                AvgFreqBROffPath(0,iThres) = SUM(freqBROffPath,MASK = matcond)/r_num
                AvgFlagBRAll(0,iThres) = SUM(FlagBRAll,MASK = matcond)/r_num
                AvgFlagBROnPath(0,iThres) = SUM(FlagBROnPath,MASK = matcond)/r_num
                AvgFlagBROffPath(0,iThres) = SUM(FlagBROffPath,MASK = matcond)/r_num
                !
                r_num = DBLE(numCycleLength(iThres))
                AvgFreqEQAll(iThres) = SUM(freqEQAll,MASK = cond)/r_num
                AvgFreqEQOnPath(iThres) = SUM(freqEQOnPath,MASK = cond)/r_num
                AvgFreqEQOffPath(iThres) = SUM(freqEQOffPath,MASK = cond)/r_num
                AvgFlagEQAll(iThres) = SUM(FlagEQAll,MASK = cond)/r_num
                AvgFlagEQOnPath(iThres) = SUM(FlagEQOnPath,MASK = cond)/r_num
                AvgFlagEQOffPath(iThres) = SUM(FlagEQOffPath,MASK = cond)/r_num
                !
                DO iAgent = 1, numAgents
                    !
                    r_num = DBLE(numCycleLength(iThres))
                    AvgFreqBRAll(iAgent,iThres) = SUM(freqBRAll(iAgent,:),MASK = cond)/r_num
                    AvgFreqBROnPath(iAgent,iThres) = SUM(freqBROnPath(iAgent,:),MASK = cond)/r_num
                    AvgFreqBROffPath(iAgent,iThres) = SUM(freqBROffPath(iAgent,:),MASK = cond)/r_num
                    AvgFlagBRAll(iAgent,iThres) = SUM(FlagBRAll(iAgent,:),MASK = cond)/r_num
                    AvgFlagBROnPath(iAgent,iThres) = SUM(FlagBROnPath(iAgent,:),MASK = cond)/r_num
                    AvgFlagBROffPath(iAgent,iThres) = SUM(FlagBROffPath(iAgent,:),MASK = cond)/r_num
                    !
                END DO
                !
            END IF
            !
        ELSE
            !
            cond = (CycleLength .GE. ThresCycleLength(iThres))
            numCycleLength(iThres) = COUNT(cond)
            !
            IF (numCycleLength(iThres) .GT. 0) THEN
                !
                r_num = DBLE(numAgents*numCycleLength(iThres))
                matcond = SPREAD(cond,DIM = 1,NCOPIES = numAgents)
                !
                AvgFreqBRAll(0,iThres) = SUM(freqBRAll,MASK = matcond)/r_num
                AvgFreqBROnPath(0,iThres) = SUM(freqBROnPath,MASK = matcond)/r_num
                AvgFreqBROffPath(0,iThres) = SUM(freqBROffPath,MASK = matcond)/r_num
                AvgFlagBRAll(0,iThres) = SUM(FlagBRAll,MASK = matcond)/r_num
                AvgFlagBROnPath(0,iThres) = SUM(FlagBROnPath,MASK = matcond)/r_num
                AvgFlagBROffPath(0,iThres) = SUM(FlagBROffPath,MASK = matcond)/r_num
                !
                r_num = DBLE(numCycleLength(iThres))
                AvgFreqEQAll(iThres) = SUM(freqEQAll,MASK = cond)/r_num
                AvgFreqEQOnPath(iThres) = SUM(freqEQOnPath,MASK = cond)/r_num
                AvgFreqEQOffPath(iThres) = SUM(freqEQOffPath,MASK = cond)/r_num
                AvgFlagEQAll(iThres) = SUM(FlagEQAll,MASK = cond)/r_num
                AvgFlagEQOnPath(iThres) = SUM(FlagEQOnPath,MASK = cond)/r_num
                AvgFlagEQOffPath(iThres) = SUM(FlagEQOffPath,MASK = cond)/r_num
                !
                DO iAgent = 1, numAgents
                    !
                    r_num = DBLE(numCycleLength(iThres))
                    AvgFreqBRAll(iAgent,iThres) = SUM(freqBRAll(iAgent,:),MASK = cond)/r_num
                    AvgFreqBROnPath(iAgent,iThres) = SUM(freqBROnPath(iAgent,:),MASK = cond)/r_num
                    AvgFreqBROffPath(iAgent,iThres) = SUM(freqBROffPath(iAgent,:),MASK = cond)/r_num
                    AvgFlagBRAll(iAgent,iThres) = SUM(FlagBRAll(iAgent,:),MASK = cond)/r_num
                    AvgFlagBROnPath(iAgent,iThres) = SUM(FlagBROnPath(iAgent,:),MASK = cond)/r_num
                    AvgFlagBROffPath(iAgent,iThres) = SUM(FlagBROffPath(iAgent,:),MASK = cond)/r_num
                    !
                END DO
                !
            END IF
            !
        END IF
        !
    END DO
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Printing averages 
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    IF (iExperiment .EQ. 1) THEN
        !
        WRITE(10004,1) &
            (i, i = 1, numAgents), &
            (i, i = 1, numExplorationParameters), &
            (i, (j, i, j = 1, numAgents), i = 1, numAgents), &
            (i, i = 1, numDemandParameters), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents),  &
            (i, i = 1, numAgents), (i, i = 1, numAgents),  &
            ((i, j, j = 1, numPrices), i = 1, numAgents), &
            (iThres, iThres = 0, numThresCycleLength), &
            ((iThres, i = 1, 12), iThres = 0, numThresCycleLength), &
            (((iAgent, iThres, i = 1, 6), iThres = 0, numThresCycleLength), &
                    iAgent = 1, numAgents)
1       FORMAT('Experiment ', &
            <numAgents>('    alpha', I1, ' '), &
            <numExplorationParameters>('     beta', I1, ' '), '     delta ', &
            <numAgents>('typeQini', I1, ' ', <numAgents>('par', I1, 'Qini', I1, ' ')), &
            <numDemandParameters>('  DemPar', I0.2, ' '), &
            <numAgents>('NashPrice', I1, ' '), <numAgents>('CoopPrice', I1, ' '), &
            <numAgents>('NashProft', I1, ' '), <numAgents>('CoopProft', I1, ' '), &
            <numAgents>('NashMktSh', I1, ' '), <numAgents>('CoopMktSh', I1, ' '), &
            <numAgents>(<numPrices>('Ag', I1, 'Price', I2.2, ' ')), &
            <numThresCycleLength+1>('num_Len', I2.2, 1X), &
            <numThresCycleLength+1>('FlagEQAll_Len', I2.2, 1X, &
                                    'FlagEQOnPath_Len', I2.2, 1X, &
                                    'FlagEQOffPath_Len', I2.2, 1X, &
                                    'FreqEQAll_Len', I2.2, 1X, &
                                    'FreqEQOnPath_Len', I2.2, 1X, &
                                    'FreqEQOffPath_Len', I2.2, 1X, &
                                    'FlagBRAll_Ag0_Len', I2.2, 1X, &
                                    'FlagBROnPath_Ag0_Len', I2.2, 1X, &
                                    'FlagBROffPath_Ag0_Len', I2.2, 1X, &
                                    'FreqBRAll_Ag0_Len', I2.2, 1X, &
                                    'FreqBROnPath_Ag0_Len', I2.2, 1X, &
                                    'FreqBROffPath_Ag0_Len', I2.2, 1X), &
            <numAgents>( &
                <numThresCycleLength+1>('FlagBRAll_Ag', I1.1, '_Len', I2.2, 1X, &
                                        'FlagBROnPath_Ag', I1.1, '_Len', I2.2, 1X, &
                                        'FlagBROffPath_Ag', I1.1, '_Len', I2.2, 1X, &
                                        'FreqBRAll_Ag', I1.1, '_Len', I2.2, 1X, &
                                        'FreqBROnPath_Ag', I1.1, '_Len', I2.2, 1X, &
                                        'FreqBROffPath_Ag', I1.1, '_Len', I2.2, 1X)) &
            )
        !
    END IF
    !
    WRITE(10004,2) codExperiment, &
        alpha, MExpl, delta, &
        (typeQInitialization(i), parQInitialization(i, :), i = 1, numAgents), &
        DemandParameters, &
        NashPrices, CoopPrices, NashProfits, CoopProfits, NashMarketShares, CoopMarketShares, &
        (PricesGrids(:,i), i = 1, numAgents), &
        (numCycleLength(iThres), iThres = 0, numThresCycleLength), &
        (AvgFlagEQAll(iThres), AvgFlagEQOnPath(iThres), AvgFlagEQOffPath(iThres), &
          AvgFreqEQAll(iThres), AvgFreqEQOnPath(iThres), AvgFreqEQOffPath(iThres), &
          AvgFlagBRAll(0,iThres), AvgFlagBROnPath(0,iThres), AvgFlagBROffPath(0,iThres), &
          AvgFreqBRAll(0,iThres), AvgFreqBROnPath(0,iThres), AvgFreqBROffPath(0,iThres), &
            iThres = 0, numThresCycleLength), &
        ((AvgFlagBRAll(iAgent,iThres), AvgFlagBROnPath(iAgent,iThres), AvgFlagBROffPath(iAgent,iThres), &
          AvgFreqBRAll(iAgent,iThres), AvgFreqBROnPath(iAgent,iThres), AvgFreqBROffPath(iAgent,iThres), &
            iThres = 0, numThresCycleLength), &
                iAgent = 1, numAgents)
2   FORMAT(I10, 1X, &
        <numAgents>(F10.5, 1X), <numExplorationParameters>(F10.5, 1X), F10.5, 1X, &
        <numAgents>(A9, 1X, <numAgents>(F9.2, 1X)), &
        <numDemandParameters>(F10.5, 1X), &
        <6*numAgents>(F10.5, 1X), &
        <numPrices*numAgents>(F10.5, 1X), &
        <numThresCycleLength+1>(I9, 1X), &
        <numThresCycleLength+1>(<2>(F15.7, 1X, F18.7, 1X, F19.7, 1X), <2>(F19.7, 1X, F22.7, 1X, F23.7, 1X)), &
        <numAgents>(<numThresCycleLength+1>(<2>(F19.7, 1X, F22.7, 1X, F23.7, 1X))) &
        )
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computeEqCheck
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
