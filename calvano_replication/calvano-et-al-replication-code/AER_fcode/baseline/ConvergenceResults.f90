MODULE ConvergenceResults
!
USE globals
USE QL_routines
!
! Computes profit gains and frequency of states of strategies at convergence
!
IMPLICIT NONE
!
CONTAINS
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE ComputeConvResults ( iExperiment )
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
    INTEGER :: i, j, iSession, rSession, iPeriod, iState, iAgent, CycleLength
    INTEGER :: p(DepthState,numAgents), pPrime(numAgents)
    INTEGER :: OptimalStrategyVec(lengthStrategies), LastStateVec(LengthStates)
    INTEGER :: VisitedStates(numPeriods), OptimalStrategy(numStates,numAgents), &
        LastObservedPrices(DepthState,numAgents)
    INTEGER :: pHist(numPeriods,numAgents)
    REAL(8) :: Profits(numSessions,numAgents), VisitedProfits(numPeriods,numAgents), AvgProfits(numSessions)
    REAL(8), DIMENSION(numAgents) :: meanProfit, seProfit, meanProfitGain, seProfitGain
    REAL(8) :: meanAvgProfit, seAvgProfit, meanAvgProfitGain, seAvgProfitGain
    REAL(8) :: FreqStates(numSessions,numStates), meanFreqStates(numStates)
    !
    ! Beginning execution
    !
    PRINT*, 'Computing convergence results (average profits and frequency of prices)'
    !
    ! Initializing variables
    !
    Profits = 0.d0
    FreqStates = 0.d0
    !
    ! Reading strategies and states at convergence from file
    !
    OPEN(UNIT = 998,FILE = FileNameInfoExperiment,STATUS = "OLD")
    DO iSession = 1, numSessions
        !
        IF (MOD(iSession,100) .EQ. 0) PRINT*, 'Read ', iSession, ' strategies'
        READ(998,*) rSession
        READ(998,*) converged(rSession)
        READ(998,*) timeToConvergence(rSession)
        READ(998,*) indexLastState(:,rSession)
        DO iState = 1, numStates
            !
            READ(998,*) (indexStrategies((iAgent-1)*numStates+iState,rSession), iAgent = 1, numAgents)
            !
        END DO
        !
    END DO
    CLOSE(UNIT = 998)                   ! Close InfoExperiment file
    !
    OPEN(UNIT = 999,FILE = FileNameInfoExperiment,STATUS = "REPLACE")        ! Open InfoExperiment file
    !
    ! Beginning loop over sessions
    !
    DO iSession = 1, numSessions        ! Start of loop aver sessions
        !
        PRINT*, 'iSession = ', iSession
        !
        OptimalStrategyVec = indexStrategies(:,iSession)
        LastStateVec = indexLastState(:,iSession)
        !
        OptimalStrategy = RESHAPE(OptimalStrategyVec, (/ numStates,numAgents /))
        IF (DepthState0 .EQ. 0) THEN
            !
            LastObservedPrices = OptimalStrategy
            !
        ELSE IF (DepthState0 .GE. 1) THEN
            !
            LastObservedPrices = RESHAPE(LastStateVec, (/ DepthState,numAgents /))
            !
        END IF
        !
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ! Convergence analysis
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !
        VisitedStates = 0
        VisitedProfits = 0.d0
        pHist = 0
        p = LastObservedPrices
        pPrime = OptimalStrategy(computeStateNumber(p),:)
        DO iPeriod = 1, numPeriods
            !
            IF (DepthState .GT. 1) p(2:DepthState,:) = p(1:DepthState-1,:)
            p(1,:) = pPrime
            pHist(iPeriod,:) = pPrime
            VisitedStates(iPeriod) = computeStateNumber(p)
            DO iAgent = 1, numAgents
                !
                VisitedProfits(iPeriod,iAgent) = PI(computeActionNumber(pPrime),iAgent)
                !
            END DO
            !
            ! Check if the state has already been visited
            !
            IF ((iPeriod .GE. 2) .AND. (ANY(VisitedStates(:iPeriod-1) .EQ. VisitedStates(iPeriod)))) EXIT
            !
            ! Update pPrime and iterate
            !
            pPrime = OptimalStrategy(VisitedStates(iPeriod),:)
            !
        END DO
        !
        CycleLength = iPeriod-MINVAL(MINLOC((VisitedStates(:iPeriod-1)-VisitedStates(iPeriod))**2))
        Profits(iSession,:) = SUM(VisitedProfits(iPeriod-CycleLength+1:iPeriod,:),DIM = 1)/ &
                DBLE(CycleLength)
        FreqStates(iSession,VisitedStates(iPeriod-CycleLength+1:iPeriod)) = 1.d0/DBLE(CycleLength)
        !
        ! Computing and writing price cycles
        !
        pHist(:CycleLength,:) = pHist(iPeriod-CycleLength+1:iPeriod,:)
        pHist(CycleLength+1:,:) = 0.d0
        VisitedStates(:CycleLength) = VisitedStates(iPeriod-CycleLength+1:iPeriod)
        VisitedStates(CycleLength+1:) = 0
        VisitedProfits(:CycleLength,:) = VisitedProfits(iPeriod-CycleLength+1:iPeriod,:)
        VisitedProfits(CycleLength+1:,:) = 0.d0
        !
        ! Write session info to InfoExperiment file
        !
        WRITE(999,9961) iSession, &
            converged(iSession), &
            timeToConvergence(iSession), &
            CycleLength, &
            VisitedStates(:CycleLength), &
            (pHist(:CycleLength,iAgent), iAgent = 1, numAgents), &
            (VisitedProfits(:CycleLength,iAgent), iAgent = 1, numAgents), &
            (OptimalStrategy(iState,:), iState = 1, numStates)
9961    FORMAT(1X, I8, /, &
            1X, I1, /, &
            1X, F9.2, /, &
            1X, I8, /, &
            <CycleLength>(1X, I<LengthFormatStatesPrint>), /, &
            <numAgents>(<CycleLength>(1X, I<lengthFormatActionPrint>)), /, &
            <numAgents>(<CycleLength>(1X, F8.5)), /, &
            <numStates>(<numAgents>(1X, I<lengthFormatActionPrint>), /))
        !
    END DO        ! End of loop over sessions
    !
    CLOSE(UNIT = 999)                   ! Close InfoExperiment file
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Computing averages and descriptive statistics
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    ! Profits
    !
    DO iAgent = 1, numAgents
        !
        meanProfit(iAgent) = SUM(Profits(:,iAgent))/DBLE(numSessions)
        seProfit(iAgent) = SQRT(ABS((SUM(Profits(:,iAgent)**2)/DBLE(numSessions)-meanProfit(iAgent)**2)))
        !
    END DO
    AvgProfits = SUM(Profits,DIM = 2)/DBLE(numAgents)
    meanAvgProfit = SUM(AvgProfits)/DBLE(numSessions)
    seAvgProfit = SQRT(ABS((SUM(AvgProfits**2)/DBLE(numSessions)-meanAvgProfit**2)))
    meanProfitGain = (meanProfit-NashProfits)/(CoopProfits-NashProfits)
    seProfitGain = seProfit/(CoopProfits-NashProfits)
    meanNashProfit = SUM(NashProfits)/numAgents
    meanCoopProfit = SUM(CoopProfits)/numAgents
    meanAvgProfitGain = (meanAvgProfit-meanNashProfit)/(meanCoopProfit-meanNashProfit)
    seAvgProfitGain = seAvgProfit/(meanCoopProfit-meanNashProfit)
    !
    ! States
    !
    DO i = 1, numStates
        !
        meanFreqStates(i) = SUM(freqStates(:,i))/DBLE(numSessions)
        !
    END DO
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Printing averages and descriptive statistics
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    IF (iExperiment .EQ. 1) THEN
        !
        WRITE(100022,1) &
            (i, i = 1, numAgents), &
            (i, i = 1, numExplorationParameters), &
            (i, (j, i, j = 1, numAgents), i = 1, numAgents), &
            (i, i = 1, numDemandParameters), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents),  &
            (i, i = 1, numAgents), (i, i = 1, numAgents),  &
            ((i, j, j = 1, numPrices), i = 1, numAgents), &
            (i, i, i = 1, numAgents), (i, i, i = 1, numAgents), &
            (labelStates(j), j = 1, numStates)
1       FORMAT('Experiment ', &
            <numAgents>('    alpha', I1, ' '), &
            <numExplorationParameters>('     beta', I1, ' '), '     delta ', &
            <numAgents>('typeQini', I1, ' ', <numAgents>('par', I1, 'Qini', I1, ' ')), &
            <numDemandParameters>('  DemPar', I0.2, ' '), &
            <numAgents>('NashPrice', I1, ' '), <numAgents>('CoopPrice', I1, ' '), &
            <numAgents>('NashProft', I1, ' '), <numAgents>('CoopProft', I1, ' '), &
            <numAgents>('NashMktSh', I1, ' '), <numAgents>('CoopMktSh', I1, ' '), &
            <numAgents>(<numPrices>('Ag', I1, 'Price', I2.2, ' ')), &
            <numAgents>('  avgProf', I1, 1X, '   seProf', I1, 1X), '   avgProf     seProf ', &
            <numAgents>('avgPrGain', I1, 1X, ' sePrGain', I1, 1X), ' avgPrGain   sePrGain ', &
            <numStates>(A<MAX(10,3+LengthFormatStatesPrint)>, ' ') &
            )
        !
    END IF
    !
    WRITE(100022,2) codExperiment, &
        alpha, MExpl, delta, &
        (typeQInitialization(i), parQInitialization(i, :), i = 1, numAgents), &
        DemandParameters, &
        NashPrices, CoopPrices, NashProfits, CoopProfits, NashMarketShares, CoopMarketShares, &
        (PricesGrids(:,i), i = 1, numAgents), &
        (meanProfit(i), seProfit(i), i = 1, numAgents), meanAvgProfit, seAvgProfit, &
        (meanProfitGain(i), seProfitGain(i), i = 1, numAgents), meanAvgProfitGain, seAvgProfitGain, &
        (meanFreqStates(i), i = 1, numStates)
2   FORMAT(I10, 1X, &
        <numAgents>(F10.5, 1X), <numExplorationParameters>(F10.5, 1X), F10.5, 1X, &
        <numAgents>(A9, 1X, <numAgents>(F9.2, 1X)), &
        <numDemandParameters>(F10.5, 1X), &
        <6*numAgents>(F10.5, 1X), &
        <numPrices*numAgents>(F10.5, 1X), &
        <2*(numAgents+1)>(F10.5, 1X), &
        <2*(numAgents+1)>(F10.5, 1X), &
        <numStates>(F<MAX(10,3+LengthFormatStatesPrint)>.6, 1X) &
        )
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE ComputeConvResults
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
END MODULE ConvergenceResults
