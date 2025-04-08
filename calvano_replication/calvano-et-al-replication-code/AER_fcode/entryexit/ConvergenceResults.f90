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
    INTEGER :: i, j, iSession, rSession, iPeriod, iState, iAgent, CycleLength, iInOut
    INTEGER :: p(numAgents), pPrime(numAgents)
    INTEGER :: OptimalStrategyVec(lengthStrategies), LastStateVec(LengthStates)
    INTEGER :: VisitedStates(numPeriods), OptimalStrategy(numStates,numAgents), &
        LastObservedPrices(numAgents), ItersInOut(numStates)
    INTEGER :: pHist(numPeriods,numAgents)
    INTEGER, DIMENSION(numSessions,2) :: CountItersInOut
    INTEGER, DIMENSION(2) :: TotCountItersInOut
    REAL(8) :: Profits(numSessions,numAgents,2), VisitedProfits(numPeriods,numAgents), &
        AvgProfitsIn(numSessions), AvgProfitsOut(numSessions)
    REAL(8), DIMENSION(numAgents,2) :: meanProfit, seProfit, meanProfitGain, seProfitGain
    REAL(8), DIMENSION(2) :: meanAvgProfit, seAvgProfit, meanAvgProfitGain, seAvgProfitGain
    REAL(8) :: FreqStates(numSessions,numStates), meanFreqStates(numStates)
    REAL(8) :: den
    LOGICAL, DIMENSION(numSessions,2) :: MaskCountItersInOut
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
    CountItersInOut = 0
    DO iSession = 1, numSessions        ! Start of loop aver sessions
        !
        PRINT*, 'iSession = ', iSession
        !
        OptimalStrategyVec = indexStrategies(:,iSession)
        LastStateVec = indexLastState(:,iSession)
        !
        OptimalStrategy = RESHAPE(OptimalStrategyVec, (/ numStates,numAgents /))
        LastObservedPrices = LastStateVec
        !
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ! Convergence analysis
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !
        VisitedStates = 0
        VisitedProfits = 0.d0
        pHist = 0
        ItersInOut = 0
        p = LastObservedPrices
        pPrime = OptimalStrategy(computeStateNumber(p),:)
        DO iPeriod = 1, numPeriods
            !
            p = pPrime
            pHist(iPeriod,:) = pPrime
            VisitedStates(iPeriod) = computeStateNumber(p)
            IF (p(numAgents) .LT. numPrices) THEN
                !
                ItersInOut(iPeriod) = 1
                !
            ELSE IF (p(numAgents) .EQ. numPrices) THEN
                !
                ItersInOut(iPeriod) = 2
                !
            END IF
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
        CountItersInOut(iSession,1) = COUNT(ItersInOut(iPeriod-CycleLength+1:iPeriod) .EQ. 1)
        CountItersInOut(iSession,2) = COUNT(ItersInOut(iPeriod-CycleLength+1:iPeriod) .EQ. 2)
        DO iAgent = 1, numAgents
            !
            DO iInOut = 1, 2
                !
                IF (CountItersInOut(iSession,iInOut) .NE. 0) THEN
                    !
                    Profits(iSession,iAgent,iInOut) = &
                        SUM(visitedProfits(iPeriod-CycleLength+1:iPeriod,iAgent), &
                            MASK = ItersInOut(iPeriod-CycleLength+1:iPeriod) .EQ. iInOut)/ &
                                DBLE(CountItersInOut(iSession,iInOut))
                    !
                END IF
                !
            END DO
            !
        END DO
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
    meanNashProfitIn = SUM(NashProfitsIn)/numAgents
    meanNashProfitOut = SUM(NashProfitsOut(1:numAgents-1))/DBLE(numAgents-1)
    meanCoopProfitIn = SUM(CoopProfitsIn)/numAgents
    meanCoopProfitOut = SUM(CoopProfitsOut(1:numAgents-1))/DBLE(numAgents-1)
    AvgProfitsIn = SUM(Profits(:,:,1),DIM = 2)/DBLE(numAgents)
    AvgProfitsOut = SUM(Profits(:,1:numAgents-1,2),DIM = 2)/DBLE(numAgents-1)
    MaskCountItersInOut = (CountItersInOut .NE. 0)
    TotCountItersInOut = COUNT(CountItersInOut .NE. 0,DIM = 1)
    !
    DO iAgent = 1, numAgents
        !
        ! In
        !
        den = DBLE(TotCountItersInOut(1))
        meanProfit(iAgent,1) = SUM(Profits(:,iAgent,1),MASK = MaskCountItersInOut(:,1))/den
        seProfit(iAgent,1) = SQRT(ABS((SUM(Profits(:,iAgent,1)**2,MASK = MaskCountItersInOut(:,1))/den-meanProfit(iAgent,1)**2)))
        meanProfitGain(iAgent,1) = (meanProfit(iAgent,1)-NashProfitsIn(iAgent))/(CoopProfitsIn(iAgent)-NashProfitsIn(iAgent))
        seProfitGain(iAgent,1) = seProfit(iAgent,1)/(CoopProfitsIn(iAgent)-NashProfitsIn(iAgent))
        !
        ! Out
        !
        IF (iAgent .NE. numAgents) THEN
            !
            den = DBLE(TotCountItersInOut(2))
            meanProfit(iAgent,2) = SUM(Profits(:,iAgent,2),MASK = MaskCountItersInOut(:,2))/den
            seProfit(iAgent,2) = SQRT(ABS((SUM(Profits(:,iAgent,2)**2,MASK = MaskCountItersInOut(:,2))/den-meanProfit(iAgent,2)**2)))
            meanProfitGain(iAgent,2) = (meanProfit(iAgent,2)-NashProfitsOut(iAgent))/(CoopProfitsOut(iAgent)-NashProfitsOut(iAgent))
            seProfitGain(iAgent,2) = seProfit(iAgent,2)/(CoopProfitsOut(iAgent)-NashProfitsOut(iAgent))
            !
        END IF
        !
    END DO
    !
    ! In
    !
    den = DBLE(TotCountItersInOut(1))
    meanAvgProfit(1) = SUM(AvgProfitsIn,MASK = MaskCountItersInOut(:,1))/den
    seAvgProfit(1) = SQRT(ABS((SUM(AvgProfitsIn**2,MASK = MaskCountItersInOut(:,1))/den-meanAvgProfit(1)**2)))
    meanAvgProfitGain(1) = (meanAvgProfit(1)-meanNashProfitIn)/(meanCoopProfitIn-meanNashProfitIn)
    seAvgProfitGain(1) = seAvgProfit(1)/(meanCoopProfitIn-meanNashProfitIn)
    !
    ! Out
    !
    den = DBLE(TotCountItersInOut(2))
    meanAvgProfit(2) = SUM(AvgProfitsOut,MASK = MaskCountItersInOut(:,2))/den
    seAvgProfit(2) = SQRT(ABS((SUM(AvgProfitsOut**2,MASK = MaskCountItersInOut(:,2))/den-meanAvgProfit(2)**2)))
    meanAvgProfitGain(2) = (meanAvgProfit(2)-meanNashProfitOut)/(meanCoopProfitOut-meanNashProfitOut)
    seAvgProfitGain(2) = seAvgProfit(2)/(meanCoopProfitOut-meanNashProfitOut)
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
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            ((i, j, j = 1, numPrices), i = 1, numAgents), &
            (i, i, i = 1, numAgents), (i, i, i = 1, numAgents), &
            (i, i, i = 1, numAgents), (i, i, i = 1, numAgents), &
            (labelStates(j), j = 1, numStates)
1       FORMAT('Experiment ', &
            <numAgents>('    alpha', I1, ' '), &
            <numExplorationParameters>('     beta', I1, ' '), '     delta ', &
            <numAgents>('typeQini', I1, ' ', <numAgents>('par', I1, 'Qini', I1, ' ')), &
            <numDemandParameters>('  DemPar', I0.2, ' '), &
            <numAgents>(' NashPriceIn', I1, ' '), <numAgents>('NashPriceOut', I1, ' '), &
            <numAgents>(' CoopPriceIn', I1, ' '), <numAgents>('CoopPriceOut', I1, ' '), &
            <numAgents>(' NashProftIn', I1, ' '), <numAgents>('NashProftOut', I1, ' '), &
            <numAgents>(' CoopProftIn', I1, ' '), <numAgents>('CoopProftOut', I1, ' '), &
            <numAgents>(' NashMktShIn', I1, ' '), <numAgents>('NashMktShOut', I1, ' '), &
            <numAgents>(' CoopMktShIn', I1, ' '), <numAgents>('CoopMktShOut', I1, ' '), &
            <numAgents>(<numPrices>('Ag', I1, 'Price', I2.2, ' ')), &
            <numAgents>('   avgProf', I1, 'In', 1X, '    seProf', I1, 'In', 1X), '    avgProfIn      seProfIn ', &
            <numAgents>('  avgProf', I1, 'Out', 1X, '   seProf', I1, 'Out', 1X), '   avgProfOut     seProfOut ', &
            <numAgents>(' avgPrGain', I1, 'In', 1X, '  sePrGain', I1, 'In', 1X), '  avgPrGainIn    sePrGainIn ', &
            <numAgents>('avgPrGain', I1, 'Out', 1X, ' sePrGain', I1, 'Out', 1X), ' avgPrGainOut   sePrGainOut ', &
            <numStates>(A<MAX(10,3+LengthFormatStatesPrint)>, ' ') &
            )
        !
    END IF
    !
    WRITE(100022,2) codExperiment, &
        alpha, MExpl, delta, &
        (typeQInitialization(i), parQInitialization(i, :), i = 1, numAgents), &
        DemandParameters, &
        NashPricesIn, NashPricesOut, CoopPricesIn, CoopPricesOut, &
        NashProfitsIn, NashProfitsOut, CoopProfitsIn, CoopProfitsOut, &
        NashMarketSharesIn, CoopMarketSharesIn, NashMarketSharesOut, CoopMarketSharesOut, &
        (PricesGrids(:,i), i = 1, numAgents), &
        (meanProfit(i,1), seProfit(i,1), i = 1, numAgents), meanAvgProfit(1), seAvgProfit(1), &
        (meanProfit(i,2), seProfit(i,2), i = 1, numAgents), meanAvgProfit(2), seAvgProfit(2), &
        (meanProfitGain(i,1), seProfitGain(i,1), i = 1, numAgents), meanAvgProfitGain(1), seAvgProfitGain(1), &
        (meanProfitGain(i,2), seProfitGain(i,2), i = 1, numAgents), meanAvgProfitGain(2), seAvgProfitGain(2), &
        (meanFreqStates(i), i = 1, numStates)
2   FORMAT(I5, 1X, &
        <numAgents>(F10.5, 1X), <numExplorationParameters>(F10.5, 1X), F10.5, 1X, &
        <numAgents>(A9, 1X, <numAgents>(F9.2, 1X)), &
        <numDemandParameters>(F10.5, 1X), &
        <6*numAgents+6*numAgents>(F13.7, 1X), &
        <numPrices*numAgents>(F10.5, 1X), &
        <2*numAgents+2>(F13.5, 1X), &
        <2*numAgents+2>(F13.5, 1X), &
        <2*numAgents+2>(F13.5, 1X), &
        <2*numAgents+2>(F13.5, 1X), &
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
