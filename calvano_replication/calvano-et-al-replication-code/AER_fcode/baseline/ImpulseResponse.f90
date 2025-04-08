MODULE ImpulseResponse
!
USE globals
USE QL_routines
USE EquilibriumCheck
!
! Computes Impulse Response analysis 
!
IMPLICIT NONE
!
CONTAINS
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computeIRAnalysis ( iExperiment, UnitNumber, IRType )
    !
    ! Computes and prints to file the Impulse Response summary statistics for a price deviation
    !
    ! INPUT:
    !
    ! - iExperiment             : model index
    ! - UnitNumber         : output unit number
    ! - IRType             : index to select the deviation type:
    !                        IRType <=  -1      : one-period deviation to the IRType-th price
    !                        IRType =    0      : one-period deviation to static BR
    !                        1 <= IRType <= 999 : IRType-period deviation to Nash 
    !                        IRType = 1000      : permanent deviation to Nash
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: iExperiment, UnitNumber, IRType
    !
    ! Declaring local variable
    !
    INTEGER, PARAMETER :: numThresPeriodsLength = 10
    INTEGER, PARAMETER :: numThresPeriodsLength0 = numThresPeriodsLength+1
    INTEGER, PARAMETER :: numShockPeriodsPrint = 25
    !
    INTEGER :: i, j, iSession, iAgent, jAgent, iPeriod, iStatePre, iState, &
        PosThres, PeriodsLengthPre, DevPrice, DevLength
    INTEGER, DIMENSION(numPeriods) :: VisitedStatesPre
    INTEGER, DIMENSION(numThresPeriodsLength) :: FreqPreLength
    INTEGER, DIMENSION(numShockPeriodsPrint) :: ShockStates
    INTEGER :: OptimalStrategyVec(lengthStrategies), OptimalStrategy(numStates,numAgents)
    INTEGER :: ShockLength, PostLength, PunishmentStrategy
    INTEGER, DIMENSION(numAgents,numThresPeriodsLength) :: FreqShockLength, FreqPostLength
    INTEGER :: FreqPunishmentStrategy(numAgents,0:numThresPeriodsLength)
    INTEGER :: ThresPeriodsLength(numThresPeriodsLength), ThresPeriodsLength0(numThresPeriodsLength0)
    INTEGER, DIMENSION(numShockPeriodsPrint,numAgents) :: ShockIndPrices
    !
    REAL(8) :: r_tmp, nn
    REAL(8), DIMENSION(numPeriods,numAgents) :: PrePrices, PreProfits
    REAL(8), DIMENSION(numAgents) :: AvgPrePrices, AvgPreProfits, AvgPrePricesQ, AvgPreProfitsQ
    REAL(8), DIMENSION(numShockPeriodsPrint,numAgents) :: ShockPrices, ShockProfits, &
        AvgShockPricesTmp, AvgShockProfitsTmp
    REAL(8), DIMENSION(numAgents) :: PostPrices, PostProfits, &
        AvgPostPricesTmp, AvgPostProfitsTmp
    REAL(8), DIMENSION(numShockPeriodsPrint,numAgents,numAgents) :: &
        AvgShockPrices, AvgShockProfits, AvgShockPricesQ, AvgShockProfitsQ
    REAL(8), DIMENSION(numAgents,numAgents) :: &
        AvgPostPrices, AvgPostProfits, AvgPostPricesQ, AvgPostProfitsQ
    REAL(8) :: AggrPrePrices, AggrDevPostPrices, AggrNonDevPostPrices, AggrPreProfits, AggrDevPostProfits, AggrNonDevPostProfits
    REAL(8), DIMENSION(numShockPeriodsPrint) :: &
        AggrDevShockPrices, AggrNonDevShockPrices, AggrDevShockProfits, AggrNonDevShockProfits
    REAL(8) :: AggrPrePricesQ, AggrDevPostPricesQ, AggrNonDevPostPricesQ, AggrPreProfitsQ, AggrDevPostProfitsQ, AggrNonDevPostProfitsQ
    REAL(8), DIMENSION(numShockPeriodsPrint) :: &
        AggrDevShockPricesQ, AggrNonDevShockPricesQ, AggrDevShockProfitsQ, AggrNonDevShockProfitsQ
    !
    ! Beginning execution
    !
    PRINT*, 'Computing Impulse Responses'
    !
    ! Reading strategies and states at convergence from file
    !
    CALL ReadInfoExperiment()
    !
    ! Initializing variables
    !
    !$ CALL OMP_SET_NUM_THREADS(numCores)
    !
    ThresPeriodsLength = (/ ( i, i = 1, numThresPeriodsLength ) /)
    ThresPeriodsLength0 = (/ 0, ThresPeriodsLength /)
    !
    FreqPreLength = 0
    AvgPrePrices = 0.d0
    AvgPreProfits = 0.d0
    AvgPrePricesQ = 0.d0
    AvgPreProfitsQ = 0.d0
    !
    FreqShockLength = 0
    FreqPunishmentStrategy = 0
    AvgShockPrices = 0.d0
    AvgShockPricesQ = 0.d0
    AvgShockProfits = 0.d0
    AvgShockProfitsQ = 0.d0
    !
    FreqPostLength = 0
    AvgPostPrices = 0.d0
    AvgPostPricesQ = 0.d0
    AvgPostProfits = 0.d0
    AvgPostProfitsQ = 0.d0
    !
    ! Beginning loop over sessions
    !
    !$omp parallel do &
    !$omp private(OptimalStrategyVec,OptimalStrategy,PeriodsLengthPre,PosThres, &
    !$omp   VisitedStatesPre,PrePrices,PreProfits,iPeriod,iAgent, &
    !$omp   AvgShockPricesTmp,AvgShockProfitsTmp,AvgPostPricesTmp,AvgPostProfitsTmp, &
    !$omp   iStatePre,DevPrice,DevLength,r_tmp,ShockStates,ShockIndPrices,ShockPrices,ShockProfits,PostPrices,PostProfits, &
    !$omp   ShockLength,PunishmentStrategy,PostLength) &
    !$omp firstprivate(PI,PricesGrids) &
    !$omp reduction(+ : FreqPreLength,FreqShockLength,FreqPunishmentStrategy,FreqPostLength, &
    !$omp   AvgPrePrices,AvgPreProfits,AvgShockPrices,AvgShockProfits,AvgPostPrices,AvgPostProfits, &
    !$omp   AvgPrePricesQ,AvgPreProfitsQ,AvgShockPricesQ,AvgShockProfitsQ,AvgPostPricesQ,AvgPostProfitsQ)
    DO iSession = 1, numSessions        ! Start of loop aver sessions
        !
        PRINT*, 'iSession = ', iSession
        !
        !$omp critical
        OptimalStrategyVec = indexStrategies(:,iSession)
        !$omp end critical
        !
        OptimalStrategy = RESHAPE(OptimalStrategyVec, (/ numStates,numAgents /) )
        !
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ! Pre-shock period analysis
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !
        PeriodsLengthPre = CycleLength(iSession)
        PosThres = MIN(numThresPeriodsLength,PeriodsLengthPre)
        FreqPreLength(PosThres) = FreqPreLength(PosThres)+1
        VisitedStatesPre = 0
        PrePrices = 0.d0
        PreProfits = 0.d0
        DO iPeriod = 1, PeriodsLengthPre
            !
            VisitedStatesPre(iPeriod) = CycleStates(iPeriod,iSession)
            DO iAgent = 1, numAgents
                !
                PrePrices(iPeriod,iAgent) = PricesGrids(CyclePrices(iAgent,iPeriod,iSession),iAgent)
                PreProfits(iPeriod,iAgent) = CycleProfits(iAgent,iPeriod,iSession)
                !
            END DO
            !
        END DO
        !
        AvgPrePrices = AvgPrePrices+SUM(PrePrices(:PeriodsLengthPre,:),DIM = 1)/DBLE(PeriodsLengthPre)
        AvgPrePricesQ = AvgPrePricesQ+(SUM(PrePrices(:PeriodsLengthPre,:),DIM = 1)/DBLE(PeriodsLengthPre))**2
        AvgPreProfits = AvgPreProfits+SUM(PreProfits(:PeriodsLengthPre,:),DIM = 1)/DBLE(PeriodsLengthPre)
        AvgPreProfitsQ = AvgPreProfitsQ+(SUM(PreProfits(:PeriodsLengthPre,:),DIM = 1)/DBLE(PeriodsLengthPre))**2
        !
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ! Shock period analysis
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !
        DO iAgent = 1, numAgents        ! Start of loop aver shocking agent 
            !
            AvgShockPricesTmp = 0.d0
            AvgShockProfitsTmp = 0.d0
            AvgPostPricesTmp = 0.d0
            AvgPostProfitsTmp = 0.d0
            !
            DO iStatePre = 1, PeriodsLengthPre      ! Start of loop over pre-shock cycle states
                !
                ! Selecting the deviation type
                !
                IF (IRType .LE. -1) THEN
                    !
                    ! One-period deviation to the IRType-th price
                    !
                    DevPrice = -IRType
                    DevLength = 1
                    !
                ELSE IF (IRType .EQ. 0) THEN
                    !
                    ! One-period deviation to static BR
                    !
                    CALL ComputeStaticBestResponse(OptimalStrategy,iStatePre,iAgent,DevPrice,r_tmp)
                    DevLength = 1
                    !
                ELSE IF (IRType .GE. 1) THEN
                    !
                    ! DevLength-period deviation to Nash
                    !
                    DevPrice = MINVAL(MINLOC((pricesGrids(:,iAgent)-NashPrices(iAgent))**2))
                    DevLength = IRType
                    !
                END IF
                !
                ! Computing individual IRs
                !
                CALL computeIndividualIR(OptimalStrategy,VisitedStatesPre(iStatePre),iAgent,DevPrice,DevLength, &
                    numShockPeriodsPrint,PeriodsLengthPre,VisitedStatesPre(:PeriodsLengthPre), &
                    ShockStates,ShockIndPrices,ShockPrices,ShockProfits,PostPrices,PostProfits, &
                    ShockLength,PunishmentStrategy,PostLength)
                !
                ! Computing running averages
                !
                nn = DBLE(iStatePre)
                AvgShockPricesTmp = (nn-1.d0)/nn*AvgShockPricesTmp+ShockPrices/nn
                AvgShockProfitsTmp = (nn-1.d0)/nn*AvgShockProfitsTmp+ShockProfits/nn
                !
                AvgPostPricesTmp = (nn-1.d0)/nn*AvgPostPricesTmp+PostPrices/nn
                AvgPostProfitsTmp = (nn-1.d0)/nn*AvgPostProfitsTmp+PostProfits/nn
                !
                FreqShockLength(iAgent,MIN(numThresPeriodsLength,ShockLength)) = &
                    FreqShockLength(iAgent,MIN(numThresPeriodsLength,ShockLength))+1
                FreqPunishmentStrategy(iAgent,MIN(numThresPeriodsLength,PunishmentStrategy)) = &
                    FreqPunishmentStrategy(iAgent,MIN(numThresPeriodsLength,PunishmentStrategy))+1
                FreqPostLength(iAgent,MIN(numThresPeriodsLength,PostLength)) = &
                    FreqPostLength(iAgent,MIN(numThresPeriodsLength,PostLength))+1
                !
            END DO                          ! End of loop over pre-shock cycle states
            !
            ! Compute average prices and profits over pre-shock cycle states
            !
            AvgShockPrices(:,iAgent,:) = AvgShockPrices(:,iAgent,:)+AvgShockPricesTmp
            AvgShockPricesQ(:,iAgent,:) = AvgShockPricesQ(:,iAgent,:)+AvgShockPricesTmp**2
            AvgShockProfits(:,iAgent,:) = AvgShockProfits(:,iAgent,:)+AvgShockProfitsTmp
            AvgShockProfitsQ(:,iAgent,:) = AvgShockProfitsQ(:,iAgent,:)+AvgShockProfitsTmp**2
            AvgPostPrices(iAgent,:) = AvgPostPrices(iAgent,:)+AvgPostPricesTmp
            AvgPostPricesQ(iAgent,:) = AvgPostPricesQ(iAgent,:)+AvgPostPricesTmp**2
            AvgPostProfits(iAgent,:) = AvgPostProfits(iAgent,:)+AvgPostProfitsTmp
            AvgPostProfitsQ(iAgent,:) = AvgPostProfitsQ(iAgent,:)+AvgPostProfitsTmp**2
            !
        END DO                          ! End of loop aver shocking agent 
        !
    END DO        ! End of loop over sessions
    !$omp end parallel do
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Computing averages and descriptive statistics
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    ! Averages of prices and profits
    !
    AvgPrePrices = AvgPrePrices/DBLE(numSessions)
    AvgPreProfits = AvgPreProfits/DBLE(numSessions)
    AvgShockPrices = AvgShockPrices/DBLE(numSessions)
    AvgShockProfits = AvgShockProfits/DBLE(numSessions)
    AvgPostPrices = AvgPostPrices/DBLE(numSessions)
    AvgPostProfits = AvgPostProfits/DBLE(numSessions)
    AvgPrePricesQ = AvgPrePricesQ/DBLE(numSessions)
    AvgPreProfitsQ = AvgPreProfitsQ/DBLE(numSessions)
    AvgShockPricesQ = AvgShockPricesQ/DBLE(numSessions)
    AvgShockProfitsQ = AvgShockProfitsQ/DBLE(numSessions)
    AvgPostPricesQ = AvgPostPricesQ/DBLE(numSessions)
    AvgPostProfitsQ = AvgPostProfitsQ/DBLE(numSessions)
    !
    ! Computing aggregate (deviating and non-deviating) averages of prices and profits
    !
    AggrPrePrices = SUM(AvgPrePrices)/DBLE(numAgents)
    AggrPreProfits = SUM(AvgPreProfits)/DBLE(numAgents)
    AggrPrePricesQ = SUM(AvgPrePricesQ)/DBLE(numAgents)
    AggrPreProfitsQ = SUM(AvgPreProfitsQ)/DBLE(numAgents)
    !
    AggrDevShockPrices = 0.d0
    AggrDevShockProfits = 0.d0
    AggrDevShockPricesQ = 0.d0
    AggrDevShockProfitsQ = 0.d0
    DO iPeriod = 1, numShockPeriodsPrint
        !
        DO iAgent = 1, numAgents
            !
            AggrDevShockPrices(iPeriod) = AggrDevShockPrices(iPeriod)+AvgShockPrices(iPeriod,iAgent,iAgent)
            AggrDevShockProfits(iPeriod) = AggrDevShockProfits(iPeriod)+AvgShockProfits(iPeriod,iAgent,iAgent)
            AggrDevShockPricesQ(iPeriod) = AggrDevShockPricesQ(iPeriod)+AvgShockPricesQ(iPeriod,iAgent,iAgent)
            AggrDevShockProfitsQ(iPeriod) = AggrDevShockProfitsQ(iPeriod)+AvgShockProfitsQ(iPeriod,iAgent,iAgent)
            !
        END DO
        AggrNonDevShockPrices(iPeriod) = (SUM(AvgShockPrices(iPeriod,:,:))-AggrDevShockPrices(iPeriod))/DBLE(numAgents*(numAgents-1))
        AggrDevShockPrices(iPeriod) = AggrDevShockPrices(iPeriod)/DBLE(numAgents)
        AggrNonDevShockProfits(iPeriod) = (SUM(AvgShockProfits(iPeriod,:,:))-AggrDevShockProfits(iPeriod))/DBLE(numAgents*(numAgents-1))
        AggrDevShockProfits(iPeriod) = AggrDevShockProfits(iPeriod)/DBLE(numAgents)
        AggrNonDevShockPricesQ(iPeriod) = (SUM(AvgShockPricesQ(iPeriod,:,:))-AggrDevShockPricesQ(iPeriod))/DBLE(numAgents*(numAgents-1))
        AggrDevShockPricesQ(iPeriod) = AggrDevShockPricesQ(iPeriod)/DBLE(numAgents)
        AggrNonDevShockProfitsQ(iPeriod) = (SUM(AvgShockProfitsQ(iPeriod,:,:))-AggrDevShockProfitsQ(iPeriod))/DBLE(numAgents*(numAgents-1))
        AggrDevShockProfitsQ(iPeriod) = AggrDevShockProfitsQ(iPeriod)/DBLE(numAgents)
        !
    END DO
    !
    AggrDevPostPrices = 0.d0
    AggrDevPostProfits = 0.d0
    AggrDevPostPricesQ = 0.d0
    AggrDevPostProfitsQ = 0.d0
    DO iAgent = 1, numAgents
        !
        AggrDevPostPrices = AggrDevPostPrices+AvgPostPrices(iAgent,iAgent)
        AggrDevPostProfits = AggrDevPostProfits+AvgPostProfits(iAgent,iAgent)
        AggrDevPostPricesQ = AggrDevPostPricesQ+AvgPostPricesQ(iAgent,iAgent)
        AggrDevPostProfitsQ = AggrDevPostProfitsQ+AvgPostProfitsQ(iAgent,iAgent)
        !
    END DO
    AggrNonDevPostPrices = (SUM(AvgPostPrices(:,:))-AggrDevPostPrices)/DBLE(numAgents*(numAgents-1))
    AggrDevPostPrices = AggrDevPostPrices/DBLE(numAgents)
    AggrNonDevPostProfits = (SUM(AvgPostProfits(:,:))-AggrDevPostProfits)/DBLE(numAgents*(numAgents-1))
    AggrDevPostProfits = AggrDevPostProfits/DBLE(numAgents)
    AggrNonDevPostPricesQ = (SUM(AvgPostPricesQ(:,:))-AggrDevPostPricesQ)/DBLE(numAgents*(numAgents-1))
    AggrDevPostPricesQ = AggrDevPostPricesQ/DBLE(numAgents)
    AggrNonDevPostProfitsQ = (SUM(AvgPostProfitsQ(:,:))-AggrDevPostProfitsQ)/DBLE(numAgents*(numAgents-1))
    AggrDevPostProfitsQ = AggrDevPostProfitsQ/DBLE(numAgents)
    !
    ! Computing standard errors
    !
    AvgPrePricesQ = SQRT(ABS((AvgPrePricesQ-AvgPrePrices**2)))
    AvgPreProfitsQ = SQRT(ABS((AvgPreProfitsQ-AvgPreProfits**2)))
    AvgShockPricesQ = SQRT(ABS((AvgShockPricesQ-AvgShockPrices**2)))
    AvgShockProfitsQ = SQRT(ABS((AvgShockProfitsQ-AvgShockProfits**2)))
    AvgPostPricesQ = SQRT(ABS((AvgPostPricesQ-AvgPostPrices**2)))
    AvgPostProfitsQ = SQRT(ABS((AvgPostProfitsQ-AvgPostProfits**2)))
    !
    AggrPrePricesQ = SQRT(ABS((AggrPrePricesQ-AggrPrePrices**2)))
    AggrPreProfitsQ = SQRT(ABS((AggrPreProfitsQ-AggrPreProfits**2)))
    DO iPeriod = 1, numShockPeriodsPrint
        !
        AggrNonDevShockPricesQ(iPeriod) = SQRT(ABS((AggrNonDevShockPricesQ(iPeriod)-AggrNonDevShockPrices(iPeriod)**2)))
        AggrDevShockPricesQ(iPeriod) = SQRT(ABS((AggrDevShockPricesQ(iPeriod)-AggrDevShockPrices(iPeriod)**2)))
        AggrNonDevShockProfitsQ(iPeriod) = SQRT(ABS((AggrNonDevShockProfitsQ(iPeriod)-AggrNonDevShockProfits(iPeriod)**2)))
        AggrDevShockProfitsQ(iPeriod) = SQRT(ABS((AggrDevShockProfitsQ(iPeriod)-AggrDevShockProfits(iPeriod)**2)))
        !
    END DO
    AggrNonDevPostPricesQ = SQRT(ABS((AggrNonDevPostPricesQ-AggrNonDevPostPrices**2)))
    AggrDevPostPricesQ = SQRT(ABS((AggrDevPostPricesQ-AggrDevPostPrices**2)))
    AggrNonDevPostProfitsQ = SQRT(ABS((AggrNonDevPostProfitsQ-AggrNonDevPostProfits**2)))
    AggrDevPostProfitsQ = SQRT(ABS((AggrDevPostProfitsQ-AggrDevPostProfits**2)))
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Printing averages and descriptive statistics
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    IF (iExperiment .EQ. 1) THEN
        !
        WRITE(UnitNumber,1) &
            (i, i = 1, numAgents), &
            (i, i = 1, numExplorationParameters), &
            (i, (j, i, j = 1, numAgents), i = 1, numAgents), &
            (i, i = 1, numDemandParameters), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents),  &
            (i, i = 1, numAgents), (i, i = 1, numAgents),  &
            ((i, j, j = 1, numPrices), i = 1, numAgents), &
            (iPeriod, iPeriod = 1, numShockPeriodsPrint), (iPeriod, iPeriod = 1, numShockPeriodsPrint), &
            (iPeriod, iPeriod = 1, numShockPeriodsPrint), (iPeriod, iPeriod = 1, numShockPeriodsPrint), &
            (iPeriod, iPeriod = 1, numShockPeriodsPrint), (iPeriod, iPeriod = 1, numShockPeriodsPrint), &
            (iPeriod, iPeriod = 1, numShockPeriodsPrint), (iPeriod, iPeriod = 1, numShockPeriodsPrint), &
            (ThresPeriodsLength(i), i = 1, numThresPeriodsLength), &
            ((iAgent, ThresPeriodsLength(i), i = 1, numThresPeriodsLength), iAgent = 1, numAgents), &
            ((iAgent, ThresPeriodsLength(i), i = 1, numThresPeriodsLength), iAgent = 1, numAgents), &
            ((iAgent, ThresPeriodsLength0(i), i = 1, numThresPeriodsLength0), iAgent = 1, numAgents), &
            ((jAgent, (jAgent, iAgent, iPeriod, iPeriod = 1, numShockPeriodsPrint), jAgent, iAgent, &
                jAgent = 1, numAgents), iAgent = 1, numAgents), &  ! AvgPrices
            ((jAgent, (jAgent, iAgent, iPeriod, iPeriod = 1, numShockPeriodsPrint), jAgent, iAgent, &
                jAgent = 1, numAgents), iAgent = 1, numAgents), &  ! sePrices
            ((jAgent, (jAgent, iAgent, iPeriod, iPeriod = 1, numShockPeriodsPrint), jAgent, iAgent, &
                jAgent = 1, numAgents), iAgent = 1, numAgents), &  ! AvgProfits
            ((jAgent, (jAgent, iAgent, iPeriod, iPeriod = 1, numShockPeriodsPrint), jAgent, iAgent, &
                jAgent = 1, numAgents), iAgent = 1, numAgents)     ! seProfits        
1       FORMAT('Experiment IRType ', &
            <numAgents>('    alpha', I1, ' '), &
            <numExplorationParameters>('     beta', I1, ' '), '     delta ', &
            <numAgents>('typeQini', I1, ' ', <numAgents>('par', I1, 'Qini', I1, ' ')), &
            <numDemandParameters>('  DemPar', I0.2, ' '), &
            <numAgents>('NashPrice', I1, ' '), <numAgents>('CoopPrice', I1, ' '), &
            <numAgents>('NashProft', I1, ' '), <numAgents>('CoopProft', I1, ' '), &
            <numAgents>('NashMktSh', I1, ' '), <numAgents>('CoopMktSh', I1, ' '), &
            <numAgents>(<numPrices>('Ag', I1, 'Price', I2.2, ' ')), &
            'AggrPricePre ', <numShockPeriodsPrint>('AggrDevPriceShockPer', I3.3, ' '), 'AggrDevPricePost ', &
                <numShockPeriodsPrint>('AggrNonDevPriceShockPer', I3.3, ' '), 'AggrNonDevPricePost ', &
            'seAggrPricePre ', <numShockPeriodsPrint>('seAggrDevPriceShockPer', I3.3, ' '), 'seAggrDevPricePost ', &
                <numShockPeriodsPrint>('seAggrNonDevPriceShockPer', I3.3, ' '), 'seAggrNonDevPricePost ', &
            'AggrProfitPre ', <numShockPeriodsPrint>('AggrDevProfitShockPer', I3.3, ' '), 'AggrDevProfitPost ', &
                <numShockPeriodsPrint>('AggrNonDevProfitShockPer', I3.3, ' '), 'AggrNonDevProfitPost ', &
            'seAggrProfitPre ', <numShockPeriodsPrint>('seAggrDevProfitShockPer', I3.3, ' '), 'seAggrDevProfitPost ', &
                <numShockPeriodsPrint>('seAggrNonDevProfitShockPer', I3.3, ' '), 'seAggrNonDevProfitPost ', &
            <numThresPeriodsLength>('#PerLenPre=', I2.2, ' '), &
            <numAgents>(<numThresPeriodsLength>('#PerLenPostAg', I1, '=', I2.2, ' ')), &
            <numAgents>(<numThresPeriodsLength>('#PerLenShockAg', I1, '=', I2.2, ' ')), &
            <numAgents>(<numThresPeriodsLength+1>('#PunishStratAg', I1, '=', I2.2, ' ')), &
            <numAgents>(<numAgents>('Ag', I1, 'AvgPricePre', ' ', &
                <numShockPeriodsPrint>('Ag', I1, 'AvgPriceShockAg', I1, 'Per', I3.3, ' '), &
                'Ag', I1, 'AvgPricePostAg', I1, ' ')), &
            <numAgents>(<numAgents>('seAg', I1, 'AvgPricePre', ' ', &
                <numShockPeriodsPrint>('seAg', I1, 'AvgPriceShockAg', I1, 'Per', I3.3, ' '), &
                'seAg', I1, 'AvgPricePostAg', I1, ' ')), &
            <numAgents>(<numAgents>('Ag', I1, 'AvgProfitPre', ' ', &
                <numShockPeriodsPrint>('Ag', I1, 'AvgProfitShockAg', I1, 'Per', I3.3, ' '), &
                'Ag', I1, 'AvgProfitPostAg', I1, ' ')), &
            <numAgents>(<numAgents>('seAg', I1, 'AvgProfitPre', ' ', &
                <numShockPeriodsPrint>('seAg', I1, 'AvgProfitShockAg', I1, 'Per', I3.3, ' '), &
                'seAg', I1, 'AvgProfitPostAg', I1, ' ')) &
            )
        !
    END IF
    !
    WRITE(UnitNumber,2) codExperiment, IRType, &
        alpha, MExpl, delta, &
        (typeQInitialization(i), parQInitialization(i, :), i = 1, numAgents), &
        DemandParameters, &
        NashPrices, CoopPrices, NashProfits, CoopProfits, NashMarketShares, CoopMarketShares, &
        (PricesGrids(:,i), i = 1, numAgents), &
        AggrPrePrices, AggrDevShockPrices, AggrDevPostPrices, AggrNonDevShockPrices, AggrNonDevPostPrices, &
        AggrPrePricesQ, AggrDevShockPricesQ, AggrDevPostPricesQ, AggrNonDevShockPricesQ, AggrNonDevPostPricesQ, &
        AggrPreProfits, AggrDevShockProfits, AggrDevPostProfits, AggrNonDevShockProfits, AggrNonDevPostProfits, &
        AggrPreProfitsQ, AggrDevShockProfitsQ, AggrDevPostProfitsQ, AggrNonDevShockProfitsQ, AggrNonDevPostProfitsQ, &
        FreqPreLength, &
        ((FreqPostLength(iAgent,i), i = 1, numThresPeriodsLength), iAgent = 1, numAgents), &
        ((FreqShockLength(iAgent,i), i = 1, numThresPeriodsLength), iAgent = 1, numAgents), &
        ((FreqPunishmentStrategy(iAgent,i), i = 0, numThresPeriodsLength), iAgent = 1, numAgents), &
        ((AvgPrePrices(jAgent), (AvgShockPrices(iPeriod,iAgent,jAgent), iPeriod = 1, numShockPeriodsPrint), AvgPostPrices(iAgent,jAgent), &
            jAgent = 1, numAgents), iAgent = 1, numAgents), &
        ((AvgPrePricesQ(jAgent), (AvgShockPricesQ(iPeriod,iAgent,jAgent), iPeriod = 1, numShockPeriodsPrint), AvgPostPricesQ(iAgent,jAgent), &
            jAgent = 1, numAgents), iAgent = 1, numAgents), &
        ((AvgPreProfits(jAgent), (AvgShockProfits(iPeriod,iAgent,jAgent), iPeriod = 1, numShockPeriodsPrint), AvgPostProfits(iAgent,jAgent), &
            jAgent = 1, numAgents), iAgent = 1, numAgents), &
        ((AvgPreProfitsQ(jAgent), (AvgShockProfitsQ(iPeriod,iAgent,jAgent), iPeriod = 1, numShockPeriodsPrint), AvgPostProfitsQ(iAgent,jAgent), &
            jAgent = 1, numAgents), iAgent = 1, numAgents)
2   FORMAT(I10, 1X, I6, 1X, &
        <numAgents>(F10.5, 1X), <numExplorationParameters>(F10.5, 1X), F10.5, 1X, &
        <numAgents>(A9, 1X, <numAgents>(F9.2, 1X)), &
        <numDemandParameters>(F10.5, 1X), &
        <6*numAgents>(F10.5, 1X), &
        <numPrices*numAgents>(F10.5, 1X), &
        F12.7, 1X, <numShockPeriodsPrint>(F23.7,1X), F16.7, 1X, <numShockPeriodsPrint>(F26.7,1X), F19.7, 1X, &
        F14.7, 1X, <numShockPeriodsPrint>(F25.7,1X), F18.7, 1X, <numShockPeriodsPrint>(F28.7,1X), F21.7, 1X, &
        F13.7, 1X, <numShockPeriodsPrint>(F24.7,1X), F17.7, 1X, <numShockPeriodsPrint>(F27.7,1X), F20.7, 1X, &
        F15.7, 1X, <numShockPeriodsPrint>(F26.7,1X), F19.7, 1X, <numShockPeriodsPrint>(F29.7,1X), F22.7, 1X, &
        <numThresPeriodsLength>(I13, 1X), &
        <numAgents>(<numThresPeriodsLength>(I17, 1X)), &
        <numAgents>(<numThresPeriodsLength>(I18, 1X)), &
        <numAgents>(<numThresPeriodsLength0>(I18, 1X)), &
        <numAgents>(<numAgents>(F14.7, 1X, <numShockPeriodsPrint>(F25.7, 1X), F18.7, 1X)), &
        <numAgents>(<numAgents>(F16.7, 1X, <numShockPeriodsPrint>(F27.7, 1X), F20.7, 1X)), &
        <numAgents>(<numAgents>(F15.7, 1X, <numShockPeriodsPrint>(F26.7, 1X), F19.7, 1X)), &
        <numAgents>(<numAgents>(F17.7, 1X, <numShockPeriodsPrint>(F28.7, 1X), F21.7, 1X)) &
        )
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computeIRAnalysis
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE ComputeStaticBestResponse ( OptimalStrategy, iState, iAgent, IndexStaticBR, PIStaticBR )
    !
    ! Computes static best response of iAgent given all agents' strategies 
    ! 'Best' means that the selected price maximizes iAgent's profits assuming 
    ! that rivals play according to their strategies
    !
    ! INPUT:
    !
    ! - OptimalStrategy     : strategy for all agents
    ! - iState              : current state
    ! - iAgent              : agent index
    !
    ! OUTPUT:
    !
    ! - IndexStaticBR       : static BR price index
    ! - PIStaticBR          : iAgent's one-period profit when playing IndexStaticBR
    !
    IMPLICIT NONE
    !
    ! Declare dummy variables
    !
    INTEGER, INTENT(IN) :: OptimalStrategy(numStates,numAgents)
    INTEGER, INTENT(IN) :: iState, iAgent
    INTEGER, INTENT(OUT) :: IndexStaticBR
    REAL(8), INTENT(OUT) :: PIStaticBR
    !
    ! Declare local variables
    !
    INTEGER :: iPrice
    INTEGER, DIMENSION(numAgents) :: pPrime
    REAL(8), DIMENSION(numPrices) :: selProfits
    !
    ! Beginning execution
    !
    pPrime = OptimalStrategy(iState,:)
    selProfits = 0.d0
    DO iPrice = 1, numPrices
        !
        pPrime(iAgent) = iPrice
        selProfits(iPrice) = PI(computeActionNumber(pPrime),iAgent)
        !
    END DO
    IndexStaticBR = MINVAL(MAXLOC(selProfits))
    PIStaticBR = MAXVAL(selProfits)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE ComputeStaticBestResponse    
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE ComputeDynamicBestResponse ( OptimalStrategy, iState, iAgent, delta, IndexDynamicBR, QDynamicBR )
    !
    ! Computes dynamic best response of one agent given all agents' strategies 
    ! 'Best' means that the selected price maximizes Q given the state and assuming 
    ! that opponents play according to their strategies
    !
    ! INPUT:
    !
    ! - OptimalStrategy     : strategy for all agents
    ! - iState              : current state
    ! - iAgent              : agent index
    ! - delta               : discount factor
    !
    ! OUTPUT:
    !
    ! - IndexDynamicBR      : dynamic BR price index
    ! - QDynamicBR          : Q(iState,IndexDynamicBR,iAgent)
    !
    IMPLICIT NONE
    !
    ! Declare dummy variables
    !
    INTEGER, INTENT(IN) :: OptimalStrategy(numStates,numAgents)
    INTEGER, INTENT(IN) :: iState
    INTEGER, INTENT(IN) :: iAgent
    REAL(8), INTENT(IN) :: delta
    INTEGER, INTENT(OUT) :: IndexDynamicBR
    REAL(8), INTENT(OUT) :: QDynamicBR
    !
    ! Declare local variables
    !
    INTEGER :: iPrice, PreCycleLength, CycleLength
    INTEGER, DIMENSION(numPeriods) :: VisitedStates
    REAL(8), DIMENSION(numPrices) :: selQ
    !
    ! Beginning execution
    !
    selQ = 0.d0
    DO iPrice = 1, numPrices
        !
        CALL computeQCell(OptimalStrategy,iState,iPrice,iAgent,delta, &
            selQ(iPrice),VisitedStates,PreCycleLength,CycleLength)
        !
    END DO
    IndexDynamicBR = MINVAL(MAXLOC(selQ))
    QDynamicBR = MAXVAL(selQ)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE ComputeDynamicBestResponse    
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computeIndividualIR ( OptimalStrategy, InitialState, DevAgent, DevPrice, DevLength, &
        DevObsLength, PreCycleLength, PreCycleStates, &
        ShockStates, ShockIndPrices, ShockPrices, ShockProfits, AvgPostPrices, AvgPostProfits, &
        ShockLength, PunishmentStrategy, PostLength )
    !
    ! Computes the Impulse Response for a price deviation on a single replication
    !
    ! INPUT:
    !
    ! - OptimalStrategy    : strategy for all agents
    ! - InitialState       : initial state
    ! - DevAgent           : deviating agent index
    ! - DevPrice           : deviation price index
    ! - DevLength          : deviation period length 
    ! - DevObsLength       : length of the observation interval of the deviation period
    ! - PreCycleLength     : length of the pre-deviation cycle
    ! - PreCycleStates     : pre-deviation cycle states
    !
    ! OUTPUT:
    !
    ! - ShockStates        : trajectory of states in the deviation interval
    ! - ShockIndPrices     : trajectory of all agents' price indexes in the deviation interval
    ! - ShockPrices        : trajectory of all agents' prices in the deviation interval
    ! - ShockProfits       : trajectory of all agents' profits in the deviation interval
    ! - AvgPostPrices      : average of all agents' prices in the post-deviation cycle
    ! - AvgPostProfits     : average of all agents' profits in the post-deviation cycle
    ! - ShockLength        : length of the non-cyclic deviation interval
    ! - PunishmentStrategy : indicator. After the deviation:
    !                        = 0: the system returns to a cycle different from the pre-deviation cycle
    !                        > 0: the system returns to the pre-deviation cycle after PunishmentStrategy periods
    ! - PostLength         : length of the post-deviation cycle
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, DIMENSION(numStates,numAgents), INTENT(IN) :: OptimalStrategy
    INTEGER, INTENT(IN) :: InitialState, DevAgent, DevPrice, DevLength, DevObsLength, PreCycleLength
    INTEGER, DIMENSION(PreCycleLength), INTENT(IN) :: PreCycleStates
    INTEGER, DIMENSION(DevObsLength), INTENT(OUT) :: ShockStates
    INTEGER, DIMENSION(DevObsLength,numAgents), INTENT(OUT) :: ShockIndPrices
    REAL(8), DIMENSION(DevObsLength,numAgents), INTENT(OUT) :: ShockPrices, ShockProfits
    REAL(8), DIMENSION(numAgents), INTENT(OUT) :: AvgPostPrices, AvgPostProfits
    INTEGER, INTENT(OUT) :: ShockLength, PunishmentStrategy, PostLength
    !
    ! Declaring local variables
    !
    INTEGER :: iPeriod, jAgent
    INTEGER :: p(DepthState,numAgents), pPrime(numAgents) 
    INTEGER :: VisitedStates(MAX(DevObsLength,numPeriods))
    INTEGER :: indexShockState(LengthStates)
    !
    REAL(8), DIMENSION(numPeriods,numAgents) :: visitedPrices, VisitedProfits
    !
    LOGICAL :: FlagReturnedToState
    !
    ! Beginning execution
    !
    p = RESHAPE(convertNumberBase(InitialState-1,numPrices,numAgents*DepthState),(/ DepthState,numAgents /))
    pPrime = OptimalStrategy(InitialState,:)
    !
    ! Agent "DevAgent" selects the best deviation price,
    ! the other agents stick to the strategy at convergence
    !
    pPrime(DevAgent) = DevPrice
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Loop over deviation period
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    VisitedStates = 0
    ShockStates = 0
    ShockIndPrices = 0
    ShockPrices = 0.d0
    ShockProfits = 0.d0
    flagReturnedToState = .FALSE.
    DO iPeriod = 1, MAX(DevObsLength,numPeriods)
        !
        IF (DepthState .GT. 1) p(2:DepthState,:) = p(1:DepthState-1,:)
        p(1,:) = pPrime
        VisitedStates(iPeriod) = computeStateNumber(p)
        DO jAgent = 1, numAgents
            !
            IF (iPeriod .LE. DevObsLength) THEN
                !
                ShockStates(iPeriod) = VisitedStates(iPeriod)
                ShockIndPrices(iPeriod,jAgent) = pPrime(jAgent)
                ShockPrices(iPeriod,jAgent) = PricesGrids(pPrime(jAgent),jAgent)
                ShockProfits(iPeriod,jAgent) = PI(computeActionNumber(pPrime),jAgent)
                !
            END IF
            !
        END DO
        !
        ! Check if the state has already been visited
        ! Case 1: the state retuns to one of the states in the pre-shock cycle
        !
        IF ((.NOT.(flagReturnedToState)) .AND. (ANY(PreCycleStates .EQ. VisitedStates(iPeriod)))) THEN
            !
            ShockLength = iPeriod
            PunishmentStrategy = iPeriod
            indexShockState = RESHAPE(p,(/ LengthStates /))
            flagReturnedToState = .TRUE.
            !
        END IF
        !
        ! Case 2: after some time, the state starts cycling among a new set of states
        !
        IF ((iPeriod .GE. 2) .AND. (.NOT.(flagReturnedToState)) .AND. &
            (ANY(VisitedStates(:iPeriod-1) .EQ. VisitedStates(iPeriod)))) THEN
            !
            ShockLength = MINVAL(MINLOC((VisitedStates(:iPeriod-1)-VisitedStates(iPeriod))**2))
            PunishmentStrategy = 0
            indexShockState = RESHAPE(p,(/ LengthStates /))
            flagReturnedToState = .TRUE.
            !
        END IF
        !
        ! Update pPrime according to the deviation length    
        !
        pPrime = OptimalStrategy(VisitedStates(iPeriod),:)
        IF (DevLength .EQ. 1000) pPrime(DevAgent) = DevPrice          ! Permanent deviation
        IF (DevLength .GT. iPeriod) pPrime(DevAgent) = DevPrice       ! Temporary deviation
        !
    END DO
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Post-shock period 
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    VisitedStates = 0
    VisitedPrices = 0.d0
    VisitedProfits = 0.d0
    p = RESHAPE(indexShockState, (/ DepthState,numAgents /) )
    pPrime = OptimalStrategy(computeStateNumber(p),:)
    DO iPeriod = 1, numPeriods
        !
        IF (DepthState .GT. 1) p(2:DepthState,:) = p(1:DepthState-1,:)
        p(1,:) = pPrime
        VisitedStates(iPeriod) = computeStateNumber(p)
        DO jAgent = 1, numAgents
            !
            VisitedPrices(iPeriod,jAgent) = PricesGrids(pPrime(jAgent),jAgent)
            VisitedProfits(iPeriod,jAgent) = PI(computeActionNumber(pPrime),jAgent)
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
    PostLength = iPeriod-MINVAL(MINLOC((VisitedStates(:iPeriod-1)-VisitedStates(iPeriod))**2))
    !
    AvgPostPrices = SUM(visitedPrices(iPeriod-PostLength+1:iPeriod,:),DIM = 1)/DBLE(PostLength)
    AvgPostProfits = SUM(visitedProfits(iPeriod-PostLength+1:iPeriod,:),DIM = 1)/DBLE(PostLength)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computeIndividualIR    
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
END MODULE ImpulseResponse
