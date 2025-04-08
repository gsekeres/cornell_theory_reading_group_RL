MODULE globals
!
USE generic_routines
!
! Declare global parameters and variables 
!
IMPLICIT NONE
!
! Parameters
!
INTEGER, PARAMETER :: numShockPeriodsPrint = 10
INTEGER, PARAMETER :: numThresCycleLength = 10
!
! Variables
!
INTEGER :: numExperiments, totExperiments, numCores, numSessions, itersPerEpisode, maxNumEpisodes, maxIters, &
    itersInPerfMeasPeriod, printQ, codExperiment, numPrices, &
    typeExplorationMechanism, DepthState0, DepthState, LengthStates, numStates, lengthStrategies, &
    LengthFormatStatesPrint, LengthFormatActionPrint, LengthFormatTotExperimentsPrint,LengthFormatNumSessionsPrint, &
    typePayoffInput, numAgents, numActions, numDemandParameters, numPeriods, &
    numExplorationParameters, &
    SwitchImpulseResponseToBR, SwitchImpulseResponseToNash, SwitchImpulseResponseToAll, &
    SwitchEquilibriumCheck, SwitchQGapToMaximum, ParamsLearningTrajectory(2), &
    SwitchDetailedAnalysis
REAL(8) :: delta, PerfMeasPeriodLength, meanNashProfit, meanCoopProfit, gammaSinghVives
CHARACTER(len = 50) :: ExperimentNumber, FileNameInfoExperiment
!
INTEGER, ALLOCATABLE :: converged(:), indexStrategies(:,:), indexLastState(:,:), CycleLength(:), &
    CycleStates(:,:), CyclePrices(:,:,:), &
    indexActions(:,:), cStates(:), cActions(:), &
    indexNashPrices(:), indexCoopPrices(:)
REAL(8), ALLOCATABLE :: timeToConvergence(:), CycleProfits(:,:,:), &
    NashProfits(:), CoopProfits(:), maxValQ(:,:), NashPrices(:), CoopPrices(:), &
    PI(:,:), PIQ(:,:), avgPI(:), avgPIQ(:), &
    PG(:,:), PGQ(:,:), avgPG(:), avgPGQ(:), &
    alpha(:), DiscountFactors(:), & 
    DemandParameters(:), MExpl(:), ExplorationParameters(:), &
    NashMarketShares(:), CoopMarketShares(:), PricesGrids(:,:), &
    parQInitialization(:,:)
CHARACTER(len = :), ALLOCATABLE :: labelStates(:)
CHARACTER(len = :), ALLOCATABLE :: QFileFolderName(:)
CHARACTER(len = 1), ALLOCATABLE :: typeQInitialization(:)
!
CONTAINS
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE readBatchVariables ( unitNumber )
    !
    ! Reads input variables
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: unitNumber
    !
    ! Declaring local variables
    !
    INTEGER :: i, iAgent, iPrices, jPrices, iState, iAction, jState
    INTEGER, ALLOCATABLE :: switchedState(:), indA1(:), indA2(:)
    !
    ! Beginning execution
    !
    READ(unitNumber,'(1X)') 
    READ(unitNumber,*) numExperiments, totExperiments
    READ(unitNumber,'(1X)') 
    READ(unitNumber,*) numCores
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) numSessions
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) itersPerEpisode
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) maxNumEpisodes
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) PerfMeasPeriodLength
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) numAgents
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) DepthState0
    DepthState = MAX(1,DepthState0)          ! Accomodates the DepthState = 0 case
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) numPrices
    !
    ! Global variables
    !
    LengthFormatTotExperimentsPrint = 1+INT(LOG10(DBLE(totExperiments)))
    LengthFormatNumSessionsPrint = 1+INT(LOG10(DBLE(numSessions)))
    maxIters = maxNumEpisodes*itersPerEpisode
    itersInPerfMeasPeriod = INT(PerfMeasPeriodLength*itersPerEpisode)
    LengthStates = MAX(1,numAgents*DepthState0)
    LengthFormatStatesPrint = LengthStates*(1+FLOOR(LOG10(DBLE(numPrices))))+LengthStates-1
    numStates = numPrices**(numAgents*DepthState0)
    numPeriods = numStates+1
    numActions = numPrices**numAgents   ! Actions contain combinations of prices;
                                        ! they coincide with states when DepthState == 1
    lengthStrategies = numAgents*numStates
    lengthFormatActionPrint = FLOOR(LOG10(DBLE(numPrices)))+1
    READ(unitNumber,'(1X)')
    !
    ! Read type of exploration mechanism
    !
    READ(unitNumber,*) typeExplorationMechanism
    numExplorationParameters = numAgents           
    READ(unitNumber,'(1X)')
    !
    ! Read type of payoff input
    !
    READ(unitNumber,*) typePayoffInput
    IF (typePayoffInput .EQ. 1) numDemandParameters = 1+2                       ! gamma, extend
    IF (typePayoffInput .EQ. 2) numDemandParameters = 2*numAgents+4             ! a0, ai, ci, mu, extend
    IF (typePayoffInput .EQ. 3) numDemandParameters = 2*numAgents+4             ! a0, ai, ci, mu = 0, extend
    READ(unitNumber,'(1X)')
    !
    ! Continue reading input settings
    !
    READ(unitNumber,*) SwitchImpulseResponseToBR
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) SwitchImpulseResponseToNash
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) SwitchImpulseResponseToAll
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) SwitchEquilibriumCheck
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) SwitchQGapToMaximum
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) ParamsLearningTrajectory
    READ(unitNumber,'(1X)')
    READ(unitNumber,*) SwitchDetailedAnalysis
    READ(unitNumber,'(1X)')
    !
    ! Allocating matrices and vectors
    !
    ALLOCATE(converged(numSessions),timeToConvergence(numSessions), &
        indexStrategies(lengthStrategies,numSessions),indexLastState(lengthStates,numSessions), &
        CycleLength(numSessions),CycleStates(numPeriods,numSessions), &
        CyclePrices(numAgents,numPeriods,numSessions),CycleProfits(numAgents,numPeriods,numSessions), &
        indexActions(numActions,numAgents), &
        cStates(LengthStates),cActions(numAgents),DiscountFactors(0:numStates), &
        maxValQ(numStates,numAgents), DemandParameters(numDemandParameters), &
        ExplorationParameters(numExplorationParameters), MExpl(numExplorationParameters), &
        alpha(numAgents),NashProfits(numAgents),CoopProfits(numAgents), &
        PI(numActions,numAgents),PIQ(numActions,numAgents),avgPI(numActions),avgPIQ(numActions), &
        PG(numActions,numAgents),PGQ(numActions,numAgents),avgPG(numActions),avgPGQ(numActions), &
        indexNashPrices(numAgents),indexCoopPrices(numAgents), &
        NashPrices(numAgents),CoopPrices(numAgents), &
        typeQInitialization(numAgents),parQInitialization(numAgents,numAgents), &
        NashMarketShares(numAgents),CoopMarketShares(numAgents),PricesGrids(numPrices,numAgents))
    ALLOCATE(CHARACTER(len = 3+LengthFormatStatesPrint) :: labelStates(numStates))
    ALLOCATE(CHARACTER(len = 200) :: QFileFolderName(numAgents))
    !
    cStates = (/ (numPrices**i, i = LengthStates-1, 0, -1) /)
    cActions = (/ (numPrices**i, i = numAgents-1, 0, -1) /)
    !
    ! Actions contain the most recent prices of all agents. Actions and States
    ! coincide when DepthState == 1
    !
    DO iAction = 1, numActions
        !
        indexActions(iAction,:) = convertNumberBase(iAction-1,numPrices,numAgents)
        !
    END DO
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE readBatchVariables
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE closeBatch ( )
    !
    ! Reads input variables
    !
    IMPLICIT NONE
    !
    ! Beginning execution
    !
    DEALLOCATE(converged,timeToConvergence,indexStrategies,indexLastState, &
        CycleLength,CycleStates,CyclePrices,CycleProfits, &
        indexActions,cStates,cActions,maxValQ, &
        labelStates,NashProfits,CoopProfits,QFileFolderName,DiscountFactors, &
        alpha,MExpl,ExplorationParameters,NashPrices,CoopPrices, &
        DemandParameters,PI,PIQ,avgPI,avgPIQ,PG,PGQ,avgPG,avgPGQ, &
        indexNashPrices,indexCoopPrices,NashMarketShares,CoopMarketShares,PricesGrids, &
        typeQInitialization,parQInitialization)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE closeBatch
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE readExperimentVariables ( unitNumber )
    !
    ! Reads input variables
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: unitNumber
    !
    ! Declaring local variables
    !
    INTEGER :: i
    !
    ! Beginning execution
    !
    READ(unitNumber,*) codExperiment, printQ, alpha, MExpl, delta, &
        DemandParameters, NashPrices, CoopPrices, &
        (typeQInitialization(i), parQInitialization(i,:), i = 1, numAgents)
    IF (typeExplorationMechanism .EQ. 1) THEN
        !
        ExplorationParameters = EXP(-MExpl/DBLE(itersPerEpisode))
        !
    ELSE IF (typeExplorationMechanism .EQ. 2) THEN
        !
        ExplorationParameters = 1.d0-1.d1**MExpl
        !
    END IF
    !
    ! Sanity check: with memoryless agents, only delta = 0 makes sense
    ! If necessary, any value of delta provided on input is overridden
    !
    IF (DepthState0 .EQ. 0) delta = 0.d0
    !
    ! Compute array of discount factors
    !
    DiscountFactors = (/ (delta**i, i = 0, numPeriods-1) /)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE readExperimentVariables
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
END MODULE globals