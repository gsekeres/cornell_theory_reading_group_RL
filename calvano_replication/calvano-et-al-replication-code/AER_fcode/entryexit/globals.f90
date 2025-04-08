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
    typeExplorationMechanism, LengthStates, numStates, lengthStrategies, &
    LengthFormatStatesPrint, LengthFormatActionPrint, LengthFormatTotExperimentsPrint, &
    typePayoffInput, numAgents, numActions, numDemandParameters, numPeriods, &
    numExplorationParameters, SwitchDetailedAnalysis
REAL(8) :: delta, PerfMeasPeriodLength, &
    meanNashProfitIn, meanCoopProfitIn, meanNashProfitOut, meanCoopProfitOut, &
    gammaSinghVives
CHARACTER(len = 50) :: ExperimentNumber, FileNameInfoExperiment
!
INTEGER, ALLOCATABLE :: converged(:), indexStrategies(:,:), indexLastState(:,:), CycleLength(:), &
    CycleStates(:,:), CyclePrices(:,:,:), &
    indexActions(:,:), cStates(:), cActions(:), &
    indexNashPrices(:), indexCoopPrices(:)
REAL(8), ALLOCATABLE :: timeToConvergence(:), CycleProfits(:,:,:), &
    NashProfitsIn(:), CoopProfitsIn(:), NashProfitsOut(:), CoopProfitsOut(:), &
    maxValQ(:,:), NashPricesIn(:), CoopPricesIn(:), NashPricesOut(:), CoopPricesOut(:), &
    PI(:,:), PIQ(:,:), avgPI(:), avgPIQ(:), &
    alpha(:), DiscountFactors(:), & 
    DemandParameters(:), MExpl(:), ExplorationParameters(:), &
    NashMarketSharesIn(:), CoopMarketSharesIn(:), NashMarketSharesOut(:), CoopMarketSharesOut(:), &
    PricesGrids(:,:), parQInitialization(:,:)
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
    READ(unitNumber,*) numPrices
    !
    ! Global variables
    !
    LengthFormatTotExperimentsPrint = 1+INT(LOG10(DBLE(totExperiments)))
    maxIters = maxNumEpisodes*itersPerEpisode
    itersInPerfMeasPeriod = INT(PerfMeasPeriodLength*itersPerEpisode)
    LengthStates = numAgents
    LengthFormatStatesPrint = LengthStates*(1+FLOOR(LOG10(DBLE(numPrices))))+LengthStates-1
    numStates = numPrices**LengthStates
    numPeriods = numStates+1
    numActions = numPrices**LengthStates    
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
    IF (typePayoffInput .EQ. 2) numDemandParameters = 2*numAgents+4+2   ! a0, ai, ci, mu, extend, entryprob, exitprob
    READ(unitNumber,'(1X)')
    !
    ! Continue reading input settings
    !
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
        alpha(numAgents), &
        NashProfitsIn(numAgents),CoopProfitsIn(numAgents), &
        NashProfitsOut(numAgents),CoopProfitsOut(numAgents), &
        PI(numActions,numAgents),PIQ(numActions,numAgents),avgPI(numActions),avgPIQ(numActions), &
        indexNashPrices(numAgents),indexCoopPrices(numAgents), &
        NashPricesIn(numAgents),CoopPricesIn(numAgents), &
        NashPricesOut(numAgents),CoopPricesOut(numAgents), &
        typeQInitialization(numAgents),parQInitialization(numAgents,numAgents), &
        NashMarketSharesIn(numAgents),CoopMarketSharesIn(numAgents), &
        NashMarketSharesOut(numAgents),CoopMarketSharesOut(numAgents), &
        PricesGrids(numPrices,numAgents))
    ALLOCATE(CHARACTER(len = 3+LengthFormatStatesPrint) :: labelStates(numStates))
    ALLOCATE(CHARACTER(len = 200) :: QFileFolderName(numAgents))
    !
    cStates = (/ (numPrices**i, i = LengthStates-1, 0, -1) /)
    cActions = (/ (numPrices**i, i = numAgents-1, 0, -1) /)
    !
    ! Actions contain the most recent prices of all agents
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
        labelStates,NashProfitsIn,CoopProfitsIn,NashProfitsOut,CoopProfitsOut, &
        NashPricesIn,CoopPricesIn,NashPricesOut,CoopPricesOut, &
        QFileFolderName,DiscountFactors, &
        alpha,MExpl,ExplorationParameters, &
        DemandParameters,PI,PIQ,avgPI,avgPIQ, &
        indexNashPrices,indexCoopPrices, &
        NashMarketSharesIn,CoopMarketSharesIn,NashMarketSharesOut,CoopMarketSharesOut,  &
        PricesGrids,typeQInitialization,parQInitialization)
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
        DemandParameters, NashPricesIn, NashPricesOut, CoopPricesIn, CoopPricesOut, &
        (typeQInitialization(i), parQInitialization(i,:), i = 1, numAgents)
    !
    ExplorationParameters = EXP(-MExpl/DBLE(itersPerEpisode))
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