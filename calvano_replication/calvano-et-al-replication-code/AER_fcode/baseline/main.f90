PROGRAM main
!
USE globals
USE LearningSimulation
USE ConvergenceResults
USE ImpulseResponse
USE EquilibriumCheck
USE QGapToMaximum
USE LearningTrajectory
USE DetailedAnalysis
USE QL_routines
USE PI_routines
USE generic_routines
!
IMPLICIT NONE
!
! Declaring variables and parameters
!
INTEGER :: iExperiment, i, iAgent
CHARACTER(len = 50) :: FileName
!
! Beginning execution
!
! Opening files
!
OPEN(UNIT = 10001,FILE = "A_InputParameters.txt")
CALL readBatchVariables(10001)
!
OPEN(UNIT = 10002,FILE = "A_res.txt")
OPEN(UNIT = 100022,FILE = "A_convResults.txt")
IF (SwitchImpulseResponseToBR .EQ. 1) OPEN(UNIT = 10003,FILE = "A_irToBR.txt")
IF (SwitchImpulseResponseToNash .GE. 1) OPEN(UNIT = 100031,FILE = "A_irToNash.txt")
IF (SwitchImpulseResponseToAll .EQ. 1) OPEN(UNIT = 100032,FILE = "A_irToAll.txt")
IF (SwitchEquilibriumCheck .EQ. 1) OPEN(UNIT = 10004,FILE = "A_ec.txt")
IF (SwitchQGapToMaximum .EQ. 1) OPEN(UNIT = 10006,FILE = "A_qg.txt")
labelStates = computeStatesCodePrint()
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Loop over models
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
DO iExperiment = 1, numExperiments
    !
    ! Read model parameters
    !
    CALL readExperimentVariables(10001)
    !
    ! When using trained Q matrices, store the name of the directory containing them
    !
    DO iAgent = 1, numAgents
        !
        IF (typeQInitialization(iAgent) .EQ. 'T') THEN
            !
            QFileFolderName(iAgent) = 'trained_Q/'
            !
        END IF
        !
    END DO
    !
    ! Creating the PI matrix
    !
    IF (typePayoffInput .EQ. 1) CALL computePIMatricesSinghVives(DemandParameters,NashPrices,CoopPrices,&
        PI,NashProfits,CoopProfits, &
        indexNashPrices,indexCoopPrices,NashMarketShares,CoopMarketShares,PricesGrids)
    IF (typePayoffInput .EQ. 2) CALL computePIMatricesLogit(DemandParameters,NashPrices,CoopPrices,&
        PI,NashProfits,CoopProfits, &
        indexNashPrices,indexCoopPrices,NashMarketShares,CoopMarketShares,PricesGrids)
    IF (typePayoffInput .EQ. 3) CALL computePIMatricesLogitMu0(DemandParameters,NashPrices,CoopPrices,&
        PI,NashProfits,CoopProfits, &
        indexNashPrices,indexCoopPrices,NashMarketShares,CoopMarketShares,PricesGrids)
    PIQ = PI**2
    avgPI = SUM(PI,DIM = 2)/numAgents
    avgPIQ = avgPI**2
    !
    ! Computing profit gains
    !
    DO iAgent = 1, numAgents
        !
        PG(:,iAgent) = (PI(:,iAgent)-NashProfits(iAgent))/(CoopProfits(iAgent)-NashProfits(iAgent))
        !
    END DO
    PGQ = PG**2
    avgPG = SUM(PG,DIM = 2)/numAgents
    avgPGQ = avgPG**2
    !
    ! Creating I/O filenames
    !
    WRITE(ExperimentNumber, "(I0.<LengthFormatTotExperimentsPrint>, A4)") codExperiment, ".txt"
    FileNameInfoExperiment = "InfoExperiment_" // ExperimentNumber
    !
    ! Print message
    !
    WRITE(*,11) iExperiment, numExperiments, numCores
11  FORMAT('model = ', I6, ' / numExperiments = ', I6, ' / numCores = ', I6)  
    !
    ! Compute QL strategy 
    !
    CALL computeExperiment(iExperiment,codExperiment,alpha,ExplorationParameters,delta)
    !
    ! Results at convergence
    ! 
    CALL ComputeConvResults(iExperiment)
    !
    ! Impulse Response analysis to one-period deviation to static best response
    ! NB: The last argument in computeIRAnalysis is "IRType", and it's crucial:
    ! IRType < 0 : One-period deviation to the price IRType
    ! IRType = 0 : One-period deviation to static BR
    ! IRType > 0 : IRType-period deviation to Nash 
    ! 
    IF (SwitchImpulseResponseToBR .EQ. 1) CALL computeIRAnalysis(iExperiment,10003,0)
    !
    ! Impulse Response to a permanent or transitory deviation to Nash prices
    !
    IF (SwitchImpulseResponseToNash .GE. 1) CALL computeIRAnalysis(iExperiment,100031,SwitchImpulseResponseToNash)
    !
    ! Impulse Response analysis to one-period deviation to all prices
    !
    IF (SwitchImpulseResponseToAll .EQ. 1) THEN
        !
        DO i = 1, numPrices
            !
            CALL computeIRAnalysis(iExperiment,100032,-i)
            !
        END DO
        !
    END IF
    !
    ! Equilibrium Check
    !
    IF (SwitchEquilibriumCheck .EQ. 1) CALL computeEqCheck(iExperiment)
    !
    ! Q Gap w.r.t. Maximum
    !
    IF (SwitchQGapToMaximum .EQ. 1) CALL computeQGapToMax(iExperiment)
    !
    ! Learning Trajectory analysis
    !
    IF (ParamsLearningTrajectory(1) .GT. 0) &
        CALL ComputeLearningTrajectory(iExperiment,codExperiment,alpha,ExplorationParameters,delta)
    !
    ! Detailed Impulse Response analysis to one-period deviation to all prices
    !
    IF (SwitchDetailedAnalysis .EQ. 1) CALL ComputeDetailedAnalysis(iExperiment)
    !    
    ! End of loop over models
    !
END DO
!
! Deallocating arrays
!
CALL closeBatch()
!
! Closing output files
!
CLOSE(UNIT = 10001)
CLOSE(UNIT = 10002)
CLOSE(UNIT = 100022)
IF (SwitchImpulseResponseToBR .EQ. 1) CLOSE(UNIT = 10003)
IF (SwitchImpulseResponseToNash .GE. 1) CLOSE(UNIT = 100031)
IF (SwitchImpulseResponseToAll .EQ. 1) CLOSE(UNIT = 100032)
IF (SwitchEquilibriumCheck .EQ. 1) CLOSE(UNIT = 10004)
IF (SwitchQGapToMaximum .EQ. 1) CLOSE(UNIT = 10006)
!
! End of execution
!
END PROGRAM main