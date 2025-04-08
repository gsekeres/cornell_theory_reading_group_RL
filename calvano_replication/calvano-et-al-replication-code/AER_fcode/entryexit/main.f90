PROGRAM main
!
USE globals
USE generic_routines
USE QL_routines
USE PI_routines
USE LearningSimulation
USE ConvergenceResults
USE DetailedAnalysis
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
    IF (typePayoffInput .EQ. 2) CALL computePIMatricesLogit()
    PIQ = PI**2
    avgPI = SUM(PI,DIM = 2)/numAgents
    avgPIQ = avgPI**2
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
!
! End of execution
!
END PROGRAM main