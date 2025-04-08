MODULE LearningTrajectory
!
USE globals
USE generic_routines
USE QL_routines
USE LearningSimulation
USE ImpulseResponse
USE omp_lib
USE ifport
!
! Computes Monte Carlo Q-Learning Trajectories
!
IMPLICIT NONE
!
CONTAINS
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computeLearningTrajectory ( iExperiment, codExperiment, alpha, ExplorationParameters, delta )
    !
    ! Computes statistics for one model
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: iExperiment, codExperiment
    REAL(8), DIMENSION(numAgents), INTENT(IN) :: alpha
    REAL(8), INTENT(IN) :: delta
    REAL(8), DIMENSION(numExplorationParameters) :: ExplorationParameters
    !
    ! Declaring local variable
    !
    INTEGER :: idumIP, ivIP(32), iyIP, idum2IP, idum, iv(32), iy, idum2, idumQ, ivQ(32), iyQ, idum2Q
    INTEGER :: iIters, i, j, h, l, iSession, iAgent, jAgent, iCycle, iPrice, ss0, ss1
    INTEGER, DIMENSION(DepthState,numAgents) :: p, pLT1
    INTEGER, DIMENSION(numAgents) :: pPrime, pPrimeLT1, pPrimeLT2
    INTEGER :: state, statePrime, actionPrime
    INTEGER, DIMENSION(numStates,numAgents) :: strategy, strategyPrime
    INTEGER(8) :: numSessions_I8
    INTEGER :: CycleLengthSession, CycleStatesSession(numPeriods)
    INTEGER, DIMENSION(numPeriods) :: NUVisitedStates
    INTEGER :: NUPreCycleLength, NUCycleLength
    !
    REAL(8), DIMENSION(numStates,numPrices,numAgents) :: Q
    REAL(8) :: uIniPrice(DepthState,numAgents,numSessions), uExploration(2,numAgents)
    REAL(8) :: u(2), eps(numAgents)
    REAL(8) :: newq, oldq, profitgain(numAgents)
    REAL(8), DIMENSION(ParamsLearningTrajectory(1),numSessions) :: PGmat, ICmat, IRmat
    REAL(8), DIMENSION(ParamsLearningTrajectory(1),9) :: PGss, ICss, IRss
    REAL(8) :: tmp_r(numSessions), PGsum, ICsum, IRsum
    REAL(8) :: QRowValues(numPrices), avgPRatio
    REAL(8) :: NUPIStaticBR
    !
    CHARACTER(len = 200) :: LTrajectoryFileName
    !
    ! Beginning execution
    !
    ! Reading strategies and states at convergence from file
    !
    CALL ReadInfoExperiment()
    !
    ! Initializing various quantities
    !
    PGmat = 0.d0
    PGss = 0.d0
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Loop over numSessions
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !
    !$ CALL OMP_SET_NUM_THREADS(numCores)
    !
    ! Generating uIniPrice
    !
    idumIP = -1
    idum2IP = 123456789
    ivIP = 0
    iyIP = 0
    CALL generate_uIniPrice(uIniPrice,idumIP,ivIP,iyIP,idum2IP)  
    !
    ! Starting loop over sessions
    !
    !$omp parallel do &
    !$omp private(idum,iv,iy,idum2,idumQ,ivQ,iyQ,idum2Q,Q,maxValQ, &
    !$omp   strategyPrime,pPrime,p,statePrime,actionPrime,iIters,iCycle,ss0,ss1, &
    !$omp   pLT1,pPrimeLT1,pPrimeLT2, &
    !$omp   state,strategy,eps,uExploration,u,oldq,newq,iAgent,jAgent,CycleLengthSession,CycleStatesSession, &
    !$omp   NUVisitedStates,NUPreCycleLength,NUCycleLength,NUPIStaticBR, &
    !$omp   PGsum,QRowValues,ICsum,avgPRatio,IRsum) &
    !$omp firstprivate(numSessions,PI,PG,delta,uIniPrice,ExplorationParameters,itersPerEpisode,alpha, &
    !$omp   itersInPerfMeasPeriod,maxIters,profitgain)
    DO iSession = 1, numSessions
        !
        PRINT*, 'Session = ', iSession, ' started'
        CycleLengthSession = CycleLength(iSession)
        CycleStatesSession = 0
        CycleStatesSession(1:CycleLengthSession) = CycleStates(:,iSession)
        !
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ! Learning phase
        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !
        ! Initializing random number generators
        !
        idum = -iSession
        idum2 = 123456789
        iv = 0
        iy = 0
        !
        idumQ = -iSession
        idum2Q = 123456789
        ivQ = 0
        iyQ = 0
        !
        ! Initializing Q matrices
        !
        !$omp critical
        CALL initQMatrices(iSession,idumQ,ivQ,iyQ,idum2Q,PI,delta,Q,maxValQ,strategyPrime)
        !$omp end critical
        strategy = strategyPrime
        !
        ! Randomly initializing prices and state
        !
        CALL initState(uIniPrice(:,:,iSession),p,statePrime,actionPrime)
        state = statePrime
        !
        ! Loop
        !
        IF (typeExplorationMechanism .EQ. 1) eps = 1.d0
        IF (typeExplorationMechanism .EQ. 2) eps = 1.d3
        PGsum = 0.d0
        ICsum = 0.d0
        IRsum = 0.d0
        DO iIters = 1, ParamsLearningTrajectory(1)*ParamsLearningTrajectory(2)
            !
            ! Generating exploration random numbers
            !            
            CALL generateUExploration(uExploration,idum,iv,iy,idum2)  
            !
            ! Compute pPrime by balancing exploration vs. exploitation
            !
            CALL computePPrime(ExplorationParameters,uExploration,strategyPrime,state,iIters,pPrime,Q,eps)
            !
            ! Defining the new state
            !
            IF (DepthState .GT. 1) p(2:DepthState,:) = p(1:DepthState-1,:)
            p(1,:) = pPrime
            statePrime = computeStateNumber(p)
            actionPrime = computeActionNumber(pPrime)
            !
            ! Store LTrajectories
            !
            ! Profit Gain
            profitgain = PG(actionPrime,:)
            PGsum = PGsum+SUM(profitgain)/DBLE(numAgents)
            !
            IF (MOD(iIters,ParamsLearningTrajectory(2)) .EQ. 0) THEN
                !
                ! Profit Gain
                PGmat(iIters/ParamsLearningTrajectory(2),iSession) = &
                    PGmat(iIters/ParamsLearningTrajectory(2),iSession)+PGsum/DBLE(ParamsLearningTrajectory(2))
                PGsum = 0.d0
                !
                ! Incentive Compatibility
                DO iCycle = 1, CycleLengthSession      ! Loop over states in path at convergence
                    !
                    ss1 = CycleStatesSession(iCycle)
                    DO iAgent = 1, numAgents        ! Loop over agents
                        !
                        DO iPrice = 1, numPrices    ! Loop over prices
                            !
                            ! Compute row of true Q matrix
                            CALL computeQCell(strategy,ss1,iPrice,iAgent,delta, &
                                QRowValues(iPrice),NUVisitedStates,NUPreCycleLength,NUCycleLength)
                            !
                        END DO
                        IF (AreEqualReals(MAXVAL(QRowValues),QrowValues(strategy(ss1,iAgent)))) &
                            ICsum = ICsum+1.d0/DBLE(numAgents*CycleLengthSession)
                        !
                    END DO
                    !
                END DO
                ICmat(iIters/ParamsLearningTrajectory(2),iSession) = ICsum
                ICsum = 0.d0
                !
                ! Punishment from nondeviating agent in period t+2
                DO iCycle = 1, CycleLengthSession      ! Loop over states in path at convergence
                    !
                    DO iAgent = 1, numAgents        ! Loop over agents
                        !
                        ! Period t: Initial state
                        ss0 = CycleStatesSession(iCycle)   
                        pLT1 = RESHAPE(convertNumberBase(ss0-1,numPrices,LengthStates),(/ DepthState,numAgents /))
                        !
                        ! Period t+1: Shock to static best response
                        pPrimeLT1 = strategy(ss0,:)
                        IF (DepthState .GT. 1) pLT1(2:DepthState,:) = pLT1(1:DepthState-1,:)
                        pLT1(1,:) = pPrimeLT1
                        CALL ComputeStaticBestResponse(strategy,ss0,iAgent,pLT1(1,iAgent),NUPIStaticBR)
                        ss1 = computeStateNumber(pLT1)
                        !
                        ! Period t+2
                        pPrimeLT2 = strategy(ss1,:)
                        avgPRatio = 0.d0
                        DO jAgent = 1, numAgents
                            !
                            IF (jAgent .EQ. iAgent) CYCLE
                            avgPRatio = avgPRatio+PricesGrids(pPrimeLT2(jAgent),jAgent)/PricesGrids(pPrimeLT1(jAgent),jAgent)
                            !
                        END DO
                        avgPRatio = avgPRatio/DBLE(numAgents-1)
                        IRsum = IRsum+avgPRatio/DBLE(numAgents*CycleLengthSession)                    
                        !
                    END DO
                    !
                END DO
                IRmat(iIters/ParamsLearningTrajectory(2),iSession) = IRsum
                IRsum = 0.d0
                !
            END IF
            !
            ! Each agent collects his payoff and updates
            !
            DO iAgent = 1, numAgents
                !
                ! Q matrices and strategies update
                !
                oldq = Q(state,pPrime(iAgent),iAgent)
                newq = oldq+alpha(iAgent)*(PI(actionPrime,iAgent)+delta*maxValQ(statePrime,iAgent)-oldq)
                Q(state,pPrime(iAgent),iAgent) = newq
                IF (newq .GT. maxValQ(state,iAgent)) THEN
                    !
                    maxValQ(state,iAgent) = newq
                    IF (strategyPrime(state,iAgent) .NE. pPrime(iAgent)) strategyPrime(state,iAgent) = pPrime(iAgent)
                    !
                END IF
                IF ((newq .LT. maxValQ(state,iAgent)) .AND. (strategyPrime(state,iAgent) .EQ. pPrime(iAgent))) THEN
                    !
                    CALL MaxLocBreakTies(numPrices,Q(state,:,iAgent),idumQ,ivQ,iyQ,idum2Q, &
                        maxValQ(state,iAgent),strategyPrime(state,iAgent))
                    !
                END IF
                !
            END DO
            !
            ! Update and iterate
            !
            strategy(state,:) = strategyPrime(state,:)
            state = statePrime
            !
            ! End of loop over iterations
            !
        END DO
        !
        ! End of loop over sessions
        !
    END DO
    !$omp end parallel do
    !
    ! Compute statistics on PG and IC trajectories
    !
    PGss = ComputeRowSummaryStatistics(ParamsLearningTrajectory(1),numSessions,PGmat)
    ICss = ComputeRowSummaryStatistics(ParamsLearningTrajectory(1),numSessions,ICmat)
    IRss = ComputeRowSummaryStatistics(ParamsLearningTrajectory(1),numSessions,IRmat)
    !
    ! File name
    !
    LTrajectoryFileName = 'LTrajectories_' // ExperimentNumber 
    OPEN(UNIT = 9,FILE = LTrajectoryFileName,STATUS = "REPLACE")
    WRITE(9,8) 
8   FORMAT('         iter ', &
        '          AvgPG            SdPG           MinPG         q0025PG          q025PG           q05PG          q075PG         q0975PG           MaxPG ', &
        '          AvgIC            SdIC           MinIC         q0025IC          q025IC           q05IC          q075IC         q0975IC           MaxIC ', &
        '          AvgIR            SdIR           MinIR         q0025IR          q025IR           q05IR          q075IR         q0975IR           MaxIR ')
    DO i = 1, ParamsLearningTrajectory(1)
        !
        WRITE(9,9) i*ParamsLearningTrajectory(2), &
            PGss(i,:), ICss(i,:), IRss(i,:)
        !
    END DO
9   FORMAT(1X, I12, 1X, <9>(F15.5, 1X), <9>(F15.5, 1X), <9>(F15.5, 1X))
    CLOSE(UNIT = 9)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computeLearningTrajectory
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
END MODULE LearningTrajectory
