MODULE LearningSimulation
!
USE globals
USE QL_routines
USE omp_lib
USE ifport
!
! Computes Monte Carlo Q-Learning simulations
!
IMPLICIT NONE
!
CONTAINS
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computeExperiment ( iExperiment, codExperiment, alpha, ExplorationParameters, delta )
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
    INTEGER :: iIters, iItersFix, i, j, h, l, iSession, iItersInStrategy, convergedSession, numSessionsConverged
    INTEGER :: state, statePrime, stateFix, actionPrime
    INTEGER, DIMENSION(numStates,numAgents) :: strategy, strategyPrime, strategyFix
    INTEGER :: pPrime(numAgents), p(DepthState,numAgents)
    INTEGER :: iAgent, iState
    INTEGER :: minIndexStrategies, maxIndexStrategies
    INTEGER(8) :: numSessions_I8
    REAL(8), DIMENSION(numStates,numPrices,numAgents) :: Q
    REAL(8) :: uIniPrice(DepthState,numAgents,numSessions), uExploration(2,numAgents)
    REAL(8) :: eps(numAgents)
    REAL(8) :: newq, oldq
    REAL(8) :: meanTimeToConvergence, seTimeToConvergence, medianTimeToConvergence
    CHARACTER(len = 25) :: QFileName
    CHARACTER(len = LengthFormatTotExperimentsPrint) :: codExperimentChar
    CHARACTER(len = 10) :: iSessionsChar
    CHARACTER(len = 6) :: fmt
    LOGICAL :: maskConverged(numSessions)
    !
    ! Beginning execution
    !
    ! Initializing various quantities
    !
    converged = 0    
    indexStrategies = 0
    indexLastState = 0
    timeToConvergence = 0.d0
    WRITE(codExperimentChar,'(I0.<LengthFormatTotExperimentsPrint>)') codExperiment
    IF (printQ .EQ. 1) WRITE(fmt,'(A, I1, A)') "(I0.", LengthFormatNumSessionsPrint, ")"
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! Loop over numSessions
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    !$ CALL OMP_SET_NUM_THREADS(numCores)
    !$omp parallel do &
    !$omp private(idum,iv,iy,idum2,idumQ,ivQ,iyQ,idum2Q,Q,maxValQ, &
    !$omp   strategyPrime,pPrime,p,statePrime,actionPrime,iIters,iItersFix,iItersInStrategy,convergedSession, &
    !$omp   state,stateFix,strategy,strategyFix,eps,uExploration,oldq,newq,iAgent,iState, &
    !$omp   QFileName,iSessionsChar) &
    !$omp firstprivate(numSessions,PI,delta,uIniPrice,ExplorationParameters,itersPerEpisode,alpha, &
    !$omp   itersInPerfMeasPeriod,maxIters,printQ,codExperimentChar)
    DO iSession = 1, numSessions
        !
        PRINT*, 'Session = ', iSession, ' started'
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
        iIters = 0
        iItersInStrategy = 0
        convergedSession = -1
        IF (typeExplorationMechanism .EQ. 1) eps = 1.d0
        IF (typeExplorationMechanism .EQ. 2) eps = 1.d3
        !
        DO 
            !
            ! Iterations counter
            !
            iIters = iIters+1
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
            ! Assessing convergence
            !
            IF (ALL(strategyPrime(state,:) .EQ. strategy(state,:))) THEN
                !
                iItersInStrategy = iItersInStrategy+1
                !
            ELSE
                !
                iItersInStrategy = 1
                !
            END IF
            !
            ! Check for convergence in strategy
            !
            IF (convergedSession .EQ. -1) THEN
                !
                ! Maximum number of iterations exceeded
                IF (iIters .GT. maxIters) THEN
                    !
                    convergedSession = 0
                    strategyFix = strategy
                    stateFix = state
                    iItersFix = iIters
                    !
                END IF
                !
                ! Convergence in strategy reached
                IF (iItersInStrategy .EQ. itersInPerfMeasPeriod) THEN
                    !
                    convergedSession = 1
                    strategyFix = strategy
                    stateFix = state
                    iItersFix = iIters
                    !
                END IF
                !
            END IF
            !
            ! Check for loop exit criterion
            !
            IF (convergedSession .NE. -1) EXIT
            !
            ! If no convergence yet, update and iterate
            !
            strategy(state,:) = strategyPrime(state,:)
            state = statePrime
            !
            ! End of loop over iterations
            !
        END DO          
        !
        ! Write Q matrices to file
        !
        IF (printQ .EQ. 1) THEN
            !
            ! Open Q matrices output file
            !
            !$omp critical
            WRITE(iSessionsChar,fmt) iSession
            QFileName = 'Q_' // TRIM(codExperimentChar) // '_' // TRIM(iSessionsChar) // '.txt'
            !
            ! Write on Q matrices to file
            !
            OPEN(UNIT = iSession,FILE = QFileName,RECL = 10000)
            DO iAgent = 1, numAgents
                !
                DO iState = 1, numStates
                    !
                    WRITE(iSession,*) Q(iState,:,iAgent)
                    !
                END DO
                !
            END DO
            CLOSE(UNIT = iSession)
            !$omp end critical
            !
        END IF
        !
        ! Record results at convergence
        !
        converged(iSession) = convergedSession
        timeToConvergence(iSession) = DBLE(iItersFix-itersInPerfMeasPeriod)/itersPerEpisode
        indexLastState(:,iSession) = convertNumberBase(stateFix-1,numPrices,LengthStates)
        indexStrategies(:,iSession) = computeStrategyNumber(strategyFix)
        !
        IF (convergedSession .EQ. 1) PRINT*, 'Session = ', iSession, ' converged'
        IF (convergedSession .EQ. 0) PRINT*, 'Session = ', iSession, ' did not converge'
        !
        ! End of loop over sessions
        !
    END DO
    !$omp end parallel do
    !
    ! Print InfoExperiment file
    !
    OPEN(UNIT = 996,FILE = FileNameInfoExperiment,STATUS = "REPLACE")
    DO iSession = 1, numSessions
        !
        WRITE(996,*) iSession
        WRITE(996,*) converged(iSession)
        WRITE(996,*) timeToConvergence(iSession)
        WRITE(996,*) indexLastState(:,iSession)
        DO iState = 1, numStates
            !
            WRITE(996,*) (indexStrategies((iAgent-1)*numStates+iState,iSession), iAgent = 1, numAgents)
            !
        END DO
        !
    END DO
    CLOSE(UNIT = 996)
    !
    ! Prints the RES output file
    !
    numSessionsConverged = SUM(converged)
    maskConverged = (converged .EQ. 1)
    meanNashProfit = SUM(NashProfits)/numAgents
    meanCoopProfit = SUM(CoopProfits)/numAgents
    !
    ! Time to convergence
    !
    meanTimeToConvergence = SUM(timeToConvergence,MASK = maskConverged)/numSessionsConverged
    seTimeToConvergence = &
        SQRT(SUM(timeToConvergence**2,MASK = maskConverged)/numSessionsConverged-meanTimeToConvergence**2)
    numSessions_I8 = numSessions
    CALL SORTQQ(LOC(timeToConvergence),numSessions_I8,SRT$REAL8)
    medianTimeToConvergence = timeToConvergence(NINT(0.5d0*numSessions))
    !
    ! Print output
    !
    IF (iExperiment .EQ. 1) THEN
        !
        WRITE(10002,891) &
            (i, i = 1, numAgents), &
            (i, i = 1, numExplorationParameters), &
            (i, (j, i, j = 1, numAgents), i = 1, numAgents), &
            (i, i = 1, numDemandParameters), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            (i, i = 1, numAgents), (i, i = 1, numAgents), &
            ((i, j, j = 1, numPrices), i = 1, numAgents)
891     FORMAT('Experiment ', &
            <numAgents>('    alpha', I1, ' '), &
            <numExplorationParameters>('     beta', I1, ' '), '     delta ', &
            <numAgents>('typeQini', I1, ' ', <numAgents>('par', I1, 'Qini', I1, ' ')), &
            <numDemandParameters>('  DemPar', I0.2, ' '), &
            <numAgents>('NashPrice', I1, ' '), <numAgents>('CoopPrice', I1, ' '), &
            <numAgents>('NashProft', I1, ' '), <numAgents>('CoopProft', I1, ' '), &
            <numAgents>('NashMktSh', I1, ' '), <numAgents>('CoopMktSh', I1, ' '), &
            <numAgents>(<numPrices>('Ag', I1, 'Price', I0.2, ' ')), &
            '   numConv     avgTTC      seTTC     medTTC ')
        !
    END IF
    !
    WRITE(10002,9911) codExperiment, &
        alpha, MExpl, delta, &
        (typeQInitialization(i), parQInitialization(i, :), i = 1, numAgents), &
        DemandParameters, &
        NashPrices, CoopPrices, NashProfits, CoopProfits, NashMarketShares, CoopMarketShares, &
        (PricesGrids(:,i), i = 1, numAgents), &
        numSessionsConverged, meanTimeToConvergence, seTimeToConvergence, medianTimeToConvergence
9911 FORMAT(I10, 1X, &
        <numAgents>(F10.5, 1X), <numExplorationParameters>(F10.5, 1X), F10.5, 1X, &
        <numAgents>(A9, 1X, <numAgents>(F9.2, 1X)), &
        <numDemandParameters>(F10.5, 1X), &
        <6*numAgents>(F10.5, 1X), &
        <numPrices*numAgents>(F10.7, 1X), &
        I10, 1X, <3>(F10.2, 1X))
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computeExperiment
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computePPrime ( ExplorationParameters, uExploration, strategyPrime, state, iIters, &
        pPrime, Q, eps )
    !
    ! Computes pPrime by balancing exploration vs. exploitation
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    REAL(8), INTENT(IN) :: ExplorationParameters(numExplorationParameters)
    REAL(8), INTENT(IN) :: uExploration(2,numAgents)
    INTEGER, INTENT(IN) :: strategyPrime(numStates,numAgents)
    INTEGER, INTENT(IN) :: state, iIters
    INTEGER, INTENT(OUT) :: pPrime(numAgents)
    REAL(8), INTENT(IN) :: Q(numStates,numPrices,numAgents)
    REAL(8), INTENT(INOUT) :: eps(numAgents)
    !
    ! Declaring local variables
    !
    INTEGER :: iAgent, iPrice
    REAL(8) :: u(2), maxQ
    REAL(8) :: probs(numPrices)
    !
    ! Beginning execution
    !
    ! 1. Greedy with probability 1-epsilon, with exponentially decreasing epsilon
    !
    IF (typeExplorationMechanism .EQ. 1) THEN
        !
        DO iAgent = 1, numAgents
            !
            IF (MExpl(iAgent) .LT. 0.d0) THEN
                !
                pPrime(iAgent) = strategyPrime(state,iAgent)
                !
            ELSE
                !
                u = uExploration(:,iAgent)
                IF (u(1) .LE. eps(iAgent)) THEN
                    !
                    pPrime(iAgent) = 1+INT(numPrices*u(2))
                    !
                ELSE
                    !
                    pPrime(iAgent) = strategyPrime(state,iAgent)
                    !
                END IF
                eps(iAgent) = eps(iAgent)*ExplorationParameters(iAgent)
                !
            END IF
            !
        END DO
        !
    END IF
    !
    ! 2. Boltzmann exploration
    !
    IF (typeExplorationMechanism .EQ. 2) THEN
        !
        DO iAgent = 1, numAgents
            !
            maxQ = MAXVAL(Q(state,:,iAgent))
            probs = EXP((Q(state,:,iAgent)-maxQ)/eps(iAgent))
            u = uExploration(:,iAgent)*SUM(probs)
            DO iPrice = 1, numPrices
                !
                IF (iPrice .GT. 1) probs(iPrice) = probs(iPrice)+probs(iPrice-1)
                IF (u(1) .LE. probs(iPrice)) THEN
                    !
                    pPrime(iAgent) = iPrice
                    EXIT
                    !
                END IF
                !
            END DO
            eps(iAgent) = eps(iAgent)*ExplorationParameters(iAgent)
            !
        END DO
        !
    END IF
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computePPrime
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
END MODULE LearningSimulation
