MODULE QGapToMaximum
!
USE globals
USE QL_routines
USE generic_routines
USE EquilibriumCheck
!
! Computes gap in Q function values w.r.t. maximum
!
IMPLICIT NONE
!
CONTAINS
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computeQGapToMaxSession ( OptimalStrategy, CycleLength, CycleStates, &
        QGapTot, QGapOnPath, QGapNotOnPath, QGapNotBRAllStates, &
        QGapNotBRonPath, QGapNotEqAllStates, QGapNotEqonPath )
    !
    ! Computes Q gap w.r.t. maximum by state for an individual replication
    !
    ! INPUT:
    !
    ! - OptimalStrategy     : strategy for all agents
    ! - CycleLength         : length of the replication's equilibrium path (i.e., state cycle)
    ! - CycleStates         : replication's equilibrium path (i.e., state cycle)
    !
    ! OUTPUT:
    !
    ! - QGapTot             : Average Q gap over all states 
    ! - QGapOnPath          : Average Q gap over cycle states
    ! - QGapNotOnPath       : Average Q gap over non-cycle states
    ! - QGapNotBRAllStates  : Average Q gap over non-best responding states
    ! - QGapNotBRonPath     : Average Q gap over non-best responding, non-cycle states
    ! - QGapNotEqAllStates  : Average Q gap over non-equilibrium states
    ! - QGapNotEqonPath     : Average Q gap over non-equilibrium cycle states
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: OptimalStrategy(numStates,numAgents)
    INTEGER, INTENT(IN) :: CycleLength
    INTEGER, INTENT(IN) :: CycleStates(CycleLength)
    REAL(8), DIMENSION(0:numAgents), INTENT(OUT) :: QGapTot, QGapOnPath, QGapNotOnPath, QGapNotBRAllStates, &
        QGapNotBRonPath, QGapNotEqAllStates, QGapNotEqonPath
    !
    ! Declaring local variables
    !
    INTEGER :: iState, iAgent, iPrice
    INTEGER :: CellVisitedStates(numPeriods), CellPreCycleLength, CellCycleLength
    REAL(8), DIMENSION(numStates,numPrices,numAgents) :: QTrue
    REAL(8), DIMENSION(numStates,numAgents) :: MaxQTrue, QGap
    LOGICAL, DIMENSION(numStates,numAgents) :: IsOnPath, IsNotOnPath, &
        IsNotBRAllStates, IsNotBROnPath, IsNotEqAllStates, IsNotEqOnPath
    LOGICAL, DIMENSION(numAgents) :: IsBR
    !
    ! Beginning execution
    !
    ! 1. Compute true Q for the optimal strategy for all agents, in all states and actions
    !
    QTrue = 0.d0
    MaxQTrue = 0.d0
    QGap = 0.d0
    !
    DO iState = 1, numStates                ! Start of loop over states
        !
        DO iAgent = 1, numAgents            ! Start of loop over agents
            !
            DO iPrice = 1, numPrices        ! Start of loop over prices
                !
                ! Compute state value function of agent iAgent for the optimal strategy in (iState,iPrice)
                !
                CALL computeQCell(OptimalStrategy,iState,iPrice,iAgent,delta, &
                    QTrue(iState,iPrice,iAgent),CellVisitedStates,CellPreCycleLength,CellCycleLength)
                !
            END DO                          ! End of loop over prices
            !
            ! Compute gap in Q function values w.r.t. maximum
            !
            MaxQTrue(iState,iAgent) = MAXVAL(QTrue(iState,:,iAgent))
            QGap(iState,iAgent) = &
                (MaxQTrue(iState,iAgent)-QTrue(iState,OptimalStrategy(iState,iAgent),iAgent))/ABS(MaxQTrue(iState,iAgent))
            !
        END DO                              ! End of loop over agents
        !
    END DO                                  ! End of loop over initial states
    !
    ! 2. Compute mask matrices
    !
    IsOnPath = .FALSE.
    IsNotOnPath = .FALSE.
    IsNotBRAllStates = .FALSE.
    IsNotBROnPath = .FALSE.
    IsNotEqAllStates = .FALSE.
    IsNotEqOnPath = .FALSE.
    !
    DO iState = 1, numStates                ! Start of loop over states
        !
        IF (ANY(CycleStates .EQ. iState)) IsOnPath(iState,:) = .TRUE.
        IF (ALL(CycleStates .NE. iState)) IsNotOnPath(iState,:) = .TRUE.
        !
        IsBR = .FALSE.
        DO iAgent = 1, numAgents            ! Start of loop over agents
            !
            IF (AreEqualReals(QTrue(iState,OptimalStrategy(iState,iAgent),iAgent),MaxQTrue(iState,iAgent))) THEN
                !
                IsBR(iAgent) = .TRUE.
                !
            ELSE
                !
                IsNotBRAllStates(iState,iAgent) = .TRUE.
                IF (ANY(CycleStates .EQ. iState)) IsNotBROnPath(iState,iAgent) = .TRUE.
                !
            END IF
            !
        END DO
        !
        DO iAgent = 1, numAgents            ! Start of loop over agents
            !
            IF (NOT(ALL(IsBR))) THEN
                !
                IsNotEqAllStates(iState,iAgent) = .TRUE.
                IF (ANY(CycleStates .EQ. iState)) IsNotEqOnPath(iState,iAgent) = .TRUE.
                !
            END IF
            !
        END DO                          ! End of loop over agents
        !
    END DO                              ! End of loop over states
    !
    ! 3. Compute Q gap averages over subsets of states
    !
    QGapTot(0) = SUM(QGap)/DBLE(numAgents*numStates)
    QGapOnPath(0) = SUM(QGap,MASK = IsOnPath)/DBLE(COUNT(IsOnPath))
    QGapNotOnPath(0) = SUM(QGap,MASK = IsNotOnPath)/DBLE(COUNT(IsNotOnPath))
    QGapNotBRAllStates(0) = SUM(QGap,MASK = IsNotBRAllStates)/DBLE(COUNT(IsNotBRAllStates))
    QGapNotBRonPath(0) = SUM(QGap,MASK = IsNotBRonPath)/DBLE(COUNT(IsNotBRonPath))
    QGapNotEqAllStates(0) = SUM(QGap,MASK = IsNotEqAllStates)/DBLE(COUNT(IsNotEqAllStates))
    QGapNotEqonPath(0) = SUM(QGap,MASK = IsNotEqonPath)/DBLE(COUNT(IsNotEqonPath))
    !
    DO iAgent = 1, numAgents
        !
        QGapTot(iAgent) = SUM(QGap(:,iAgent))/DBLE(numStates)
        QGapOnPath(iAgent) = SUM(QGap(:,iAgent),MASK = IsOnPath(:,iAgent))/DBLE(COUNT(IsOnPath(:,iAgent)))
        QGapNotOnPath(iAgent) = SUM(QGap(:,iAgent),MASK = IsNotOnPath(:,iAgent))/DBLE(COUNT(IsNotOnPath(:,iAgent)))
        QGapNotBRAllStates(iAgent) = SUM(QGap(:,iAgent),MASK = IsNotBRAllStates(:,iAgent))/DBLE(COUNT(IsNotBRAllStates(:,iAgent)))
        QGapNotBRonPath(iAgent) = SUM(QGap(:,iAgent),MASK = IsNotBRonPath(:,iAgent))/DBLE(COUNT(IsNotBRonPath(:,iAgent)))
        QGapNotEqAllStates(iAgent) = SUM(QGap(:,iAgent),MASK = IsNotEqAllStates(:,iAgent))/DBLE(COUNT(IsNotEqAllStates(:,iAgent)))
        QGapNotEqonPath(iAgent) = SUM(QGap(:,iAgent),MASK = IsNotEqonPath(:,iAgent))/DBLE(COUNT(IsNotEqonPath(:,iAgent)))
        !
    END DO
    !
    WHERE (ISNAN(QGapTot)) QGapTot = -999.999d0
    WHERE (ISNAN(QGapOnPath)) QGapOnPath = -999.999d0
    WHERE (ISNAN(QGapNotOnPath)) QGapNotOnPath = -999.999d0
    WHERE (ISNAN(QGapNotBRAllStates)) QGapNotBRAllStates = -999.999d0
    WHERE (ISNAN(QGapNotBRonPath)) QGapNotBRonPath = -999.999d0
    WHERE (ISNAN(QGapNotEqAllStates)) QGapNotEqAllStates = -999.999d0
    WHERE (ISNAN(QGapNotEqonPath)) QGapNotEqonPath = -999.999d0
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computeQGapToMaxSession
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
END MODULE QGapToMaximum
