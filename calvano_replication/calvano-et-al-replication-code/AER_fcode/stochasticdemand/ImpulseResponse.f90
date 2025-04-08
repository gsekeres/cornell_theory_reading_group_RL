MODULE ImpulseResponse
!
USE globals
USE QL_routines
!
! Computes Impulse Response analysis 
!
IMPLICIT NONE
!
CONTAINS
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
        selProfits(iPrice) = ExpectedPI(computeActionNumber(pPrime),iAgent)
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
    INTEGER :: p(numAgents), pPrime(numAgents) 
    INTEGER :: VisitedStates(MAX(DevObsLength,numPeriods))
    INTEGER :: indexShockState(LengthStates)
    !
    REAL(8), DIMENSION(numPeriods,numAgents) :: visitedPrices, VisitedProfits
    !
    LOGICAL :: FlagReturnedToState
    !
    ! Beginning execution
    !
    p = convertNumberBase(InitialState-1,numPrices,numAgents)
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
        p = pPrime
        VisitedStates(iPeriod) = computeStateNumber(p)
        DO jAgent = 1, numAgents
            !
            IF (iPeriod .LE. DevObsLength) THEN
                !
                ShockStates(iPeriod) = VisitedStates(iPeriod)
                ShockIndPrices(iPeriod,jAgent) = pPrime(jAgent)
                ShockPrices(iPeriod,jAgent) = PricesGrids(pPrime(jAgent),jAgent)
                ShockProfits(iPeriod,jAgent) = ExpectedPI(computeActionNumber(pPrime),jAgent)
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
    p = indexShockState
    pPrime = OptimalStrategy(computeStateNumber(p),:)
    DO iPeriod = 1, numPeriods
        !
        p = pPrime
        VisitedStates(iPeriod) = computeStateNumber(p)
        DO jAgent = 1, numAgents
            !
            VisitedPrices(iPeriod,jAgent) = PricesGrids(pPrime(jAgent),jAgent)
            VisitedProfits(iPeriod,jAgent) = ExpectedPI(computeActionNumber(pPrime),jAgent)
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
