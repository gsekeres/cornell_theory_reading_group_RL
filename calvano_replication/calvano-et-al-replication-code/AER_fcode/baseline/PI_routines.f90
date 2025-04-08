MODULE PI_routines
!
USE globals
USE generic_routines
USE QL_routines
!
! Various routines used to compute PI matrices at runtime
!
IMPLICIT NONE
!
CONTAINS
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computePIMatricesSinghVives ( DemandParameters, NashPrices, CoopPrices, &
        PI, NashProfits, CoopProfits, &
        indexNashPrices, indexCoopPrices, NashMarketShares, CoopMarketShares, &
        PricesGrids )
    !
    ! Computes the Singh&Vives common payoff matrix PI
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    REAL(8), INTENT(IN) :: DemandParameters(numDemandParameters)
    REAL(8), DIMENSION(numAgents), INTENT(IN) :: NashPrices, CoopPrices
    REAL(8), INTENT(OUT) :: PI(numActions,numAgents)
    REAL(8), DIMENSION(numAgents), INTENT(OUT) :: NashProfits, CoopProfits, &
        NashMarketShares, CoopMarketShares
    INTEGER, DIMENSION(numAgents), INTENT(OUT) :: indexNashPrices, indexCoopPrices
    REAL(8), DIMENSION(numPrices,numAgents), INTENT(OUT) :: PricesGrids
    !
    ! Declaring local variables
    !
    REAL(8) :: gamma, extend(2), objFct
    REAL(8), DIMENSION(numAgents) :: a, c, d, stepPrices, prices
    INTEGER :: i, j, iter, iAgent
    !
    ! Beginning execution
    !
    ! Computing PI matrices
    !
    ! Extract demand parameters
    !
    gamma = DemandParameters(1)
    extend = DemandParameters(2:3)
    !
    ! 1. Compute repeated Nash profits
    !
    NashMarketShares = linearDemands(gamma,NashPrices)
    NashProfits = NashPrices*NashMarketShares
    !
    ! 2. Compute cooperation profits
    !
    CoopMarketShares = linearDemands(gamma,CoopPrices)
    CoopProfits = CoopPrices*CoopMarketShares
    !
    ! 3. Compute price grid
    !
    ! Upper and lower bounds
    !
    IF (ALL(extend .GT. 0.d0)) THEN
        !
        ! Lower bound = pNash - extend(1)*(pCoop - pNash)
        ! Upper bound = pCoop + extend(2)*(pCoop - pNash)
        !
        DO iAgent = 1, numAgents
            !
            PricesGrids(1,iAgent) = MAX(0.d0,NashPrices(iAgent)-extend(1)*(CoopPrices(iAgent)-NashPrices(iAgent)))
            PricesGrids(numPrices,iAgent) = MAX(0.d0,CoopPrices(iAgent)+extend(2)*(CoopPrices(iAgent)-NashPrices(iAgent)))
            !
        END DO
        !
    ELSE IF ((extend(1) .LT. 0.d0) .AND. (extend(2) .GE. -EPSILON(extend(2)))) THEN
        !
        ! Lower bound = 0
        ! Upper bound = (1+extend(2))*pCoop
        !
        DO iAgent = 1, numAgents
            !
            PricesGrids(1,iAgent) = 0.d0
            PricesGrids(numPrices,iAgent) = MAX(0.d0,(1.d0+extend(2))*CoopPrices(iAgent))
            !
        END DO
        !
    END IF
    !
    ! Grids
    !
    stepPrices = (PricesGrids(numPrices,:)-PricesGrids(1,:))/(numPrices-1)
    DO i = 2, numPrices-1
        !
        PricesGrids(i,:) = PricesGrids(i-1,:)+stepPrices
        !
    END DO
    !
    ! 4. Compute Pi matrices
    !
    DO i = 1, numActions
        !
        DO j = 1, numAgents
            !
            prices(j) = PricesGrids(indexActions(i,j),j)
            !
        END DO
        !
        d = linearDemands(gamma,prices)
        PI(i,:) = prices*d
        !
    END DO
    !
    ! 5. With linear demand, the repeated Nash prices do no necessarily belong to the
    !    prices grid. Hence, the indexNashPrices vector is empty. Alternatively, we could
    !    look for the row in PricesGrids that is closest to NashPrices (not implemented yet)
    !
    indexNashPrices = 0
    indexCoopPrices = 0
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computePIMatricesSinghVives
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computePIMatricesLogit ( DemandParameters, NashPrices, CoopPrices, &
        PI, NashProfits, CoopProfits, &
        indexNashPrices, indexCoopPrices, NashMarketShares, CoopMarketShares, &
        PricesGrids )
    !
    ! Computes the Logit common payoff matrix PI
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    REAL(8), INTENT(IN) :: DemandParameters(numDemandParameters)
    REAL(8), DIMENSION(numAgents), INTENT(IN) :: NashPrices, CoopPrices
    REAL(8), INTENT(OUT) :: PI(numActions,numAgents)
    REAL(8), DIMENSION(numAgents), INTENT(OUT) :: NashProfits, CoopProfits, &
        NashMarketShares, CoopMarketShares
    INTEGER, DIMENSION(numAgents), INTENT(OUT) :: indexNashPrices, indexCoopPrices
    REAL(8), DIMENSION(numPrices,numAgents), INTENT(OUT) :: PricesGrids
    !
    ! Declaring local variables
    !
    REAL(8) :: a0, mu, extend(2)
    REAL(8), DIMENSION(numAgents) :: a, c, d, stepPrices, prices
    INTEGER :: i, j, iter, iAgent
    !
    ! Beginning execution
    !
    ! Computing PI matrices
    !
    ! Extract demand parameters
    !
    a0 = DemandParameters(1)
    a = DemandParameters(2:1+numAgents)
    c = DemandParameters(2+numAgents:1+2*numAgents)
    mu = DemandParameters(2+2*numAgents)
    extend = DemandParameters(3+2*numAgents:4+2*numAgents)
    !
    ! 1. Compute repeated Nash profits
    !
    NashMarketShares = logitDemands(a0,a,c,mu,NashPrices)
    NashProfits = (NashPrices-c)*NashMarketShares
    !
    ! 2. Compute cooperation profits
    !
    CoopMarketShares = logitDemands(a0,a,c,mu,CoopPrices)
    CoopProfits = (CoopPrices-c)*CoopMarketShares
    !
    ! 3. Compute price grid
    !
    ! Upper and lower bounds
    !
    IF (ALL(extend .GT. 0.d0)) THEN
        !
        ! Lower bound = pNash - extend(1)*(pCoop - pNash)
        ! Upper bound = pCoop + extend(2)*(pCoop - pNash)
        !
        PricesGrids(1,:) = NashPrices-extend(1)*(CoopPrices-NashPrices)
        PricesGrids(numPrices,:) = CoopPrices+extend(2)*(CoopPrices-NashPrices)
        !
    ELSE IF ((extend(1) .LT. 0.d0) .AND. (extend(2) .GE. -EPSILON(extend(2)))) THEN
        !
        ! Lower bound = cost + extend(1)*cost
        ! Upper bound = pCoop + extend(2)*(CoopPrices-NashPrices)
        !
        PricesGrids(1,:) = c+extend(1)*c
        PricesGrids(numPrices,:) = CoopPrices+extend(2)*(CoopPrices-NashPrices)
        !
    END IF
    !
    ! Grids
    !
    stepPrices = (PricesGrids(numPrices,:)-PricesGrids(1,:))/(numPrices-1)
    DO i = 2, numPrices-1
        !
        PricesGrids(i,:) = PricesGrids(i-1,:)+stepPrices
        !
    END DO
    !
    ! 4. Compute Pi matrices
    !
    DO i = 1, numActions
        !
        DO j = 1, numAgents
            !
            prices(j) = PricesGrids(indexActions(i,j),j)
            !
        END DO
        !
        d = logitDemands(a0,a,c,mu,prices)
        PI(i,:) = (prices-c)*d
        !
    END DO
    !
    ! 5. With logit demand, the repeated Nash prices do no necessarily belong to the
    !    prices grid. Hence, the indexNashPrices vector is empty. Alternatively, we could
    !    look for the row in PricesGrids that is closest to NashPrices (not implemented yet)
    !
    indexNashPrices = 0
    indexCoopPrices = 0
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computePIMatricesLogit
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    SUBROUTINE computePIMatricesLogitMu0 ( DemandParameters, NashPrices, CoopPrices, &
        PI, NashProfits, CoopProfits, &
        indexNashPrices, indexCoopPrices, NashMarketShares, CoopMarketShares, &
        PricesGrids )
    !
    ! Computes the Logit common payoff matrix PI
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    REAL(8), INTENT(IN) :: DemandParameters(numDemandParameters)
    REAL(8), DIMENSION(numAgents), INTENT(IN) :: NashPrices, CoopPrices
    REAL(8), INTENT(OUT) :: PI(numActions,numAgents)
    REAL(8), DIMENSION(numAgents), INTENT(OUT) :: NashProfits, CoopProfits, &
        NashMarketShares, CoopMarketShares
    INTEGER, DIMENSION(numAgents), INTENT(OUT) :: indexNashPrices, indexCoopPrices
    REAL(8), DIMENSION(numPrices,numAgents), INTENT(OUT) :: PricesGrids
    !
    ! Declaring local variables
    !
    REAL(8) :: a0, mu, extend(2)
    REAL(8), DIMENSION(numAgents) :: a, c, d, stepPrices, pp
    INTEGER :: i, j, iter, iAgent
    !
    ! Beginning execution
    !
    IF (numAgents .GT. 2) THEN
        !
        PRINT*, 'Perfect competition only works with two agents!'
        STOP
        !
    END IF
    ! Computing PI matrices
    !
    ! Extract demand parameters
    !
    a0 = DemandParameters(1)
    a = DemandParameters(2:1+numAgents)
    c = DemandParameters(2+numAgents:1+2*numAgents)
    mu = DemandParameters(2+2*numAgents)
    extend = DemandParameters(3+2*numAgents:4+2*numAgents)
    !
    ! 1. Compute repeated Nash profits
    !
    NashMarketShares = 0.5d0
    NashProfits = (NashPrices-c)*NashMarketShares
    !
    ! 2. Compute cooperation profits
    !
    CoopMarketShares = 0.5d0
    CoopProfits = (CoopPrices-c)*CoopMarketShares
    !
    ! 3. Compute price grid
    !
    PricesGrids(1,:) = NashPrices-extend(1)*(CoopPrices-NashPrices)
    PricesGrids(numPrices,:) = CoopPrices+extend(2)*(CoopPrices-NashPrices)
    stepPrices = (PricesGrids(numPrices,:)-PricesGrids(1,:))/(numPrices-1)
    DO i = 2, numPrices-1
        !
        PricesGrids(i,:) = PricesGrids(i-1,:)+stepPrices
        !
    END DO
    !
    ! 4. Compute Pi matrices
    !
    DO i = 1, numActions
        !
        DO j = 1, numAgents
            !
            pp(j) = PricesGrids(indexActions(i,j),j)
            !
        END DO
        !
        ! Demand for agent 1
        !
        IF ((pp(1) .LT. a(1)) .AND. (pp(2) .LT. a(2)) .AND. (pp(1) .EQ. pp(2))) THEN
            d(1) = 0.5d0
        ELSE IF ((pp(1) .LT. a(1)) .AND. (pp(1) .LT. pp(2))) THEN
            d(1) = 1.d0
        ELSE IF ((pp(1) .GT. pp(2)) .AND. (pp(2) .LT. a(2))) THEN
            d(1) = 0.d0
        ELSE IF ((AreEqualReals(pp(1),a(1))) .AND. (AreEqualReals(pp(2),a(2)))) THEN
            d(1) = 1.d0/3.d0
        ELSE IF ((AreEqualReals(pp(1),a(1))) .AND. (pp(2) .GT. a(2))) THEN
            d(1) = 0.5d0
        ELSE IF ((pp(1) .GT. a(1)) .AND. (pp(2) .GE. a(2))) THEN
            d(1) = 0.d0
        END IF
        !
        ! Demand for agent 2
        !
        IF ((pp(2) .LT. a(2)) .AND. (pp(1) .LT. a(1)) .AND. (pp(2) .EQ. pp(1))) THEN
            d(2) = 0.5d0
        ELSE IF ((pp(2) .LT. a(2)) .AND. (pp(2) .LT. pp(1))) THEN
            d(2) = 1.d0
        ELSE IF ((pp(2) .GT. pp(1)) .AND. (pp(1) .LT. a(1))) THEN
            d(2) = 0.d0
        ELSE IF ((AreEqualReals(pp(2),a(2))) .AND. (AreEqualReals(pp(1),a(1)))) THEN
            d(2) = 1.d0/3.d0
        ELSE IF ((AreEqualReals(pp(2),a(2))) .AND. (pp(1) .GT. a(1))) THEN
            d(2) = 0.5d0
        ELSE IF ((pp(2) .GT. a(2)) .AND. (pp(1) .GE. a(1))) THEN
            d(2) = 0.d0
        END IF
        !
        ! Profit
        !
        PI(i,:) = (pp-c)*d
        !
    END DO
    !
    ! 5. With logit demand, the repeated Nash prices do no necessarily belong to the
    !    prices grid. Hence, the indexNashPrices vector is empty. Alternatively, we could
    !    look for the row in PricesGrids that is closest to NashPrices (not implemented yet)
    !
    indexNashPrices = 0
    indexCoopPrices = 0
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE computePIMatricesLogitMu0
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    FUNCTION logitDemands ( a0, a, c, mu, p ) 
    !
    ! Computes logit demands
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    REAL(8), INTENT(IN) :: a0, mu
    REAL(8), DIMENSION(numAgents), INTENT(IN) :: a, c, p
    !
    ! Declaring function's type
    !
    REAL(8), DIMENSION(numAgents) :: logitDemands
    !
    ! Beginning execution
    !
    logitDemands = EXP((a-p)/mu)
    logitDemands = logitDemands/(SUM(logitDemands)+EXP(a0/mu))
    !
    ! Ending execution and returning control
    !
    END FUNCTION logitDemands
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    FUNCTION linearDemands ( gamma, p ) 
    !
    ! Computes linear demands
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    REAL(8), INTENT(IN) :: gamma
    REAL(8), DIMENSION(numAgents), INTENT(IN) :: p
    !
    ! Declaring function's type
    !
    REAL(8), DIMENSION(numAgents) :: linearDemands
    !
    ! Beginning execution
    !
    linearDemands(1) = MAX(0.d0,MIN(1.d0-p(1),(1.d0-gamma-p(1)+gamma*p(2))/(1-gamma**2)))
    linearDemands(2) = MAX(0.d0,MIN(1.d0-p(2),(1.d0-gamma-p(2)+gamma*p(1))/(1-gamma**2)))
    !
    ! Ending execution and returning control
    !
    END FUNCTION linearDemands
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! End of execution
!
END MODULE PI_routines