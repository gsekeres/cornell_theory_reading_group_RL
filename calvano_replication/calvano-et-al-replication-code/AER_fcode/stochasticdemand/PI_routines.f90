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
    REAL(8), DIMENSION(numAgents,numMarkets), INTENT(IN) :: NashPrices, CoopPrices
    REAL(8), INTENT(OUT) :: PI(numActions,numAgents,numMarkets)
    REAL(8), DIMENSION(numAgents,numMarkets), INTENT(OUT) :: NashProfits, CoopProfits, &
        NashMarketShares, CoopMarketShares
    INTEGER, DIMENSION(numAgents,numMarkets), INTENT(OUT) :: indexNashPrices, indexCoopPrices
    REAL(8), DIMENSION(numPrices,numAgents), INTENT(OUT) :: PricesGrids
    !
    ! Declaring local variables
    !
    REAL(8) :: a0(numMarkets), mu, extend(2)
    REAL(8), DIMENSION(numAgents) :: a, c, d, stepPrices, prices
    INTEGER :: i, iter, iAgent, iAction, iMarket
    !
    ! Beginning execution
    !
    ! Computing PI matrices
    !
    ! Extract demand parameters
    !
    a0 = DemandParameters(1:numMarkets)
    a = DemandParameters(numMarkets+1:numMarkets+numAgents)
    c = DemandParameters(numMarkets+numAgents+1:numMarkets+2*numAgents)
    mu = DemandParameters(numMarkets+2*numAgents+1)
    extend = DemandParameters(numMarkets+2*numAgents+2:numMarkets+2*numAgents+3)
    !
    ! Loop over markets
    !
    DO iMarket = 1, numMarkets
        !
        ! 1. Compute repeated Nash profits
        !
        NashMarketShares(:,iMarket) = logitDemands(a0(iMarket),a,c,mu,NashPrices(:,iMarket))
        NashProfits(:,iMarket) = (NashPrices(:,iMarket)-c)*NashMarketShares(:,iMarket)
        !
        ! 2. Compute cooperation profits
        !
        CoopMarketShares(:,iMarket) = logitDemands(a0(iMarket),a,c,mu,CoopPrices(:,iMarket))
        CoopProfits(:,iMarket) = (CoopPrices(:,iMarket)-c)*CoopMarketShares(:,iMarket)
        !
    END DO
    ExpectedNashProfits = SUM(NashProfits,DIM = 2)/DBLE(numMarkets)
    ExpectedCoopProfits = SUM(CoopProfits,DIM = 2)/DBLE(numMarkets)
    !
    ! 3. Compute price grid
    !
    ! Upper and lower bounds
    !
	PricesGrids(1,:) = MINVAL(NashPrices)-extend(1)*(MAXVAL(CoopPrices)-MINVAL(NashPrices))
	PricesGrids(numPrices,:) = MAXVAL(CoopPrices)+extend(2)*(MAXVAL(CoopPrices)-MINVAL(NashPrices))
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
    DO iMarket = 1, numMarkets
        !
        DO iAction = 1, numActions
            !
            DO iAgent = 1, numAgents
                !
                prices(iAgent) = PricesGrids(indexActions(iAction,iAgent),iAgent)
                !
            END DO
            !
            d = logitDemands(a0(iMarket),a,c,mu,prices)
            PI(iAction,:,iMarket) = (prices-c)*d
            !
        END DO
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
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! End of execution
!
END MODULE PI_routines