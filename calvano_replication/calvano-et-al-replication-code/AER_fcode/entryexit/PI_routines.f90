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
    SUBROUTINE computePIMatricesLogit ( )
    !
    ! Computes the Logit common payoff matrix PI
    !
    IMPLICIT NONE
    !
    ! Declaring local variables
    !
    REAL(8) :: a0, mu, extend(2), EntryProb, ExitProb
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
    EntryProb = DemandParameters(5+2*numAgents)
    ExitProb = DemandParameters(6+2*numAgents)
    !
    ! 1. Compute repeated Nash profits
    !
    ! In
    !
    NashMarketSharesIn = logitDemands(a0,a,c,mu,NashPricesIn)
    NashProfitsIn = (NashPricesIn-c)*NashMarketSharesIn
    !
    ! Out
    !
    NashMarketSharesOut = logitDemands(a0,a,c,mu,NashPricesOut)
    NashProfitsOut = (NashPricesOut-c)*NashMarketSharesOut
    !
    ! 2. Compute cooperation profits
    !
    ! In
    !
    CoopMarketSharesIn = logitDemands(a0,a,c,mu,CoopPricesIn)
    CoopProfitsIn = (CoopPricesIn-c)*CoopMarketSharesIn
    !
    ! Out
    !
    CoopMarketSharesOut = logitDemands(a0,a,c,mu,CoopPricesOut)
    CoopProfitsOut = (CoopPricesOut-c)*CoopMarketSharesOut
    !
    ! 3. compute price grid
    !
    ! upper and lower bounds
    !
    PricesGrids(1,:) = NashPricesIn-extend(1)*(CoopPricesIn-NashPricesIn)
    PricesGrids(numPrices-1,:) = CoopPricesIn+extend(2)*(CoopPricesIn-NashPricesIn)
    PricesGrids(numPrices,:) = 1.d2*CoopPricesIn
    !
    ! Grids
    !
    stepPrices = (PricesGrids(numPrices-1,:)-PricesGrids(1,:))/(numPrices-2)
    DO i = 2, numPrices-2
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