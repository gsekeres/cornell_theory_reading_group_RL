MODULE generic_routines
!
USE ifport
!
! Various generic routines 
!
IMPLICIT NONE
!
CONTAINS
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    FUNCTION ComputeRowSummaryStatistics ( m, n, x )
    !
    ! Computes summary statistics on a (m x n) REAL(8) matrix X by rows
    ! Returns a (m x 9) matrix with columns equal to:
    ! 1: average
    ! 2: standard deviation
    ! 3: minimum
    ! 4: 0.025 percentile
    ! 5: 0.25 percentile
    ! 6: 0.5 percentile
    ! 7: 0.75 percentile
    ! 8: 0.975 percentile
    ! 9: maximum
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    ! 
    INTEGER, INTENT(IN) :: m, n
    REAL(8), INTENT(IN) :: x(m,n)
    !
    ! Declaring function's type
    !
    REAL(8) :: ComputeRowSummaryStatistics(m,9)
    !
    ! Declaring local variables
    !
    INTEGER :: i
    INTEGER(8) :: n_I8
    REAL(8) :: tmp_r(n), z(m,n), y(m,9)
    !
    ! Beginning execution
    !
    ! Mean and standard deviation
    y(:,1) = SUM(x,DIM = 2)/DBLE(n)
    y(:,2) = SQRT(ABS(SUM(x**2,DIM = 2)/DBLE(n)-y(:,1)**2))
    !
    ! Minimum and maximum
    y(:,3) = MINVAL(x,DIM = 2)
    y(:,9) = MAXVAL(x,DIM = 2)
    !
    ! Median and other percentiles
    n_I8 = n
    DO i = 1, m
        !
        tmp_r = x(i,:)
        CALL SORTQQ(LOC(tmp_r),n_I8,SRT$REAL8)
        z(i,:) = tmp_r
        !
    END DO
    y(:,4) = z(:,NINT(0.025d0*n))
    y(:,5) = z(:,NINT(0.25d0*n))
    y(:,6) = z(:,NINT(0.5d0*n))
    y(:,7) = z(:,NINT(0.75d0*n))
    y(:,8) = z(:,NINT(0.975d0*n))
    !
    ComputeRowSummaryStatistics = y
    !
    ! Ending execution and returning control
    !
    END FUNCTION ComputeRowSummaryStatistics
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    FUNCTION AreEqualReals ( a, b )
    !
    ! Tests the equality between a and b, two REAL(8) values
    ! Returns .TRUE. if a == b, .FALSE. if a != b
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    ! 
    REAL(8), INTENT(IN) :: a, b
    !
    ! Declaring function's type
    !
    LOGICAL :: AreEqualReals
    !
    ! Beginning execution
    !
    IF (ABS(a-b) .LE. EPSILON(a)) THEN
        !
        AreEqualReals = .TRUE.
        !
    ELSE
        !
        AreEqualReals = .FALSE.
        !
    END IF
    !
    ! Ending execution and returning control
    !
    END FUNCTION AreEqualReals
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    FUNCTION convertNumberBase ( n, b, l )
    !
    ! Converts an integer n from base 10 to base b, 
    ! generating a vector of integers of length l
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    ! 
    INTEGER, INTENT(IN) :: n, b, l
    !
    ! Declaring function's type
    !
    INTEGER, DIMENSION(l) :: convertNumberBase
    !
    ! Declaring local variables
    !
    INTEGER :: i, tmp
    !
    ! Beginning execution
    !
    tmp = n
    DO i = 1, l
        !
        convertNumberBase(l-i+1) = MOD(tmp,b)+1
        tmp = tmp/b
        !
    END DO
    !
    ! Ending execution and returning control
    !
    END FUNCTION convertNumberBase
!
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
    FUNCTION ran2 ( idum, iv, iy, idum2 )
    !
    ! Thread safe function that generates U(0,1) random deviates (to be used with OpenMP)
    ! See function RAN2 on p. 272 of NRF77
    ! Long period (> 2 × 10^18) random number generator of L’Ecuyer with Bays-Durham shuffle
    ! and added safeguards. Returns a uniform random deviate between 0.0 and 1.0 (exclusive
    ! of the endpoint values). Call with idum a negative integer to initialize; thereafter, do not
    ! alter idum between successive deviates in a sequence. RNMX should approximate the largest
    ! floating value that is less than 1.
    ! Always set idum2 = 123456789, iv = 0, iy = 0 upon initialization
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(INOUT) :: idum
    INTEGER, INTENT(INOUT) :: iv(32)
    INTEGER, INTENT(INOUT) :: iy
    INTEGER, INTENT(INOUT) :: idum2
    !
    ! Declaring function's type
    !
    REAL(8) :: ran2
    !
    ! Declaring local variables and parameters
    !
    INTEGER, PARAMETER :: IM1 = 2147483563, IM2 = 2147483399, IMM1 = IM1-1, IA1 = 40014, &
        IA2 = 40692, IQ1 = 53668, IQ2 = 52774, IR1 = 12211, IR2 = 3791, NDIV = 1+IMM1/32
    REAL(8), PARAMETER :: AM = 1.d0/IM1, EPS = 1.2d-7, RNMX = 1.d0-EPS
    INTEGER :: j, k
    !
    ! Beginning execution
    !
    ! Initializing
    !
    IF (idum .LE. 0) THEN
        !
        idum = MAX(-idum,1) 
        idum2 = idum
        DO j = 32+8,1,-1 
            !
            k = idum/IQ1
            idum = IA1*(idum-k*IQ1)-k*IR1
            IF (idum .LT. 0) idum = idum+IM1
            IF (j .LE. 32) iv(j) = idum
            !
        END DO
        !
        iy = iv(1)
        !
    END IF
    !
    ! Start here when not initializing
    !
    k = idum/IQ1 
    idum = IA1*(idum-k*IQ1)-k*IR1 
    IF (idum .LT. 0) idum = idum+IM1 
    k = idum2/IQ2
    idum2 = IA2*(idum2-k*IQ2)-k*IR2 
    IF (idum2 .LT. 0) idum2 = idum2+IM2
    j = 1+iy/NDIV 
    iy = iv(j)-idum2 
    iv(j) = idum
    !
    IF (iy .LT. 1) iy = iy+IMM1
    ran2 = MIN(AM*iy,RNMX) 
    RETURN
    !
    ! Ending execution and returning control
    !
    END function ran2
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    SUBROUTINE generateCombinations ( rows, cols, lengths, x, totrows, comb )   
    !
    ! Computes all possible (TOTROWS) combinations of the columns of 
    ! the (ROWS x COLS) integer matrix X, considering only the first
    ! LENGTHS elements in each column, with L an (COLS x 1) vector.
    ! Notice that TOTROWS is the product of the elements of LENGTHS
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: rows, cols, totrows
    INTEGER, INTENT(IN) :: lengths(cols)
    INTEGER, INTENT(IN) :: x(rows,cols)
    INTEGER, INTENT(OUT) :: comb(totrows,cols)
    !
    ! Declaring local variables
    !
    INTEGER :: itotrows, itmp, icol, index
    !
    ! Beginning execution
    !
    DO itotrows = 1, totrows
        !
        itmp = itotrows-1
        DO icol = cols, 1, -1
            !
            index = 1+MOD(itmp,lengths(icol))
            itmp = itmp/lengths(icol)
            !
            comb(itotrows,icol) = x(index,icol)
            !
        END DO
        !
    END DO
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE generateCombinations
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    SUBROUTINE updateVectorAverage ( m, n, x, xbar )   
    !
    ! Updates an real m-dimensional running average using the formula:
    ! xbar(n) = (n-1)/n*xbar(n-1) + x/n
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: m, n
    REAL(8), DIMENSION(m), INTENT(IN) :: x
    REAL(8), DIMENSION(m), INTENT(INOUT) :: xbar
    !
    ! Beginning execution
    !
    xbar = DBLE(n-1)/DBLE(n)*xbar+x/DBLE(n)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE updateVectorAverage
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    SUBROUTINE updateScalarAverage ( n, x, xbar )   
    !
    ! Updates an real scalar running average using the formula:
    ! xbar(n) = (n-1)/n*xbar(n-1) + x/n
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: n
    REAL(8), INTENT(IN) :: x
    REAL(8), INTENT(INOUT) :: xbar
    !
    ! Beginning execution
    !
    xbar = DBLE(n-1)/DBLE(n)*xbar+x/DBLE(n)
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE updateScalarAverage
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    SUBROUTINE MaxLocBreakTies ( n, x, idumQ, ivQ, iyQ, idum2Q, m, p )   
    !
    ! Given the (n x 1) real array X, finds:
    ! - the maximum M
    ! - the position of M in X
    ! Differently from MAXLOC, ties are broken randomly
    !
    IMPLICIT NONE
    !
    ! Declaring dummy variables
    !
    INTEGER, INTENT(IN) :: n
    REAL(8), INTENT(IN) :: x(n)
    INTEGER, INTENT(INOUT) :: idumQ, ivQ(32), iyQ, idum2Q
    REAL(8), INTENT(OUT) :: m
    INTEGER, INTENT(OUT) :: p
    !
    ! Declaring local variables
    !
    INTEGER :: h, i, tied(n)
    REAL(8) :: u
    !
    ! Beginning execution
    !
    tied = 0
    h = 0
    DO i = 1, n
        !
        m = MAXVAL(x)
        IF (AreEqualReals(x(i),m)) THEN
            !
            h = h+1
            tied(h) = i
            !
        END IF
        !
    END DO
    IF (h .GT. 1) THEN
        !
        u = ran2(idumQ,ivQ,iyQ,idum2Q)
        p = tied(1+INT(h*u))
        !
    ELSE
        !
        p = tied(1)
        !
    END IF
    !
    ! Ending execution and returning control
    !
    END SUBROUTINE MaxLocBreakTies
! 
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! End of execution
!
END MODULE generic_routines