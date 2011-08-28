C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_wind_perturb.F,v 1.4 2011/08/28 22:18:13 jmc Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE EBM_WIND_PERTURB( myTime, myIter, myThid )
C     *==========================================================*
C     | S/R EBM_WIND_PERTURB
C     | o Calculated random wind perturbations.
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif

C     == Routine arguments ==
      _RL    myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_EBM
# ifdef EBM_WIND_PERT

C     == Local variables ==
C     Loop counters
      INTEGER i, j, bi, bj
      _RS ya(1-OLy:sNy+OLy), ya2(1-OLy:sNy+OLy)
      _RS xa(1-OLx:sNx+OLx), xa2(1-OLx:sNx+OLx)
      _RS y(1-OLy:sNy+OLy), x(1-OLx:sNx+OLx)
      _RS temp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS temp2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS stdev(1-OLy:sNy+OLy)
      _RS std(1:40)
      data std /0.030, 0.035, 0.045, 0.053, 0.059, 0.060, 0.056,
     &          0.048, 0.041, 0.038, 0.034, 0.029, 0.023, 0.018,
     &          0.016, 0.015, 0.013, 0.011, 0.008, 0.005, 0.005,
     &          0.005, 0.008, 0.011, 0.014, 0.014, 0.017, 0.019,
     &          0.023, 0.029, 0.032, 0.038, 0.048, 0.058, 0.065,
     &          0.067, 0.063, 0.060, 0.062, 0.064 /


      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

      DO j = 1-OLy, sNy+OLy
         y(j) = 0.0
         ya(j) = 0.0
         ya2(j) =  0.0
         stdev(j) = 0.0
      ENDDO
      DO i = 1-OLx, sNx+OLx
         x(i) = 0.0
         xa(i) = 0.0
         xa2(i) =  0.0
      ENDDO
      DO i = 1-OLx, sNx+OLx
        DO j = 1-OLy, sNy+OLy
         temp(i,j) = 0.0
         temp2(i,j) = 0.0
        ENDDO
      ENDDO
      DO j = 1, sNy
         stdev(j) = std(j)
      ENDDO

cph   Generate random numbers
cph   Need to get this from somewhere!
      call random_number (temp)

C     interpolation in first dimension
C     scaling to get a process with a standard deviation of 1
      DO j = jMin, jMax
       DO i = iMin, iMax
         temp(i,j) = 1.73*(2.0*temp(i,j) - 1.0)
       ENDDO
      ENDDO

      DO j = jMin, jMax
       DO i = iMin, iMax
         x(i) = i
         xa(i) = x(i) - MOD(x(i),10.0)
         xa2(i) = xa(i)+10.0
        if ( xa2(i) .gt. sNx+Olx ) then
           xa2(i) = 0.0
         endif
         temp2(i,j) = 0.1*( (x(i)-xa(i))*temp(INT(xa2(i)),j)+
     &        (10.0-x(i)+xa(i))*temp(INT(xa(i)),j) )
       ENDDO
      ENDDO

C     interpolation in second dimension
C     multiplication with observation zonal wind stress standard deviation
C     add AR1 process
      DO i = iMin, iMax
       DO j = jMin, jMax
         y(j) = j
         ya(j) = y(j) - MOD(y(j),6.0)
         ya2(j) = ya(j)+6.0
         if ( ya2(j) .gt. sNy+Oly ) then
            ya2(j) = 0.0
         endif
c     time lag correlation coefficient, use 0.75 for temperature timescale,
c     0.98 for the momentum timescale.
         winPert(i,j,bi,bj) = maskW(i,j,k,bi,bj)*
     &        (1.0/1.66)*(0.75*winPert(i,j,bi,bj) +
     &        stdev(j)*(1.0/6.0)*
     &        ((y(j)-ya(j))*temp2(i,INT(ya2(j)))+
     &        (6.0-y(j)+ya(j))*temp2(i,INT(ya(j)))))
       ENDDO
      ENDDO

       ENDDO
      ENDDO

C      CALL PLOT_FIELD_XYRS( winPert, 'winPert',1,myThid)

      _EXCH_XY_RS(winPert  , myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
            fu(i,j,bi,bj) = fu(i,j,bi,bj)
     &                    + winPert(i,j,bi,bj)*rUnit2mass
     &                     *drF(1)*hFacW(i,j,1,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO

# endif /* EBM_WIND_PERT */
#endif /* ALLOW_EBM */

      RETURN
      END
