C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_init.F,v 1.3 2001/02/04 14:38:51 cnh Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

      SUBROUTINE ZONAL_FILT_INIT(myThid)

C     /==========================================================\
C     | S/R ZONAL_FILT_INIT                                      |
C     | o Initialise FFT filter for latitude circle.             |
C     |==========================================================|
C     | The details of particular FFT libraries may differ.      |
C     | Changing to a different library may entail modifying the |
C     | code here. However, the broad process is usually the     |
C     | same.                                                    |
C     | Note - Fourier modes for sNx and sNx+1 are damped in the |
C     |        same way. This is because we have not implemented |
C     |        a scheme that sets the damping factor for the     |
C     |        highest wave number for odd sNx. Instead the      |
C     |        highest wave number for odd sNx. Instead only     |
C     |        wave numbers 1:INT(sNx/2) are partially damped.   |
C     |        Wave number sNx/2 (if it exists) is removed       |
C     |        altogether.                                       |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ZONAL_FILT.h"
#include "FFTPACK.h"

C     == Routine arguments ==
C     myThid - Thread number of this instance of FILTER_LATCIRC_FFT_INIT
      INTEGER myThid

#ifdef ALLOW_ZONAL_FILT

C     == Local variables ==
C     alpha - Used to evaluate frequency and latitude dependent
C             amplitude damping factor.
C     wvNum - Wave number
C     lat   - Temporary holding latitude
C     nWv   - No. of waves that fit on grid.
      _RL alpha, wvNum, lat
      INTEGER I, J, bi, bj, nPoints, nWv
      _RL one
      PARAMETER( one = 1.0 )
      _RS ampfact,Y
      ampfact(Y,I) = min( one, 
     &   ( cos( abs(Y)*deg2rad )
     &      /cos( zonal_filt_lat*deg2rad ) )**zonal_filt_cospow
     &      /(sin( PI*float(I)/float(Nx) ) )**zonal_filt_sinpow
     &   )

C     o Initialise specific library FFT package
      DO bj=1,nSy
C      CALL R8FFTI( Nx, FFTPACKWS(1,bj) )
       CALL R8FFTI1( Nx, FFTPACKWS2(1,bj), FFTPACKWS3(1,bj) )
      ENDDO

C     o Set amplitude scale factor as function of latitude and mode number
      DO bj=1,nSy
       DO bi=1,nSx
        DO j=1-oLy,sNy+Oly
         ampFactor(1,J,bi,bj) = one
         ampFactorV(1,J,bi,bj) = one
         DO i=1,Nx/2-1
          ampFactor(2*I,J,bi,bj) = ampfact( yC(1,J,bi,bj) , I )
C         IF (ampFactor(2*I,J,bi,bj).LE..9) ampFactor(2*I,J,bi,bj)=0.
          ampFactor(2*I+1,J,bi,bj) = ampFactor(2*I,J,bi,bj)
          ampFactorV(2*I,J,bi,bj) = ampfact( yG(1,J,bi,bj) , I )
C         IF (ampFactorV(2*I,J,bi,bj).LE..9) ampFactorV(2*I,J,bi,bj)=0.
          ampFactorV(2*I+1,J,bi,bj) = ampFactorV(2*I,J,bi,bj)
         ENDDO
         I=Nx/2
Cajatest
         ampFactor(Nx,J,bi,bj) = 0.
         ampFactorV(Nx,J,bi,bj) = 0.
C        ampFactor(Nx,J,bi,bj) = ampfact( yC(1,J,bi,bj) , I )
C        ampFactorV(Nx,J,bi,bj) = ampfact( yG(1,J,bi,bj) , I )
Cajatest
        ENDDO
       ENDDO
      ENDDO

      CALL WRITE_REC_XY_RL( 'ampFactor', ampFactor, 1, 0, myThid )

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
