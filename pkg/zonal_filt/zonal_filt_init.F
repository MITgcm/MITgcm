C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_init.F,v 1.8 2012/08/06 16:55:37 jmc Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: ZONAL_FILT_INIT
C     !INTERFACE:
      SUBROUTINE ZONAL_FILT_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R ZONAL_FILT_INIT
C     | o Initialise FFT filter for latitude circle.
C     *==========================================================*
C     | The details of particular FFT libraries may differ.
C     | Changing to a different library may entail modifying the
C     | code here. However, the broad process is usually the
C     | same.
C     | Note - Fourier modes for sNx and sNx+1 are damped in the
C     |        same way. This is because we have not implemented
C     |        a scheme that sets the damping factor for the
C     |        highest wave number for odd sNx. Instead the
C     |        highest wave number for odd sNx. Instead only
C     |        wave numbers 1:INT(sNx/2) are partially damped.
C     |        Wave number sNx/2 (if it exists) is removed
C     |        altogether.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ZONAL_FILT.h"
#include "FFTPACK.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_ZONAL_FILT
C     !LOCAL VARIABLES:
C     == Local variables ==
C     alpha  :: Used to evaluate frequency and latitude dependent
C               amplitude damping factor.
C     wvNum  :: Wave number
C     lat    :: Temporary holding latitude
C     nWv    :: No. of waves that fit on grid.
C     msgBuf :: Informational/error message buffer
c     _RL alpha, wvNum
c     INTEGER nWv
      INTEGER i, j, bi, bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     !FUNCTIONS:
      _RL ampfact
      _RS lat
      ampfact(lat,i) = MIN( oneRL,
     &   ( COS( ABS(lat)*deg2rad )
     &      /COS( zonal_filt_lat*deg2rad ) )**zonal_filt_cospow
     &      /(SIN( PI*FLOAT(i)/FLOAT(Nx) ) )**zonal_filt_sinpow
     &   )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( sNx.NE.Nx ) THEN
         WRITE(msgBuf,'(A,I3,A)')
     &    'S/R ZONAL_FILT_INIT: Multi-tiles ( nSx*nPx=', nSx*nPx, ' )'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &    ' in Zonal (X) dir. not implemented in Zonal-Filter code'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R ZONAL_FILT_INIT'
      ENDIF

      _BEGIN_MASTER(myThid)
C     o Initialise specific library FFT package
      DO bj=1,nSy
c      CALL R8FFTI( Nx, FFTPACKWS(1,bj) )
       CALL R8FFTI1( Nx, FFTPACKWS2(1,bj), FFTPACKWS3(1,bj) )
      ENDDO

C     o Set amplitude scale factor as function of latitude and mode number
      DO bj=1,nSy
       DO bi=1,nSx
        DO j=1-oLy,sNy+Oly
         ampFactor(1,j,bi,bj) = oneRL
         ampFactorV(1,j,bi,bj) = oneRL
         DO i=1,Nx/2-1
          ampFactor(2*i,j,bi,bj) = ampfact( yC(1,j,bi,bj) , I )
c         IF (ampFactor(2*i,j,bi,bj).LE..9) ampFactor(2*i,j,bi,bj)=0.
          ampFactor(2*I+1,j,bi,bj) = ampFactor(2*i,j,bi,bj)
          ampFactorV(2*i,j,bi,bj) = ampfact( yG(1,j,bi,bj) , I )
c         IF (ampFactorV(2*i,j,bi,bj).LE..9) ampFactorV(2*i,j,bi,bj)=0.
          ampFactorV(2*I+1,j,bi,bj) = ampFactorV(2*i,j,bi,bj)
         ENDDO

         i=Nx/2
         IF ( zonal_filt_mode2dx.EQ.0 ) THEN
           ampFactor(Nx,j,bi,bj) = ampfact( yC(1,j,bi,bj) , i )
           ampFactorV(Nx,j,bi,bj) = ampfact( yG(1,j,bi,bj) , i )
         ELSE
           ampFactor(Nx,j,bi,bj) = 0.
           ampFactorV(Nx,j,bi,bj) = 0.
         ENDIF

        ENDDO
       ENDDO
      ENDDO
      _END_MASTER(myThid)
      CALL BAR2(myThid)

      CALL WRITE_REC_XY_RL( 'ampFactor', ampFactor, 1, 0, myThid )

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
