#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_ETAWS
C     !INTERFACE:
      SUBROUTINE UPDATE_ETAWS( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_ETAWS
C     | o For hybrid sigma-coordinate, derive SSH value
C     |   (etaHw & etaHs) and SSH tendency (dEtaWdt & dEtaSdt)
C     |   at grid-cell Western and Southern edges.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number
C     myThid  :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef NONLIN_FRSURF
# ifndef DISABLE_SIGMA_CODE
C     !LOCAL VARIABLES:
C     i,j,bi,bj  :: Loop counters
      INTEGER i,j,bi,bj
CEOP

      IF ( nonlinFreeSurf.GT.0 .AND. selectSigmaCoord.NE.0 ) THEN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C-     1rst bi,bj loop :

C-- copy etaHX -> dEtaXdt
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dEtaWdt(i,j,bi,bj) = etaHw(i,j,bi,bj)
            dEtaSdt(i,j,bi,bj) = etaHs(i,j,bi,bj)
          ENDDO
         ENDDO

         DO j=1,sNy+1
          DO i=1,sNx+1
            etaHw(i,j,bi,bj)   = ( etaH (i-1,j,bi,bj)
     &                           + etaH ( i ,j,bi,bj) )*0.5 _d 0
            etaHs(i,j,bi,bj)   = ( etaH (i,j-1,bi,bj)
     &                           + etaH (i, j ,bi,bj) )*0.5 _d 0
c           etaHw(i,j,bi,bj)   = 0.5 _d 0
c    &                         *(   etaH (i-1,j,bi,bj)*rA(i-1,j,bi,bj)
c    &                            + etaH ( i ,j,bi,bj)*rA( i ,j,bi,bj)
c    &                          )*recip_rAw(i,j,bi,bj)
c           etaHs(i,j,bi,bj)   = 0.5 _d 0
c    &                         *(   etaH (i,j-1,bi,bj)*rA(i,j-1,bi,bj)
c    &                            + etaH (i, j ,bi,bj)*rA(i, j ,bi,bj)
c    &                          )*recip_rAs(i,j,bi,bj)
          ENDDO
         ENDDO

C- end 1rst bi,bj loop.
        ENDDO
       ENDDO

       CALL EXCH_UV_XY_RL( etaHw, etaHs, .FALSE., myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C-     2nd bi,bj loop :

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dEtaWdt(i,j,bi,bj) = ( etaHw(i,j,bi,bj)
     &                           - dEtaWdt(i,j,bi,bj) )/deltaTFreeSurf
            dEtaSdt(i,j,bi,bj) = ( etaHs(i,j,bi,bj)
     &                           - dEtaSdt(i,j,bi,bj) )/deltaTFreeSurf
          ENDDO
         ENDDO

C- end 2nd bi,bj loop.
        ENDDO
       ENDDO

      ENDIF
# endif /* DISABLE_SIGMA_CODE */
#endif /* NONLIN_FRSURF */

      RETURN
      END
