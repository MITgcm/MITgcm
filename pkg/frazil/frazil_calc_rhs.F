C $Header: /u/gcmpack/MITgcm/pkg/frazil/frazil_calc_rhs.F,v 1.1 2012/03/02 01:45:22 dimitri Exp $
C $Name:  $

#include "FRAZIL_OPTIONS.h"

CBOP
C !ROUTINE: FRAZIL_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE FRAZIL_CALC_RHS(
     I                     myTime, myIter, myThid )

C !DESCRIPTION:
C Check water temperature and if colder than freezing
C point bring excess negative heat to the surface.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "FRAZIL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_FRAZIL
C !LOCAL VARIABLES: ====================================================
C     Tfreezing :: Freezing threshold temperature.
      INTEGER bi,bj,i,j,k,kTop
      _RL Tfreezing, Tresid, pLoc, sLoc, tLoc
      _RL a0, a1, a2, b
      PARAMETER( a0 = -0.0575   _d  0 )
      PARAMETER( a1 = 1.710523  _d -3 )
      PARAMETER( a2 = -2.154996 _d -4 )
      PARAMETER( b  = -7.53     _d -4 )

      _RL SW_TEMP
      EXTERNAL SW_TEMP
CEOP

C--   Loops on tile indices bi,bj
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 2, Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C     Check for water below freezing point.
           IF ( maskC(i,j,k-1,bi,bj) .NE. 0. _d 0 .AND.
     &          maskC(i,j,k,  bi,bj) .NE. 0. _d 0 ) THEN

            pLoc = ABS(RC(k))
            sLoc = MAX(salt(i,j,k,bi,bj), 0. _d 0)
            tLoc = SW_TEMP(sLoc,theta(i,j,k,bi,bj),pLoc,0. _d 0)

C Freezing point of seawater
C   REFERENCE: UNESCO TECH. PAPERS IN THE MARINE SCIENCE NO. 28. 1978
C   EIGHTH REPORT JPOTS
C   ANNEX 6 FREEZING POINT OF SEAWATER F.J. MILLERO PP.29-35.
C
C  UNITS:
C         PRESSURE      P          DECIBARS
C         SALINITY      S          PSS-78
C         TEMPERATURE   TF         DEGREES CELSIUS
C         FREEZING PT.
C************************************************************
C  CHECKVALUE: TF= -2.588567 DEG. C FOR S=40.0, P=500. DECIBARS 
            Tfreezing = (a0 + a1*sqrt(sLoc) + a2*sLoc) * sLoc + b*pLoc

            IF (tLoc .LT. Tfreezing) THEN
C     Move the negative heat to surface level.
               kTop = kSurfC(i,j,bi,bj)
               Tresid = Tfreezing - tloc
               frazil_TendT(i,j,k,bi,bj) = Tresid / dTtracerLev(k)
               frazil_TendT(i,j,kTop,bi,bj) = - Tresid *
     &              ( hFacC(i,j,k,bi,bj) * dRf(k) *
     &              recip_hFacC(i,j,kTop,bi,bj) * recip_drF(kTop) ) /
     &              dTtracerLev(kTop)
            ENDIF

           ENDIF
          ENDDO
         ENDDO
        ENDDO
C--   end bi,bj loops.
       ENDDO
      ENDDO

#endif /* ALLOW_FRAZIL */

      RETURN
      END
