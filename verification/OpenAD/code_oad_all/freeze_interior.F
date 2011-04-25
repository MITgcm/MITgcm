C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code_oad_all/Attic/freeze_interior.F,v 1.1 2011/04/25 14:02:20 utke Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FREEZE_INTERIOR
C     !INTERFACE:
      SUBROUTINE FREEZE_INTERIOR( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FREEZE_INTERIOR                                                
C     | o Check water temperature and if colder than freezing
C     |   point bring excess negative heat to the surface.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Tfreezing :: Freezing threshold temperature.
      INTEGER bi,bj,i,j,k,kTop
      _RL Tfreezing, Tresid, pLoc, sLoc, tLoc
      _RL a0, a1, a2, b
      PARAMETER( a0 = -0.0575   _d  0 )
      PARAMETER( a1 = 1.710523  _d -3 )
      PARAMETER( a2 = -2.154996 _d -4 )
      PARAMETER( b  = -7.53     _d -4 )


CEOP

C     Check for water below freezing point.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 2, Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( maskC(i,j,k-1,bi,bj) .NE. 0. _d 0 .AND.
     &          maskC(i,j,k,  bi,bj) .NE. 0. _d 0 ) THEN

            pLoc = ABS(RC(k))
            sLoc = MAX(salt(i,j,k,bi,bj), 0. _d 0)
            call SW_TEMP(sLoc,theta(i,j,k,bi,bj),pLoc,0. _d 0, tloc)

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
               theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj) + Tresid
               theta(i,j,kTop,bi,bj) = theta(i,j,kTop,bi,bj) -
     &              Tresid * hFacC(i,j,k,bi,bj) * dRf(k) *
     &              recip_hFacC(i,j,kTop,bi,bj) * recip_drF(kTop)
            ENDIF

           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
