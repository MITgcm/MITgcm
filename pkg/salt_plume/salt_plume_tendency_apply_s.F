C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_tendency_apply_s.F,v 1.8 2012/07/10 15:45:11 gforget Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_TENDENCY_APPLY_S
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_TENDENCY_APPLY_S(
     &     iMin, iMax, jMin, jMax,
     &     bi,bj,kLev,myTime,myThid)

C     !DESCRIPTION:
C     Add salt_plume tendency terms to S tendency.
C     Routine works for one level at a time.
C     SaltPlume is the amount of salt rejected by ice while freezing;
C     it is here redistributed to multiple vertical levels as per
C     Duffy et al. (GRL 1999).

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
      integer i, j
      _RL minusone
      parameter(minusone = -1.)
      _RL plumefrac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL plumetend(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef TARGET_NEC_SX
      integer imt
      parameter( imt=(sNx+2*OLx)*(sNy+2*OLy) )
      _RL plumekb2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#else
      integer two2
      parameter(two2 = 2)
      _RL plumekb(two2), SPdepth(two2)
#endif

#ifdef TARGET_NEC_SX
C     The vector version computes plumekb2D at each grid point, but this
C     is still faster than non-vector code.
      IF ( kLev .LT. Nr ) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         plumekb2D(i,j)=abs(rF(kLev))
        ENDDO
       ENDDO
       CALL SALT_PLUME_FRAC(
     I      imt,minusone,SaltPlumeDepth(1-Olx,1-Oly,bi,bj),
     U      plumekb2D,
     I      myTime, 1, myThid )
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         plumefrac(I,J) = plumekb2D(i,j)*maskC(i,j,klev,bi,bj)
         plumekb2D(i,j) = abs(rF(kLev+1))
        ENDDO
       ENDDO
       CALL SALT_PLUME_FRAC(
     I      imt,minusone,SaltPlumeDepth(1-Olx,1-Oly,bi,bj),
     U      plumekb2D,
     I      myTime, 1, myThid )
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
C     Penetrating saltplume fraction:
         IF ( SaltPlumeDepth(i,j,bi,bj) .GT. -rF(kLev) ) THEN
          plumefrac(i,j)     = plumefrac(i,j)
     &         -plumekb2D(i,j)*maskC(i,j,klev,bi,bj)
          plumetend(I,J) = saltPlumeFlux(i,j,bi,bj)*plumefrac(I,J)
          gS(i,j,kLev,bi,bj) = gS(i,j,kLev,bi,bj)+plumetend(I,J)
     &        *recip_drF(kLev)*mass2rUnit*_recip_hFacC(i,j,kLev,bi,bj)
         ELSE
          plumefrac(i,j)=0. _d 0         
          plumetend(I,J) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#else
      DO j=jMin,jMax
       DO i=iMin,iMax
C Penetrating saltplume fraction:
        IF ( SaltPlumeDepth(i,j,bi,bj) .GT. -rF(kLev) ) THEN
         plumefrac(I,J) = 0. _d 0
         plumekb(1)=abs(rF(klev))
         plumekb(2)=abs(rF(klev+1))
         SPdepth(1)=SaltPlumeDepth(i,j,bi,bj)
         SPdepth(2)=SaltPlumeDepth(i,j,bi,bj)
         CALL SALT_PLUME_FRAC(
     I                   two2,minusone,SPdepth,
     U                   plumekb,
     I                   myTime, 1, myThid )
         plumefrac(I,J)=(plumekb(1)-plumekb(2))*maskC(i,j,klev,bi,bj)
         plumetend(I,J) = saltPlumeFlux(i,j,bi,bj)*plumefrac(I,J)
         gS(i,j,kLev,bi,bj)=gS(i,j,kLev,bi,bj)+plumetend(I,J)
     &        *recip_drF(kLev)*mass2rUnit*_recip_hFacC(i,j,kLev,bi,bj)
        ELSE
         plumefrac(I,J) = 0. _d 0
         plumetend(I,J) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO
#endif /* TARGET_NEC_SX */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL (
     &      plumefrac,'PLUMEKB ',kLev,1,2,bi,bj,myThid )
       CALL DIAGNOSTICS_FILL (
     &      plumetend,'oceSPtnd',kLev,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      
#endif /* ALLOW_SALT_PLUME */
      
      RETURN
      END
