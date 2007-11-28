C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_tendency_apply_s.F,v 1.2 2007/11/28 09:26:16 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C !ROUTINE: SALT_PLUME_TENDENCY_APPLY_S

C !INTERFACE:
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

C     !LOCAL VARIABLES:
      integer i, j, kp1, two2
      parameter(two2 = 2)
      _RL plumefrac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL plumekb(two2), SPdepth(two2), minusone
      parameter(minusone = -1.)

      do j=jMin,jMax
       do i=iMin,iMax
C Penetrating saltplume fraction:
          plumefrac(I,J) = 0. _d 0
          plumekb(1)=abs(rF(klev))
          plumekb(2)=abs(rF(klev+1))
          SPdepth(1)=SaltPlumeDepth(i,j,bi,bj)
          SPdepth(2)=SaltPlumeDepth(i,j,bi,bj)
          CALL SALT_PLUME_FRAC(
     I                   two2,minusone,SPdepth,
     U                   plumekb,
     I                   myTime, 1, myThid )
          kp1 = klev+1
          IF (klev.EQ.Nr) THEN
           kp1 = klev
           plumekb(2)=0. _d 0
          ENDIF
          IF ( SaltPlumeDepth(i,j,bi,bj) .GT. -rF(kLev) ) THEN
           plumefrac(I,J) = plumekb(1)*maskC(i,j,klev,bi,bj)
     &            -plumekb(2)*maskC(i,j,kp1, bi,bj)
           gS(i,j,kLev,bi,bj)=gS(i,j,kLev,bi,bj)
     &            +saltPlumeFlux(i,j,bi,bj)*plumefrac(I,J)*mass2rUnit
     &            *recip_drF(kLev)*_recip_hFacC(i,j,kLev,bi,bj)
          ENDIF
        ENDDO
       ENDDO

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL (
     &         plumefrac,'PLUMEKB ',kLev,1,2,bi,bj,myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      return
      end
