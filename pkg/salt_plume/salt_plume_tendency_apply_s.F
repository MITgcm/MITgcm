C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_tendency_apply_s.F,v 1.11 2014/07/09 17:00:49 jmc Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_TENDENCY_APPLY_S
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_TENDENCY_APPLY_S(
     U                      gS_arr,
     I                      iMin,iMax,jMin,jMax, k, bi, bj,
     I                      myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add salt_plume tendency terms to S tendency.
C     Routine works for one level at a time.
C     SaltPlume is the amount of salt rejected by ice while freezing;
C     it is here redistributed to multiple vertical levels as per
C     Duffy et al. (GRL 1999).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "SALT_PLUME.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
C#ifndef SALT_PLUME_VOLUME

C     !LOCAL VARIABLES:
      INTEGER i, j
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
#ifdef SALT_PLUME_SPLIT_BASIN
      _RL lon(two2), lat(two2)
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif

#ifdef TARGET_NEC_SX
C     The vector version computes plumekb2D at each grid point, but this
C     is still faster than non-vector code.
      IF ( k .LT. Nr ) THEN
#ifndef SALT_PLUME_VOLUME
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         plumekb2D(i,j)=ABS(rF(k))
        ENDDO
       ENDDO
       CALL SALT_PLUME_FRAC(
     I      imt,minusone,SaltPlumeDepth(1-OLx,1-OLy,bi,bj),
#ifdef SALT_PLUME_SPLIT_BASIN
     I      XC(1-OLx,1-OLy,bi,bj),YC(1-OLx,1-OLy,bi,bj),
#endif /* SALT_PLUME_SPLIT_BASIN */
     U      plumekb2D,
     I      myTime, 1, myThid )
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         plumefrac(I,J) = plumekb2D(i,j)
         plumekb2D(i,j) = ABS(rF(k+1))
        ENDDO
       ENDDO
       CALL SALT_PLUME_FRAC(
     I      imt,minusone,SaltPlumeDepth(1-OLx,1-OLy,bi,bj),
#ifdef SALT_PLUME_SPLIT_BASIN
     I      XC(1-OLx,1-OLy,bi,bj),YC(1-OLx,1-OLy,bi,bj),
#endif /* SALT_PLUME_SPLIT_BASIN */
     U      plumekb2D,
     I      myTime, 1, myThid )
#endif /* SALT_PLUME_VOLUME */
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
C     Penetrating saltplume fraction:cumSP(k+1)-cumSP(k)
         IF ( SaltPlumeDepth(i,j,bi,bj) .GT. ABS(rF(k)) ) THEN
#ifndef SALT_PLUME_VOLUME
          plumefrac(i,j) = ( plumekb2D(i,j)-plumefrac(i,j) )
     &                     *maskC(i,j,k,bi,bj)
          plumetend(I,J) = saltPlumeFlux(i,j,bi,bj)*plumefrac(I,J)
#else /* SALT_PLUME_VOLUME */
          plumetend(I,J) = SPforcingS(i,j,k,bi,bj)
#endif /* SALT_PLUME_VOLUME */
          gS_arr(i,j) = gS_arr(i,j) + plumetend(I,J)
     &        *recip_drF(k)*mass2rUnit*_recip_hFacC(i,j,k,bi,bj)
         ELSE
          plumefrac(i,j) = 0. _d 0
          plumetend(I,J) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#else
      DO j=jMin,jMax
       DO i=iMin,iMax
C Penetrating saltplume fraction:cumulativeSP(k+1)-cumulativeSP(k)
        IF ( SaltPlumeDepth(i,j,bi,bj) .GT. ABS(rF(k)) ) THEN
         plumefrac(I,J) = 0. _d 0
#ifndef SALT_PLUME_VOLUME
         plumekb(1)=ABS(rF(k))
         plumekb(2)=ABS(rF(k+1))
         SPdepth(1)=SaltPlumeDepth(i,j,bi,bj)
         SPdepth(2)=SaltPlumeDepth(i,j,bi,bj)
#ifdef SALT_PLUME_SPLIT_BASIN
         lon(1) = XC(i,j,bi,bj)
         lon(2) = XC(i,j,bi,bj)
         lat(1) = YC(i,j,bi,bj)
         lat(2) = YC(i,j,bi,bj)
#endif /* SALT_PLUME_SPLIT_BASIN */
         CALL SALT_PLUME_FRAC(
     I                   two2,minusone,SPdepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I                   lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U                   plumekb,
     I                   myTime, 1, myThid )
         plumefrac(I,J) = (plumekb(2)-plumekb(1))*maskC(i,j,k,bi,bj)
         plumetend(I,J) = saltPlumeFlux(i,j,bi,bj)*plumefrac(I,J)
#else /* SALT_PLUME_VOLUME */
         plumetend(i,j) = SPforcingS(i,j,k,bi,bj)
#endif /* SALT_PLUME_VOLUME */
         gS_arr(i,j) = gS_arr(i,j) + plumetend(I,J)
     &        *recip_drF(k)*mass2rUnit*_recip_hFacC(i,j,k,bi,bj)
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
     &      plumefrac,'PLUMEKB ',k,1,2,bi,bj,myThid )
       CALL DIAGNOSTICS_FILL (
     &      plumetend,'oceSPtnd',k,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C#endif /* SALT_PLUME_VOLUME */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
