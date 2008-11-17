C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_radiation.F,v 1.5 2008/11/17 23:09:35 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_RADIATION(myTime, myIter, myThid)

C     ==================================================================
C     SUBROUTINE exf_radiation
C     ==================================================================
C
C     o Set radiative fluxes at the surface.
C
C     ==================================================================
C     SUBROUTINE exf_radiation
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

C     == routine arguments ==

      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DOWNWARD_RADIATION
C     == local variables ==

      INTEGER bi,bj
      INTEGER i,j
#ifdef ALLOW_ATM_TEMP
      INTEGER k
      _RL Tsf, SSTtmp, TsfSq
#endif

C     == end of interface ==

C--   Use atmospheric state to compute surface fluxes.

C--   Compute net from downward and downward from net longwave and
C     shortwave radiation, IF needed.
C     lwflux = Stefan-Boltzmann constant * emissivity * SST - lwdown
C     swflux = - ( 1 - albedo ) * swdown

#ifdef ALLOW_ATM_TEMP
      k = 1

      IF ( lwfluxfile .EQ. ' ' .AND. lwdownfile .NE. ' ' ) THEN
C     Loop over tiles.
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)

         IF ( sstExtrapol.GT.0. _d 0 ) THEN
          DO j = 1,sNy
           DO i = 1,sNx
            Tsf = theta(i,j,1,bi,bj) + cen2kel
            SSTtmp = sstExtrapol
     &             *( theta(i,j,1,bi,bj)-theta(i,j,2,bi,bj) )
     &             *  maskC(i,j,2,bi,bj)
            Tsf = Tsf + MAX( SSTtmp, 0. _d 0 )
            TsfSq = Tsf*Tsf
            lwflux(i,j,bi,bj) =
     &          ocean_emissivity*stefanBoltzmann*TsfSq*TsfSq
     &          - lwdown(i,j,bi,bj)
           ENDDO
          ENDDO
         ELSE
          DO j = 1,sNy
           DO i = 1,sNx
            lwflux(i,j,bi,bj) =
     &          ocean_emissivity*stefanBoltzmann*
     &          ((theta(i,j,k,bi,bj)+cen2kel)**4)
     &          - lwdown(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF

C--   end bi,bj loops
        ENDDO
       ENDDO
      ENDIF

C-jmc: commented out: no need to compute Downward-LW (not used) from Net-LW
c     IF ( lwfluxfile .NE. ' ' .AND. lwdownfile .EQ. ' ' ) THEN
C     Loop over tiles.
c      DO bj = myByLo(myThid),myByHi(myThid)
c       DO bi = myBxLo(myThid),myBxHi(myThid)
c        DO j = 1,sNy
c         DO i = 1,sNx
c          lwdown(i,j,bi,bj) =
c    &          ocean_emissivity*stefanBoltzmann*
c    &          ((theta(i,j,k,bi,bj)+cen2kel)**4)
c    &          - lwflux(i,j,bi,bj)
c         ENDDO
c        ENDDO
c       ENDDO
c      ENDDO
c     ENDIF
#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      IF ( swfluxfile .EQ. ' ' .AND. swdownfile .NE. ' ' ) THEN
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           swflux(i,j,bi,bj) = -(1.0-exf_albedo) * swdown(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C-jmc: commented out: no need to compute Downward-SW (not used) from Net-SW
c     IF ( swfluxfile .NE. ' ' .AND. swdownfile .EQ. ' ' ) THEN
c      DO bj = myByLo(myThid),myByHi(myThid)
c       DO bi = myBxLo(myThid),myBxHi(myThid)
c        DO j = 1,sNy
c         DO i = 1,sNx
c          swdown(i,j,bi,bj) = -swflux(i,j,bi,bj) / (1.0-exf_albedo)
c         ENDDO
c        ENDDO
c       ENDDO
c      ENDDO
c     ENDIF
#endif

#endif /* ALLOW_DOWNWARD_RADIATION */

      RETURN
      END
