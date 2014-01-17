C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_forcing_surf.F,v 1.1 2014/01/17 21:56:30 jmc Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SHELFICE_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE SHELFICE_FORCING_SURF(
     I           bi, bj, iMin, iMax, jMin, jMax,
     I           myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHELFICE_FORCING_SURF
C     | o Contains problem specific surface forcing
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj     :: Current tile indices
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: Thread Id number
      INTEGER bi, bj
      INTEGER iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j   :: Loop counters
      INTEGER i, j
CEOP

c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Forcing term

        IF ( .NOT.SHELFICEboundaryLayer ) THEN
C-    for now, forcing using SHELFICEboundaryLayer is done separately
C     (calling SHELFICE_FORCING_T & _S from EXTERNAL_FORCING_T & _S)

         DO j=1,sNy
          DO i=1,sNx
           IF ( R_shelfIce(i,j,bi,bj).LT.zeroRS ) THEN
            surfaceForcingT(i,j,bi,bj) = shelficeForcingT(i,j,bi,bj)
            surfaceForcingS(i,j,bi,bj) = shelficeForcingS(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO

c        IF ( staggerTimeStep ) THEN
c         DO j=1-OLy,sNy+OLy
c          DO i=1-OLx,sNx+OLx
c           PmEpR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)
c          ENDDO
c         ENDDO
c        ENDIF

C--   end if not SHELFICEboundaryLayer
        ENDIF

        IF ( usingZCoords ) THEN
         DO j = jMin, jMax
          DO i = iMin, iMax
            phi0surf(i,j,bi,bj) = phi0surf(i,j,bi,bj)
     &         + shelficeLoadAnomaly(i,j,bi,bj)*recip_rhoConst
          ENDDO
         ENDDO
        ENDIF

c      ENDDO
c     ENDDO

#endif /* ALLOW_SHELFICE */
      RETURN
      END
