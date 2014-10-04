C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_toolbox.F,v 1.1 2014/10/04 20:28:20 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_zero
C     !INTERFACE:
      subroutine ecco_zero( fld, nnzloc, zeroloc, myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ecco_zero
C     ==================================================================
C     fill a field with zeroloc
C     ==================================================================
C     SUBROUTINE ecco_zero
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzloc
      _RL zeroloc
      _RL fld   (1-olx:snx+olx,1-oly:sny+oly,nnzloc,nsx,nsy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

CEOP


c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzloc
             do j = jmin,jmax
              do i = imin,imax
                fld(i,j,k,bi,bj) = zeroloc
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

c     with arguments localobs + localbar + msk + range + postpro

      RETURN
      END

