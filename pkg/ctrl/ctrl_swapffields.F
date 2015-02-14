C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_swapffields.F,v 1.2 2015/02/14 16:32:46 dgoldberg Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

C--  File CTRL_SWAPFFIELDS.F: Routines to swap 2 fields
C--   Contents
C--   o CTRL_SWAPFFIELDS
C--   o CTRL_SWAPFFIELDS_3D
C--   o CTRL_SWAPFFIELDS_XZ
C--   o CTRL_SWAPFFIELDS_YZ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE CTRL_SWAPFFIELDS(
     O                            ffld0,
     U                            ffld1,
     I                            myThid )

C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS
C     ==================================================================
C
C     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C
C     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
C
C              - Restructured the code in order to create a package
C                for the MITgcmUV.
C
C              Christian Eckert eckert@mit.edu  12-Feb-2000
C
C              - Changed Routine names (package prefix: exf_)
C
C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     == routine arguments ==
      _RL ffld0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ffld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C     == local variables ==
      INTEGER bi, bj
      INTEGER i, j

C     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = 1,sNy
            DO i = 1,sNx
              ffld0(i,j,bi,bj) = ffld1(i,j,bi,bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = 1,sNy
            DO i = 1,sNx
              ffld1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE CTRL_SWAPFFIELDS_3D(
     O                            ffld0,
     U                            ffld1,
     I                            nNz, myThid )

C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS_3d
C     ==================================================================
C
C     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
C
C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     == routine arguments ==
C     nNz   :: number of levels to process
      INTEGER nNz
      _RL ffld0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL ffld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      INTEGER myThid

C     == local variables ==
      INTEGER bi, bj
      INTEGER i, j, k

C     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k = 1,nNz
            DO j = 1,sNy
              DO i = 1,sNx
                ffld0(i,j,k,bi,bj) = ffld1(i,j,k,bi,bj)
                ffld1(i,j,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE CTRL_SWAPFFIELDS_XZ(
     O                               ffld0,
     U                               ffld1,
     I                               nNz, myThid )

C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS_xz
C     ==================================================================
C
C     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
C
C     started: heimbach@mit.edu 01-May-2001
C
C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS_xz
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     == routine arguments ==
C     nNz   :: number of levels to process
      INTEGER nNz
      _RL ffld0(1-OLx:sNx+OLx,nNz,nSx,nSy)
      _RL ffld1(1-OLx:sNx+OLx,nNz,nSx,nSy)
      INTEGER myThid

C     == local variables ==
      INTEGER bi, bj
      INTEGER i, k

C     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k = 1,nNz
            DO i = 1,sNx
              ffld0(i,k,bi,bj) = ffld1(i,k,bi,bj)
              ffld1(i,k,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE CTRL_SWAPFFIELDS_YZ(
     O                               ffld0,
     U                               ffld1,
     I                               nNz, myThid )

C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS_yz
C     ==================================================================
C
C     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
C
C     started: heimbach@mit.edu 01-May-2001
C
C     ==================================================================
C     SUBROUTINE CTRL_SWAPFFIELDS_yz
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     == routine arguments ==
C     nNz   :: number of levels to process
      INTEGER nNz
      _RL ffld0(1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL ffld1(1-OLy:sNy+OLy,nNz,nSx,nSy)
      INTEGER myThid

C     == local variables ==
      INTEGER bi, bj
      INTEGER j, k

C     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k = 1,nNz
            DO j = 1,sNy
              ffld0(j,k,bi,bj) = ffld1(j,k,bi,bj)
              ffld1(j,k,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END
