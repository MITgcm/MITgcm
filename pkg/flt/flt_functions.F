C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_functions.F,v 1.4 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

C--   Contents
C--   o GLOBAL2LOCAL_I
C--   o GLOBAL2LOCAL_J
C--   o GLOBAL2LOCAL_K

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RL  FUNCTION GLOBAL2LOCAL_I(
     I                              rG, bi, bj, myThid )

C     ==================================================================
C     FUNCTION global2local_i
C     ==================================================================
C
C     o Converts global x-coordinates (grid) to local index for specific
C       tile
C
C     ==================================================================
C     FUNCTION global2local_i
C     ==================================================================

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

C     == routine arguments ==
      _RL rG
      INTEGER bi, bj, myThid

C     == local variables ==
      _RL fp, fm, dist, xlo, xhi
      INTEGER i, iG

C     == end of interface ==
      DO i=0,sNx

         iG = myXGlobalLo + (bi-1)*sNx
         xlo = xc(i,  1,bi,bj)
         IF (i.EQ.0)   xlo = xc(i,1,bi,bj)-delX(iG)
         xhi = xc(i+1,1,bi,bj)
         IF (i.EQ.sNx) xhi = xc(i,1,bi,bj)+delX(iG+i-1)

         IF (xlo .LE. rG .AND. xhi .GT. rG) THEN
            dist = xhi - xlo
            fp = (xhi - rG) / dist
            fm = (rG - xlo) / dist
            global2local_i = fp*i + fm*(i+1)
            GOTO 100
         ENDIF
      ENDDO

 100  CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RL  FUNCTION GLOBAL2LOCAL_J(
     I                              rG, bi, bj, myThid )

C     ==================================================================
C     FUNCTION global2local_j
C     ==================================================================
C
C     o Converts global y-coordinates (grid) to local index for specific
C       tile
C
C
C     started: Arne Biastoch abiastoch@ucsd.edu  07-AUG-2000
C
C
C     ==================================================================
C     FUNCTION global2local_j
C     ==================================================================

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

C     == routine arguments ==
      _RL rG
      INTEGER bi, bj, myThid

C     == local variables ==
      _RL fp, fm, dist, ylo, yhi
      INTEGER j, jG

C     == end of interface ==

      DO j=0,sNy

         jG = myYGlobalLo + (bj-1)*sNy
         ylo = yc(1,j,  bi,bj)
         IF (j.EQ.0)   ylo = yc(1,j,bi,bj)-delY(jG)
         yhi = yc(1,j+1,bi,bj)
         IF (j.EQ.sNy) yhi = yc(1,j,bi,bj)+delY(jG+j-1)

         IF (ylo .LE. rG .AND. yhi .GT. rG) THEN
            dist = yhi - ylo
            fp = (yhi - rG) / dist
            fm = (rG - ylo) / dist
            global2local_j = fp*j + fm*(j+1)
            GOTO 100
         ENDIF
      ENDDO

 100  CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RL  FUNCTION GLOBAL2LOCAL_K(
     I                              rG, bi, bj, myThid )

C     ==================================================================
C     FUNCTION global2local_k
C     ==================================================================
C
C     o Converts global z-coordinates (grid) to local index for specific
C       tile
C
C
C     started: Arne Biastoch abiastoch@ucsd.edu  07-AUG-2000
C
C
C     ==================================================================
C     FUNCTION global2local_k
C     ==================================================================

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

C     == routine arguments ==
      _RL rG
      INTEGER bi, bj, myThid

C     == local variables ==
      _RL fp, fm, dist, zlo, zhi
      INTEGER k

C     == end of interface ==
      DO k=0,Nr

         zlo = rC(k)
         IF (k.EQ.0)  zlo = rC(k)-drF(1)*0.5
         zhi = rC(k+1)
         IF (k.EQ.Nr) zhi = rC(k)+drF(Nr)*0.5

         IF (zlo .LE. rG .AND. zhi .GT. rG) THEN
            dist = zhi - zlo
            fp = (zhi - rG) / dist
            fm = (rG - zlo) / dist
            global2local_k = fp*k + fm*(k+1)
            GOTO 100
         ENDIF
      ENDDO

 100  CONTINUE

      RETURN
      END

