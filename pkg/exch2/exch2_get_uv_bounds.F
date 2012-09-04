C $Header: /u/gcmpack/MITgcm/pkg/exch2/exch2_get_uv_bounds.F,v 1.4 2012/09/04 00:40:25 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EXCH2_GET_UV_BOUNDS

C     !INTERFACE:
      SUBROUTINE EXCH2_GET_UV_BOUNDS(
     I                 fCode, eWdth, updateCorners,
     I                 tgTile, tgNb,
     O                 tIlo1, tIhi1, tJlo1, tJhi1,
     O                 tIlo2, tIhi2, tJlo2, tJhi2,
     O                 tiStride, tjStride,
     O                 e2_oi1, e2_oj1, e2_oi2, e2_oj2,
     I                 myThid )

C     !DESCRIPTION:
C     Return the index range & step of the part of the array (overlap-region)
C     which is going to be updated by the exchange with 1 neighbour.
C     2 components vector field (UV) version.


C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fCode         :: field code (position on staggered grid)
C     eWdth         :: width of data region to exchange
C     updateCorners :: flag, do update corner halo region if true
C     tgTile        :: target tile
C     tgNb          :: target Neighbour entry
C     tIlo1, tIhi1  :: index range in I that will be filled in 1rst comp. array
C     tJlo1, tJhi1  :: index range in J that will be filled in 1rst comp. array
C     tIlo2, tIhi2  :: index range in I that will be filled in 2nd  comp. array
C     tJlo2, tJhi2  :: index range in J that will be filled in 2nd  comp. array
C     tiStride      :: index step  in I that will be filled in target arrays
C     tjStride      :: index step  in J that will be filled in target arrays
C     e2_oi1        :: index offset in target to source-1 index relation
C     e2_oj1        :: index offset in target to source-1 index relation
C     e2_oi2        :: index offset in target to source-2 index relation
C     e2_oj2        :: index offset in target to source-2 index relation
C     myThid        :: my Thread Id. number

      CHARACTER*2 fCode
      INTEGER     eWdth
      LOGICAL     updateCorners
      INTEGER     tgTile, tgNb
      INTEGER     tIlo1, tIhi1, tJlo1, tJhi1
      INTEGER     tIlo2, tIhi2, tJlo2, tJhi2
      INTEGER     tiStride, tjStride
      INTEGER     e2_oi1, e2_oj1
      INTEGER     e2_oi2, e2_oj2
      INTEGER     myThid
C
C     !LOCAL VARIABLES:
C     soTile        :: source tile
C     soNb          :: source Neighbour entry
      INTEGER  soTile
      INTEGER  soNb
      INTEGER  tIlo,  tIhi,  tJlo,  tJhi
      INTEGER  i, e2_pij(4)

C---  exch2 target to source index relation:
C     is = pij(1)*it + pij(2)*jt + oi
C     js = pij(3)*it + pij(4)*jt + oj

C---  Initialise index range from Topology values:
      tIlo = exch2_iLo(tgNb,tgTile)
      tIhi = exch2_iHi(tgNb,tgTile)
      tJlo = exch2_jLo(tgNb,tgTile)
      tJhi = exch2_jHi(tgNb,tgTile)
      soNb = exch2_opposingSend(tgNb,tgTile)
      soTile = exch2_neighbourId(tgNb,tgTile)
      e2_oi1 = exch2_oi(soNb,soTile)
      e2_oj1 = exch2_oj(soNb,soTile)
      DO i=1,4
        e2_pij(i) = exch2_pij(i,soNb,soTile)
      ENDDO

C---  Expand index range according to exchange-Width "eWdth"
      IF ( tIlo.EQ.tIhi .AND. tIlo.EQ.0 ) THEN
C      Filling a west edge overlap
       tIlo1 = 1-eWdth
       tIhi1 = 0
       tiStride = 1
       IF ( tJlo.LE.tJhi ) THEN
        tjStride=1
       ELSE
        tjStride=-1
       ENDIF
       IF ( updateCorners ) THEN
        tJlo1 = tJlo-tjStride*(eWdth-1)
        tJhi1 = tJhi+tjStride*(eWdth-1)
       ELSE
        tJlo1 = tJlo+tjStride
        tJhi1 = tJhi-tjStride
       ENDIF
      ENDIF
      IF ( tIlo.EQ.tIhi .AND. tIlo.GT.1 ) THEN
C      Filling an east edge overlap
       tIlo1 = tIlo
       tIhi1 = tIhi+eWdth-1
       tiStride = 1
       IF ( tJlo.LE.tJhi ) THEN
        tjStride = 1
       ELSE
        tjStride =-1
       ENDIF
       IF ( updateCorners ) THEN
        tJlo1 = tJlo-tjStride*(eWdth-1)
        tJhi1 = tJhi+tjStride*(eWdth-1)
       ELSE
        tJlo1 = tJlo+tjStride
        tJhi1 = tJhi-tjStride
       ENDIF
      ENDIF
      IF ( tJlo.EQ.tJhi .AND. tJlo.EQ.0 ) THEN
C      Filling a south edge overlap
       tJlo1 = 1-eWdth
       tJhi1 = 0
       tjStride = 1
       IF ( tIlo .LE. tIhi ) THEN
        tiStride = 1
       ELSE
        tiStride =-1
       ENDIF
       IF ( updateCorners ) THEN
        tIlo1 = tIlo-tiStride*(eWdth-1)
        tIhi1 = tIhi+tiStride*(eWdth-1)
       ELSE
        tIlo1 = tIlo+tiStride
        tIhi1 = tIhi-tiStride
       ENDIF
      ENDIF
      IF ( tJlo.EQ.tJhi .AND. tJlo.GT.1 ) THEN
C      Filling a north edge overlap
       tJlo1 = tJlo
       tJhi1 = tJhi+eWdth-1
       tjStride = 1
       IF ( tIlo.LE.tIhi ) THEN
        tiStride = 1
       ELSE
        tiStride =-1
       ENDIF
       IF ( updateCorners ) THEN
        tIlo1 = tIlo-tiStride*(eWdth-1)
        tIhi1 = tIhi+tiStride*(eWdth-1)
       ELSE
        tIlo1 = tIlo+tiStride
        tIhi1 = tIhi-tiStride
       ENDIF
      ENDIF

C---  copy to 2nd set of indices
      tIlo2 = tIlo1
      tIhi2 = tIhi1
      tJlo2 = tJlo1
      tJhi2 = tJhi1
      e2_oi2 = e2_oi1
      e2_oj2 = e2_oj1

      IF ( fCode.EQ.'Cg' ) THEN
C---  UV C-Grid specific code: start here

C---  half grid-cell location with inverse index relation
C     => increase the offset by 1 (relative to tracer cell-centered offset)
C     if pij(1) is -1 then +i in source aligns with -i in target
C     if pij(3) is -1 then +j in source aligns with -i in target
        IF ( e2_pij(1) .EQ. -1 ) THEN
         e2_oi1 = e2_oi1 + 1
        ENDIF
        IF ( e2_pij(3) .EQ. -1 ) THEN
         e2_oj1 = e2_oj1 + 1
        ENDIF
C     if pij(2) is -1 then +i in source aligns with -j in target
C     if pij(4) is -1 then +j in source aligns with -j in target
        IF ( e2_pij(2) .EQ. -1 ) THEN
         e2_oi2 = e2_oi2 + 1
        ENDIF
        IF ( e2_pij(4) .EQ. -1 ) THEN
         e2_oj2 = e2_oj2 + 1
        ENDIF

C---  adjust index lower and upper bounds (fct of updateCorners):
       IF ( updateCorners ) THEN

C--   as a consequence, need also to increase the index lower bound
C     (avoid "out-of bounds" problems ; formerly itlreduce,jtlreduce)
        IF ( e2_pij(1).EQ.-1 .OR. e2_pij(3).EQ.-1 ) tIlo1 = tIlo1+1
        IF ( e2_pij(2).EQ.-1 .OR. e2_pij(4).EQ.-1 ) tJlo2 = tJlo2+1

C---  Avoid updating (some) tile-corner halo region if across faces
c       IF ( tIlo.EQ.tIhi .AND. tIlo.EQ.0 ) THEN
c         IF ( exch2_isSedge(tgTile).EQ.1 ) THEN
C-      West edge is touching the face S edge
c           tJlo1 = tJlo+1
c           tJlo2 = tJlo+1
c         ENDIF
c         IF ( exch2_isNedge(tgTile).EQ.1 ) THEN
C-      West edge is touching the face N edge
c           tJhi1 = tJhi-1
c           tJhi2 = tJhi
c         ENDIF
c       ENDIF
        IF ( tIlo.EQ.tIhi .AND. tIlo.GT.1 ) THEN
          IF ( exch2_isSedge(tgTile).EQ.1 ) THEN
C-      East edge is touching the face S edge
            tJlo1 = tJlo+1
            tJlo2 = tJlo+1
          ENDIF
          IF ( exch2_isNedge(tgTile).EQ.1 ) THEN
C-      East edge is touching the face N edge
            tJhi1 = tJhi-1
            tJhi2 = tJhi
          ENDIF
        ENDIF
c       IF ( tJlo.EQ.tJhi .AND. tJlo.EQ.0 ) THEN
c         IF ( exch2_isWedge(tgTile).EQ.1 ) THEN
C-      South edge is touching the face W edge
c           tIlo1 = tIlo+1
c           tIlo2 = tIlo+1
c         ENDIF
c         IF ( exch2_isEedge(tgTile).EQ.1 ) THEN
C-      South Edge is touching the face E edge
c           tIhi1 = tIhi
c           tIhi2 = tIhi-1
c         ENDIF
c       ENDIF
        IF ( tJlo.EQ.tJhi .AND. tJlo.GT.1 ) THEN
          IF ( exch2_isWedge(tgTile).EQ.1 ) THEN
C-      North edge is touching the face W edge
            tIlo1 = tIlo+1
            tIlo2 = tIlo+1
          ENDIF
          IF ( exch2_isEedge(tgTile).EQ.1 ) THEN
C-      North Edge is touching the face E edge
            tIhi1 = tIhi
            tIhi2 = tIhi-1
          ENDIF
        ENDIF

       ELSE
C---  adjust index lower and upper bounds (updateCorners = F case):
        IF ( e2_pij(1).EQ.-1 .OR. e2_pij(3).EQ.-1 ) THEN
          tIlo1 = tIlo1+1
          tIhi1 = tIhi1+1
        ENDIF
        IF ( e2_pij(2).EQ.-1 .OR. e2_pij(4).EQ.-1 ) THEN
          tJlo2 = tJlo2+1
          tJhi2 = tJhi2+1
        ENDIF
       ENDIF

C---  UV C-Grid specific code: end

      ELSEIF ( fCode.NE.'Ag' ) THEN
        STOP 'ABNORMAL END: S/R EXCH2_GET_UV_BOUNDS (wrong fCode)'
      ENDIF

      RETURN
      END
