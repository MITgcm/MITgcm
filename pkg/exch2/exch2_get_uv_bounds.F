C $Header: /u/gcmpack/MITgcm/pkg/exch2/exch2_get_uv_bounds.F,v 1.1 2008/08/01 00:45:16 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: EXCH2_GET_UV_BOUNDS

C     !INTERFACE:
      SUBROUTINE EXCH2_GET_UV_BOUNDS(
     I           fCode, eWdth,
     I           fWedge, fEedge, fSedge, fNedge,
     U           tIlo1, tIhi1, tJlo1, tJhi1,
     O           tIlo2, tIhi2, tJlo2, tJhi2,
     O           tiStride, tjStride,
     I           e2_pij,
     U           e2_oi1, e2_oj1,
     O           e2_oi2, e2_oj2,
     I           myThid )

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE
C     == Global data ==

C     !INPUT/OUTPUT PARAMETERS:

      CHARACTER*2 fCode
      INTEGER     eWdth
      INTEGER     fWedge, fEedge, fSedge, fNedge
      INTEGER     tIlo1, tIhi1, tJlo1, tJhi1
      INTEGER     tIlo2, tIhi2, tJlo2, tJhi2
      INTEGER     tiStride, tjStride
      INTEGER     e2_pij(4)
      INTEGER     e2_oi1, e2_oj1
      INTEGER     e2_oi2, e2_oj2
      INTEGER     myThid
C
C     !LOCAL VARIABLES:
      INTEGER     tIlo,  tIhi,  tJlo,  tJhi

C---  exch2 target to source index relation:
C     is = pij(1)*it + pij(2)*jt + oi
C     js = pij(3)*it + pij(4)*jt + oj

C---  Save input in local variable
      tIlo  = tIlo1
      tIhi  = tIhi1
      tJlo  = tJlo1
      tJhi  = tJhi1

C---  Expand index range according to exchange-Width "eWdth"
      IF ( tIlo.EQ.tIhi .AND. tIlo.EQ.0 ) THEN
C      Sending to a west edge
       tIlo1 = 1-eWdth
       tIhi1 = 0
       tiStride=1
       IF ( tJlo.LE.tJhi ) THEN
        tJlo1 = tJlo-eWdth+1
        tJhi1 = tJhi+eWdth-1
        tjStride=1
       ELSE
        tJlo1 = tJlo+eWdth-1
        tJhi1 = tJhi-eWdth+1
        tjStride=-1
       ENDIF
      ENDIF
      IF ( tIlo.EQ.tIhi .AND. tIlo.GT.1 ) THEN
C      Sending to an east edge
       tIhi1 = tIhi+eWdth-1
       tiStride=1
       IF ( tJlo.LE.tJhi ) THEN
        tJlo1 = tJlo-eWdth+1
        tJhi1 = tJhi+eWdth-1
        tjStride=1
       ELSE
        tJlo1 = tJlo+eWdth-1
        tJhi1 = tJhi-eWdth+1
        tjStride=-1
       ENDIF
      ENDIF
      IF ( tJlo.EQ.tJhi .AND. tJlo.EQ.0 ) THEN
C      Sending to a south edge
       tJlo1 = 1-eWdth
       tJhi1 = 0
       tjStride=1
       IF ( tIlo .LE. tIhi ) THEN
        tIlo1 = tIlo-eWdth+1
        tIhi1 = tIhi+eWdth-1
        tiStride=1
       ELSE
        tIlo1 = tIlo+eWdth-1
        tIhi1 = tIhi-eWdth+1
        tiStride=-1
       ENDIF
      ENDIF
      IF ( tJlo.EQ.tJhi .AND. tJlo.GT.1 ) THEN
C      Sending to a north edge
       tJhi1 = tJhi+eWdth-1
       tjStride=1
       IF ( tIlo.LE.tIhi ) THEN
        tIlo1 = tIlo-eWdth+1
        tIhi1 = tIhi+eWdth-1
        tiStride=1
       ELSE
        tIlo1 = tIlo+eWdth-1
        tIhi1 = tIhi-eWdth+1
        tiStride=-1
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
C--   as a consequence, need also to increase the index lower bound
C     (avoid "out-of bounds" problems ; formerly itlreduce,jtlreduce)
        IF ( e2_pij(1).EQ.-1 .OR. e2_pij(3).EQ.-1 ) tIlo1 = tIlo1+1
        IF ( e2_pij(2).EQ.-1 .OR. e2_pij(4).EQ.-1 ) tJlo2 = tJlo2+1

C---  Avoid updating (some) tile-corner halo region if across faces
c       IF ( tIlo.EQ.tIhi .AND. tIlo.EQ.0 ) THEN
c         IF ( fSedge.EQ.1 ) THEN
C-      West edge is touching the face S edge
c           tJlo1 = tJlo+1
c           tJlo2 = tJlo+1
c         ENDIF
c         IF ( fNedge.EQ.1 ) THEN
C-      West edge is touching the face N edge
c           tJhi1 = tJhi-1
c           tJhi2 = tJhi
c         ENDIF
c       ENDIF
        IF ( tIlo.EQ.tIhi .AND. tIlo.GT.1 ) THEN
          IF ( fSedge.EQ.1 ) THEN
C-      East edge is touching the face S edge
            tJlo1 = tJlo+1
            tJlo2 = tJlo+1
          ENDIF
          IF ( fNedge.EQ.1 ) THEN
C-      East edge is touching the face N edge
            tJhi1 = tJhi-1
            tJhi2 = tJhi
          ENDIF
        ENDIF
c       IF ( tJlo.EQ.tJhi .AND. tJlo.EQ.0 ) THEN
c         IF ( fWedge.EQ.1 ) THEN
C-      South edge is touching the face W edge
c           tIlo1 = tIlo+1
c           tIlo2 = tIlo+1
c         ENDIF
c         IF ( fEedge.EQ.1 ) THEN
C-      South Edge is touching the face E edge
c           tIhi1 = tIhi
c           tIhi2 = tIhi-1
c         ENDIF
c       ENDIF
        IF ( tJlo.EQ.tJhi .AND. tJlo.GT.1 ) THEN
          IF ( fWedge.EQ.1 ) THEN
C-      North edge is touching the face W edge
            tIlo1 = tIlo+1
            tIlo2 = tIlo+1
          ENDIF
          IF ( fEedge.EQ.1 ) THEN
C-      North Edge is touching the face E edge
            tIhi1 = tIhi
            tIhi2 = tIhi-1
          ENDIF
        ENDIF

C---  UV C-Grid specific code: end

      ELSEIF ( fCode.NE.'Ag' ) THEN
        STOP 'ABNORMAL END: S/R EXCH2_GET_UV_BOUNDS (wrong fCode)'
      ENDIF

      RETURN
      END
