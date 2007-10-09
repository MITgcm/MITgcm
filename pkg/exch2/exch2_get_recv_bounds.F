C $Header: /u/gcmpack/MITgcm/pkg/exch2/Attic/exch2_get_recv_bounds.F,v 1.3 2007/10/09 00:03:33 jmc Exp $
C $Name:  $

      SUBROUTINE EXCH2_GET_RECV_BOUNDS(
     I           fCode, eWdth,
     U           tiStride, tjStride,
     U           tIlo, tIhi, tJlo, tJhi )
C
      IMPLICIT NONE
C
      CHARACTER*2 fCode
      INTEGER     eWdth
      INTEGER     tIlo, tIhi, tJlo, tJhi
      INTEGER     tiStride, tjStride
C

      IF ( tIlo .EQ. tIhi .AND. tIlo .EQ. 0 ) THEN
C      Receiving to a west edge
       tIlo=1-eWdth
       tIhi=0
       tistride=1
       IF ( tJlo .LE. tJhi ) THEN
        tJlo=tJlo-eWdth+1
        tJhi=tJhi+eWdth-1
        tjstride=1
       ELSE
        tJlo=tJlo+eWdth-1
        tJhi=tJhi-eWdth+1
        tjstride=-1
       ENDIF
      ENDIF
      IF ( tIlo .EQ. tIhi .AND. tIlo .GT. 1 ) THEN
C      Receiving to an east edge
       tIhi=tIhi+eWdth-1
       tistride=1
       IF ( tJlo .LE. tJhi ) THEN
        tJlo=tJlo-eWdth+1
        tJhi=tJhi+eWdth-1
        tjstride=1
       ELSE
        tJlo=tJlo+eWdth-1
        tJhi=tJhi-eWdth+1
        tjstride=-1
       ENDIF
      ENDIF
      IF ( tJlo .EQ. tJhi .AND. tJlo .EQ. 0 ) THEN
C      Receiving to a south edge
       tJlo=1-eWdth
       tJhi=0
       tjstride=1
       IF ( tIlo .LE. tIhi ) THEN
        tIlo=tIlo-eWdth+1
        tIhi=tIhi+eWdth-1
        tistride=1
       ELSE
        tIlo=tIlo+eWdth-1
        tIhi=tIhi-eWdth+1
        tistride=-1
       ENDIF
      ENDIF
      IF ( tJlo .EQ. tJhi .AND. tJlo .GT. 1 ) THEN
C      Receiving to an north edge
       tJhi=tJhi+eWdth-1
       tjstride=1
       IF ( tIlo .LE. tIhi ) THEN
        tIlo=tIlo-eWdth+1
        tIhi=tIhi+eWdth-1
        tistride=1
       ELSE
        tIlo=tIlo+eWdth-1
        tIhi=tIhi-eWdth+1
        tistride=-1
       ENDIF
      ENDIF

      RETURN
      END
