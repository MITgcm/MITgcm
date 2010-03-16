C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_set_iolabel.F,v 1.2 2010/03/16 00:22:26 jmc Exp $
C $Name:  $

#undef STAND_ALONE_IOLABEL_TESTING
C     to test the S/R above, #define the above C-PreProcessor flag
C     and compile this fortran source code alone.

#ifdef STAND_ALONE_IOLABEL_TESTING
      PROGRAM MAIN
      INTEGER NLL, I
      PARAMETER (NLL=62*62)
      CHARACTER*2 LL(NLL)

      CALL PTRACERS_SET_IOLABEL( LL, NLL, 1 )
      DO I=1, NLL
       PRINT *, LL(I)
      ENDDO
      END
#endif /* STAND_ALONE_IOLABEL_TESTING */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: PTRACERS_SET_IOLABEL

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_SET_IOLABEL(
     O                    ioLbl,
     I                    nbLbl, myThid )

C !DESCRIPTION:
C   S/R  PTRACERS_SET_IOLABEL
C   Set pTracers IO & diagnostics label (2 characters long)
C
C   Set sequenced label list 00, 02, 03, ... 99, 0a...0Z...9a...9Z,a0...ZZ
C    to more than 99 TRACERS but without requiring more than two digit labels.
C   Sequence below allows 3843 (=62**2 -1) tracers.
C   First 99 are numbered in decimal ;
C   Then, from 100 to 619, analog to base 52 counting:
C    0-9 1rst digit , a-z,A-Z (=52 id) 2nd digit ;
C   And from 620 to 3843, analog to base 62 counting:
C    a-z,A-Z 1rst digit ; 0-9,a-z,A-Z (=62 id) 2nd digit ;
C ======================================================================

C !USES:
      IMPLICIT NONE

C !INPUT PARAMETERS: ===================================================
C     nbLbl       :: number of labels to define
C     myThid      :: my Thread Id number
      INTEGER     nbLbl
      INTEGER     myThid

C !OUTPUT PARAMETERS: ==================================================
C     ioLbl       :: io-label
      CHARACTER*2 ioLbl(nbLbl)

C !LOCAL VARIABLES: ====================================================
C     c1Set1      :: 1rst digit (from left) of 1rst set of labels
C     c2Set1      ::  2nd digit (from left) of 1rst set of labels
C     c1Set2      :: 1rst digit (from left) of  2nd set of labels
C     c2Set2      ::  2nd digit (from left) of  2nd set of labels
C     c1Set3      :: 1rst digit (from left) of  3rd set of labels
C     c2Set3      ::  2nd digit (from left) of  3rd set of labels
C     l1Set       :: length of 1rst digit list
C     l2Set       :: length of  2nd digit list
C     i,j,n       :: loop indices
      CHARACTER*10 c1Set1
      CHARACTER*10 c2Set1
      CHARACTER*10 c1Set2
      CHARACTER*52 c2Set2
      CHARACTER*52 c1Set3
      CHARACTER*62 c2Set3
      INTEGER l1Set, l2Set
      INTEGER i,j,n
CEOP

      c1Set1 = '0123456789'
      c2Set1 = c1Set1

      c1Set2 = c1Set1
      c2Set2 = 'abcdefghijklmnopqrstuvwxyz'
     &       //'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      c1Set3 = c2Set2
      c2Set3 = c1Set1//c2Set2

C--   Set a default.
C     This should not show up unless there is a problem
C     where nbLbl is equal or greater than 10*10 + 10*52 + 52*62 = 62**2
      DO n=1,nbLbl
       ioLbl(n) = '--'
      ENDDO

      n = 0
C--   First set of labels:
      l1Set = LEN(c1Set1)
      l2Set = LEN(c2Set1)
      DO j=1,l1Set
       DO i=1,l2Set
C-    skip label "00" (since we start tracer numberi from 1)
        IF ( i.NE.1 .OR. j.NE.1 ) THEN
         n=n+1
         IF ( n.LE.nbLbl ) THEN
          ioLbl(n)(1:1) = c1Set1(j:j)
          ioLbl(n)(2:2) = c2Set1(i:i)
         ENDIF
        ENDIF
       ENDDO
      ENDDO

C--   2nd set of labels:
      l1Set = LEN(c1Set2)
      l2Set = LEN(c2Set2)
      DO j=1,l1Set
       DO i=1,l2Set
        n=n+1
        IF ( n.LE.nbLbl ) THEN
          ioLbl(n)(1:1) = c1Set2(j:j)
          ioLbl(n)(2:2) = c2Set2(i:i)
        ENDIF
       ENDDO
      ENDDO

C--   3rd set of labels:
      l1Set = LEN(c1Set3)
      l2Set = LEN(c2Set3)
      DO j=1,l1Set
       DO i=1,l2Set
        n=n+1
        IF ( n.LE.nbLbl ) THEN
          ioLbl(n)(1:1) = c1Set3(j:j)
          ioLbl(n)(2:2) = c2Set3(i:i)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
