C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_utils.T,v 1.3 2004/01/27 05:47:32 edhill Exp $
C $Name:  $

C     MNC utilities duplicating things in MITgcm
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Copied from: eesupp/src/utils.F

      INTEGER FUNCTION IFNBLNK( string )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION IFNBLNK                                         |
C     | o Find first non-blank in character string.              |
C     *==========================================================*
C
C     !INPUT PARAMETERS:
C     string :: String to find first non-blank in.
      CHARACTER*(*) string

C     !LOCAL VARIABLES:
C     L, LS :: Temps for string locations
      INTEGER L, LS
CEOP
C
      LS     = LEN(string)
      IFNBLNK = 0
      DO 10 L = 1, LS
       IF ( string(L:L) .EQ. ' ' ) GOTO 10
        IFNBLNK = L
        GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Copied from: eesupp/src/utils.F

      INTEGER FUNCTION ILNBLNK( string )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     *==========================================================*

C     !INPUT PARAMETERS:
C     string :: string to scan
      CHARACTER*(*) string

C     !LOCAL VARIABLES:
C     L, LS :: Temps. used in scanning string
      INTEGER L, LS
CEOP
C
      LS      = LEN(string)
c     ILNBLNK = LS
      ILNBLNK = 0
      DO 10 L = LS, 1, -1
        IF ( string(L:L) .EQ. ' ' ) GOTO 10
         ILNBLNK = L
         GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Adapted from: eesupp/src/print.F

      SUBROUTINE PRINT_ERROR( message , myThid )
      IMPLICIT NONE

      CHARACTER*(*) message
      INTEGER       myThid

      print*, '=== BEGIN ERROR ==='
      print*, 'MyThid: ', myThid
      print*, 'Message:'
      print*, message
      print*, '=== END ERROR ==='

      RETURN
      END

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
