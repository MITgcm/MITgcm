C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_utils.T,v 1.1 2004/01/05 21:38:27 edhill Exp $
C $Name:  $

C     MNC utilities duplicating things in MITgcm
      
C====================================================================
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

C====================================================================
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
