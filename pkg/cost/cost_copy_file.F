#include "COST_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: COST_COPY_FILE

C !INTERFACE:
      SUBROUTINE COST_COPY_FILE( oUnit, myThid )
C !DESCRIPTION:
C     Helper routine to hide irreducible statements from TAF:
C     Copy content of costfunction_${pkg}XXXX to oUnit

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#ifdef ALLOW_CTRL
# include "OPTIMCYCLE.h"
#endif

C !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
C     oUnit  :: file unit of output file (set in cost_final.F)
      INTEGER myThid
      INTEGER oUnit
CEOP

C !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES:
      INTEGER iUnit
      INTEGER IL
      INTEGER nPkg, iPkg
      PARAMETER ( nPkg = 7 )
      CHARACTER*8 pkgNames(nPkg)
      CHARACTER*(MAX_LEN_FNAM) cfname
      CHARACTER*(MAX_LEN_PREC) RECORD
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL cfexist

C     This list can be appened as necessary with the only limitation
C     that the character variables need to be all of the same length.
      DATA pkgNames /
     &     'profiles',
     &     'obsfit  ',
     &     'ecco    ',
     &     'ctrl    ',
     &     'obcs    ',
     &     'seaice  ',
     &     'shelfice' /

      iUnit = scrUnit1
      DO ipkg = 1, nPkg
       cfexist = .FALSE.
       IL = ILNBLNK( pkgNames(ipkg) )
       WRITE(cfname,'(A,A,I4.4)')
     &      'costfunction_',pkgNames(ipkg)(1:IL),optimcycle
       IL = ILNBLNK( cfname )
       INQUIRE(FILE=cfname(1:IL), EXIST=cfexist)
       IF ( cfexist ) THEN
        WRITE(msgBuf,'(A,A)')
     &       'Reading cost function info from ', cfname(1:IL)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        OPEN(UNIT=iUnit,FILE=cfname(1:IL),
     &       _READONLY_ACTION STATUS='OLD')

        DO WHILE ( .TRUE. )
         READ( iUnit, FMT='(A)', END=1001 ) RECORD
         IL = MAX(ILNBLNK( RECORD ),1)
         WRITE( oUnit, FMT='(A)') RECORD(:IL)
        ENDDO
 1001   CONTINUE

        CLOSE(iUnit)
       ENDIF
      ENDDO

      RETURN
      END
