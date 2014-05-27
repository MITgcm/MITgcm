C $Header: /u/gcmpack/MITgcm/model/src/packages_unused_msg.F,v 1.1 2014/05/27 21:23:07 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PACKAGES_UNUSED_MSG
C     !INTERFACE:
      SUBROUTINE PACKAGES_UNUSED_MSG( sw_name, sr_name, df_sufx )

C     !DESCRIPTION: \bv
C     *==============================================================*
C     | SUBROUTINE PACKAGES_UNUSED_MSG
C     | o This routine is called (within the corresponding
C     |   {PKG}_READPARAMS routine) when this {PKG} is not used; it
C     |   prints a (weak) warning if {PKG} parameter file is found.
C     *==============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     sw_name :: package on/off switch flag name
C     sr_name :: subroutine name which calls this S/R
C     df_sufx :: package parameter file sufix (prefix='data.')
C     myThid  ::  My thread Id number
      CHARACTER*(*) sw_name, sr_name, df_sufx
c     INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     === Local variables ===
C     caller_sub :: name of subroutine which is calling this S/R
C     data_file  :: parameter file to open and copy
C     pkgLwc     :: PKG name (Lower case)
C     pkgUpc     :: PKG name (Upper case)
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_FNAM) data_file
      CHARACTER*(MAX_LEN_MBUF) caller_sub
      CHARACTER*(MAX_LEN_MBUF) pkgLwc, pkgUpc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iLen, iLen1, iLen2, iLen3
      INTEGER myThid
      LOGICAL existing
CEOP

      WRITE(caller_sub,'(A)') ' '
      WRITE(data_file, '(A)') ' '

      iLen1 = ILNBLNK(sw_name)
      iLen2 = ILNBLNK(sr_name)
      iLen3 = ILNBLNK(df_sufx)

      IF ( iLen1.GE.4 ) THEN
        iLen = iLen1 - 3
        pkgLwc = sw_name(4:iLen1)
        CALL LCASE(pkgLwc(1:iLen))
        pkgUpc = sw_name(4:iLen1)
        CALL UCASE(pkgUpc(1:iLen))
          WRITE(data_file,'(2A)') 'data.', sw_name(4:iLen1)
      ELSE
        iLen = 7
        pkgLwc = 'unknown'
        pkgUpc = 'UNKNOWN'
      ENDIF
      IF ( iLen2.EQ.0 ) THEN
        WRITE(caller_sub,'(2A)') pkgUpc(1:iLen), '_READPARMS'
        iLen2 = iLen + 10
      ELSE
        WRITE(caller_sub,'(2A)') sr_name(1:iLen2)
      ENDIF
      IF ( iLen3.EQ.0 ) THEN
        WRITE(data_file,'(2A)') 'data.', pkgLwc(1:iLen)
        iLen3 = 5 + iLen
      ELSE
        WRITE(data_file,'(2A)') 'data.', df_sufx(1:iLen3)
        iLen3 = 5 + iLen3
      ENDIF

c       WRITE(errorMessageUnit,'(I4,3A)')
c    &        iLen1, ' >', sw_name(1:iLen1),    '<'
c       WRITE(errorMessageUnit,'(I4,3A)')
c    &        iLen2, ' >', caller_sub(1:iLen2), '<'
c       WRITE(errorMessageUnit,'(I4,3A)')
c    &        iLen3, ' >', data_file(1:iLen3),  '<'

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   PKG exf is not used: print a (weak) warning if data_file is found
      myThid = 1
      IF ( iLen1.GE.1 ) THEN
        INQUIRE( FILE=data_file, EXIST=existing )
        IF ( existing ) THEN
           WRITE(msgBuf,'(5A)') '** Warning ** ', caller_sub(1:iLen2),
     &       ': ignores "', data_file(1:iLen3), '" file since'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(5A)') '** Warning ** ', caller_sub(1:iLen2),
     &       ': ', sw_name(1:iLen1), '= F (set from "data.pkg")'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

      RETURN
      END
