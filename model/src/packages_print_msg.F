C $Header: /u/gcmpack/MITgcm/model/src/packages_print_msg.F,v 1.1 2014/01/19 23:58:46 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PACKAGES_PRINT_MSG
C     !INTERFACE:
      SUBROUTINE PACKAGES_PRINT_MSG( usePKG, pkgName, pkgMsg )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_PRINT_MSG
C     | o Print status (used or not) of a compiled package
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     pkgName ::  package name
C     pkgMsg  ::  package On/Off flag name
C     usePKG  ::  package On/Off flag
C     myThid  ::  My thread Id number
      LOGICAL usePKG
      CHARACTER*(*) pkgName, pkgMsg
c     INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_MBUF) pkgLwc
      CHARACTER*(MAX_LEN_MBUF) pkgUse
      CHARACTER*(20) prtFmt
      INTEGER iLen, pLen
      PARAMETER( pLen = 24 )
CEOP

      WRITE(pkgLwc,'(A)') ' '
      WRITE(pkgUse,'(A)') ' '
      WRITE(prtFmt,'(A,I2,A)') '(2(A,A', pLen, ',A),L2,A)'
c     print *, prtFmt

      iLen = LEN(pkgName)
      pkgLwc(1:iLen+4) = 'pkg/'//pkgName
      CALL LCASE(pkgLwc(1:iLen+4))

      IF ( pkgMsg .EQ. 'None' ) THEN
        WRITE(standardMessageUnit,prtFmt)
     &   ' ', pkgLwc(1:pLen), ' compiled   ( used ? )'
        RETURN
      ELSEIF ( pkgMsg .EQ. 'Used' ) THEN
        WRITE(standardMessageUnit,prtFmt)
     &   ' ', pkgLwc(1:pLen), ' compiled   and   used'
        RETURN
      ENDIF

      IF ( pkgMsg .EQ. ' ' ) THEN
        pkgUse(1:iLen+3) = 'use'//pkgName
      ELSE
        iLen = LEN(pkgMsg)
        pkgUse(1:iLen) = pkgMsg
      ENDIF
      IF ( usePKG ) THEN
        WRITE(standardMessageUnit,prtFmt)
     &   ' ', pkgLwc(1:pLen), ' compiled   and   used',
     &   ' ( ', pkgUse(1:pLen), ' =', usePKG, ' )'
      ELSE
        WRITE(standardMessageUnit,prtFmt)
     &   ' ', pkgLwc(1:pLen), ' compiled but not used',
     &   ' ( ', pkgUse(1:pLen), ' =', usePKG, ' )'
      ENDIF

      RETURN
      END
