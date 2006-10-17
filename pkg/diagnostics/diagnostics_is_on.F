C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_is_on.F,v 1.4 2006/10/17 18:56:31 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_IS_ON

C     !INTERFACE:
      LOGICAL FUNCTION DIAGNOSTICS_IS_ON( diagName, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION DIAGNOSTIC_IS_ON
C     | o Return TRUE if diagnostics "diagName" is Active
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     diagName   ::  diagnostic identificator name (8 characters long)
C     myThid     ::  my thread Id number
      CHARACTER*8  diagName
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER n,m
      INTEGER ip,iSp
      INTEGER bi,bj

      DIAGNOSTICS_IS_ON = .FALSE.
      bi = myBxLo(myThid)
      bj = myByLo(myThid)

C-    search for this diag. in the active 2D/3D diagnostics list
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( diagName.EQ.flds(m,n) .AND. idiag(m,n).NE.0 ) THEN
          ip = ABS(idiag(m,n))
          IF ( ndiag(ip,bi,bj).GE.0 ) DIAGNOSTICS_IS_ON = .TRUE.
        ENDIF
       ENDDO
      ENDDO

      IF ( .NOT.DIAGNOSTICS_IS_ON ) THEN
C-    search for this diag. in the active statistics-diagnostics list
       DO n=1,diagSt_nbLists
        DO m=1,diagSt_nbActv(n)
         IF ( diagName.EQ.diagSt_Flds(m,n)
     &               .AND.iSdiag(m,n).NE.0 ) THEN
           iSp = ABS(iSdiag(m,n))
           IF (qSdiag(0,0,iSp,bi,bj).GE.0.) DIAGNOSTICS_IS_ON = .TRUE.
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
