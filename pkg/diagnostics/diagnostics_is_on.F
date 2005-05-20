C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_is_on.F,v 1.2 2005/05/20 07:28:50 jmc Exp $
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
      INTEGER j,n,m
      INTEGER iSp

      DIAGNOSTICS_IS_ON = .FALSE.

C-    search for this diag. in the active 2D/3D diagnostics list
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( diagName.EQ.flds(m,n) ) THEN
          j = jdiag(m,n)     
          IF ( idiag(j).NE.0 .AND. ndiag(j).GE.0 ) 
     &         DIAGNOSTICS_IS_ON = .TRUE.
        ENDIF
       ENDDO
      ENDDO

      IF ( .NOT.DIAGNOSTICS_IS_ON ) THEN
C-    search for this diag. in the active statistics-diagnostics list
       DO n=1,diagSt_nbLists
        DO m=1,diagSt_nbActv(n)
          IF ( diagName.EQ.diagSt_Flds(m,n) ) THEN
            j = jSdiag(m,n)
            iSp = iSdiag(j)
            IF ( iSp.NE.0 ) THEN
              IF (qSdiag(0,0,iSp,1,1).LT.0.) iSp = 0
            ENDIF
            IF ( iSp.NE.0 ) DIAGNOSTICS_IS_ON = .TRUE.
          ENDIF
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
