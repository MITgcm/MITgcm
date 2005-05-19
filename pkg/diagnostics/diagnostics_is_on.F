C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_is_on.F,v 1.1 2005/05/19 01:18:31 jmc Exp $
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

      DIAGNOSTICS_IS_ON = .FALSE.
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( diagName.EQ.flds(m,n) ) THEN
          j = jdiag(m,n)     
          IF ( idiag(j).NE.0 .AND. ndiag(j).GE.0 ) 
     &         DIAGNOSTICS_IS_ON = .TRUE.
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
