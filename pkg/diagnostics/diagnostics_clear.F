C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_clear.F,v 1.1 2005/06/26 16:51:49 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP 0
C     !ROUTINE: DIAGNOSTICS_CLEAR
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_CLEAR (listId, myThid)

C     !DESCRIPTION:
C***********************************************************************
C     Driver to clear diagnostics specified in diagnostic index list
C***********************************************************************

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     listId   ::  diagnostics list number
C     myThid :: my Thread Id number
      INTEGER listId, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER m, ndId, ipt

      DO m=1,nActive(listId)
       IF ( idiag(m,listId).GT.0 ) THEN
         ndId  = jdiag(m,listId)
         ipt   = idiag(m,listId)
         CALL DIAGNOSTICS_CLRDIAG ( ndId, ipt, myThid )
       ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_CLRDIAG
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_CLRDIAG ( ndId, ipt, myThid )

C     !DESCRIPTION:
C***********************************************************************
C     Zero out model diagnostic array elements
C***********************************************************************

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     ndId   :: diagnostic Id number (in available diagnostics list)
C     ipt    :: diagnostic pointer to storage array
C     myThid :: my Thread Id number
      INTEGER ndId, ipt
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k

C **********************************************************************
C ****              SET DIAGNOSTIC AND COUNTER TO ZERO              ****
C **********************************************************************

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k = 1,kdiag(ndId)
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           qdiag(i,j,ipt+k-1,bi,bj) = 0.0
          ENDDO
         ENDDO
        ENDDO
        ndiag(ipt,bi,bj) = 0
       ENDDO
      ENDDO


      RETURN
      END
