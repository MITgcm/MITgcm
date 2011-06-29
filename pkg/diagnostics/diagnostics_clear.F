C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_clear.F,v 1.3 2011/06/29 21:59:55 jmc Exp $
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
      INTEGER m, ndId, ipt, nLev

      DO m=1,nActive(listId)
       IF ( idiag(m,listId).GT.0 ) THEN
         ndId  = jdiag(m,listId)
         ipt   = idiag(m,listId)
         nLev  = kdiag(ndId)*averageCycle(listId)
c        WRITE(0,'(2(A,I3),2A,2(A,I5))') 'CLRDIAG (listId=',listId,
c    &     ' )',nLev,' levs, diag: ',cdiag(ndId),
c    &     ' ipt=',ipt,' , ndiag=',ndiag(ipt,1,1)
         CALL DIAGNOSTICS_CLRDIAG( ipt, nLev, myThid )
       ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_CLRDIAG
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_CLRDIAG ( ipt, nLev, myThid )

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
C     ipt    :: diagnostic pointer to storage array
C     nLev   :: number of levels (in storage array) to reset
C     myThid :: my Thread Id number
      INTEGER ipt, nLev
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER kl

C **********************************************************************
C ****              SET DIAGNOSTIC AND COUNTER TO ZERO              ****
C **********************************************************************

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k = 1,nLev
         kl = ipt+k-1
         ndiag(kl,bi,bj) = 0
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           qdiag(i,j,kl,bi,bj) = 0.0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO


      RETURN
      END
