C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_clear.F,v 1.1 2005/05/20 07:28:50 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGSTATS_CLEAR

C     !INTERFACE:
      SUBROUTINE DIAGSTATS_CLEAR( listId, myThid)

C     !DESCRIPTION:
C     Clear statistics-diagnostics specified in 1 output stream list

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     listId :: Diagnostics list number being written
C     myThid  :: my Thread Id number
      INTEGER  listId, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER l, n

      DO l=1,diagSt_nbActv(listId)
       n = jSdiag(l,listId)
       IF ( iSdiag(n).NE.0 ) THEN
         CALL DIAGSTATS_CLRDIAG( n, myThid )
       ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGSTATS_CLEAR

C     !INTERFACE:
      SUBROUTINE DIAGSTATS_CLRDIAG( ndId, myThid )

C     !DESCRIPTION:
C     Zero out 1 statistics-diagnostic array 

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     ndId    :: Diagnostics Id Number (in available diag. list) to zero out
C     myThid  :: my Thread Id number
      INTEGER ndId, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k

C **********************************************************************
C ****              SET DIAGNOSTIC AND COUNTER TO ZERO              ****
C **********************************************************************

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k = iSdiag(ndId),iSdiag(ndId)+kdiag(ndId)-1
         DO j = 0,nRegions
          DO i = 0,nStats
           qSdiag(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
