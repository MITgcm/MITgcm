C $Header: /u/gcmpack/MITgcm/pkg/down_slope/dwnslp_init_varia.F,v 1.2 2010/04/23 13:19:26 jmc Exp $
C $Name:  $

#include "DWNSLP_OPTIONS.h"

CBOP
C     !ROUTINE: DWNSLP_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE DWNSLP_INIT_VARIA( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DWNSLP_INIT_VARIA
C     | o Initialize Down_Slope variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "DWNSLP_SIZE.h"
#include "DWNSLP_PARAMS.h"
#include "DWNSLP_VARS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    :: my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER n, bi, bj

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
        DO n=1,DWNSLP_size
          DWNSLP_deepK (n,bi,bj) = 0
          DWNSLP_Transp(n,bi,bj) = 0. _d 0
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END
