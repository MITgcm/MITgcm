C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_init_varia.F,v 1.2 2016/09/12 20:00:28 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_INIT_VARIA( myThid )

C     ==========================================================
C     | subroutine bling_init_varia
C     | o Initialize variable quantities
C     ==========================================================

      implicit none
      
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#ifdef ALLOW_BLING_COST
# include "BLING_COST.h"
#endif
C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING
      INTEGER i,j, bi,bj
      INTEGER k
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialise global variables
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1, Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            omegaC(i,j,k,bi,bj)     = 0. _d 0
            omegaAr(i,j,k,bi,bj)    = 0. _d 0
            irr_mem(i,j,k,bi,bj)    = 1. _d 0
            chl(i,j,k,bi,bj)        = 0. _d 0
            phyto_sm(i,j,k,bi,bj)   = 4. _d -7 / permil
            phyto_lg(i,j,k,bi,bj)   = 4. _d -7 / permil
            phyto_diaz(i,j,k,bi,bj) = 4. _d -7 / permil
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

# ifdef ALLOW_BLING_COST
      totcost =  0. _d 0
# endif

#endif /* ALLOW_BLING */

      RETURN
      END
