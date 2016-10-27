C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_init_varia.F,v 1.3 2016/10/27 17:55:31 mmazloff Exp $
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
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
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

C     Now read phyto plankton abundance ICs and exchange
      IF (nIter0.EQ.PTRACERS_Iter0) THEN
        IF ( bling_psmFile .NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RL(bling_psmFile,' ',
     &        phyto_sm(1-OLx,1-OLy,1,1,1),0,myThid)
         _EXCH_XYZ_RL(phyto_sm(1-OLx,1-OLy,1,1,1),myThid)
        ENDIF
        IF (bling_plgFile .NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RL(bling_plgFile,' ',
     &        phyto_lg(1-OLx,1-OLy,1,1,1),0,myThid)
         _EXCH_XYZ_RL(phyto_lg(1-OLx,1-OLy,1,1,1),myThid)
        ENDIF
        IF ( bling_PdiazFile .NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RL(bling_pdiazFile,' ',
     &        phyto_diaz(1-OLx,1-OLy,1,1,1),0,myThid)
         _EXCH_XYZ_RL(phyto_diaz(1-OLx,1-OLy,1,1,1),myThid)
        ENDIF
      ENDIF
C     Apply mask
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF (maskC(i,j,k,bi,bj).EQ.0.) THEN
             phyto_sm(i,j,k,bi,bj)   = 0. _d 0
             phyto_lg(i,j,k,bi,bj)   = 0. _d 0
             phyto_diaz(i,j,k,bi,bj) = 0. _d 0
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      IF ( nIter0.GT.PTRACERS_Iter0 .OR.
C     &    (nIter0.EQ.PTRACERS_Iter0 .AND. pickupSuff.NE.' ')
C IF ABOVE TRUE phyto READ FROM PICKUP AND OVERWRITES IC READ
C PICKUP CALLED FROM bling_carbonate_init.F
C

# ifdef ALLOW_BLING_COST
      totcost =  0. _d 0
# endif

#endif /* ALLOW_BLING */

      RETURN
      END
