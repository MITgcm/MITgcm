#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_INIT_VARIA( myThid )

C     ==========================================================
C     | subroutine bling_init_varia
C     | o Initialize variable parameters
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
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          DO k = 1, Nr
            omegaC(i,j,k,bi,bj)     = 0. _d 0
            omegaAr(i,j,k,bi,bj)    = 0. _d 0
            irr_mem(i,j,k,bi,bj)    = 1. _d 0
            chl(i,j,k,bi,bj)        = 0. _d 0
            poc(i,j,k,bi,bj)        = 0. _d 0
            phyto_sm(i,j,k,bi,bj)   = 4. _d -7 / permil
            phyto_lg(i,j,k,bi,bj)   = 4. _d -7 / permil
            phyto_diaz(i,j,k,bi,bj) = 4. _d -7 / permil
          ENDDO
          Pc_2d(i,j,bi,bj)         = Pc_0
          Pc_2d_diaz(i,j,bi,bj)    = Pc_0_diaz
          alpha_photo2d(i,j,bi,bj) = alpha_photo
          k_Fe2d(i,j,bi,bj)        = k_Fe
          k_Fe_diaz2d(i,j,bi,bj)   = k_Fe_diaz
          gamma_POM2d(i,j,bi,bj)   = gamma_POM
          wsink0_2d(i,j,bi,bj)     = wsink0 
          phi_DOM2d(i,j,bi,bj)     = phi_DOM
          phi_sm2d(i,j,bi,bj)      = phi_sm
          phi_lg2d(i,j,bi,bj)      = phi_lg
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C Read in 2d bio related variables
        IF ( bling_Pc_2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_Pc_2dFile, ' ', Pc_2d, 0, myThid )
        _EXCH_XY_RL(Pc_2d,myThid)
        ENDIF
        IF ( bling_Pc_2d_diazFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_Pc_2d_diazFile,' ', 
     &                          Pc_2d_diaz, 0, myThid)
        _EXCH_XY_RL(Pc_2d_diaz,myThid)
        ENDIF
        IF (   bling_alpha_photo2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_alpha_photo2dFile, ' ',
     &                          alpha_photo2d, 0, myThid )
        _EXCH_XY_RL(alpha_photo2d,myThid)
        ENDIF
        IF ( bling_k_Fe2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_k_Fe2dFile, ' ', 
     &                          k_Fe2d, 0, myThid )
        _EXCH_XY_RL(k_Fe2d,myThid)
        ENDIF
        IF ( bling_k_Fe_diaz2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_k_Fe_diaz2dFile, ' ',
     &                          k_Fe_diaz2d, 0, myThid )
        _EXCH_XY_RL(k_Fe_diaz2d,myThid)
        ENDIF
        IF ( bling_gamma_POM2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_gamma_POM2dFile, ' ',
     &                          gamma_POM2d, 0, myThid )
        _EXCH_XY_RL(gamma_POM2d,myThid)
        ENDIF
        IF ( bling_wsink0_2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_wsink0_2dFile, ' ',
     &                          wsink0_2d, 0, myThid )
        _EXCH_XY_RL(wsink0_2d,myThid)
        ENDIF
        IF ( bling_phi_DOM2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_phi_DOM2dFile, ' ',
     &                          phi_DOM2d, 0, myThid )
        _EXCH_XY_RL(phi_DOM2d,myThid)
        ENDIF
        IF ( bling_phi_sm2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_phi_sm2dFile, ' ',
     &                          phi_sm2d, 0, myThid )
        _EXCH_XY_RL(phi_sm2d,myThid)
        ENDIF
        IF ( bling_phi_lg2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_phi_lg2dFile, ' ',
     &                          phi_lg2d, 0, myThid )
        _EXCH_XY_RL(phi_lg2d,myThid)
        ENDIF

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

#endif /* ALLOW_BLING */

      RETURN
      END
