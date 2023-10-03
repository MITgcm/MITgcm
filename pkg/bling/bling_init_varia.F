#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_INIT_VARIA( myThid )

C     ==========================================================
C     | subroutine bling_init_varia
C     | o Initialize variable parameters
C     ==========================================================

      IMPLICIT NONE

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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DO k = 1, Nr
            omegaC(i,j,k,bi,bj)     = 0. _d 0
            omegaAr(i,j,k,bi,bj)    = 0. _d 0
            irr_mem(i,j,k,bi,bj)    = 1. _d 0
            chl(i,j,k,bi,bj)        = 0. _d 0
            poc(i,j,k,bi,bj)        = 0. _d 0
            phyto_sm(i,j,k,bi,bj)   = 4. _d -7 / permil
            phyto_lg(i,j,k,bi,bj)   = 4. _d -7 / permil
#ifndef USE_BLING_V1
            phyto_diaz(i,j,k,bi,bj) = 4. _d -7 / permil
#endif
          ENDDO
          chl_sat(i,j,bi,bj)        = 0. _d 0
          gamma_POM_2d(i,j,bi,bj)   = gamma_POM
          wsink0_2d(i,j,bi,bj)      = wsink0
          phi_DOM_2d(i,j,bi,bj)     = phi_DOM
          phi_sm_2d(i,j,bi,bj)      = phi_sm
          phi_lg_2d(i,j,bi,bj)      = phi_lg
          Pc_0_2d(i,j,bi,bj)        = Pc_0
          k_Fe_2d(i,j,bi,bj)        = k_Fe
#ifndef USE_BLING_V1
          k_Fe_diaz_2d(i,j,bi,bj)   = k_Fe_diaz
          Pc_0_diaz_2d(i,j,bi,bj)   = Pc_0_diaz
          alpha_photo_2d(i,j,bi,bj) = alpha_photo
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C Read in 2d bio related variables
        IF ( bling_Pc_2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_Pc_2dFile, ' ',
     &                          Pc_0_2d, 0, myThid )
        _EXCH_XY_RL(Pc_0_2d,myThid)
        ENDIF
        IF ( bling_k_Fe2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_k_Fe2dFile, ' ',
     &                          k_Fe_2d, 0, myThid )
        _EXCH_XY_RL(k_Fe_2d,myThid)
        ENDIF
        IF ( bling_gamma_POM2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_gamma_POM2dFile, ' ',
     &                          gamma_POM_2d, 0, myThid )
        _EXCH_XY_RL(gamma_POM_2d,myThid)
        ENDIF
        IF ( bling_wsink0_2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_wsink0_2dFile, ' ',
     &                          wsink0_2d, 0, myThid )
        _EXCH_XY_RL(wsink0_2d,myThid)
        ENDIF
        IF ( bling_phi_DOM2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_phi_DOM2dFile, ' ',
     &                          phi_DOM_2d, 0, myThid )
        _EXCH_XY_RL(phi_DOM_2d,myThid)
        ENDIF
        IF ( bling_phi_sm2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_phi_sm2dFile, ' ',
     &                          phi_sm_2d, 0, myThid )
        _EXCH_XY_RL(phi_sm_2d,myThid)
        ENDIF
        IF ( bling_phi_lg2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_phi_lg2dFile, ' ',
     &                          phi_lg_2d, 0, myThid )
        _EXCH_XY_RL(phi_lg_2d,myThid)
        ENDIF
#ifndef USE_BLING_V1
        IF ( bling_Pc_2d_diazFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_Pc_2d_diazFile,' ',
     &                          Pc_0_diaz_2d, 0, myThid)
        _EXCH_XY_RL(Pc_0_diaz_2d,myThid)
        ENDIF
        IF (   bling_alpha_photo2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_alpha_photo2dFile, ' ',
     &                          alpha_photo_2d, 0, myThid )
        _EXCH_XY_RL(alpha_photo_2d,myThid)
        ENDIF
        IF ( bling_k_Fe_diaz2dFile .NE. ' ' ) THEN
           CALL READ_FLD_XY_RL( bling_k_Fe_diaz2dFile, ' ',
     &                          k_Fe_diaz_2d, 0, myThid )
        _EXCH_XY_RL(k_Fe_diaz_2d,myThid)
        ENDIF
#endif

#ifdef ADVECT_PHYTO
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            phyto_sm(i,j,k,bi,bj)   = 0.5 _d 0
            phyto_lg(i,j,k,bi,bj)   = 0.5 _d 0
            phyto_diaz(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#else
C     Read phyto plankton abundance ICs and exchange
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
#ifndef USE_BLING_V1
        IF ( bling_PdiazFile .NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RL(bling_pdiazFile,' ',
     &        phyto_diaz(1-OLx,1-OLy,1,1,1),0,myThid)
         _EXCH_XYZ_RL(phyto_diaz(1-OLx,1-OLy,1,1,1),myThid)
        ENDIF
#endif
      ENDIF
#endif /* ifdef ADVECT_PHYTO */
C     Apply mask
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF (maskC(i,j,k,bi,bj).EQ.0.) THEN
             phyto_sm(i,j,k,bi,bj)   = 0. _d 0
             phyto_lg(i,j,k,bi,bj)   = 0. _d 0
#ifndef USE_BLING_V1
             phyto_diaz(i,j,k,bi,bj) = 0. _d 0
#endif
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
