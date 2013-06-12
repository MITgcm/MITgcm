C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_taub.F,v 1.1 2013/06/12 21:30:22 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_TAUB ( myThid )

C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE
#ifdef STREAMICE_HYBRID_STRESS


C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k, l
      _RL ux, uy, vx, vy, exx, eyy, exy, second_inv, unorm
      _RL umid, vmid

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
cph(
          streamice_taubx (i,j,bi,bj) = 0.
          streamice_tauby (i,j,bi,bj) = 0.
cph)
          IF (STREAMICE_hmask(i,j,bi,bj).eq.1) THEN
  
!            streamice_taubx (i,j,bi,bj) = 
!      &      tau_beta_eff_streamice(i,j,bi,bj) * U_streamice(i,j,bi,bj)
!            streamice_tauby (i,j,bi,bj) = 
!      &      tau_beta_eff_streamice(i,j,bi,bj) * V_streamice(i,j,bi,bj)

           umid = 0.0
           vmid = 0.0

           DO k=0,1
            DO l=0,1
             umid = umid + 0.25 * 
     &        dxG(i,j+l,bi,bj)*dyG(i+k,j,bi,bj) * 
     &        recip_rA(i,j,bi,bj) *
     &        U_streamice(i+k,j+l,bi,bj) 
             vmid = vmid + 0.25 * 
     &        dxG(i,j+l,bi,bj)*dyG(i+k,j,bi,bj) * 
     &        recip_rA(i,j,bi,bj) *
     &        V_streamice(i+k,j+l,bi,bj)
            ENDDO
           ENDDO
  
           streamice_taubx (i,j,bi,bj) = 
     &      tau_beta_eff_streamice(i,j,bi,bj) * umid
           streamice_tauby (i,j,bi,bj) = 
     &      tau_beta_eff_streamice(i,j,bi,bj) * vmid
           
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO


#endif
#endif
      RETURN
      END
