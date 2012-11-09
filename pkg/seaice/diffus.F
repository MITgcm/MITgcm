C $Header: /u/gcmpack/MITgcm/pkg/seaice/diffus.F,v 1.18 2012/11/09 22:15:18 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: DIFFUS
C     !INTERFACE:
      SUBROUTINE DIFFUS(
     U                   fld,
     I                   DIFFA, iceMsk, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIFFUS
C     | o Calculate laplacian of input field
C     |   and return the result in the same array
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     fld        :: In: input ice-field ; Out: laplacian of input field
C     DIFFA      :: grid length scale
C     iceMsk     :: Ocean/Land mask
C     myThid     :: my Thread Id. number
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DIFFA  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceMsk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
      _RL DELTXX, DELTYY
      _RL tmpFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( SEAICEuseFluxForm ) THEN
C--   Use flux form for MITgcm compliance, unfortunately changes results

          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            dfx(i,j) = 0. _d 0
            dfy(i,j) = 0. _d 0
           ENDDO
          ENDDO
C--   first compute fluxes across cell faces
          DO j=1,sNy+1
           DO i=1,sNx+1
            dfx(i,j) = _dyG(i,j,bi,bj) * _recip_dxC(i,j,bi,bj)
     &         * (fld(i,j,bi,bj)-fld(i-1,j,bi,bj))
     &         * cosFacU(j,bi,bj)
     &         * iceMsk(i,j,bi,bj)*iceMsk(i-1,j,bi,bj)
     &         * ( DIFFA(i,j,bi,bj)+DIFFA(i-1,j,bi,bj) )*HALF
#ifdef ALLOW_OBCS
     &         * maskInW(i,j,bi,bj)
#endif
            dfy(i,j) = _dxG(i,j,bi,bj) * _recip_dyC(i,j,bi,bj)
     &         * (fld(i,j,bi,bj)-fld(i,j-1,bi,bj))
#ifdef ISOTROPIC_COS_SCALING
     &         * cosFacV(j,bi,bj)
#endif
     &         * iceMsk(i,j,bi,bj)*iceMsk(i,j-1,bi,bj)
     &         * ( DIFFA(i,j,bi,bj)+DIFFA(i,j-1,bi,bj) )*HALF
#ifdef ALLOW_OBCS
     &         * maskInS(i,j,bi,bj)
#endif
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            fld(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
C--   compute Laplacian as flux divergence
          DO j=1,sNy
           DO i=1,sNx
            fld(i,j,bi,bj) = (
     &                         ( dfx(i+1,j) - dfx(i,j) )
     &                       + ( dfy(i,j+1) - dfy(i,j) )
     &                       ) * recip_rA(i,j,bi,bj)
           ENDDO
          ENDDO

        ELSE
C NOW DO DIFFUSION WITH NUIT CONVERSION

          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            tmpFld(i,j) = 0.0 _d 0
           ENDDO
          ENDDO

          DO j=1,sNy
           DO i=1,sNx
            DELTXX = DIFFA(i,j,bi,bj)
     &             * _recip_dxF(i,j,bi,bj)*_recip_dxF(i,j,bi,bj)
            DELTYY = DIFFA(i,j,bi,bj)
     &             * _recip_dyF(i,j,bi,bj)*_recip_dyF(i,j,bi,bj)
     &             * _recip_dxF(i,j,bi,bj)
            tmpFld(i,j) =
     &        DELTXX*(
     &                 (fld(i+1,j,bi,bj)-fld(i,  j,bi,bj))
     &                 *iceMsk(i+1,j,bi,bj)
#ifdef ALLOW_OBCS
     &                 *maskInW(i+1,j,bi,bj)
#endif
     &                -(fld(i,  j,bi,bj)-fld(i-1,j,bi,bj))
     &                 *iceMsk(i-1,j,bi,bj)
#ifdef ALLOW_OBCS
     &                 *maskInW(i,j,bi,bj)
#endif
     &               )
     &       +DELTYY*(
     &                 (fld(i,j+1,bi,bj)-fld(i,j,  bi,bj))
     &                 * _dxG(i,j+1,bi,bj)*iceMsk(i,j+1,bi,bj)
#ifdef ALLOW_OBCS
     &                 *maskInS(i,j+1,bi,bj)
#endif
     &                -(fld(i,j,  bi,bj)-fld(i,j-1,bi,bj))
     &                 * _dxG(i,j,  bi,bj)*iceMsk(i,j-1,bi,bj)
#ifdef ALLOW_OBCS
     &                 *maskInS(i,j,bi,bj)
#endif
     &               )
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            fld(i,j,bi,bj) = tmpFld(i,j)
           ENDDO
          ENDDO

C--  end flux-form / non flux-form
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
