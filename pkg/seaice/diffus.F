C $Header: /u/gcmpack/MITgcm/pkg/seaice/diffus.F,v 1.16 2011/05/25 20:42:49 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE DIFFUS( fld, DIFFA, iceMsk, DELTT, myThid )
C     *==========================================================*
C     | SUBROUTINE DIFFUS                                        |
C     | o Add diffusion terms to ice mass conservation equations |
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "SEAICE_PARAMS.h"
CML#include "SEAICE_GRID.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceMsk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DIFFA  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DELTT
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL DELTXX1, DELTYY1, DELTXX, DELTYY
C-    MPI+MTH: apply exch (sure with exch1) only to array in common block
      COMMON / LOCAL_DIFFUS / iceFld
      _RL iceFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dfx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          iceFld(I,J,bi,bj)=0.0 _d 0
         ENDDO
        ENDDO

        IF ( .NOT. SEAICEuseFluxForm ) THEN
C NOW DO DIFFUSION WITH NUIT CONVERSION
        DO j=1,sNy
         DO i=1,sNx
          DELTXX1=DELTT*DIFFA(I,J,bi,bj)
          DELTYY1=DELTT*DIFFA(I,J,bi,bj)
          DELTXX=DELTXX1 * _recip_dxF(I,J,bi,bj)* _recip_dxF(I,J,bi,bj)
          DELTYY=DELTYY1 * _recip_dyF(I,J,bi,bj)* _recip_dyF(I,J,bi,bj)
     &          * _recip_dxF(I,J,bi,bj)
          iceFld(I,J,bi,bj)=DELTXX*(
     &         (fld(I+1,J,bi,bj)-fld(I,  J,bi,bj))
#ifdef ALLOW_OBCS
     &         *maskInW(i+1,j,bi,bj)
#endif
     &         *iceMsk(I+1,J,bi,bj)
     &        -(fld(I,  J,bi,bj)-fld(I-1,J,bi,bj))
#ifdef ALLOW_OBCS
     &         *maskInW(i,j,bi,bj)
#endif
     &         *iceMsk(I-1,J,bi,bj))
     &         +DELTYY*(
     &         (fld(I,J+1,bi,bj)-fld(I,J,  bi,bj))
#ifdef ALLOW_OBCS
     &         *maskInS(i,j+1,bi,bj)
#endif
     &         * _dxG(I,J+1,bi,bj)*iceMsk(I,J+1,bi,bj)
     &        -(fld(I,J,  bi,bj)-fld(I,J-1,bi,bj))
#ifdef ALLOW_OBCS
     &         *maskInS(i,j,bi,bj)
#endif
     &         * _dxG(I,J,  bi,bj)*iceMsk(I,J-1,bi,bj))
         ENDDO
        ENDDO
        ELSE
C--   Use flux form for MITgcm compliance, unfortunately changes results
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          dfx(I,J) = 0. _d 0
          dfy(I,J) = 0. _d 0
         ENDDO
        ENDDO
C--   first compute fluxes across cell faces
        DO J=1,sNy+1
         DO I=1,sNx+1
          dfx(I,J) = _dyG(I,J,bi,bj) * _recip_dxC(I,J,bi,bj)
     &         * (fld(I,J,bi,bj)-fld(I-1,J,bi,bj))
     &         * cosFacU(J,bi,bj)
     &         * iceMsk(I,J,bi,bj) * iceMsk(I-1,J,bi,bj)
#ifdef ALLOW_OBCS
     &         * maskInW(i,j,bi,bj)
#endif
          dfy(I,J) = _dxG(I,J,bi,bj) * _recip_dyC(I,J,bi,bj)
     &         * (fld(I,J,bi,bj)-fld(I,J-1,bi,bj))
#ifdef ISOTROPIC_COS_SCALING
     &         * cosFacV(J,bi,bj)
#endif
     &         * iceMsk(I,J,bi,bj) * iceMsk(I,J-1,bi,bj)
#ifdef ALLOW_OBCS
     &         * maskInS(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          iceFld(I,J,bi,bj)=
     &         DELTT*DIFFA(I,J,bi,bj) * (
     &             dfx(I+1,J) - dfx(I,J)
     &           + dfy(I,J+1) - dfy(I,J)
     &         ) * recip_rA(I,J,bi,bj)
         ENDDO
        ENDDO
        ENDIF

       ENDDO
      ENDDO

      _EXCH_XY_RL(iceFld, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fld(I,J,bi,bj)=iceFld(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
