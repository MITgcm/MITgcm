C $Header: /u/gcmpack/MITgcm/pkg/seaice/diffus.F,v 1.14 2009/06/22 15:55:27 mlosch Exp $
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
     &         *iceMsk(I+1,J,bi,bj)
     &        -(fld(I,  J,bi,bj)-fld(I-1,J,bi,bj))
     &         *iceMsk(I-1,J,bi,bj))
     &         +DELTYY*(
     &         (fld(I,J+1,bi,bj)-fld(I,J,  bi,bj))
     &         * _dxG(I+1,J+1,bi,bj)*iceMsk(I,J+1,bi,bj)
     &        -(fld(I,J,  bi,bj)-fld(I,J-1,bi,bj))
     &         * _dxG(I+1,J,  bi,bj)*iceMsk(I,J-1,bi,bj))
         ENDDO
        ENDDO
        ELSE
        DO j=1,sNy
         DO i=1,sNx
C--   Use flux form for MITgcm compliance, unfortunately changes results
          iceFld(I,J,bi,bj)=
     &         DELTT*DIFFA(I,J,bi,bj) * recip_rA(I,J,bi,bj) * (
C     x - compontent
     &        + _dyG(I+1,J,bi,bj) * _recip_dxC(I+1,J,bi,bj)
     &         * (fld(I+1,J,bi,bj)-fld(I,  J,bi,bj))
     &         * cosFacU(J,bi,bj) * iceMsk(I+1,J,bi,bj)
     &        - _dyG(I  ,J,bi,bj) * _recip_dxC(I  ,J,bi,bj)
     &         * (fld(I,  J,bi,bj)-fld(I-1,J,bi,bj))
     &         * cosFacU(J,bi,bj) * iceMsk(I-1,J,bi,bj)
C     y - compontent
     &        + _dxG(I,J+1,bi,bj) * _recip_dyC(I,J+1,bi,bj)
     &         * (fld(I,J+1,bi,bj)-fld(I,J,  bi,bj))
#ifdef ISOTROPIC_COS_SCALING
     &         * cosFacV(J+1,bi,bj)
#endif
     &         * iceMsk(I,J+1,bi,bj)
     &        - _dxG(I,J,bi,bj) * _recip_dyC(I,J,bi,bj)
     &         * (fld(I,J,  bi,bj)-fld(I,J-1,bi,bj))
#ifdef ISOTROPIC_COS_SCALING
     &         * cosFacV(J,bi,bj)
#endif
     &         * iceMsk(I,J-1,bi,bj)
C     apply mask for safety
     &         ) * iceMsk(I,J,bi,bj)
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
