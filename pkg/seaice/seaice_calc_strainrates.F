C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_strainrates.F,v 1.1 2007/04/20 18:29:58 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_CALC_STRAINRATES( 
     I     uFld, vFld,
     O     e11, e22, e12,
     I     myThid )
C     /==========================================================\
C     | SUBROUTINE  SEAICE_CALC_STRAINRATES                      |
C     | o compute strain rates from ice velocities               |
C     |==========================================================|
C     | written by Martin Losch, Apr 2007                        |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
C     ice velocities
      _RL uFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL vFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     strain rate tensor
      _RL e11 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEndOfInterface

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS
C     === Local variables ===
C     i,j,bi,bj - Loop counters
      INTEGER i, j, bi, bj

C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
C NOW EVALUATE STRAIN RATES
          e11(I,J,bi,bj)= _recip_dxF(I,J,bi,bj) *
     &         (uFld(I+1,J,bi,bj)-uFld(I,J,bi,bj))
     &         -HALF*
     &         (vFld(I,J,bi,bj)+vFld(I,J+1,bi,bj))
     &         * _tanPhiAtU(I,J,bi,bj)*recip_rSphere
          e22(I,J,bi,bj)= _recip_dyF(I,J,bi,bj) *
     &         (vFld(I,J+1,bi,bj)-vFld(I,J,bi,bj))
C     one metric term is missing
          e12(I,J,bi,bj)=HALF*(
     &         (uFld(I  ,J  ,bi,bj) * _dxC(I  ,J  ,bi,bj)
     &         -uFld(I  ,J-1,bi,bj) * _dxC(I  ,J-1,bi,bj)
     &         +vFld(I  ,J  ,bi,bj) * _dyC(I  ,J  ,bi,bj)
     &         -vFld(I-1,J  ,bi,bj) * _dyC(I-1,J  ,bi,bj))
     &         * recip_rAz(I,J,bi,bj)
     &         +
     &         0.25 _d 0 * (uFld(I,J,bi,bj)+uFld(I  ,J-1,bi,bj))
     &         * ( _tanPhiAtU(I,J,bi,bj) + _tanPhiAtU(I,J-1,bi,bj) )
     &         *recip_rSphere
     &         )
C     one metric term is missing
CML          e12(I,J,bi,bj)=HALF*(
CML     &         (uFld(I,J+1,bi,bj)+uFld(I+1,J+1,bi,bj)
CML     &         -uFld(I,J-1,bi,bj)-uFld(I+1,J-1,bi,bj))
CML     &         * 1. _d 0 / (dyC(I,J,bi,bj) + dyC(I,J-1,bi,bj))
CML     &         +
CML     &         (vFld(I+1,J+1,bi,bj)+vFld(I+1,J,bi,bj)
CML     &         -vFld(I-1,J+1,bi,bj)-vFld(I-1,J,bi,bj))
CML     &         * 1. _d 0 / (dxC(I,J,bi,bj) + dxC(I-1,J,bi,bj))
CML     &         +HALF*
CML     &         (uFld(I,  J,  bi,bj)+uFld(I+1,J,  bi,bj))
CML     &         * _tanPhiAtU(I,J,bi,bj)*recip_rSphere)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */
      RETURN
      END
