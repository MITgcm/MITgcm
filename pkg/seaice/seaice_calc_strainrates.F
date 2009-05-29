C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_strainrates.F,v 1.12 2009/05/29 10:18:02 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_CALC_STRAINRATES(
     I     uFld, vFld,
     O     e11Loc, e22Loc, e12Loc,
     I     kSize, iStep, myTime, myIter, myThid )
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
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     iStep  :: Sub-time-step number
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: My Thread Id. number
C     kSize  :: length of 3rd dimension of velocity variables
      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER kSize
C     ice velocities
      _RL uFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize,nSx,nSy)
      _RL vFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize,nSx,nSy)
C     strain rate tensor
      _RL e11Loc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22Loc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12Loc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEndOfInterface

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS
C     === Local variables ===
C     i,j,bi,bj - Loop counters
      INTEGER i, j, bi, bj
C  hFacU, hFacV - determine the no-slip boundary condition
      INTEGER k
      _RS hFacU, hFacV, noSlipFac

      k = 1
      noSlipFac = 0. _d 0
      IF ( SEAICE_no_slip ) noSlipFac = 1. _d 0
C
#ifndef SEAICE_OLD_AND_BAD_DISCRETIZATION
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
C     evaluate strain rates
          e11Loc(I,J,bi,bj) = _recip_dxF(I,J,bi,bj) *
     &         (uFld(I+1,J,1,bi,bj)-uFld(I,J,1,bi,bj))
     &         +HALF*
     &         (vFld(I,J,1,bi,bj)+vFld(I,J+1,1,bi,bj))
     &         * k2AtC(I,J,bi,bj)
          e22Loc(I,J,bi,bj) = _recip_dyF(I,J,bi,bj) *
     &         (vFld(I,J+1,1,bi,bj)-vFld(I,J,1,bi,bj))
     &         +HALF*
     &         (uFld(I,J,1,bi,bj)+uFld(I+1,J,1,bi,bj))
     &         * k1AtC(I,J,bi,bj)
C     one metric term is missing
         ENDDO
        ENDDO
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          hFacU = _maskW(i,j,k,bi,bj) - _maskW(i,j-1,k,bi,bj)
          hFacV = _maskS(i,j,k,bi,bj) - _maskS(i-1,j,k,bi,bj)
          e12Loc(I,J,bi,bj) = HALF*(
     &           ( uFld(I,J,1,bi,bj) - uFld(I  ,J-1,1,bi,bj) )
     &         * _recip_dyU(I,J,bi,bj)
     &         + ( vFld(I,J,1,bi,bj) - vFld(I-1,J  ,1,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj) 
     &         - k1AtZ(I,J,bi,bj)
     &         * 0.5 _d 0 * (vFld(I,J,1,bi,bj)+vFld(I-1,J  ,1,bi,bj))
     &         - k2AtZ(I,J,bi,bj)
     &         * 0.5 _d 0 * (uFld(I,J,1,bi,bj)+uFld(I  ,J-1,1,bi,bj))
     &         )
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
     &         + 2.0 _d 0 * noSlipFac * (
     &           ( uFld(I,J,1,bi,bj) + uFld(I  ,J-1,1,bi,bj) )
     &         * _recip_dyU(I,J,bi,bj) * hFacU
     &         + ( vFld(I,J,1,bi,bj) + vFld(I-1,J  ,1,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj) * hFacV
     &         )
C     no slip at the boundary implies u(j)+u(j-1)=0 and v(i)+v(i-1)=0
C     accross the boundary; this is already accomplished by masking so
C     that the following lines are not necessary
c$$$     &         - hFacV * k1AtZ(I,J,bi,bj)
c$$$     &         * 0.5 _d 0 * (vFld(I,J,1,bi,bj)+vFld(I-1,J  ,1,bi,bj))
c$$$     &         - hFacU * k2AtZ(I,J,bi,bj)
c$$$     &         * 0.5 _d 0 * (uFld(I,J,1,bi,bj)+uFld(I  ,J-1,1,bi,bj))
         ENDDO
        ENDDO

c$$$        ENDIF
       ENDDO
      ENDDO
#else 
C     this the old and incomplete discretization, here I also erroneously
C     used finite-volumes to discretize the strain rates
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
C     evaluate strain rates
          e11Loc(I,J,bi,bj) = _recip_dxF(I,J,bi,bj) *
     &         (uFld(I+1,J,1,bi,bj)-uFld(I,J,1,bi,bj))
     &         -HALF*
     &         (vFld(I,J,1,bi,bj)+vFld(I,J+1,1,bi,bj))
     &         * _tanPhiAtU(I,J,bi,bj)*recip_rSphere
          e22Loc(I,J,bi,bj) = _recip_dyF(I,J,bi,bj) *
     &         (vFld(I,J+1,1,bi,bj)-vFld(I,J,1,bi,bj))
C     one metric term is missing
         ENDDO
        ENDDO
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          e12Loc(I,J,bi,bj) = HALF*(
     &         (uFld(I  ,J  ,1,bi,bj) * _dxC(I  ,J  ,bi,bj)
     &         -uFld(I  ,J-1,1,bi,bj) * _dxC(I  ,J-1,bi,bj)
     &         +vFld(I  ,J  ,1,bi,bj) * _dyC(I  ,J  ,bi,bj)
     &         -vFld(I-1,J  ,1,bi,bj) * _dyC(I-1,J  ,bi,bj))
     &         * recip_rAz(I,J,bi,bj)
     &         +
     &         0.25 _d 0 * (uFld(I,J,1,bi,bj)+uFld(I  ,J-1,1,bi,bj))
     &         * ( _tanPhiAtU(I,J,bi,bj) + _tanPhiAtU(I,J-1,bi,bj) )
     &         *recip_rSphere
     &         )
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
C     one metric term is missing
         ENDDO
        ENDDO
        IF ( SEAICE_no_slip ) THEN
C     no slip boundary conditions apply only to e12Loc
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
           hFacU = _maskW(i,j,k,bi,bj) - _maskW(i,j-1,k,bi,bj)
           hFacV = _maskS(i,j,k,bi,bj) - _maskS(i-1,j,k,bi,bj)

           e12Loc(I,J,bi,bj) = e12Loc(I,J,bi,bj)
     &          + recip_rAz(i,j,bi,bj) * 2. _d 0 *
     &          ( hFacU * ( _dxC(i,j-1,bi,bj)*uFld(i,j  ,1,bi,bj)
     &                    + _dxC(i,j,  bi,bj)*uFld(i,j-1,1,bi,bj) )
     &          + hFacV * ( _dyC(i-1,j,bi,bj)*vFld(i  ,j,1,bi,bj)
     &                    + _dyC(i,  j,bi,bj)*vFld(i-1,j,1,bi,bj) ) )
     &         - hFacU
     &         * 0.25 _d 0 * (uFld(I,J,1,bi,bj)+uFld(I  ,J-1,1,bi,bj))
     &         * ( _tanPhiAtU(I,J,bi,bj) + _tanPhiAtU(I,J-1,bi,bj) )
     &         *recip_rSphere
C     one metric term is missing
          ENDDO
         ENDDO

        ENDIF
       ENDDO
      ENDDO
#endif /* SEAICE_OLD_AND_BAD_DISCRETIZATION */
#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */
      RETURN
      END
