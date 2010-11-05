C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_strainrates.F,v 1.17 2010/11/05 08:13:03 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_CALC_STRAINRATES(
     I     uFld, vFld,
     O     e11Loc, e22Loc, e12Loc,
     I     iStep, myTime, myIter, myThid )
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
      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     ice velocities
      _RL uFld   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL vFld   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
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
C     hFacU, hFacV - determine the no-slip boundary condition
      INTEGER k
      _RS hFacU, hFacV, noSlipFac
C     auxillary variables that help writing code that
C     vectorizes even after TAFization
      _RL dudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dvdy (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dudy (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dvdx (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uave (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vave (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      k = 1
      noSlipFac = 0. _d 0
      IF ( SEAICE_no_slip ) noSlipFac = 1. _d 0
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     abbreviations on C-points, need to do them in separate loops
C     for vectorization
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          dudx(I,J) = _recip_dxF(I,J,bi,bj) *
     &         (uFld(I+1,J,bi,bj)-uFld(I,J,bi,bj))
          uave(I,J) = 0.5 _d 0 * (uFld(I,J,bi,bj)+uFld(I+1,J,bi,bj))
         ENDDO
        ENDDO
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          dvdy(I,J) = _recip_dyF(I,J,bi,bj) *
     &         (vFld(I,J+1,bi,bj)-vFld(I,J,bi,bj))
          vave(I,J) = 0.5 _d 0 * (vFld(I,J,bi,bj)+vFld(I,J+1,bi,bj))
         ENDDO
        ENDDO
C     evaluate strain rates at C-points
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          e11Loc(I,J,bi,bj) = dudx(I,J) + vave(I,J) * k2AtC(I,J,bi,bj)
          e22Loc(I,J,bi,bj) = dvdy(I,J) + uave(I,J) * k1AtC(I,J,bi,bj)
         ENDDO
        ENDDO
C     abbreviations at Z-points, need to do them in separate loops
C     for vectorization
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          dudy(I,J) = ( uFld(I,J,bi,bj) - uFld(I  ,J-1,bi,bj) )
     &         * _recip_dyU(I,J,bi,bj)
          uave(I,J) = 0.5 _d 0 * (uFld(I,J,bi,bj)+uFld(I  ,J-1,bi,bj))
         ENDDO
        ENDDO
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          dvdx(I,J) = ( vFld(I,J,bi,bj) - vFld(I-1,J  ,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj) 
          vave(I,J) = 0.5 _d 0 * (vFld(I,J,bi,bj)+vFld(I-1,J  ,bi,bj))
         ENDDO
        ENDDO
C     evaluate strain rates at Z-points
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          hFacU = _maskW(i,j,k,bi,bj) - _maskW(i,j-1,k,bi,bj)
          hFacV = _maskS(i,j,k,bi,bj) - _maskS(i-1,j,k,bi,bj)
          e12Loc(I,J,bi,bj) = 0.5 _d 0 * (
     &         dudy(I,J) + dvdx(I,J)
     &         - k1AtZ(I,J,bi,bj) * vave(I,J)
     &         - k2AtZ(I,J,bi,bj) * uave(I,J)
     &         )
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
     &         + 2.0 _d 0 * noSlipFac * (
     &           2.0 _d 0 * uave(I,J) * _recip_dyU(I,J,bi,bj) * hFacU
     &         + 2.0 _d 0 * vave(I,J) * _recip_dxV(I,J,bi,bj) * hFacV
     &         )
C     no slip at the boundary implies u(j)+u(j-1)=0 and v(i)+v(i-1)=0
C     accross the boundary; this is already accomplished by masking so
C     that the following lines are not necessary
c$$$     &         - hFacV * k1AtZ(I,J,bi,bj) * vave(I,J)
c$$$     &         - hFacU * k2AtZ(I,J,bi,bj) * uave(I,J)
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_DYN_STABLE_ADJOINT
cgf zero out adjoint fields to stabilize pkg/seaice dyna. adjoint
      CALL ZERO_ADJ( 1, e11Loc, myThid)
      CALL ZERO_ADJ( 1, e12Loc, myThid)
      CALL ZERO_ADJ( 1, e22Loc, myThid)
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */
      RETURN
      END
