C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_strainrates.F,v 1.21 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#else
# define OBCS_UVICE_OLD
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_CALC_STRAINRATES
C     !INTERFACE:
      SUBROUTINE SEAICE_CALC_STRAINRATES(
     I     uFld, vFld,
     O     e11Loc, e22Loc, e12Loc,
     I     iStep, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE  SEAICE_CALC_STRAINRATES
C     | o compute strain rates from ice velocities
C     *==========================================================*
C     | written by Martin Losch, Apr 2007
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     uFld   :: ice velocity, u-component
C     vFld   :: ice velocity, v-component
C     e11Loc :: strain rate tensor, component 1,1
C     e22Loc :: strain rate tensor, component 2,2
C     e12Loc :: strain rate tensor, component 1,2
C     iStep  :: Sub-time-step number
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: My Thread Id. number
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e11Loc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22Loc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12Loc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
C     hFacU, hFacV :: determine the no-slip boundary condition
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
C     in order repoduce results before fixing a bug in r1.20 comment out
C     the following line
CML      IF ( SEAICE_no_slip ) noSlipFac = 2. _d 0
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     abbreviations on C-points, need to do them in separate loops
C     for vectorization
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          dudx(i,j) = _recip_dxF(i,j,bi,bj) *
     &         (uFld(i+1,j,bi,bj)-uFld(i,j,bi,bj))
          uave(i,j) = 0.5 _d 0 * (uFld(i,j,bi,bj)+uFld(i+1,j,bi,bj))
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          dvdy(i,j) = _recip_dyF(i,j,bi,bj) *
     &         (vFld(i,j+1,bi,bj)-vFld(i,j,bi,bj))
          vave(i,j) = 0.5 _d 0 * (vFld(i,j,bi,bj)+vFld(i,j+1,bi,bj))
         ENDDO
        ENDDO
C     evaluate strain rates at C-points
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          e11Loc(i,j,bi,bj) = dudx(i,j) + vave(i,j) * k2AtC(i,j,bi,bj)
          e22Loc(i,j,bi,bj) = dvdy(i,j) + uave(i,j) * k1AtC(i,j,bi,bj)
         ENDDO
        ENDDO
#ifndef OBCS_UVICE_OLD
C--     for OBCS: assume no gradient beyong OB
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          e11Loc(i,j,bi,bj) = e11Loc(i,j,bi,bj)*maskInC(i,j,bi,bj)
          e22Loc(i,j,bi,bj) = e22Loc(i,j,bi,bj)*maskInC(i,j,bi,bj)
         ENDDO
        ENDDO
#endif /* OBCS_UVICE_OLD */

C     abbreviations at Z-points, need to do them in separate loops
C     for vectorization
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          dudy(i,j) = ( uFld(i,j,bi,bj) - uFld(i  ,j-1,bi,bj) )
     &         * _recip_dyU(i,j,bi,bj)
          uave(i,j) = 0.5 _d 0 * (uFld(i,j,bi,bj)+uFld(i  ,j-1,bi,bj))
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          dvdx(i,j) = ( vFld(i,j,bi,bj) - vFld(i-1,j  ,bi,bj) )
     &         * _recip_dxV(i,j,bi,bj)
          vave(i,j) = 0.5 _d 0 * (vFld(i,j,bi,bj)+vFld(i-1,j  ,bi,bj))
         ENDDO
        ENDDO
C     evaluate strain rates at Z-points
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          hFacU = _maskW(i,j,k,bi,bj) - _maskW(i,j-1,k,bi,bj)
          hFacV = _maskS(i,j,k,bi,bj) - _maskS(i-1,j,k,bi,bj)
          e12Loc(i,j,bi,bj) = 0.5 _d 0 * (
     &         dudy(i,j) + dvdx(i,j)
     &         - k1AtZ(i,j,bi,bj) * vave(i,j)
     &         - k2AtZ(i,j,bi,bj) * uave(i,j)
     &         )
     &         *maskC(i  ,j  ,k,bi,bj)*maskC(i-1,j  ,k,bi,bj)
     &         *maskC(i  ,j-1,k,bi,bj)*maskC(i-1,j-1,k,bi,bj)
     &         + noSlipFac * (
     &           2.0 _d 0 * uave(i,j) * _recip_dyU(i,j,bi,bj) * hFacU
     &         + 2.0 _d 0 * vave(i,j) * _recip_dxV(i,j,bi,bj) * hFacV
     &         )
C     no slip at the boundary implies u(j)+u(j-1)=0 and v(i)+v(i-1)=0
C     accross the boundary; this is already accomplished by masking so
C     that the following lines are not necessary
c$$$     &         - hFacV * k1AtZ(i,j,bi,bj) * vave(i,j)
c$$$     &         - hFacU * k2AtZ(i,j,bi,bj) * uave(i,j)
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
