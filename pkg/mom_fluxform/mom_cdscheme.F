C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_cdscheme.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_CDSCHEME( 
     I        bi,bj,k,phi_hyd,
     I        myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     == Routine arguments ==
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      INTEGER bi,bj,K
      _RL phi_hyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

#ifdef INCLUDE_CD_CODE

C     == Local variables ==
      _RL pF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL aF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,iMin,iMax,jMin,jMax
      _RL ab15,ab05

C     Compute ranges
      iMin=1-Olx+1
      iMax=sNx+Olx-1
      jMin=1-Oly+1
      jMax=sNy+Oly-1

C     Adams-Bashforth weighting factors
      ab15   =  1.5 + abEps
      ab05   = -0.5 - abEps

C     Pressure extrapolated forward in time
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        pf(i,j) = 
     &   ab15*(  etaN(i,j,bi,bj)*Bo_surf(i,j,bi,bj) )
     &  +ab05*(etaNm1(i,j,bi,bj)*Bo_surf(i,j,bi,bj) )
       ENDDO
      ENDDO
      IF (staggerTimeStep) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         pf(i,j) = pf(i,j)+phi_hyd(i,j,k)
        ENDDO
       ENDDO
      ENDIF

C--   Zonal velocity coriolis term
C     Note. As coded here, coriolis will not work with "thin walls"
C--   Coriolis with CD scheme allowed
C     grady(p) + gV
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        af(i,j) = -_maskS(i,j,k,bi,bj)
     &            *_recip_dyC(i,j,bi,bj)
     &            *(pf(i,j)-pf(i,j-1))
     &            +gV(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Average to Vd point and add coriolis
      DO j=jMin,jMax
       DO i=iMin,iMax
        vf(i,j) =      
     &    0.25*( af(i  ,j)+af(i  ,j+1)
     &          +af(i-1,j)+af(i-1,j+1)
     &         )*_maskW(i,j,k,bi,bj)
     &   -0.5*(_fCori(i,j,bi,bj)+
     &         _fCori(i-1,j,bi,bj))
     &         *uVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Step forward Vd
      DO j=jMin,jMax
       DO i=iMin,iMax
        vVelD(i,j,k,bi,bj) = vVelD(i,j,k,bi,bj) +
     &                        deltaTmom*vf(i,j)
       ENDDO
      ENDDO
C     Relax D grid V to C grid V
      DO j=jMin,jMax
       DO i=iMin,iMax
         vVelD(i,j,k,bi,bj) = rCD*vVelD(i,j,k,bi,bj)
     &   +(1. - rCD)*(
     &    ab15*0.25*( 
     &                vVel(i  ,j  ,k,bi,bj)+vVel(i  ,j+1,k,bi,bj)
     &               +vVel(i-1,j  ,k,bi,bj)+vVel(i-1,j+1,k,bi,bj)
     &              )*_maskW(i,j,k,bi,bj)
     &     +
     &    ab05*0.25*( 
     &                vNM1(i  ,j  ,k,bi,bj)+vNM1(i  ,j+1,k,bi,bj)
     &               +vNM1(i-1,j  ,k,bi,bj)+vNM1(i-1,j+1,k,bi,bj)
     &              )*_maskW(i,j,k,bi,bj)
     &   )
       ENDDO
      ENDDO
C     Calculate coriolis force on U
      DO j=jMin,jMax
       DO i=iMin,iMax
        guCD(i,j,k,bi,bj) = 
     &    0.5*( _fCori(i  ,j,bi,bj) + 
     &          _fCori(i-1,j,bi,bj)  )
     &   *vVelD(i,j,k,bi,bj)*cfFacMom
       ENDDO
      ENDDO

C--   Meridional velocity coriolis term
C     gradx(p)+gU
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        af(i,j) = -_maskW(i,j,k,bi,bj)
     &            *_recip_dxC(i,j,bi,bj)*
     &            (pf(i,j)-pf(i-1,j))
     &            +gU(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Average to Ud point and add coriolis
      DO j=jMin,jMax
       DO i=iMin,iMax
        vf(i,j) =
     &    0.25*( af(i  ,j)+af(i  ,j-1)
     &          +af(i+1,j)+af(i+1,j-1)
     &         )*_maskS(i,j,k,bi,bj)
     &   +0.5*( _fCori(i,j,bi,bj)
     &         +_fCori(i,j-1,bi,bj))
     &         *vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Step forward Ud
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVelD(i,j,k,bi,bj) = uVelD(i,j,k,bi,bj) +
     &   deltaTmom*vf(i,j)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Relax D grid U to C grid U
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVelD(i,j,k,bi,bj) = rCD*uVelD(i,j,k,bi,bj) 
     &   +(1. - rCD)*(
     &    ab15*0.25*( 
     &                uVel(i,j  ,k,bi,bj)+uVel(i+1,j  ,k,bi,bj)
     &               +uVel(i,j-1,k,bi,bj)+uVel(i+1,j-1,k,bi,bj)
     &              )*_maskS(i,j,k,bi,bj)
     &     +
     &    ab05*0.25*( 
     &                uNM1(i,j  ,k,bi,bj)+uNM1(i+1,j  ,k,bi,bj)
     &               +uNM1(i,j-1,k,bi,bj)+uNM1(i+1,j-1,k,bi,bj)
     &              )*_maskS(i,j,k,bi,bj)
     &   )
       ENDDO
      ENDDO
C     Calculate coriolis force on V
      DO j=jMin,jMax
       DO i=iMin,iMax
        gvCD(i,j,k,bi,bj) =
     &    -0.5*( _fCori(i  ,j,bi,bj) 
     &          +_fCori(i,j-1,bi,bj)  )
     &   *uVelD(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)*cfFacMom
       ENDDO
      ENDDO

C--   Save "previous time level" variables
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uNM1(i,j,k,bi,bj) =  uVel(i,j,k,bi,bj)
         vNM1(i,j,k,bi,bj) =  vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO

#endif /* INCLUDE_CD_CODE */

      RETURN
      END
