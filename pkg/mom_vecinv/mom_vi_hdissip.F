C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_hdissip.F,v 1.18 2005/03/11 00:53:17 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_HDISSIP(
     I        bi,bj,k,
     I        hDiv,vort3,hFacZ,dStar,zStar,
     O        uDissip,vDissip,
     I        myThid)

cph(
cph The following line was commented in the argument list
cph TAMC cannot digest commented lines within continuing lines
c    I        viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
cph)

      IMPLICIT NONE
C
C     Calculate horizontal dissipation terms
C     [del^2 - del^4] (u,v)
C

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER I,J
      _RL Zip,Zij,Zpj,Dim,Dij,Dmj,uD2,vD2,uD4,vD4
      _RL Alin,AlinMin,AlinMax,Alth2,Alth4,grdVrt,grdDiv
      _RL vg2,vg2Min,vg2Max,vg4,vg4Min,vg4Max
      LOGICAL useVariableViscosity

      useVariableViscosity=
     &      (viscAhGrid.NE.0.)
     &  .OR.(viscA4Grid.NE.0.)
     &  .OR.(viscC2leith.NE.0.)
     &  .OR.(viscC2leithD.NE.0.)
     &  .OR.(viscC4leith.NE.0.)
     &  .OR.(viscC4leithD.NE.0.)

      IF (deltaTmom.NE.0.) THEN
       vg2=viscAhGrid/deltaTmom
       vg2Min=viscAhGridMin/deltaTmom
       vg2Max=viscAhGridMax/deltaTmom
       vg4=viscA4Grid/deltaTmom
       vg4Min=viscA4GridMin/deltaTmom
       vg4Max=viscA4GridMax/deltaTmom
      ELSE
       vg2=0.
       vg2Min=0.
       vg2Max=0.
       vg4=0.
       vg4Min=0.
       vg4Max=0.
      ENDIF

C     - Viscosity
      IF (useVariableViscosity) THEN
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
         IF (useFullLeith) THEN
C This is the vector magnitude of the vorticity gradient squared
          grdVrt=0.25*(
     &     ((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))**2
     &     +((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj))**2
     &     +((vort3(i+1,j+1)-vort3(i,j+1))*recip_DXG(i,j+1,bi,bj))**2
     &     +((vort3(i+1,j+1)-vort3(i+1,j))*recip_DYG(i+1,j,bi,bj))**2)

C This is the vector magnitude of grad (div.v) squared
C Using it in Leith serves to damp instabilities in w.
           grdDiv=0.25*(
     &      ((hDiv(i+1,j)-hDiv(i,j))*recip_DXG(i,j,bi,bj))**2
     &      +((hDiv(i,j+1)-hDiv(i,j))*recip_DYG(i,j,bi,bj))**2
     &      +((hDiv(i-1,j)-hDiv(i,j))*recip_DXG(i-1,j,bi,bj))**2
     &      +((hDiv(i,j-1)-hDiv(i,j))*recip_DYG(i,j-1,bi,bj))**2)

           Alth2=sqrt(viscC2leith**2*grdVrt+viscC2leithD**2*grdDiv)
     &         *(rA(i,j,bi,bj)**1.5)
           Alth4=sqrt(viscC4leith**2*grdVrt+viscC4leithD**2*grdDiv)
     &          *0.125*(rA(i,j,bi,bj)**2.5)
         ELSE
C but this approximation will work on cube
c (and differs by as much as 4X)
           grdVrt=abs((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))
           grdVrt=max(grdVrt,
     &      abs((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj)))
           grdVrt=max(grdVrt,
     &      abs((vort3(i+1,j+1)-vort3(i,j+1))*recip_DXG(i,j+1,bi,bj)))
           grdVrt=max(grdVrt,
     &      abs((vort3(i+1,j+1)-vort3(i+1,j))*recip_DYG(i+1,j,bi,bj)))

           Alth2=viscC2leith*grdVrt*(rA(i,j,bi,bj)**1.5)
           Alth4=viscC4leith*grdVrt*0.125*(rA(i,j,bi,bj)**2.5)
         ENDIF

C  Harmonic Modified Leith on Div.u points
         Alin=viscAhD+vg2*rA ( i , j ,bi,bj)
         viscAh_D(i,j)=min(viscAhMax,Alin+Alth2)
         IF (vg2Max.GT.0.) THEN
            AlinMax=vg2Max*(rA ( i , j ,bi,bj))
            viscAh_D(i,j)=min(AlinMax,viscAh_D(i,j))
         ENDIF
         AlinMin=vg2Min*(rA ( i , j ,bi,bj))
         viscAh_D(i,j)=max(AlinMin,viscAh_D(i,j))

C  BiHarmonic Modified Leith on Div.u points
         Alin=viscA4D+vg4*(rA ( i , j ,bi,bj)**2)
         viscA4_D(i,j)=min(viscA4Max,Alin+Alth4)
         IF (vg4Max.GT.0.) THEN
            AlinMax=vg4Max*(rA ( i , j ,bi,bj)**2)
            viscA4_D(i,j)=min(AlinMax,viscA4_D(i,j))
         ENDIF
         AlinMin=vg4Min*(rA ( i , j ,bi,bj)**2)
         viscA4_D(i,j)=max(AlinMin,viscA4_D(i,j))

C This is the vector magnitude of the vorticity gradient squared
         IF (useFullLeith) THEN
           grdVrt=0.25*(
     &      ((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))**2
     &      +((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj))**2
     &      +((vort3(i-1,j)-vort3(i,j))*recip_DXG(i-1,j,bi,bj))**2
     &      +((vort3(i,j-1)-vort3(i,j))*recip_DYG(i,j-1,bi,bj))**2)

C This is the vector magnitude of grad(div.v) squared
           grdDiv=0.25*(
     &       ((hDiv(i+1,j)-hDiv(i,j))*recip_DXG(i,j,bi,bj))**2
     &      +((hDiv(i,j+1)-hDiv(i,j))*recip_DYG(i,j,bi,bj))**2
     &      +((hDiv(i+1,j+1)-hDiv(i,j+1))*recip_DXG(i,j+1,bi,bj))**2
     &      +((hDiv(i+1,j+1)-hDiv(i+1,j))*recip_DYG(i+1,j,bi,bj))**2)

           Alth2=sqrt(viscC2leith**2*grdVrt+viscC2leithD**2*grdDiv)
     &         *(rAz(i,j,bi,bj)**1.5)
           Alth4=sqrt(viscC4leith**2*grdVrt+viscC4leithD**2*grdDiv)
     &          *0.125*(rAz(i,j,bi,bj)**2.5)
         ELSE
C but this approximation will work on cube (and differs by as much as 4X)
           grdVrt=abs((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))
           grdVrt=max(grdVrt,
     &       abs((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj)))
           grdVrt=max(grdVrt,
     &       abs((vort3(i-1,j)-vort3(i,j))*recip_DXG(i-1,j,bi,bj)))
           grdVrt=max(grdVrt,
     &       abs((vort3(i,j-1)-vort3(i,j))*recip_DYG(i,j-1,bi,bj)))

           Alth2=viscC2leith*grdVrt*(rAz(i,j,bi,bj)**1.5)
           Alth4=viscC4leith*grdVrt*0.125*(rAz(i,j,bi,bj)**2.5)
         ENDIF

C  Harmonic Modified Leith on Zeta points
         Alin=viscAhZ+vg2*rAz( i , j ,bi,bj)
         viscAh_Z(i,j)=min(viscAhMax,Alin+Alth2)
         IF (vg2Max.GT.0.) THEN
            AlinMax=vg2Max*(rAz( i , j ,bi,bj))
            viscAh_Z(i,j)=min(AlinMax,viscAh_Z(i,j))
         ENDIF
         AlinMin=vg2Min*(rAz( i , j ,bi,bj))
         viscAh_Z(i,j)=max(AlinMin,viscAh_Z(i,j))

C  BiHarmonic Modified Leith on Zeta Points
         Alin=viscA4Z+vg4*(rAz( i , j ,bi,bj)**2)
         viscA4_Z(i,j)=min(viscA4Max,Alin+Alth4)
         IF (vg4Max.GT.0.) THEN
            AlinMax=vg4Max*(rAz( i , j ,bi,bj)**2)
            viscA4_Z(i,j)=min(AlinMax,viscA4_Z(i,j))
         ENDIF
         AlinMin=vg4Min*(rAz( i , j ,bi,bj)**2)
         viscA4_Z(i,j)=max(AlinMin,viscA4_Z(i,j))
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         viscAh_D(i,j)=viscAhD
         viscAh_Z(i,j)=viscAhZ
         viscA4_D(i,j)=viscA4D
         viscA4_Z(i,j)=viscA4Z
        ENDDO
       ENDDO
      ENDIF

C     - Laplacian  terms
      IF ( viscC2leith.NE.0. .OR. viscAhGrid.NE.0.
     &    .OR. viscAhD.NE.0. .OR. viscAhZ.NE.0. ) THEN
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1

         Dim=hDiv( i ,j-1)
         Dij=hDiv( i , j )
         Dmj=hDiv(i-1, j )
         Zip=hFacZ( i ,j+1)*vort3( i ,j+1)
         Zij=hFacZ( i , j )*vort3( i , j )
         Zpj=hFacZ(i+1, j )*vort3(i+1, j )

C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01 
         IF (useVariableViscosity) THEN
          Dij=Dij*viscAh_D(i,j)
          Dim=Dim*viscAh_D(i,j-1)
          Dmj=Dmj*viscAh_D(i-1,j)
          Zij=Zij*viscAh_Z(i,j)
          Zip=Zip*viscAh_Z(i,j+1)
          Zpj=Zpj*viscAh_Z(i+1,j)
          uD2 = (
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &      -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
          vD2 = (
     &       recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
         ELSE
          uD2 = viscAhD*
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &        - viscAhZ*recip_hFacW(i,j,k,bi,bj)*
     &                                ( Zip-Zij )*recip_DYG(i,j,bi,bj)
          vD2 = viscAhZ*recip_hFacS(i,j,k,bi,bj)*
     &               cosFacV(j,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &        + viscAhD*              ( Dij-Dim )*recip_DYC(i,j,bi,bj)
         ENDIF

         uDissip(i,j) = uD2
         vDissip(i,j) = vD2

        ENDDO
       ENDDO
      ELSE
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
         uDissip(i,j) = 0.
         vDissip(i,j) = 0.
        ENDDO
       ENDDO
      ENDIF

C     - Bi-harmonic terms
      IF ( viscC4leith.NE.0. .OR. viscA4Grid.NE.0.
     &    .OR. viscA4D.NE.0. .OR. viscA4Z.NE.0. ) THEN
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1

#ifdef MOM_VI_ORIGINAL_VISCA4
         Dim=dyF( i ,j-1,bi,bj)*dStar( i ,j-1)
         Dij=dyF( i , j ,bi,bj)*dStar( i , j )
         Dmj=dyF(i-1, j ,bi,bj)*dStar(i-1, j )

         Zip=dxV( i ,j+1,bi,bj)*hFacZ( i ,j+1)*zStar( i ,j+1)
         Zij=dxV( i , j ,bi,bj)*hFacZ( i , j )*zStar( i , j )
         Zpj=dxV(i+1, j ,bi,bj)*hFacZ(i+1, j )*zStar(i+1, j )
#else
         Dim=dStar( i ,j-1)
         Dij=dStar( i , j )
         Dmj=dStar(i-1, j )

         Zip=hFacZ( i ,j+1)*zStar( i ,j+1)
         Zij=hFacZ( i , j )*zStar( i , j )
         Zpj=hFacZ(i+1, j )*zStar(i+1, j )
#endif

C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01 
         IF (useVariableViscosity) THEN
          Dij=Dij*viscA4_D(i,j)
          Dim=Dim*viscA4_D(i,j-1)
          Dmj=Dmj*viscA4_D(i-1,j)
          Zij=Zij*viscA4_Z(i,j)
          Zip=Zip*viscA4_Z(i,j+1)
          Zpj=Zpj*viscA4_Z(i+1,j)

#ifdef MOM_VI_ORIGINAL_VISCA4
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             ( (Dij-Dmj)*cosFacU(j,bi,bj) )
     &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij ) )
          vD4 = recip_rAs(i,j,bi,bj)*(
     &    recip_hFacS(i,j,k,bi,bj)*( (Zpj-Zij)*cosFacV(j,bi,bj) )
     &   +                         ( Dij-Dim ) )
         ELSE
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             viscA4*( (Dij-Dmj)*cosFacU(j,bi,bj) )
     &   -recip_hFacW(i,j,k,bi,bj)*viscA4*( Zip-Zij ) )
          vD4 = recip_rAs(i,j,bi,bj)*(
     &    recip_hFacS(i,j,k,bi,bj)*viscA4*( (Zpj-Zij)*cosFacV(j,bi,bj) )
     &   +                         viscA4*( Dij-Dim ) )
#else /* MOM_VI_ORIGINAL_VISCA4 */
          uD4 = (
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &      -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
          vD4 = (
     &       recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
         ELSE
          uD4 = viscA4D*
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &        - viscA4Z*recip_hFacW(i,j,k,bi,bj)*
     &                                ( Zip-Zij )*recip_DYG(i,j,bi,bj)
          vD4 = viscA4Z*recip_hFacS(i,j,k,bi,bj)*
     &               cosFacV(j,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &        + viscA4D*              ( Dij-Dim )*recip_DYC(i,j,bi,bj)
#endif /* MOM_VI_ORIGINAL_VISCA4 */
         ENDIF

         uDissip(i,j) = uDissip(i,j) - uD4
         vDissip(i,j) = vDissip(i,j) - vD4

        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
       CALL DIAGNOSTICS_FILL(viscAh_D,'VISCAH  ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_D,'VISCA4  ',k,1,2,bi,bj,myThid)
      ENDIF
#endif

      RETURN
      END
