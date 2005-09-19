C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_visc.F,v 1.2 2005/09/19 20:46:23 baylor Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

      SUBROUTINE MOM_CALC_VISC(
     I        bi,bj,k,
     O        viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
     O        harmonic,biharmonic,useVariableViscosity,
     I        hDiv,vort3,tension,strain,KE,
     I        myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
      LOGICAL harmonic,biharmonic,useVariableViscosity

C     == Local variables ==
      INTEGER I,J
      _RL ASmag2, ASmag4, smag2fac, smag4fac
      _RL vg2Min, vg2Max, AlinMax, AlinMin
      _RL lenA2, lenAz2
      _RL Alin,Alth2,Alth4,grdVrt,grdDiv
      _RL vg2,vg4,vg4Min,vg4Max
      _RL recip_dt,L2,L3,L4,L5,L2rdt,L4rdt


      useVariableViscosity=
     &      (viscAhGrid.NE.0.)
     &  .OR.(viscA4Grid.NE.0.)
     &  .OR.(viscC2leith.NE.0.)
     &  .OR.(viscC2leithD.NE.0.)
     &  .OR.(viscC4leith.NE.0.)
     &  .OR.(viscC4leithD.NE.0.)
     &  .OR.(viscC2smag.NE.0.)
     &  .OR.(viscC4smag.NE.0.)

      harmonic=
     &      (viscAh.NE.0.)
     &  .OR.(viscAhD.NE.0.)
     &  .OR.(viscAhZ.NE.0.)
     &  .OR.(viscAhGrid.NE.0.)
     &  .OR.(viscC2leith.NE.0.)
     &  .OR.(viscC2leithD.NE.0.)
     &  .OR.(viscC2smag.NE.0.)

      biharmonic=
     &      (viscA4.NE.0.)
     &  .OR.(viscA4D.NE.0.)
     &  .OR.(viscA4Z.NE.0.)
     &  .OR.(viscA4Grid.NE.0.)
     &  .OR.(viscC4leith.NE.0.)
     &  .OR.(viscC4leithD.NE.0.)
     &  .OR.(viscC4smag.NE.0.)

      IF (deltaTmom.NE.0.) THEN
       recip_dt=1./deltaTmom
      ELSE
       recip_dt=0.
      ENDIF

      vg2=viscAhGrid*recip_dt
      vg2Min=viscAhGridMin*recip_dt
      vg2Max=viscAhGridMax*recip_dt
      vg4=viscA4Grid*recip_dt
      vg4Min=viscA4GridMin*recip_dt
      vg4Max=viscA4GridMax*recip_dt

      smag2fac=(viscC2smag/pi)**2
      smag4fac=0.125*(viscC4smag/pi)**2

C     - Viscosity
      IF (useVariableViscosity) THEN
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
CCCCCCCCCCCCCCC Divergence Point CalculationsCCCCCCCCCCCCCCCCCCCC
C These are (powers of) length scales 
         L2=rA(i,j,bi,bj)
         L3=(L2**1.5)
         L4=(L2**2)
         L5=0.125*(L2**2.5)
         IF (useAnisotropicViscAGridMax) THEN
          L2rdt=recip_dt/( 2.*(recip_DXF(I,J,bi,bj)**2
     &                       +recip_DYF(I,J,bi,bj)**2) )
          L4rdt=recip_dt/( 6.*(recip_DXF(I,J,bi,bj)**4
     &                       +recip_DYF(I,J,bi,bj)**4)
     &                   +8.*((recip_DXF(I,J,bi,bj)
     &                        *recip_DYF(I,J,bi,bj))**2) )
         ENDIF

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
     &      ((hDiv(i+1,j)-hDiv(i,j))*recip_DXC(i+1,j,bi,bj))**2
     &      +((hDiv(i,j+1)-hDiv(i,j))*recip_DYC(i,j+1,bi,bj))**2
     &      +((hDiv(i,j)-hDiv(i-1,j))*recip_DXC(i,j,bi,bj))**2
     &      +((hDiv(i,j)-hDiv(i,j-1))*recip_DYC(i,j,bi,bj))**2)

           IF ( (viscC2leith**2*grdVrt+viscC2leithD**2*grdDiv)
     &          .NE. 0. ) THEN
            Alth2=sqrt(viscC2leith**2*grdVrt+viscC2leithD**2*grdDiv)*L3
           ELSE
            Alth2=0. _d 0
           ENDIF
           IF ( (viscC4leith**2*grdVrt+viscC4leithD**2*grdDiv)
     &          .NE. 0. ) THEN
            Alth4=sqrt(viscC4leith**2*grdVrt+viscC4leithD**2*grdDiv)*L5
           ELSE
            Alth4=0. _d 0
           ENDIF
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

           grdDiv=abs((hDiv(i+1,j)-hDiv(i,j))*recip_DXC(i+1,j,bi,bj))
           grdDiv=max(grdDiv,
     &      abs((hDiv(i,j+1)-hDiv(i,j))*recip_DYC(i,j+1,bi,bj)))
           grdDiv=max(grdDiv,
     &      abs((hDiv(i,j)-hDiv(i-1,j))*recip_DXC(i,j,bi,bj)))
           grdDiv=max(grdDiv,
     &      abs((hDiv(i,j)-hDiv(i,j-1))*recip_DYC(i,j,bi,bj)))

c This approximation is good to the same order as above...
           Alth2=(viscC2leith*grdVrt+(viscC2leithD*grdDiv))*L3
           Alth4=(viscC4leith*grdVrt+(viscC4leithD*grdDiv))*L5
         ENDIF

         IF (smag2fac.NE.0.) THEN
          Asmag2=smag2fac*L2
     &           *sqrt(tension(i,j)**2
     &           +0.25*(strain(i+1, j )**2+strain( i ,j+1)**2
     &                 +strain(i-1, j )**2+strain( i ,j-1)**2))
         ELSE
          Asmag2=0d0
         ENDIF

         IF (smag4fac.NE.0.) THEN
          Asmag4=smag4fac*L4
     &             *sqrt(tension(i,j)**2
     &             +0.25*(strain(i+1, j )**2+strain( i ,j+1)**2
     &                   +strain(i-1, j )**2+strain( i ,j-1)**2))
         ELSE
          Asmag4=0d0
         ENDIF

C  Harmonic on Div.u points
         Alin=viscAhD+vg2*L2+Alth2+Asmag2
         viscAh_D(i,j)=min(viscAhMax,Alin)
         IF (useAnisotropicViscAGridMax) THEN
          AlinMax=viscAhGridMax*L2rdt
          viscAh_D(i,j)=min(AlinMax,viscAh_D(i,j))
         ELSE
          IF (vg2Max.GT.0.) THEN
           AlinMax=vg2Max*L2
           viscAh_D(i,j)=min(AlinMax,viscAh_D(i,j))
          ENDIF
         ENDIF
         AlinMin=vg2Min*L2
         viscAh_D(i,j)=max(AlinMin,viscAh_D(i,j))

C  BiHarmonic on Div.u points
         Alin=viscA4D+vg4*L4+Alth4+Asmag4
         viscA4_D(i,j)=min(viscA4Max,Alin)
         IF (useAnisotropicViscAGridMax) THEN
            AlinMax=viscA4GridMax*L4rdt
            viscA4_D(i,j)=min(AlinMax,viscA4_D(i,j))
         ELSE
         IF (vg4Max.GT.0.) THEN
            AlinMax=vg4Max*L4
            viscA4_D(i,j)=min(AlinMax,viscA4_D(i,j))
         ENDIF
         ENDIF
         AlinMin=vg4Min*L4
         viscA4_D(i,j)=max(AlinMin,viscA4_D(i,j))

CCCCCCCCCCCCC Vorticity Point CalculationsCCCCCCCCCCCCCCCCCC
C These are (powers of) length scales 
         L2=rAz(i,j,bi,bj)
         L3=(L2**1.5)
         L4=(L2**2)
         L5=0.125*(L2**2.5)
         IF (useAnisotropicViscAGridMax) THEN
         L2rdt=recip_dt/( 2.*(recip_DXV(I,J,bi,bj)**2
     &                       +recip_DYU(I,J,bi,bj)**2) )
         L4rdt=recip_dt/( 6.*(recip_DXV(I,J,bi,bj)**4
     &                       +recip_DYU(I,J,bi,bj)**4)
     &                   +8.*((recip_DXV(I,J,bi,bj)
     &                        *recip_DYU(I,J,bi,bj))**2) )
         ENDIF

C This is the vector magnitude of the vorticity gradient squared
         IF (useFullLeith) THEN
           grdVrt=0.25*(
     &      ((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))**2
     &      +((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj))**2
     &      +((vort3(i-1,j)-vort3(i,j))*recip_DXG(i-1,j,bi,bj))**2
     &      +((vort3(i,j-1)-vort3(i,j))*recip_DYG(i,j-1,bi,bj))**2)

C This is the vector magnitude of grad(div.v) squared
           grdDiv=0.25*(
     &       ((hDiv(i,j)-hDiv(i-1,j))*recip_DXC(i,j,bi,bj))**2
     &      +((hDiv(i,j)-hDiv(i,j-1))*recip_DYC(i,j,bi,bj))**2
     &      +((hDiv(i,j-1)-hDiv(i-1,j-1))*recip_DXC(i,j-1,bi,bj))**2
     &      +((hDiv(i-1,j)-hDiv(i-1,j-1))*recip_DYC(i-1,j,bi,bj))**2)

           IF ( (viscC2leith**2*grdVrt+viscC2leithD**2*grdDiv)
     &          .NE. 0. ) THEN
            Alth2=sqrt(viscC2leith**2*grdVrt+viscC2leithD**2*grdDiv)*L3
           ELSE
            Alth2=0. _d 0
           ENDIF
           IF ( (viscC4leith**2*grdVrt+viscC4leithD**2*grdDiv)
     &          .NE. 0. ) THEN
            Alth4=sqrt(viscC4leith**2*grdVrt+viscC4leithD**2*grdDiv)*L5
           ELSE
            Alth4=0. _d 0
           ENDIF
         ELSE
C but this approximation will work on cube (and differs by 4X)
           grdVrt=abs((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))
           grdVrt=max(grdVrt,
     &       abs((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj)))
           grdVrt=max(grdVrt,
     &       abs((vort3(i-1,j)-vort3(i,j))*recip_DXG(i-1,j,bi,bj)))
           grdVrt=max(grdVrt,
     &       abs((vort3(i,j-1)-vort3(i,j))*recip_DYG(i,j-1,bi,bj)))

           grdDiv=abs((hDiv(i,j)-hDiv(i-1,j))*recip_DXC(i,j,bi,bj))
           grdDiv=max(grdDiv,
     &      abs((hDiv(i,j)-hDiv(i,j-1))*recip_DYC(i,j,bi,bj)))
           grdDiv=max(grdDiv,
     &      abs((hDiv(i,j-1)-hDiv(i-1,j-1))*recip_DXG(i,j-1,bi,bj)))
           grdDiv=max(grdDiv,
     &      abs((hDiv(i-1,j)-hDiv(i-1,j-1))*recip_DYG(i-1,j,bi,bj)))


C This if statement is just to prevent bitwise changes when leithd=0
           Alth2=(viscC2leith*grdVrt+(viscC2leithD*grdDiv))*L3
           Alth4=(viscC4leith*grdVrt+(viscC4leithD*grdDiv))*L5
         ENDIF

         IF (smag2fac.NE.0.) THEN
          Asmag2=smag2fac*L2
     &           *sqrt(strain(i,j)**2
     &              +0.25*(tension( i , j )**2+tension( i ,j+1)**2
     &                +tension(i+1, j )**2+tension(i+1,j+1)**2))
         ELSE
          Asmag2=0d0
         ENDIF

         IF (smag4fac.NE.0.) THEN
          Asmag4=smag4fac*L4
     &             *sqrt(strain(i,j)**2
     &             +0.25*(tension( i , j )**2+tension( i ,j+1)**2
     &                +tension(i+1, j )**2+tension(i+1,j+1)**2))
         ELSE
          Asmag4=0d0
         ENDIF

C  Harmonic on Zeta points
         Alin=viscAhZ+vg2*L2+Alth2+Asmag2
         viscAh_Z(i,j)=min(viscAhMax,Alin)
         IF (useAnisotropicViscAGridMax) THEN
            AlinMax=viscAhGridMax*L2rdt
            viscAh_Z(i,j)=min(AlinMax,viscAh_Z(i,j))
         ELSE
         IF (vg2Max.GT.0.) THEN
            AlinMax=vg2Max*L2
            viscAh_Z(i,j)=min(AlinMax,viscAh_Z(i,j))
         ENDIF
         ENDIF
         AlinMin=vg2Min*L2
         viscAh_Z(i,j)=max(AlinMin,viscAh_Z(i,j))

C  BiHarmonic on Zeta Points
         Alin=viscA4Z+vg4*L4+Alth4+Asmag4
         viscA4_Z(i,j)=min(viscA4Max,Alin)
         IF (useAnisotropicViscAGridMax) THEN
            AlinMax=viscA4GridMax*L4rdt
            viscA4_Z(i,j)=min(AlinMax,viscA4_Z(i,j))
         ELSE
         IF (vg4Max.GT.0.) THEN
            AlinMax=vg4Max*L4
            viscA4_Z(i,j)=min(AlinMax,viscA4_Z(i,j))
         ENDIF
         ENDIF
         AlinMin=vg4Min*L4
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

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
       CALL DIAGNOSTICS_FILL(viscAh_D,'VISCAHD ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_D,'VISCA4D ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_Z,'VISCAHZ ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_Z,'VISCA4Z ',k,1,2,bi,bj,myThid)
      ENDIF
#endif

      RETURN
      END
