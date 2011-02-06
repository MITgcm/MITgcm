c $Header: /u/gcmpack/MITgcm/pkg/cheapaml/gad_2d_calc_rhs.F,v 1.3 2011/02/06 00:18:05 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_2d_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_2d_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           uTrans,vTrans,
     I           uVel, vVel,
     I           diffKh, Tracer, zu,
     U           gTracer,deltaTtracer,useFluxLimit,
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C Calculates the tendency of a tracer due to advection and diffusion.
C Because horizontal velocity field is potential divergent, it is
C necessary to compute the vertical velocity and to compute the
c vertical flux divergence.
C The fluxes in each direction are computed independently and then
C the tendency is set to the divergence of these fluxes.
C In Cheapaml, it is always assumed the boundaries are open, and
C a simple open boundary implementation is used, whereby if the
C transport is outward directed, upwind weighting is used
C for the advective flux and the diffusive flux is shut off.
C If the transport is inward directed, the advective flux is computed
C using the Tr file, as is the diffusive flux.
C
C The tendency is the divergence of the fluxes:
C \begin{equation*}
C G_\theta = G_\theta + \nabla \cdot {\bf F}
C \end{equation*}
C
C The tendency is assumed to contain data on entry.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C bi,bj            :: tile indices
C iMin,iMax        :: loop range for called routines
C jMin,jMax        :: loop range for called routines
C uTrans,vTrans    :: 2-D arrays of volume transports at U,V points
C uVel,vVel,       :: 2 components of the velcity field (2-D array)
C diffKh           :: horizontal diffusion coefficient
C Tracer           :: tracer field
c deltaTtracer     :: atmospheric tracer time step
C myTime           :: current time
C myIter           :: iteration number
C myThid           :: thread number
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL diffKh,deltaTtracer,zu
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter, myThid
      LOGICAL useFluxLimit

C !OUTPUT PARAMETERS: ==================================================
C gTracer          :: tendency array
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C fZon             :: zonal fluxes
C fMer             :: meridional fluxes
c fVer             :: vertical fluxes
C af               :: advective fluxes
C df               :: diffusive flux
C localT           :: local copy of tracer field
c localW           :: local vertical velocity
C MaskLocW         :: local copy of West Face Land Mask
C MaskLocS         :: local copy of South Face Land Mask
c#ifdef ALLOW_DIAGNOSTICS
c     CHARACTER*8 diagName
c     CHARACTER*4 GAD_DIAG_SUFX, diagSufx
c     EXTERNAL    GAD_DIAG_SUFX
c#endif
      INTEGER i,j,iG,jG,iiii
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL MaskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL MaskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          df(i,j)        = 0. _d 0
        ENDDO
      ENDDO

C--   Make local copy of tracer array
c--   and compute w
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          localT(i,j)=tracer(i,j,bi,bj)
          localW(i,j)=-(recip_dxC(i,j,bi,bj)*
     &(uVel(i+1,j,bi,bj)-uVel(i,j,bi,bj))+
     &(vVel(i,j+1,bi,bj)-vVel(i,j,bi,bj))*
     &recip_dyC(i,j,bi,bj))*2. _d 0 *zu
        ENDDO
      ENDDO

C--   Initialize net flux in X direction
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          fZon(i,j) = 0. _d 0
        ENDDO
      ENDDO

c     prepare boundary tracer values
      DO j=1,sNy
        DO i=1,sNx
          iG=myXGlobalLo-1+(bi-1)*sNx+i
          if(iG.eq.2)then
            if(uVel(i,j,bi,bj).lt.0. _d 0)then
                do iiii=1-Olx,1
              localT(iiii,j)=localT(i,j)
                enddo
            endif
          elseif(iG.eq.Nx-1)then
            if(uVel(i+1,j,bi,bj).gt.0. _d 0)then
                do iiii=sNx,sNx+Olx
              localT(iiii,j)=localT(i,j)
                enddo
            endif
          endif
        enddo
      enddo
C     -    Advective flux in X
      if(useFluxLimit)then
c     make local copy of west land mask
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            MaskLocW(i,j)=MaskW(i,j,1,bi,bj)
          ENDDO
        ENDDO
        call GAD_DST3FL_ADV_X(
     I     bi,bj,1,.TRUE., deltaTtracer,
     I     uTrans, uVel,
     I     maskLocW, localT,
     O     af,
     I     myThid )
      else
        CALL GAD_C2_2d_ADV_X(bi,bj,uVel,localT,af,myThid)
      endif

      do j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          fZon(i,j)=fZon(i,j)+af(i,j)
        enddo
      enddo

C     -    Diffusive flux in X
      IF (diffKh.NE.0.) THEN
        CALL GAD_DIFF_2d_X(bi,bj,diffKh,localT,df,myThid)
      ELSE

        DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            df(i,j) = 0. _d 0
          ENDDO
        ENDDO
      ENDIF

      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          fZon(i,j) = fZon(i,j) + df(i,j)
        ENDDO
      ENDDO

c     repair boundary tracer values
      DO j=1,sNy
        DO i=1,sNx
          iG=myXGlobalLo-1+(bi-1)*sNx+i
          if(iG.eq.2)then
            if(uVel(i,j,bi,bj).lt.0. _d 0)then
                do iiii=1-Olx,1
              localT(iiii,j)=tracer(iiii,j,bi,bj)
                enddo
            endif
          elseif(iG.eq.Nx-1)then
            if(uVel(i+1,j,bi,bj).gt.0. _d 0)then
                do iiii=sNx,sNx+Olx
              localT(iiii,j)=tracer(iiii,j,bi,bj)
                enddo
            endif
          endif
        enddo
      enddo

C--   Initialize net flux in Y direction
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          fMer(i,j) = 0. _d 0
        ENDDO
      ENDDO

C     -    Advective flux in Y
c     prepare boundary tracer values
      DO j=1,sNy
        jG = myYGlobalLo-1+(bj-1)*sNy+j
        DO i=1,sNx
          if(jG.eq.2)then
            if(vVel(i,j,bi,bj).lt.0. _d 0)then
                do iiii=1-Oly,1
              localT(i,iiii)=localT(i,j)
                enddo
            endif
          elseif(jG.eq.Ny-1)then
            if(vVel(i,j+1,bi,bj).gt.0. _d 0)then
                do iiii=sNy,sNy+Oly
              localT(i,iiii)=localT(i,j)
                enddo
            endif
          endif
        enddo
      enddo

      if(useFluxLimit)then
c     make local copy of south land mask
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            MaskLocS(i,j)=MaskS(i,j,1,bi,bj)
          ENDDO
        ENDDO
        call GAD_DST3FL_ADV_Y(
     I     bi,bj,1,.TRUE., deltaTtracer,
     I     vTrans, vVel,
     I     maskLocS, localT,
     O     af,
     I     myThid )
      else
        CALL GAD_C2_2d_ADV_Y(bi,bj,vVel,localT,af,myThid)
      endif

      DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
          fMer(i,j) = fMer(i,j) + af(i,j)
        enddo
      enddo

C     -    Diffusive flux in Y
      IF (diffKh.NE.0.) THEN
        CALL GAD_DIFF_2d_Y(bi,bj,diffKh,localT,df,myThid)
      ELSE
        DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            df(i,j) = 0. _d 0
          ENDDO
        ENDDO
      ENDIF

      DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
          fMer(i,j) = fMer(i,j) + df(i,j)
        enddo
      enddo

C--   Initialize net flux in R direction
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          fVer(i,j) = 0. _d 0
        ENDDO
      ENDDO

C     -    Advective flux in R
C          at top of the cell

      if(useFluxLimit)then
        call CHEAPAML_GAD_DST3FL_ADV_R(
     I           bi,bj,deltaTtracer,
     I           localW,
     I           tracer,
     O           af,
     I           myThid )
      else
        CALL CHEAPAML_GAD_C2_ADV_R(
     I           bi,bj,
     I           localW,
     I           tracer,
     O           af,
     I           myThid )
      endif

      DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
          fVer(i,j) = fVer(i,j) + af(i,j)
        enddo
      enddo

c     repair boundary tracer values
      DO j=1,sNy
        do i=1,sNx
          jG = myYGlobalLo-1+(bj-1)*sNy+j
          if(jG.eq.2)then
            if(vVel(i,j+1,bi,bj).lt.0. _d 0)then
                do iiii=1-Oly,1
              localT(i,iiii)=tracer(i,iiii,bi,bj)
                enddo
            endif
          elseif(jG.eq.Ny-1)then
            if(vVel(i,j+1,bi,bj).gt.0. _d 0)then
                do iiii=sNy,sNy+Oly
              localT(i,iiii)=tracer(i,iiii,bi,bj)
                enddo
            endif
          endif
        enddo
      enddo

C--   Divergence of fluxes
      DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
          gTracer(i,j,bi,bj)=gTracer(i,j,bi,bj)
     &       -(recip_dxC(i,j,bi,bj)
     &       *(fZon(i+1,j)-fZon(i,j))
     &       +recip_dyC(i,j,bi,bj)*
     &       (fMer(i,j+1)-fMer(i,j))
     &       +1. _d 0/2. _d 0/zu*
     &       fVer(i,j) )
        ENDDO
      ENDDO

      RETURN
      END
