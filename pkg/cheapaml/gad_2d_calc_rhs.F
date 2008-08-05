C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/gad_2d_calc_rhs.F,v 1.1 2008/08/05 21:49:31 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_2d_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_2d_CALC_RHS( 
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           uTrans,vTrans,
     I           uVel, vVel, 
     I           diffKh, Tracer,
     U           gTracer,
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C Calculates the tendancy of a tracer due to advection and diffusion.
C It calculates the fluxes in each direction independently and then
C sets the tendancy to the divergence of these fluxes. The advective
C fluxes are only calculated here when using the linear advection schemes
C otherwise only the diffusive and parameterized fluxes are calculated.
C
C Contributions to the flux are calculated and added:
C \begin{equation*}
C {\bf F} = {\bf F}_{adv} + {\bf F}_{diff} +{\bf F}_{GM} + {\bf F}_{KPP}
C \end{equation*}
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
C myTime           :: current time
C myIter           :: iteration number
C myThid           :: thread number
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL diffKh
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C gTracer          :: tendancy array
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C fZon             :: zonal flux
C fMer             :: meridional flux
C af               :: advective flux
C df               :: diffusive flux
C localT           :: local copy of tracer field
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 GAD_DIAG_SUFX, diagSufx 
      EXTERNAL    GAD_DIAG_SUFX
#endif
      INTEGER i,j
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL advFac
CEOP

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fZon(i,j)      = 0. _d 0
        fMer(i,j)      = 0. _d 0
        df(i,j)        = 0. _d 0
       ENDDO
      ENDDO

C--   Make local copy of tracer array
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        localT(i,j)=tracer(i,j,bi,bj)
       ENDDO
      ENDDO

C--   Initialize net flux in X direction
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fZon(i,j) = 0. _d 0
       ENDDO
      ENDDO

C-    Advective flux in X
      CALL GAD_C2_2d_ADV_X(bi,bj,uVel,localT,af,myThid)
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fZon(i,j) = fZon(i,j) + af(i,j)
       ENDDO
      ENDDO

C-    Diffusive flux in X
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

C--   Initialize net flux in Y direction
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = 0. _d 0
       ENDDO
      ENDDO

C-    Advective flux in Y
          CALL GAD_C2_2d_ADV_Y(bi,bj,vVel,localT,af,myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fMer(i,j) = fMer(i,j) + af(i,j)
         ENDDO
        ENDDO

C-    Diffusive flux in Y
       CALL GAD_DIFF_2d_Y(bi,bj,diffKh,localT,df,myThid)

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = fMer(i,j) + df(i,j)
       ENDDO
      ENDDO

C--   Divergence of fluxes
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        gTracer(i,j,bi,bj)=gTracer(i,j,bi,bj)
     &   -(recip_dxC(i,j,bi,bj)
     &   *(fZon(i+1,j)-fZon(i,j))
     &     +recip_dyC(i,j,bi,bj)*
     &	   (fMer(i,j+1)-fMer(i,j))
     &                  )
       ENDDO
      ENDDO

      RETURN
      END
