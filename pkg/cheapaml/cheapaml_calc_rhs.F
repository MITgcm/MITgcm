#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: CHEAPAML_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE CHEAPAML_CALC_RHS(
     I           bi,bj, iMin,iMax,jMin,jMax,
     I           uTrans, vTrans,
     I           uVel, vVel,
     I           diffKh, Tracer,
     I           deltaTtracer, zu,
     I           useFluxLimit, cheapamlXperiodic, cheapamlYperiodic,
     O           wVel,
     U           gTracer,
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C Calculates the tendency of a tracer due to advection and diffusion.
C Because horizontal velocity field is potential divergent, it is
C necessary to compute the vertical velocity and to compute the
C vertical flux divergence.
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
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C bi,bj            :: tile indices
C iMin,iMax        :: loop range for called routines
C jMin,jMax        :: loop range for called routines
C uTrans,vTrans    :: 2-D arrays of volume transports at U,V points
C uVel,vVel,       :: 2 components of the velcity field (2-D array)
C diffKh           :: horizontal diffusion coefficient
C Tracer           :: tracer field
C deltaTtracer     :: atmospheric tracer time step
C zu               ::
C useFluxLimit     ::
C cheapamlXperiodic :: cheapaml domain is periodic in X dir
C cheapamlYperiodic :: cheapaml domain is periodic in Y dir
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
      _RL deltaTtracer, zu
      LOGICAL useFluxLimit
      LOGICAL cheapamlXperiodic, cheapamlYperiodic
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C wVel             ::
C gTracer          :: tendency array
      _RL wVel   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C fZon             :: zonal fluxes
C fMer             :: meridional fluxes
C fVer             :: vertical fluxes
C af               :: advective fluxes
C df               :: diffusive flux
C localT           :: local copy of tracer field
C horizDiv         :: divergence of the horizontal wind
C maskLocW         :: local copy of West Face Land Mask
C maskLocS         :: local copy of South Face Land Mask

      INTEGER i,j, iG,jG, ii, jj
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL horizDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C--   Initialize fluxes
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          df(i,j)   = 0. _d 0
          fZon(i,j) = 0. _d 0
          fMer(i,j) = 0. _d 0
        ENDDO
      ENDDO

C--   Make local copy of tracer array
C--   and compute w
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          localT(i,j)=tracer(i,j,bi,bj)
        ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
          horizDiv(i,j) = (
     &       ( uTrans(i+1,j) - uTrans(i,j) )
     &     + ( vTrans(i,j+1) - vTrans(i,j) )
     &                    )*recip_rA(i,j,bi,bj)
          wVel(i,j,bi,bj) = -horizDiv(i,j)
        ENDDO
      ENDDO

C     prepare boundary tracer values
      IF(.NOT.cheapamlXperiodic)THEN
       DO j=1,sNy
        DO i=1,sNx
         iG=myXGlobalLo-1+(bi-1)*sNx+i
         IF (iG.EQ.2) THEN
           IF (uVel(i,j,bi,bj).LT.0. _d 0) THEN
             DO ii=1-OLx,1
              localT(ii,j)=localT(i,j)
             ENDDO
           ENDIF
         ELSEIF (iG.EQ.Nx-1) THEN
           IF (uVel(i+1,j,bi,bj).GT.0. _d 0) THEN
             DO ii=sNx,sNx+OLx
              localT(ii,j)=localT(i,j)
             ENDDO
           ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDIF
C     -    Advective flux in X
      IF (useFluxLimit) THEN
C     make local copy of west land mask
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
c            maskLocW(i,j)=maskW(i,j,1,bi,bj)
C     change to allow advection on land
             maskLocW(i,j)=1. _d 0
          ENDDO
        ENDDO
        CALL GAD_DST3FL_ADV_X(
     I     bi,bj,1,.TRUE., deltaTtracer,
     I     uTrans, uVel(1-OLx,1-OLy,bi,bj),
     I     maskLocW, localT,
     O     fZon,
     I     myThid )
      ELSE
        CALL GAD_C2_ADV_X( bi,bj,1,uTrans,localT,fZon,myThid )
      ENDIF

C     -    Diffusive flux in X
      IF ( diffKh.NE.zeroRL ) THEN
        CALL GAD_DIFF_X( bi,bj,1, dyG(1-OLx,1-OLy,bi,bj),
     I                   diffKh, localT,
     O                   df, myThid )
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            fZon(i,j) = fZon(i,j) + df(i,j)
          ENDDO
        ENDDO
      ENDIF

C     repair boundary tracer values
      IF(.NOT.cheapamlXperiodic)THEN
       DO j=1,sNy
        DO i=1,sNx
         iG=myXGlobalLo-1+(bi-1)*sNx+i
         IF (iG.EQ.2) THEN
           IF (uVel(i,j,bi,bj).LT.0. _d 0) THEN
             DO ii=1-OLx,1
              localT(ii,j)=tracer(ii,j,bi,bj)
             ENDDO
           ENDIF
          ELSEIF (iG.EQ.Nx-1) THEN
           IF (uVel(i+1,j,bi,bj).GT.0. _d 0) THEN
             DO ii=sNx,sNx+OLx
              localT(ii,j)=tracer(ii,j,bi,bj)
             ENDDO
           ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C     -    Advective flux in Y

C     prepare boundary tracer values
      IF(.NOT.cheapamlYperiodic)THEN
       DO j=1,sNy
        jG = myYGlobalLo-1+(bj-1)*sNy+j
        DO i=1,sNx
         IF (jG.EQ.2) THEN
           IF (vVel(i,j,bi,bj).LT.0. _d 0) THEN
             DO jj=1-OLy,1
              localT(i,jj)=localT(i,j)
             ENDDO
           ENDIF
         ELSEIF (jG.EQ.Ny-1) THEN
           IF (vVel(i,j+1,bi,bj).GT.0. _d 0) THEN
             DO jj=sNy,sNy+OLy
              localT(i,jj)=localT(i,j)
             ENDDO
           ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      IF (useFluxLimit) THEN
C     make local copy of south land mask
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
c            maskLocS(i,j)=maskS(i,j,1,bi,bj)
C Change to allow advection on land
            maskLocS(i,j)= 1. _d 0
          ENDDO
        ENDDO
        CALL GAD_DST3FL_ADV_Y(
     I     bi,bj,1,.TRUE., deltaTtracer,
     I     vTrans, vVel(1-OLx,1-OLy,bi,bj),
     I     maskLocS, localT,
     O     fMer,
     I     myThid )
      ELSE
        CALL GAD_C2_ADV_Y( bi,bj,1,vTrans,localT,fMer,myThid )
      ENDIF

C     -    Diffusive flux in Y
      IF ( diffKh.NE.zeroRL ) THEN
        CALL GAD_DIFF_Y( bi,bj,1, dxG(1-OLx,1-OLy,bi,bj),
     I                   diffKh, localT,
     O                   df, myThid )
        IF ( .NOT.cheapamlYperiodic ) THEN
         DO j=1,sNy+1
          jG = myYGlobalLo-1+(bj-1)*sNy+j
          IF ( jG.EQ.1 .OR. jG.EQ.Ny ) THEN
C- Note: conditions above ire strange. Instead, should it be:
c         IF ( jG.EQ.1 .OR. jG.EQ.Ny+1 ) THEN
           DO i=1-OLx,sNx+OLx
            df(i,j) = 0. _d 0
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            fMer(i,j) = fMer(i,j) + df(i,j)
          ENDDO
        ENDDO
      ENDIF

C--   Divergence of fluxes
      DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
          gTracer(i,j,bi,bj) =
     &       -( ( fZon(i+1,j) - fZon(i,j) )
     &        + ( fMer(i,j+1) - fMer(i,j) )
     &        )*recip_rA(i,j,bi,bj)
     &       + horizDiv(i,j)*localT(i,j)
        ENDDO
      ENDDO

      RETURN
      END
