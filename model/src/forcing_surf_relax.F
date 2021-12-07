#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FORCING_SURF_RELAX
C     !INTERFACE:
      SUBROUTINE FORCING_SURF_RELAX(
     I                   iMin, iMax, jMin, jMax,
     I                   myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE FORCING_SURF_RELAX
C     | o Calculate relaxation surface forcing terms
C     |   for temperature and salinity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE_PARAMS.h"
# include "SEAICE.h"
#endif /* ALLOW_SEAICE */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iMin,iMax, jMin,jMax :: Range of points for calculation
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: Thread no. that called this routine.
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     bi,bj  :: tile indices
C     i,j    :: loop indices
C     ks     :: index of surface interface layer
      INTEGER bi,bj
      INTEGER i,j
      INTEGER ks
CEOP
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFac
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_BALANCE_RELAX
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL sumTile(nSx,nSy), sumGlob, globAver
#endif /* ALLOW_BALANCE_RELAX */

      IF ( usingPCoords ) THEN
       ks        = Nr
      ELSE
       ks        = 1
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_SEAICE
       IF ( useSEAICE .AND. (.NOT. SEAICErestoreUnderIce) ) THEN
C     Do not restore under sea-ice
        DO j = jMin, jMax
         DO i = iMin, iMax
C     Heat Flux (restoring term) :
          surfaceForcingT(i,j,bi,bj) =
     &      -lambdaThetaClimRelax(i,j,bi,bj)*(1.-AREA(i,j,bi,bj))
     &         *(theta(i,j,ks,bi,bj)-SST(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
C     Salt Flux (restoring term) :
          surfaceForcingS(i,j,bi,bj) =
     &      -lambdaSaltClimRelax(i,j,bi,bj) *(1.-AREA(i,j,bi,bj))
     &         *(salt(i,j,ks,bi,bj)-SSS(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
         ENDDO
        ENDDO
       ELSE
#endif /* ALLOW_SEAICE */
        DO j = jMin, jMax
         DO i = iMin, iMax
C     Heat Flux (restoring term) :
          surfaceForcingT(i,j,bi,bj) =
     &      -lambdaThetaClimRelax(i,j,bi,bj)
     &         *(theta(i,j,ks,bi,bj)-SST(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
C     Salt Flux (restoring term) :
          surfaceForcingS(i,j,bi,bj) =
     &      -lambdaSaltClimRelax(i,j,bi,bj)
     &         *(salt(i,j,ks,bi,bj)-SSS(i,j,bi,bj))
     &         *drF(ks)*_hFacC(i,j,ks,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_SEAICE
       ENDIF
#endif /* ALLOW_SEAICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef NONLIN_FRSURF
C-    T,S surface forcing will be applied (thermodynamics) after the update
C     of surf.thickness (hFac): account for change in surf.thickness
       IF (staggerTimeStep.AND.nonlinFreeSurf.GT.0) THEN
        IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
         DO j=jMin,jMax
          DO i=iMin,iMax
            surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &                                  * rStarExpC(i,j,bi,bj)
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &                                  * rStarExpC(i,j,bi,bj)
          ENDDO
         ENDDO
# endif /* DISABLE_RSTAR_CODE */
        ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
         DO j=jMin,jMax
          DO i=iMin,iMax
            surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &        *( 1. _d 0 + dEtaHdt(i,j,bi,bj)*deltaTFreeSurf
     &                    *dBHybSigF(ks)*recip_drF(ks)
     &                    *recip_hFacC(i,j,ks,bi,bj)
     &         )
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &        *( 1. _d 0 + dEtaHdt(i,j,bi,bj)*deltaTFreeSurf
     &                    *dBHybSigF(ks)*recip_drF(ks)
     &                    *recip_hFacC(i,j,ks,bi,bj)
     &         )
          ENDDO
         ENDDO
# endif /* DISABLE_SIGMA_CODE */
        ELSE
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF (ks.EQ.kSurfC(i,j,bi,bj)) THEN
            surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &             *_recip_hFacC(i,j,ks,bi,bj)*hFac_surfC(i,j,bi,bj)
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &             *_recip_hFacC(i,j,ks,bi,bj)*hFac_surfC(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDIF
#endif /* NONLIN_FRSURF */

C--   end bi,bj loops.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_BALANCE_RELAX

      IF ( balanceThetaClimRelax ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          sumTile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
             sumTile(bi,bj) = sumTile(bi,bj)
     &                      + surfaceForcingT(i,j,bi,bj)
     &                       *rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )
        globAver = sumGlob
        IF ( globalArea.GT.zeroRL ) globAver = globAver / globalArea
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &                                  - globAver
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        IF ( balancePrintMean ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(2A,1PE21.14,A,I10)') 'rm Global mean of',
     &                ' SSTrelax= ', globAver, '  @ it=', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF
      ENDIF

      IF ( balanceSaltClimRelax ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          sumTile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
             sumTile(bi,bj) = sumTile(bi,bj)
     &                      + surfaceForcingS(i,j,bi,bj)
     &                       *rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )
        globAver = sumGlob
        IF ( globalArea.GT.zeroRL ) globAver = globAver / globalArea
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &                                  - globAver
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        IF ( balancePrintMean ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(2A,1PE21.14,A,I10)') 'rm Global mean of',
     &                ' SSSrelax= ', globAver, '  @ it=', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF
      ENDIF

#endif /* ALLOW_BALANCE_RELAX */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

C     tRelax (temperature relaxation [W/m2], positive <-> increasing Theta)
        tmpFac = HeatCapacity_Cp*rUnit2mass
        CALL DIAGNOSTICS_SCALE_FILL( surfaceForcingT, tmpFac, 1,
     &                              'TRELAX  ', 0, 1, 0,1,1, myThid )

C     sRelax (salt relaxation [g/m2/s], positive <-> increasing Salt)
        tmpFac = rUnit2mass
        CALL DIAGNOSTICS_SCALE_FILL( surfaceForcingS, tmpFac, 1,
     &                              'SRELAX  ', 0, 1, 0,1,1, myThid )

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
