#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_APPLY
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_APPLY(
     I            trIdentity, bi, bj,
     I            recip_hFac_arg,
     I            tracer,trApplyFlag,
     I            myTime, myIter, myThid )

C     U            trStar,

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SALT_PLUME_APPLY
C     | o Apply the salt_pume-transport to tracer field
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SALT_PLUME.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     trIdentity :: tracer identification number
C     bi,bj      :: Tile indices
C     recip_drF  :: Reciprol of cell thickness
C     recip_hFac_arg :: Reciprol of cell open-depth factor
C     tracer     :: tracer field at current time (input)
C     trApplyFlag:: [0]=update saltplume forcing T/S terms
C                :: [1]=update gTr tendency
C     trStar     :: future tracer field (modified)
C     myTime     :: Current time in simulation
C     myIter     :: Current time-step number
C     myThid     :: my Thread Id. number

      INTEGER trIdentity, trApplyFlag
      INTEGER bi, bj
      _RS recip_hFac_arg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracer        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
C      _RL trStar        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_SALT_PLUME
#ifdef SALT_PLUME_VOLUME

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     plumetend  :: forcing terms [W/m2 or g/m2/s]
C     work       :: working array
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i, j, k
      INTEGER upward
      LOGICAL onOffFlag
      _RL gTr_Surf2kLev, gTr_Below2kLev, gTr_kLev2Above,
     &    dSPvolBelow2kLev, gTr_totSurf2Below,
     &    SurfVal, SEAICE_Tfrz, ConvertFac, recip_ConvertFac
      integer kp1, Nrp1
      _RL         plumetend(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL         work(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)

#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     doDiagSPtend
C-    Functions:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#ifdef ALLOW_GENERIC_ADVDIFF
      CHARACTER*5 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif /* ALLOW_GENERIC_ADVDIFF */
#endif /* ALLOW_DIAGNOSTICS */

CEOP

C      WRITE(msgBuf,'(A,2i4)') 'trApplyFlag,trIdentity: ',
C     & trApplyFlag,trIdentity
C         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                      SQUEEZE_RIGHT, myThid )

      IF ( trApplyFlag.LT.0 .OR. trApplyFlag.GT.1) THEN
       STOP 'S/R SALT_PLUME_APPLY: incorrect setting of trApplyFlag!'
      ELSE

       SEAICE_Tfrz = -1.96 _d 0

       onOffFlag = .FALSE.
#ifdef ALLOW_GENERIC_ADVDIFF
       IF ( trIdentity.EQ.GAD_TEMPERATURE ) onOffFlag = .TRUE.
       IF ( trIdentity.EQ.GAD_SALINITY    ) onOffFlag = .TRUE.
#endif
       IF ( onOffFlag ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c       upward = rkSign*NINT(-gravitySign)
        upward = 1
        IF (usingZCoords) upward = -1

        IF ( trIdentity.EQ.GAD_TEMPERATURE ) THEN
         SurfVal          = SEAICE_Tfrz
         ConvertFac       = HeatCapacity_Cp*rhoConst
         recip_ConvertFac = mass2rUnit/HeatCapacity_Cp
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) diagSufx = '_TH '
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF
        IF ( trIdentity.EQ.GAD_SALINITY ) THEN
         SurfVal          = SPbrineSconst
         ConvertFac       = rhoConst
         recip_ConvertFac = mass2rUnit
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) diagSufx = '_SLT'
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF

#ifdef ALLOW_DIAGNOSTICS
        doDiagSPtend = .FALSE.
        diagName = 'SPtd'
        IF ( useDiagnostics ) THEN
C--   Set diagnostic suffix for the current tracer
#ifdef ALLOW_GENERIC_ADVDIFF
         diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
#endif /* ALLOW_GENERIC_ADVDIFF */
         diagName = 'SPtd'//diagSufx
         doDiagSPtend = DIAGNOSTICS_IS_ON(diagName,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-- initializing:
        Nrp1=Nr+1
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           plumetend(i,j,k) = 0. _d 0
           work(i,j,k) = tracer(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           work(i,j,Nrp1) = 0. _d 0
         ENDDO
        ENDDO
C-----------------

Catn: After discussion with JM, it is cleaner to remove the negative
c     buoyancy associated with saltplumeflux from the surface lev
c     here instead of inside salt_plume_forcing_surf.F and
c     kpp_forcing_surf.F:

        IF(trApplyFlag.LT.1) THEN
C ======= if trApplyFlag==0: calc SPforcing[T,S] =================
         IF(trIdentity.EQ.GAD_TEMPERATURE) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             SPforcingT(i,j,k,bi,bj)=0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         IF(trIdentity.EQ.GAD_SALINITY) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             SPforcingS(i,j,k,bi,bj)=0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDIF

         DO k=Nr,1,-1
          kp1=k+1
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
C           IF(trIdentity.EQ.GAD_SALINITY) SurfVal=SPbrineSalt(i,j,bi,bj)
           IF(trIdentity.EQ.GAD_TEMPERATURE) SurfVal=tracer(i,j,1,bi,bj)
Catn: m/s*[degC,g/kg]
            gTr_totSurf2Below = SPbrineVolFlux(i,j,bi,bj)*SurfVal
            gTr_Surf2kLev = dSPvolSurf2kLev(i,j,k,bi,bj) * SurfVal
            dSPvolBelow2kLev = -dSPvolkLev2Above(i,j,kp1,bi,bj)
            gTr_Below2kLev= dSPvolBelow2kLev * work(i,j,kp1)
Catn: gTr_kLev2Above works even for kLev=1 because this is how much
C volume of original [salinity,heat] associated with [SSS,SST]
C was replaced by same volume of brine [salt,heat(from SEAICE_Tfrz)].
C Note: by design, dSPvolkLev2Above already is negative
            gTr_kLev2Above= dSPvolkLev2Above(i,j,k,bi,bj) * work(i,j,k)

C salt: [m/s * g/m3] = [g/s/m2] = unit of saltPlumeFlux
C theta:[m/s * kg/m3 * J/kg/degC * degC] = [W/m2]
            plumetend(i,j,k) = ConvertFac *
     &          ( gTr_Surf2kLev + gTr_Below2kLev + gTr_kLev2Above )
            IF(k.eq.1) plumetend(i,j,k) = ConvertFac *
     &          ( gTr_Surf2kLev + gTr_Below2kLev)
C remove negative buoyancy from surface,
C used to be in salt_plume_forcing_surf.F
C           IF(k.EQ.1) THEN
C            plumetend(i,j,k) = plumetend(i,j,k)
C     &                         - ConvertFac*gTr_totSurf2Below
C           ENDIF

Catn: report T/S SPforcing[T,S] related to saltplumeflux for kpp
C     and return zero to do_oceanic_phys.F; unit: [g/m2/s or W/m2]
C            trStar(i,j,k,bi,bj) = 0. _d 0
            IF (trIdentity.EQ.GAD_SALINITY) THEN
             SPforcingS(i,j,k,bi,bj)=plumetend(i,j,k)
            ENDIF
            IF (trIdentity.EQ.GAD_TEMPERATURE) THEN
             SPforcingT(i,j,k,bi,bj)=plumetend(i,j,k)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagSPtend )
     &   CALL DIAGNOSTICS_FILL(plumetend, diagName, 0,Nr,2,bi,bj,myThid)
#endif /* ALLOW_DIAGNOSTICS */

        ELSE
C ============ updating gTr =====================================
Catn: updating tendency gTr (gS,gT); unit: [g/kg/s or degC/s]
C         WRITE(msgBuf,'(a,2e24.17)') 'b4SPap: [Tr,gTr](1,1,1,bi,bj):',
C     &     tracer(1,1,1,bi,bj),trStar(1,1,1,bi,bj)
C         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                      SQUEEZE_RIGHT, myThid )
         DO k=Nr,1,-1
          kp1=k+1
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF (trIdentity.EQ.GAD_TEMPERATURE)
     &       plumetend(i,j,k)=SPforcingT(i,j,k,bi,bj)
            IF (trIdentity.EQ.GAD_SALINITY) THEN
             plumetend(i,j,k)=SPforcingS(i,j,k,bi,bj)
            ENDIF

C            trStar(i,j,k,bi,bj)=trStar(i,j,k,bi,bj)+plumetend(i,j,k)
C     &      *recip_drF(k)*recip_hFac_arg(i,j,k)
C     &      *recip_ConvertFac
           ENDDO
          ENDDO
         ENDDO

        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--    end if on-off-flag
       ENDIF
C--   end trApplyFlag
      ENDIF

#endif /* SALT_PLUME_VOLUME */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
