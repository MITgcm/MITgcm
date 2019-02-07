#include "EXF_OPTIONS.h"

CBOP 0
C     !ROUTINE: EXF_WEIGHT_SFX_DIAGS
C     !INTERFACE:
      SUBROUTINE EXF_WEIGHT_SFX_DIAGS(
     I                                 siceFrac, diagGroup,
     I                                 myTime, myIter, myThid )

C     !DESCRIPTION:
C     Apply open-ocean fraction weight to surface fluxes
C     before filling-in corresponding diagnostics

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"

C     !INPUT PARAMETERS:
C     siceFrac  :: seaice fraction [0-1]
C     diagGroup :: select which group of surface fluxes to process
C               :: =1 : wind-stress ; =2 : other surf. fluxes
C     myTime    :: current time in simulation [s]
C     myIter    :: current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL     siceFrac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER diagGroup
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

C     !LOCAL VARIABLES:
C     tmpFld    :: open-ocean weighted surface flux
C     bi,bj     :: tile indices
C     i,j       :: grid point indices
      _RL     tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj
      INTEGER i, j
CEOP

      IF ( useDiagnostics .AND. diags_opOceWeighted ) THEN

C--   First group of surface-flux: Momentum fluxes
        IF ( diagGroup.EQ.-1 ) THEN
          CALL DIAGNOSTICS_FILL( ustress,'EXFtaux ', 0,1,0,1,1,myThid )
          CALL DIAGNOSTICS_FILL( vstress,'EXFtauy ', 0,1,0,1,1,myThid )
        ELSEIF ( diagGroup.EQ.1 ) THEN

         IF ( DIAGNOSTICS_IS_ON('EXFtaux ',myThid) ) THEN
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
            IF ( stressIsOnCgrid ) THEN
             DO j = 1,sNy
              DO i = 1,sNx+1
               tmpFld(i,j,bi,bj) = ustress(i,j,bi,bj)
     &            *( oneRL - halfRL*
     &               ( siceFrac(i-1,j,bi,bj) + siceFrac(i,j,bi,bj) ) )
              ENDDO
             ENDDO
            ELSE
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
               tmpFld(i,j,bi,bj) = ustress(i,j,bi,bj)
     &                           *( oneRL - siceFrac(i,j,bi,bj) )
              ENDDO
             ENDDO
            ENDIF
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL( tmpFld, 'EXFtaux ', 0,1,0,1,1,myThid )
         ENDIF

         IF ( DIAGNOSTICS_IS_ON('EXFtauy ',myThid) ) THEN
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
            IF ( stressIsOnCgrid ) THEN
             DO j = 1,sNy+1
              DO i = 1,sNx
               tmpFld(i,j,bi,bj) = vstress(i,j,bi,bj)
     &            *( oneRL - halfRL*
     &               ( siceFrac(i,j-1,bi,bj) + siceFrac(i,j,bi,bj) ) )
              ENDDO
             ENDDO
            ELSE
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
               tmpFld(i,j,bi,bj) = vstress(i,j,bi,bj)
     &                           *( oneRL - siceFrac(i,j,bi,bj) )
              ENDDO
             ENDDO
            ENDIF
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL( tmpFld, 'EXFtauy ', 0,1,0,1,1,myThid )
         ENDIF

C--   end first group
        ENDIF

C--   Second group of surface-flux: Buoyancy fluxes
        IF ( diagGroup.EQ.-2 ) THEN
          CALL DIAGNOSTICS_FILL( hflux,  'EXFqnet ', 0,1,0,1,1,myThid )
          CALL DIAGNOSTICS_FILL( sflux,  'EXFempmr', 0,1,0,1,1,myThid )
#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
          CALL DIAGNOSTICS_FILL( swflux, 'EXFswnet', 0,1,0,1,1,myThid )
#endif
        ELSEIF ( diagGroup.EQ.2 ) THEN

         IF ( DIAGNOSTICS_IS_ON('EXFqnet ',myThid) ) THEN
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
               tmpFld(i,j,bi,bj) = hflux(i,j,bi,bj)
     &                           *( oneRL - siceFrac(i,j,bi,bj) )
              ENDDO
             ENDDO
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL( tmpFld, 'EXFqnet ', 0,1,0,1,1,myThid )
         ENDIF

         IF ( DIAGNOSTICS_IS_ON('EXFempmr',myThid) ) THEN
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
               tmpFld(i,j,bi,bj) = sflux(i,j,bi,bj)
     &                           *( oneRL - siceFrac(i,j,bi,bj) )
              ENDDO
             ENDDO
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL( tmpFld, 'EXFempmr', 0,1,0,1,1,myThid )
         ENDIF

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
         IF ( DIAGNOSTICS_IS_ON('EXFswnet',myThid) ) THEN
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
               tmpFld(i,j,bi,bj) = swflux(i,j,bi,bj)
     &                           *( oneRL - siceFrac(i,j,bi,bj) )
              ENDDO
             ENDDO
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL( tmpFld, 'EXFswnet', 0,1,0,1,1,myThid )
         ENDIF
#endif

C- Note: Below is a list of potential surface fluxes that we may want also to
C        weight by open-ocean fraction. But at this point not sure if there is
C        a real interest for this.
#ifdef ALLOW_ATM_TEMP
c         CALL DIAGNOSTICS_FILL( hs    , 'EXFhs   ', 0,1,0,1,1,myThid )
c         CALL DIAGNOSTICS_FILL( hl    , 'EXFhl   ', 0,1,0,1,1,myThid )
c         CALL DIAGNOSTICS_FILL( lwflux, 'EXFlwnet', 0,1,0,1,1,myThid )
c         CALL DIAGNOSTICS_FILL( evap,   'EXFevap ', 0,1,0,1,1,myThid )
#endif
#ifdef ALLOW_SALTFLX
c         CALL DIAGNOSTICS_FILL( saltflx,'EXFsalfx', 0,1,0,1,1,myThid )
#endif

C--   end second group
        ENDIF

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
