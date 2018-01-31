#include "CHEAPAML_OPTIONS.h"

C     !ROUTINE: CHEAPAML_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE CHEAPAML_FIELDS_LOAD( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "CHEAPAML.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: Simulation timestep number
C     myTime :: Simulation time
C     myThid :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === in common block ===
C     trair[01]   :: Relaxation temp. profile for air temperature
C     qrair[01]   :: Relaxation specific humidity profile for air
C     solar[01]   :: short wave flux
C     uWind[01]   :: wind speed [m/s], u-component (on C-grid)
C     vWind[01]   :: wind speed [m/s], v-component (on C-grid)
C     ustress[01] :: wind stress [N/m2], u-component (on A-grid)
C     vstress[01] :: wind stress [N/m2], v-component (on A-grid)
C     wavesh[01]  ::
C     wavesp[01]  ::
C     rair[01]    ::
C     CheaptracerR[01] :: Relaxation profile for passive tracer

      COMMON /BULKFFIELDS/
     &         trair0,   trair1,
     &         qrair0,   qrair1,
     &         Solar0,   Solar1,
     &         uWind0,   uWind1,
     &         vWind0,   vWind1,
     &         ustress0, ustress1,
     &         vstress0, vstress1,
     &         wavesh0,  wavesh1,
     &         wavesp0,  wavesp1,
c    &         rair0,    rair1,
     &         CheaptracerR0, CheaptracerR1,
     &         cheaph0, cheaph1,
     &         cheapcl0, cheapcl1,
     &         cheaplw0, cheaplw1,
     &         cheappr0, cheappr1

      _RL  trair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  trair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  qrair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  qrair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Solar0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Solar1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uWind0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uWind1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vWind0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vWind1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ustress0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ustress1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vstress0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vstress1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesh0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesh1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesp0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesp1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  rair0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  rair1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  CheaptracerR0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  CheaptracerR1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheaph0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheaph1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheapcl0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheapcl1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheaplw0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheaplw1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheappr0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheappr1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     === Local arrays ===
C     cheaph[01]  ::
C     cheapcl[01] ::
C     cheaplw[01] ::
C     aWght,bWght :: Interpolation weights

      _RL aWght, bWght

      INTEGER bi, bj
      INTEGER i, j, iG, jG
      INTEGER intimeP, intime0, intime1
      _RL recipNym1, local, u, ssqa

      recipNym1 = Ny - 1
      IF ( Ny.GT.1 ) recipNym1 = 1. _d 0 / recipNym1

      IF ( periodicExternalForcing_cheap ) THEN

C the objective here is to give cheapaml a default periodic forcing
C consisting only of annually varying solar forcing, and thus Trelaxation
C variation.  everything else, relative humidity, wind, are fixed.  This
C keys off of solardata.  if a solar data file exists, the model will
C assume there are files to be read and interpolated between, as is standard
C for the MITgcm.

        IF ( useStressOption ) THEN
          WRITE(*,*)' stress option is turned on.  this is not ',
     &       'consistent with the default time dependent forcing option'
          STOP
        ENDIF

C here for usual interpolative forcings
C First call requires that we initialize everything to zero for safety
        IF ( myIter .EQ. nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             trair0  (i,j,bi,bj) = 0.
             trair1  (i,j,bi,bj) = 0.
             qrair0  (i,j,bi,bj) = 0.
             qrair1  (i,j,bi,bj) = 0.
             solar0  (i,j,bi,bj) = 0.
             solar1  (i,j,bi,bj) = 0.
             uWind0  (i,j,bi,bj) = 0.
             uWind1  (i,j,bi,bj) = 0.
             vWind0  (i,j,bi,bj) = 0.
             vWind1  (i,j,bi,bj) = 0.
             cheaph0 (i,j,bi,bj) = 0.
             cheaph1 (i,j,bi,bj) = 0.
             cheapcl0(i,j,bi,bj) = 0.5
             cheapcl1(i,j,bi,bj) = 0.5
             cheaplw0(i,j,bi,bj) = 0.
             cheaplw1(i,j,bi,bj) = 0.
             cheappr0(i,j,bi,bj) = 0.
             cheappr1(i,j,bi,bj) = 0.
             ustress0(i,j,bi,bj) = 0.
             ustress1(i,j,bi,bj) = 0.
             vstress0(i,j,bi,bj) = 0.
             vstress1(i,j,bi,bj) = 0.
             wavesh0 (i,j,bi,bj) = 0.
             wavesh1 (i,j,bi,bj) = 0.
             wavesp0 (i,j,bi,bj) = 0.
             wavesp1 (i,j,bi,bj) = 0.
c            rair0   (i,j,bi,bj) = 0.
c            rair1   (i,j,bi,bj) = 0.
             CheaptracerR0 (i,j,bi,bj) = 0.
             CheaptracerR1 (i,j,bi,bj) = 0.
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C--   Now calculate whether it is time to update the forcing arrays
        CALL GET_PERIODIC_INTERVAL(
     O           intimeP, intime0, intime1, bWght, aWght,
     I           externForcingCycle_cheap, externForcingPeriod_cheap,
     I           deltaTclock, myTime, myThid )

        IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
         WRITE(*,*) 'S/R CHEAPAML_FIELDS_LOAD'
         IF ( SolarFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( SolarFile,solar0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( SolarFile,solar1,intime1,
     &                         myIter, myThid )
        ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
              DO j=1,sNy
                DO i=1,sNx
                  Solar(i,j,bi,bj) = 0.0 _d 0
                ENDDO
              ENDDO
            ENDDO
         ENDDO
         _EXCH_XY_RL( solar, myThid )

        ENDIF
         IF ( TrFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( TRFile,trair0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( TRFile,trair1,intime1,
     &                         myIter, myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              TR(i,j,bi,bj) = 0.0 _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _EXCH_XY_RL( TR, myThid )

        ENDIF
         IF ( QrFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( QrFile,qrair0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( QrFile,qrair1,intime1,
     &                         myIter, myThid )
        ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
              DO j=1,sNy
                DO i=1,sNx
                  qr(i,j,bi,bj) = 0.0 _d 0
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         _EXCH_XY_RL( qr, myThid )

        ENDIF
        IF ( UWindFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( UWindFile,uWind0,intime0,
     &       myIter, myThid )
          CALL READ_REC_XY_RL( UWindFile,uWind1,intime1,
     &       myIter, myThid )
        ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              uWind(i,j,bi,bj) = 0.0 _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         _EXCH_XY_RL( uWind, myThid )
!         CALL EXCH_UV_XY_RL( uWind, vWind, .TRUE., myThid )

       ENDIF

         IF ( VWindFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( VWindFile,vWind0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( VWindFile,vWind1,intime1,
     &                         myIter, myThid )
        ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              vWind(i,j,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         _EXCH_XY_RL( vWind, myThid )
!         CALL EXCH_UV_XY_RL( uWind, vWind, .TRUE., myThid )

        ENDIF
         IF ( useTimeVarBLH .AND. cheap_hFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( cheap_hFile,cheaph0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( cheap_hFile,cheaph1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useClouds .AND. cheap_clFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( cheap_clFile,cheapcl0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( cheap_clFile,cheapcl1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useDLongWave .AND. cheap_dlwFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( cheap_dlwFile,cheaplw0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( cheap_dlwFile,cheaplw1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( (usePrecip) .AND. cheap_prFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( cheap_prFile,cheappr0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( cheap_prFile,cheappr1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useStressOption .AND. UStressFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( UStressFile,ustress0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( UStressFile,ustress1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useStressOption .AND. VStressFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( VStressFile,vstress0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( VStressFile,vstress1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( FluxFormula.EQ.'COARE3' .AND. WaveHFile.NE.' ' ) THEN
          CALL READ_REC_XY_RL( WaveHFile,wavesh0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( WaveHFile,wavesh1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( FluxFormula.EQ.'COARE3' .AND. WavePFile.NE.' ' ) THEN
          CALL READ_REC_XY_RL( WavePFile,wavesp0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( WavePFile,wavesp1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useCheapTracer .AND. TracerRFile .NE. ' ' ) THEN
           CALL READ_REC_XY_RL( TracerRFile,CheaptracerR0,intime0,
     &                         myIter, myThid )
           CALL READ_REC_XY_RL( TracerRFile,CheaptracerR1,intime1,
     &                         myIter, myThid )
         ELSEIF( useCheapTracer ) THEN
           DO bj = myByLo(myThid), myByHi(myThid)
             DO bi = myBxLo(myThid), myBxHi(myThid)
               DO j=1,sNy
                 DO i=1,sNx
                   CheaptracerR(i,j,bi,bj) = 0. _d 0
                 ENDDO
               ENDDO
             ENDDO
           ENDDO
          _EXCH_XY_RL( CheaptracerR, myThid )

         ENDIF
         _EXCH_XY_RL( trair0 , myThid )
         _EXCH_XY_RL( trair1 , myThid )
         _EXCH_XY_RL( qrair0 , myThid )
         _EXCH_XY_RL( qrair1 , myThid )
         _EXCH_XY_RL( solar0 , myThid )
         _EXCH_XY_RL( solar1 , myThid )
         CALL EXCH_UV_XY_RL( uWind0, vWind0, .TRUE., myThid )
         CALL EXCH_UV_XY_RL( uWind1, vWind1, .TRUE., myThid )
         IF ( useTimeVarBLH ) THEN
         _EXCH_XY_RL( cheaph0, myThid )
         _EXCH_XY_RL( cheaph1 , myThid )
         ENDIF
         IF ( useClouds ) THEN
         _EXCH_XY_RL( cheapcl0, myThid )
         _EXCH_XY_RL( cheapcl1 , myThid )
         ENDIF
         IF ( useDLongWave ) THEN
         _EXCH_XY_RL( cheaplw0, myThid )
         _EXCH_XY_RL( cheaplw1 , myThid )
         ENDIF
         IF ( usePrecip ) THEN
         _EXCH_XY_RL( cheappr0, myThid )
         _EXCH_XY_RL( cheappr1 , myThid )
         ENDIF
         IF ( useStressOption ) THEN
         CALL EXCH_UV_AGRID_3D_RL(ustress0,vstress0, .TRUE.,1,myThid )
         CALL EXCH_UV_AGRID_3D_RL(ustress1,vstress1, .TRUE.,1,myThid )
         ENDIF
         IF ( FluxFormula.EQ.'COARE3' ) THEN
         _EXCH_XY_RL( wavesp0 , myThid )
         _EXCH_XY_RL( wavesp1 , myThid )
         _EXCH_XY_RL( wavesh0 , myThid )
         _EXCH_XY_RL( wavesh1 , myThid )
         ENDIF
         IF ( useCheapTracer ) THEN
         _EXCH_XY_RL( CheaptracerR0 , myThid )
         _EXCH_XY_RL( CheaptracerR1, myThid )
         ENDIF

C     end of loading new fields block
        ENDIF

C--   Interpolate TR, QR, SOLAR
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            TR(i,j,bi,bj)           = bWght*trair0(i,j,bi,bj)
     &                              + aWght*trair1(i,j,bi,bj)   !+273.15
            qr(i,j,bi,bj)           = bWght*qrair0(i,j,bi,bj)
     &                              + aWght*qrair1(i,j,bi,bj)
            uWind(i,j,bi,bj)        = bWght*uWind0(i,j,bi,bj)
     &                              + aWght*uWind1(i,j,bi,bj)
            vWind(i,j,bi,bj)        = bWght*vWind0(i,j,bi,bj)
     &                              + aWght*vWind1(i,j,bi,bj)
            solar(i,j,bi,bj)        = bWght*solar0(i,j,bi,bj)
     &                              + aWght*solar1(i,j,bi,bj)
            IF ( useStressOption ) THEN
             ustress(i,j,bi,bj)     = bWght*ustress0(i,j,bi,bj)
     &                              + aWght*ustress1(i,j,bi,bj)
             vstress(i,j,bi,bj)     = bWght*vstress0(i,j,bi,bj)
     &                              + aWght*vstress1(i,j,bi,bj)
            ENDIF
            IF ( useTimeVarBLH ) THEN
             CheapHgrid(i,j,bi,bj)  = bWght*cheaph0(i,j,bi,bj)
     &                              + aWght*cheaph1(i,j,bi,bj)
            ENDIF
            IF ( useClouds ) THEN
             cheapclouds(i,j,bi,bj) = bWght*cheapcl0(i,j,bi,bj)
     &                              + aWght*cheapcl1(i,j,bi,bj)
            ENDIF
            IF ( useDLongWave ) THEN
             cheapdlongwave(i,j,bi,bj) = bWght*cheaplw0(i,j,bi,bj)
     &                                 + aWght*cheaplw1(i,j,bi,bj)
            ENDIF
            IF ( usePrecip ) THEN
             cheapPrecip(i,j,bi,bj) = bWght*cheappr0(i,j,bi,bj)
     &                                 + aWght*cheappr1(i,j,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
             CheaptracerR(i,j,bi,bj) = bWght*CheaptracerR0(i,j,bi,bj)
     &                               + aWght*CheaptracerR1(i,j,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
             IF ( WaveHFile.NE.' ' ) THEN
              wavesh(i,j,bi,bj)     = bWght*wavesh0(i,j,bi,bj)
     &                              + aWght*wavesh1(i,j,bi,bj)
             ENDIF
             IF ( WavePFile.NE.' ' ) THEN
              wavesp(i,j,bi,bj)     = bWght*wavesp0(i,j,bi,bj)
     &                              + aWght*wavesp1(i,j,bi,bj)
             ENDIF
            ELSE
             u = uWind(i,j,bi,bj)**2 + vWind(i,j,bi,bj)**2
             u = SQRT(u)
             wavesp(i,j,bi,bj) = 0.729 _d 0 * u
             wavesh(i,j,bi,bj) = 0.018 _d 0 * u*u*(1. + .015 _d 0 *u)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C end of periodic forcing options, on to steady option
      ELSE

       IF ( myIter .EQ. nIter0 ) THEN

        IF ( SolarFile .NE. ' ' ) THEN
         CALL READ_FLD_XY_RL( SolarFile,' ',solar,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              local = 225. _d 0 - (jG-1)*recipNym1*37.5 _d 0
              Solar(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        _EXCH_XY_RL( solar, myThid )

        IF ( TrFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RL( TrFile,' ',tr,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              local = solar(i,j,bi,bj)
              local = (2. _d 0*local/stefan)**(0.25 _d 0) - celsius2K
              TR(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        _EXCH_XY_RL( TR, myThid )

C do specific humidity
        IF ( QrFile .NE. ' ') THEN
         CALL READ_FLD_XY_RL( QrFile,' ',qr,0,myThid )
        ELSE
C default specific humidity profile to 80% relative humidity
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              local = Tr(i,j,bi,bj) + celsius2K
              ssqa = ssq0*EXP( lath*(ssq1-ssq2/local)) / p0
              qr(i,j,bi,bj) = 0.8 _d 0*ssqa
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        _EXCH_XY_RL( qr, myThid )

        IF ( UWindFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RL( UWindFile,' ',uWind,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              jG = myYGlobalLo-1+(bj-1)*sNy+j
C mod for debug
C to return  to original code, uncomment following line
C comment out 2nd line
              local = -5. _d 0*COS( 2. _d 0*PI*(jG-1)*recipNym1 )
c             local =  0. _d 0*COS( 2. _d 0*PI*(jG-1)*recipNym1 )
              uWind(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF

        IF ( VWindFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RL( VWindFile,' ',vWind,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              vWind(i,j,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        CALL EXCH_UV_XY_RL( uWind, vWind, .TRUE., myThid )

        IF ( useStressOption ) THEN
         IF ( UStressFile .NE. ' '  ) THEN
          CALL READ_FLD_XY_RL( UStressFile,' ',ustress,0,myThid )
         ELSE
          WRITE(*,*)' U Stress File absent with stress option'
          STOP
         ENDIF
         IF ( VStressFile .NE. ' '  ) THEN
          CALL READ_FLD_XY_RL( VStressFile,' ',vstress,0,myThid )
         ELSE
          WRITE(*,*)' V Stress File absent with stress option'
          STOP
         ENDIF
         CALL EXCH_UV_AGRID_3D_RL( ustress,vstress, .TRUE.,1,myThid )
        ENDIF
        IF ( useCheapTracer ) THEN
         IF ( TracerRFile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL( TracerRFile,' ',CheaptracerR,0,myThid )
         ELSE
           DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
             DO j=1,sNy
              DO i=1,sNx
               CheaptracerR(i,j,bi,bj)=290. _d 0
              ENDDO
             ENDDO
            ENDDO
           ENDDO
         ENDIF
         _EXCH_XY_RL( CheaptracerR, myThid )
        ENDIF
        IF ( FluxFormula.EQ.'COARE3' ) THEN
         IF ( WaveHFile.NE.' ' ) THEN
          CALL READ_FLD_XY_RL( WaveHFile,' ',wavesh,0,myThid )
         ENDIF
         IF ( WavePFile.NE.' ' ) THEN
          CALL READ_FLD_XY_RL( WavePFile,' ',wavesp,0,myThid )
         ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1,sNy
             DO i=1,sNx
              u = uWind(i,j,bi,bj)**2 + vWind(i,j,bi,bj)**2
              u = SQRT(u)
              wavesp(i,j,bi,bj)=0.729 _d 0 * u
              wavesh(i,j,bi,bj)=0.018 _d 0 * u*u*(1. + .015 _d 0 *u)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         _EXCH_XY_RL( wavesp, myThid )
         _EXCH_XY_RL( wavesh, myThid )
        ENDIF

C     BL height is done in cheapaml_ini_varia

        IF ( useClouds ) THEN
         IF ( cheap_clFile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL( cheap_clFile,' ',Cheapclouds,0,myThid )
         ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1,sNy
             DO i=1,sNx
               Cheapclouds(i,j,bi,bj)=0.5
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         _EXCH_XY_RL( Cheapclouds, myThid )
        ENDIF

        IF ( useDLongWave ) THEN
         IF ( cheap_dlwFile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL(cheap_dlwFile,' ',Cheapdlongwave,0,myThid)
         ELSE
          WRITE(*,*) 'with useDLongWave = true,  you must provide',
     $               ' a downward longwave file'
          STOP
         ENDIF
         _EXCH_XY_RL( Cheapdlongwave, myThid )
        ENDIF

        IF ( usePrecip ) THEN
         IF ( cheap_prFile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL(cheap_prFile,' ',CheapPrecip,0,myThid)
         ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1,sNy
             DO i=1,sNx
               CheapPrecip(i,j,bi,bj)=0.0
             ENDDO
            ENDDO
           ENDDO
          ENDDO

        ENDIF
         _EXCH_XY_RL( CheapPrecip, myThid )
        ENDIF

C--    endif myIter = nIter0
       ENDIF

C endif for Steady Option
      ENDIF

C fill in outer edges

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          jG = myYGlobalLo-1+(bj-1)*sNy+j
          DO i=1-OLx,sNx+OLx
           iG=myXGlobalLo-1+(bi-1)*sNx+i

           IF ( .NOT.cheapamlXperiodic .AND. iG.LT.1 ) THEN

            Tr(i,j,bi,bj)=Tr(1,j,bi,bj)
            qr(i,j,bi,bj)=qr(1,j,bi,bj)
            uWind(i,j,bi,bj)=uWind(2,j,bi,bj)
            vWind(i,j,bi,bj)=vWind(1,j,bi,bj)
            Solar(i,j,bi,bj)=Solar(1,j,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(1,j,bi,bj)
              vstress(i,j,bi,bj)=vstress(1,j,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(1,j,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(1,j,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(1,j,bi,bj)
            ENDIF
            IF ( useClouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(1,j,bi,bj)
            ENDIF
            IF ( useDLongWave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(1,j,bi,bj)
            ENDIF

           ELSEIF ( .NOT.cheapamlXperiodic .AND. iG.EQ.1 ) THEN

            uWind(i,j,bi,bj)=uWind(2,j,bi,bj)

           ELSEIF ( .NOT.cheapamlXperiodic .AND. iG.GT.Nx ) THEN

            Tr(i,j,bi,bj)=Tr(sNx,j,bi,bj)
            qr(i,j,bi,bj)=qr(sNx,j,bi,bj)
            uWind(i,j,bi,bj)=uWind(sNx,j,bi,bj)
            vWind(i,j,bi,bj)=vWind(sNx,j,bi,bj)
            Solar(i,j,bi,bj)=Solar(sNx,j,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(sNx,j,bi,bj)
              vstress(i,j,bi,bj)=vstress(sNx,j,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(sNx,j,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(sNx,j,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(sNx,j,bi,bj)
            ENDIF
            IF ( useClouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(sNx,j,bi,bj)
            ENDIF
            IF ( useDLongWave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(sNx,j,bi,bj)
            ENDIF

           ELSEIF ( .NOT.cheapamlYperiodic .AND. jG.LT.1 ) THEN

            Tr(i,j,bi,bj)=Tr(i,1,bi,bj)
            qr(i,j,bi,bj)=qr(i,1,bi,bj)
            uWind(i,j,bi,bj)=uWind(i,1,bi,bj)
            vWind(i,j,bi,bj)=vWind(i,2,bi,bj)
            Solar(i,j,bi,bj)=Solar(i,1,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(i,1,bi,bj)
              vstress(i,j,bi,bj)=vstress(i,1,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(i,1,bi,bj)
            ENDIF
            IF ( useClouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(i,1,bi,bj)
            ENDIF
            IF ( useDLongWave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(i,1,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(i,1,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(i,1,bi,bj)
            ENDIF

           ELSEIF ( .NOT.cheapamlYperiodic .AND. jG.EQ.1 ) THEN

            vWind(i,j,bi,bj)=vWind(i,2,bi,bj)

           ELSEIF ( .NOT.cheapamlYperiodic .AND. jG.GT.Ny ) THEN

            Tr(i,j,bi,bj)=Tr(i,sNy,bi,bj)
            qr(i,j,bi,bj)=qr(i,sNy,bi,bj)
            uWind(i,j,bi,bj)=uWind(i,sNy,bi,bj)
            vWind(i,j,bi,bj)=vWind(i,sNy,bi,bj)
            Solar(i,j,bi,bj)=Solar(i,sNy,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(i,sNy,bi,bj)
              vstress(i,j,bi,bj)=vstress(i,sNy,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(i,sNy,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(i,sNy,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(i,sNy,bi,bj)
            ENDIF
            IF ( useClouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(i,sNy,bi,bj)
            ENDIF
            IF ( useDLongWave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(i,sNy,bi,bj)
            ENDIF

           ENDIF
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      RETURN
      END
