C $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/Attic/aim_do_atmos_physics.F,v 1.4 2001/03/06 17:45:40 jmc Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_DO_ATMOS_PHYSICS( phi_hyd, currentTime, myThid )
C     /==================================================================\
C     | S/R AIM_DO_ATMOS_PHYSICS                                         |
C     |==================================================================|
C     | Interface interface between atmospheric physics package and the  |
C     | dynamical model.                                                 |
C     | Routine calls physics pacakge after mapping model variables to   |
C     | the package grid. Package should derive and set tendency terms   |
C     | which can be included as external forcing terms in the dynamical |
C     | tendency routines. Packages should communicate this information  |
C     | through common blocks.                                           |
C     \==================================================================/

C     -------------- Global variables ------------------------------------
C     Physics package
#include "atparam.h"
#include "atparam1.h"
      INTEGER NGP
      INTEGER NLON
      INTEGER NLAT
      INTEGER NLEV
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
#include "com_physvar.h"
#include "com_forcing1.h"
#include "Lev_def.h"
#include "AIM_DDT.h"
C     MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     -------------- Routine arguments -----------------------------------
      _RL phi_hyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL currentTime

#ifdef ALLOW_AIM
C     -------------- Local variables -------------------------------------
C     I,J,K,I2,J2   - Loop counters
C     tYear         - Fraction into year
C     mnthIndex     - Current month
C     prevMnthIndex - Month last time this routine was called.
C     tmp4          - I/O buffer ( 32-bit precision )
C     fNam          - Work space for file names
C     mnthNam       - Month strings
C     hInital       - Initial height of pressure surfaces (m)
C     pSurfs        - Pressure surfaces (Pa)
C     Katm          - Atmospheric K index
      INTEGER I
      INTEGER I2
      INTEGER J
      INTEGER J2
      INTEGER K
      INTEGER IG0
      INTEGER JG0
      REAL    tYear
      INTEGER mnthIndex
      INTEGER prevMnthIndex
      DATA    prevMnthIndex / 0 /
      SAVE    prevMnthIndex
      LOGICAL FirstCall
      DATA    FirstCall /.TRUE./
      SAVE    FirstCall
      LOGICAL CALLFirst
      DATA    CALLFirst /.TRUE./
      SAVE    CALLFirst
      INTEGER nxIo 
      INTEGER nyIo
      PARAMETER ( nxIo = 128, nyIo = 64 )
      Real*4  tmp4(nxIo,nyIo)
      CHARACTER*16 fNam
      CHARACTER*3 mnthNam(12)
      DATA mnthNam /
     & 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 
     & 'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /
      SAVE mnthNam
      REAL    hInitial(Nr)
      REAL    hInitialW(Nr)
      DATA    hInitial / 418.038,2038.54,5296.88,10090.02,17338.0/
      SAVE    hInitial
      DATA    hInitialW / 0., 1657.54, 4087.75, 8050.96,15090.4 /
      REAL    pSurfs(Nr)
      DATA    pSurfs   / 950.D2,775.D2, 500.D2,  250.D2, 75.D2 /
      SAVE    pSurfs
      REAL    pSurfw(Nr)
      DATA    pSurfw   /1000.D2, 900.D2, 650.D2, 350.D2, 150.D2  /
      SAVE    pSurfw
      REAL    RD
      REAL    CPAIR
      REAL    RhoG1(sNx*sNy,Nr)
      INTEGER npasdt
      DATA npasdt /0/
      SAVE npasdt
      REAL Soilqmax
      REAL phiTotal(sNx,sNy,Nr)
      _RL  phiTCount
      _RL  phiTSum
      _RL  ans
      real pvoltotNiv5
      SAVE pvoltotNiv5
      real ptotalNiv5
      INTEGER bi, bj
      INTEGER Katm
C
      pGround = 1.D5
      CPAIR   = 1004
      RD      =  287

C     Assume only one tile per proc. for now
      bi  = 1
      bj  = 1
      IG0 = myXGlobalLo
      JG0 = myYGlobalLo

C      
C     Physics package works with sub-domains 1:sNx,1:sNy,1:Nr.
C     Internal index mapping is linear in X and Y with a second
C     dimension for the vertical.

C     Adjustment for heave due to mean heating/cooling
C     ( I don't think the old formula was strictly "correct" for orography
C       but I have implemented it as was for now. As implemented
C       the mean heave of the bottom (K=Nr) level is calculated rather than
C       the mean heave of the base of the atmosphere. )
      phiTCount = 0.
      phiTSum   = 0.
      DO K=1,Nr
      DO J=1,sNy
       DO I=1,sNx
        phiTotal(I,J,K)  = etaN(i,j,bi,bj)
        phiTCount        = phiTCount + hFacC(i,j,Nr,bi,bj)
       ENDDO
      ENDDO
      ENDDO
      DO K=1,Nr
       DO J=1,sNy
        DO I=1,sNx
         phiTotal(I,J,K) = phiTotal(I,J,K) +
     &   recip_rhoConst*(phi_hyd(i,j,k,bi,bj))
        ENDDO
       ENDDO
      ENDDO
      DO J=1,sNy
       DO I=1,sNx
        phiTSum = phiTSum + phiTotal(I,J,Nr)
       ENDDO
      ENDDO
      ans = phiTCount
C     _GLOBAL_SUM_R8( phiTCount, myThid )
      phiTcount = ans
      ans = phiTSum
C     _GLOBAL_SUM_R8( phiTSum, myThid )
      phiTSum = ans
C     ptotalniv5=phiTSum/phiTCount
      ptotalniv5=0.

C     Note the mapping here is only valid for one tile
C     per proc.
      DO K = 1, Nr
       DO J = 1, sNy
        DO I = 1, sNx
         I2 = (sNx)*(J-1)+I
         Katm = _KD2KA( K )
         UG1(I2,Katm)   = 0.5*(uVel(I,J,K,bi,bj)+uVel(I+1,J,K,bi,bj))
         VG1(I2,Katm)   = 0.5*(vVel(I,J,K,bi,bj)+vVel(I,J+1,K,bi,bj))
C        Phyiscs works with temperature - not potential temp.
         TG1(I2,Katm)   = theta(I,J,K,bi,bj)/((pGround/pSurfs(K))**(RD/CPAIR))
         QG1(I2,Katm)   = salt(I,J,K,bi,bj)
         PHIG1(I2,Katm) = (phiTotal(I,J,K)- ptotalniv5 ) + gravity*Hinitial(k)
         if(hFacC(i,j,k,bi,bj).eq.1.) then
           RHOG1(I2,Katm) = pSurfs(K)/RD/TG1(I2,Katm)
         else
           RHOG1(I2,Katm)=0.
         endif
        ENDDO
       ENDDO
      ENDDO

C      
C     Set geopotential surfaces
C     -------------------------
      DO J=1,sNy
       DO I=1,sNx
        I2 = (sNx)*(J-1)+I
        IF ( Nlevxy(I2) .NE. 0 ) THEN
         PHI0(I2) = gravity*Hinitialw(Nlevxy(I2))
        ELSE
         PHI0(I2) = 0.
        ENDIF
       ENDDO
      ENDDO
C
C     Physics package works with log of surface pressure
C     Get surface pressure from pbot-dpref/dz*Z'
      DO J=1,sNy
       DO I=1,sNx
        I2 = (sNx)*(J-1)+I
        IF ( Nlevxy(I2) .NE. 0 ) THEN
         PNLEVW(I2) = PsurfW(Nlevxy(I2))/pGround
        ELSE
C        Dummy value for land
         PNLEVW(I2) = PsurfW(1)/pGround
        ENDIF
        PSLG1(I2) = 0.
       ENDDO
      ENDDO
cch      write(0,*)  '(PNLEVW(I2),I2=257,384)'
cch      write(0,*)  (PNLEVW(I2),I2=257,384)
C
C
C     Physics package needs to know time of year as a fraction
      tYear = currentTime/(86400.*360.) -
     &        FLOAT(INT(currentTime/(86400.*360.)))
C     Fix solar insolation with Sun directly overhead on Equator
      tYear = 0.25
C
C     Load external data needed by physics package
C     1. Albedo
C     2. Soil moisture
C     3. Surface temperatures
C     4. Snow depth            - assume no snow for now
C     5. Sea ice               - assume no sea ice for now
C     6. Land sea mask         - infer from exact zeros in soil moisture dataset
C     7. Surface geopotential  - to be done when orography is in
C                                dynamical kernel. Assume 0. for now.
      mnthIndex = INT(tYear*12.)+1
      IF ( mnthIndex .NE. prevMnthIndex .OR.
     &     FirstCall ) THEN
       prevMnthIndex = mnthIndex
C      Read in surface albedo data (input is in % 0-100 )
C      scale to give fraction between 0-1 for Francos package.
CequChan       WRITE(fNam,'(A,A,A)' ) 'salb.',mnthNam(mnthIndex),'.sun.b'
CequChan       OPEN(1,FILE=fNam(1:14),STATUS='old',FORM='unformatted')
CequChan       READ(1) tmp4
CequChan       CLOSE(1)
CequChan       DO J=1,nYio
CequChan        DO I=1,nXio
CequChan         tmp4(I,J) = tmp4(I,J)/100.
CequChan        ENDDO
CequChan       ENDDO
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         alb0(I2) = 0.
CequChan         IF ( IG0+I-1 .LE. nxIo .AND. JG0+J-1 .LE. nyIo ) THEN
CequChan          alb0(I2) = tmp4(IG0+I-1,JG0+J-1)
CequChan         ENDIF
        ENDDO
       ENDDO
C      Read in surface temperature data (input is in absolute temperature)
CequChan       WRITE(fNam,'(A,A,A)' ) 'tsurf.',mnthNam(mnthIndex),'.sun.b'
CequChan       OPEN(1,FILE=fNam(1:15),STATUS='old',FORM='unformatted')
CequChan       READ(1) tmp4
CequChan       CLOSE(1)
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         sst1(I2) = 300.
         stl1(I2) = 300.
CequChan         IF ( IG0+I-1 .LE. nxIo .AND. JG0+J-1 .LE. nyIo ) THEN
CequChan          sst1(I2) = tmp4(IG0+I-1,JG0+J-1)
CequChan          stl1(I2) = tmp4(IG0+I-1,JG0+J-1)
CequChan         ENDIF
caja     IF ( I .GE. 64-10 .AND. I .LE. 65+10 ) THEN
caja      sst1(I2) = 310.
caja      stl1(I2) = 310.
caja     ENDIF
caja     IF ( I .GE. 64-10 .AND. I .LE. 65+10 ) THEN
caja      sst1(I2) = 300.+10.*exp( -((float(I)-64.5)/5.)**2 )
caja      stl1(I2) = sst1(I2)
caja     ENDIF
         sst1(I2) = 300.
     &    +10.*exp( -((float(I)-64.5)/25.)**2 )
     &    +10.*exp( -((float(J)-12.5)/25.)**2 )
         stl1(I2) = sst1(I2)
        ENDDO
       ENDDO
C
C      Read in soil moisture data (input is in cm in bucket of depth 20cm. )
C??? NOT CLEAR  scale for bucket depth of 75mm which is what Franco uses.
CequChan       WRITE(fNam,'(A,A,A)' ) 'smoist.',mnthNam(mnthIndex),'.sun.b'
CequChan       OPEN(1,FILE=fNam(1:16),STATUS='old',FORM='unformatted')
CequChan       READ(1) tmp4
CequChan       CLOSE(1)
CequChan       WRITE(0,*) ' Read file ', fNam(1:16), IG0, JG0
cdj       tmp4 = (tmp4*7.5/20.)*10.
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         soilq1(I2) = 0.
CequChan         IF ( IG0+I-1 .LE. nxIo .AND. JG0+J-1 .LE. nyIo ) THEN
CequChan          soilq1(I2) = tmp4(IG0+I-1,JG0+J-1)
CequChan         ENDIF
        ENDDO
       ENDDO
cdj      Soilqmax=MAxval(soilq1)
       Soilqmax=20.
cdj      if(Soilqmax.ne.0.) then
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
CequChan         soilq1(I2)=soilq1(I2)/Soilqmax
         soilq1(I2) = 1.
        ENDDO
       ENDDO
cdj      endif
      ENDIF
C
      IF ( FirstCall ) THEN
C      Set snow depth, sea ice to zero for now
C      Land-sea mask ( figure this out from where soil moisture is exactly zero ).
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         fMask1(I2) = 1.
         IF ( soilq1(I2) .EQ. 0. ) fMask1(I2) = 0.
         oice1(I2) = 0.
         snow1(I2) = 0.
        ENDDO
       ENDDO
C      open(77,file='lsmask',form='unformatted')
C      write(77) fmask1
C      close(77)
      ENDIF
C
C Addition may 15 . Reset humidity to 0. if negative
C ---------------------------------------------------
      DO K=1,Nr
       DO J=1-OLy,sNy+OLy
        DO I=1-Olx,sNx+OLx
         IF ( salt(i,j,k,bi,bj) .LT. 0. .OR. K .EQ. Nr ) THEN
          salt(i,j,k,bi,bj) = 0.
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
      CALL PDRIVER( tYear )

#ifdef ALLOW_TIMEAVE
C     Calculate diagnostics for AIM
      CALL AIM_CALC_DIAGS( bi, bj, currentTime, myThid )
#endif /* ALLOW_TIMEAVE */
C
      FirstCall = .FALSE.
C
C     For velocity tendencies on C grid need to interpolate
C     to do this in multi-processor mode we copy U and V tendencies
C     into dynamics conforming array and then exchange.
C     ** NOTE - Exchange at this point is not compatible with
C               multiple-tiles per compute thread/process.
C               However, AIM code itself is not compatible with
C               this as its global data assumes only one "tile".
      CALL AIM_UTVT2DYN( bi, bj, myThid )
      _EXCH_XYZ_R8( AIM_UT, myThid )
      _EXCH_XYZ_R8( AIM_VT, myThid )
C
#endif /* ALLOW_AIM */

      RETURN
      END
