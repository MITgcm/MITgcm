C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_lsm.F,v 1.9 2010/08/24 14:36:55 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      SUBROUTINE TILE (
     I                 NCH, DTSTEP, ITYP, TRAINL,TRAINC, TSNOW,  UM,
     I                 ETURB,  DEDQA,  DEDTC,  HSTURB, DHSDQA, DHSDTC,
     I                 TM,     QM,     CD,     SUNANG, PARDIR,  PARDIF,
     I                 SWNET,  HLWDWN, PSUR,    ZLAI,   GREEN,  Z2,
     I                 SQSCAT, RSOIL1, RSOIL2,   RDC,    U2FAC,
     I                 QSATTC, DQSDTC, ALWRAD, BLWRAD,
     U                 TC, TD, QA, SWET1, SWET2, SWET3, CAPAC, SNOW,
     O                 EVAP, SHFLUX, RUNOFF, BOMB,
     O                 EINT,   ESOI,   EVEG,   ESNO, SMELT, HLATN,
     O                 HLWUP,GDRAIN,RUNSRF,FWSOIL,
     O                 STRDG1, STRDG2, STRDG3, STRDG4,
     O                 STRDG5, STRDG6, STRDG7, STRDG8, STRDG9
     &                )

C   SCCS VERSION @(#)lsm.f      1.2 11/3/92

C****
C****       This subroutine computes the outgoing fluxes of sensible and
C**** latent heat from the land surface and updates the surface prognostic
C**** variables.
C****
      IMPLICIT NONE
C
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH)
      _RL  DTSTEP,  TRAINL(NCH), TRAINC(NCH), TSNOW(NCH), UM(NCH),
     &       ETURB(NCH),  DEDQA(NCH), HSTURB(NCH), DHSDTC(NCH),
     &         TM (NCH),     CD(NCH), SUNANG(NCH), DHSDQA(NCH),
     &         QM (NCH), PARDIR(NCH), PARDIF(NCH),  SWNET(NCH),
     &      HLWDWN(NCH),   PSUR(NCH),   ZLAI(NCH),  GREEN(NCH),
     &          Z2(NCH), SQSCAT(NCH),  DEDTC(NCH)
      _RL  RSOIL1(NCH), RSOIL2(NCH),    RDC(NCH),  U2FAC(NCH),
     &     QSATTC(NCH), DQSDTC(NCH),  ALWRAD(NCH), BLWRAD(NCH),
     &          TC(NCH),     TD(NCH),     QA(NCH),   BOMB(NCH),
     &       SWET1(NCH),  SWET2(NCH),  SWET3(NCH),  CAPAC(NCH),
     &        SNOW(NCH),   EVAP(NCH), SHFLUX(NCH), RUNOFF(NCH)
      _RL   EINT(NCH),    ESOI(NCH),   EVEG(NCH),   ESNO(NCH),
     &     STRDG1(NCH),  STRDG2(NCH), STRDG3(NCH), STRDG4(NCH),
     &     STRDG5(NCH),  STRDG6(NCH), STRDG7(NCH), STRDG8(NCH),
     &     STRDG9(NCH),
     &      SMELT(NCH),   HLATN(NCH),  HLWUP(NCH), GDRAIN(NCH),
     &     RUNSRF(NCH),  FWSOIL(NCH)
C****
      INTEGER ChNo
      _RL     SWET(nch,NLAY),   VGPSAT(NTYPS),   VGCSAT (NTYPS),
     &       VGZDEP(NLAY,NTYPS),   VGSLOP(NTYPS),            DELTC,
     &                    DELEA,    VGPH1(NTYPS),     VGPH2(NTYPS),
     &            VGRPLN(NTYPS),    CSOIL0(NTYPS),          WSOI12,
     &             VGBEE(NTYPS),    DELZ12(NTYPS),
     &            DELZ23(NTYPS)
C****

      _RL PHLAY(nch,NLAY), AKLAY(nch,NLAY), SWET12(nch),
     &      CSOIL(nch),
     &       RCUN(nch),     VPDSTR(nch),   ESATTX(nch),
     &       VPDSTX(nch),   VGBEEX(nch)
      _RL EMAXRT(nch), VGWMAX(NLAY,NTYPS), FTEMP(nch),
     &        PHR(nch),      SOILCO(nch),    RC(nch),
     &        EAX(nch),          TX(nch),   RCX(nch),
     &     DRCDTC(nch),      SATCAP(nch),   PAR(nch),
     &       PDIR(nch),       DUMMY(nch)
      _RL FTEMPX(nch),  DRCDEA(nch), VGPSAX(nch), VGCSAX(nch),
     &                VGZDEX(NLAY,nch), VGSLOX(nch), VGPH1X(nch),
     &     VGPH2X(nch),  VGRPLX(nch)
      _RL  DEDEA(nch),  DHSDEA(nch),     EM(nch), ESATTC(nch),
     &     DESDTC(nch),      EA(nch),     RA(nch),   ALHX(nch),
     &     WETEQ1(nch), WETEQ2(nch),
     &        RX1(nch),     RX2(nch), SNWFRC(nch), POTFRC(nch),
     &     ESNFRC(nch),  EIRFRC(nch),   FCAN(nch),  EPFRC,
     &     DEFCIT, EADJST, RTBS
      _RL cmpbug

C****
      DATA VGWMAX   /8.4,    621.6,      840.0,
     2               8.4,    621.6,      840.0,
     3               8.4,    621.6,      840.0,
     4               8.4,    197.4,      420.0,
     5               8.704,  204.5,      435.2,
     6               8.4,     71.4,      420.0,
     7               4.000,   4.000,    130.56,
     8               4.000,   4.000,    130.56,
     9               8.704,   73.984,    130.56,
     1               4.000,   4.000,    130.56
     &              /
      DATA VGBEE /    4.,    4.,    4.,    4.,  1.69,    4.,
     &                4.,  1.69,  1.69, 1.69/
C      DATA VGBEE /    7.,    7.,    7.,    7.,  1.69,    7.,
C     &                7.,  1.69,  1.69, 1.69/
c      DATA VGBEE /    4.,    4.,    4.,    4.,  0.95,    4.,
c     &                4.,  0.95,  0.95, 0.95/
C      DATA VGBEE /7, 7, 7, 7, 2, 7, 7, 4, 2, 4/
      DATA VGPSAT/    -0.281,     -0.281,       -0.281,       -0.281,
     5                -0.073,     -0.281,       -0.281,       -0.073,
     9                -0.073 ,    -0.073/
C      DATA VGPSAT/    -0.086,     -0.086,       -0.086,       -0.086,
C     5                -0.073,     -0.086,       -0.086,       -0.073,
C     9                -0.073 ,    -0.073/
c      DATA VGPSAT/    -0.281,     -0.281,       -0.281,       -0.281,
c     5                -0.014,     -0.281,       -0.281,       -0.014,
c     9                -0.014 ,    -0.014/
c      DATA VGCSAT /  0.00002,  0.00002,    0.00002,    0.00002,
c     5               0.0000583,  0.00002,    0.00002,    0.0000583,
c     9               0.0000583,  0.0000583
c     &              /
      DATA VGCSAT /  0.0000012,  0.0000012,    0.0000012,    0.0000012,
     5               0.0000583,  0.0000012,    0.0000012,    0.0000583,
     9               0.0000583,  0.0000583
     &              /
C**** - - - - - - - -
C**** THE FOLLOWING 3 VARIABLES MUST MAINTAIN CONSISTENT VALUES.  ZDEP12(N) =
C****  (VGZDEP(1,N)+VGZDEP(2,N))/2. ; SIMILARLY FOR ZDEP23(N).
C****
      DATA VGZDEP /.02, 1.48, 2.00,
     2             .02, 1.48, 2.00,
     3             .02, 1.48, 2.00,
     4             .02,  .47, 1.00,
     5             .02,  .47, 1.00,
     6             .02,  .17, 1.00,
     7             .0092,  .0092,  .30,
     8             .0092,  .0092,  .30,
     9             .02,  .17,  .30,
     1             .0092,  .0092,  .30
     &            /
      DATA DELZ12 / 0.75, 0.75, 0.75, 0.245, 0.245, 0.095, 0.0092,
     8        0.0092, 0.095, 0.0092 /
      DATA DELZ23 / 1.74, 1.74, 1.74, 0.735, 0.735, 0.585, 0.155,
     8        0.155, 0.235, 0.155 /
C**** - - - - - - - -
      DATA VGSLOP /    .1736,     .1736,     .1736,     .1736,
     5                 1.000,     .1736,     .1736,     1.000,
     &                 1.000 , 1.000 /
c      DATA VGSLOP /    .1736,     .1736,     .1736,     .1736,
c     5                 .0872,     .1736,     .1736,     .0872,
c     &                 .0872 , .0872 /
c      DATA VGSLOP / 1., 1., 1.,  1., 1., 1.,  1., 1., 1.,1./
      DATA VGPH1 /     -100.,     -190.,     -200.,     -120.,
     5                 -200.,     -200.,     -200.,      -10.,
     9                  -10.,      -10. /
      DATA VGPH2 /     -500.,     -250.,     -250.,     -230.,
     5                 -400.,     -400.,     -250.,     -100.,
     9                 -100.,     -100. /
      DATA VGRPLN /0.245E+09, 0.245E+09, 0.245E+09, 0.250E+09,
     5             0.250E+09, 0.250E+09, 0.245E+09,   0.1E+09,
     9               0.1E+09, 0.100E+09 /
C****
      DATA DELTC /0.01/, DELEA /0.001/

c Note: SNWMID & CSOIL0 parameters modified from (Koster/Suarez)
c       to obtain improved albedo, radswg, and annual/diurnal cycle (L.Takacs 2/17/00)
c ------------------------------------------------------------------------------------
      DATA CSOIL0 /175000.,175000.,175000.,175000.,175000.,
     .             175000.,175000.,120000.,175000., 70000./
#include "snwmid.h"

C****
C**** ---------------------------------------------------------------------
C****
CFPP$ EXPAND (TMPFAC, RCANOP, SOIL, WUPDAT, SMFAC)
C****
C**** Expand data as specified by ITYP into arrays of size NCH.
C****
      DO 50 ChNo = 1, NCH
      BOMB(CHNO) = 0.
      VGBEEX(ChNo) = VGBEE(ITYP(ChNo))
      VGPSAX(ChNo) = VGPSAT(ITYP(ChNo))
      VGCSAX(ChNo) = VGCSAT(ITYP(ChNo))
      VGZDEX(SFCLY ,ChNo) = VGZDEP(SFCLY ,ITYP(ChNo))
      VGZDEX(ROOTLY,ChNo) = VGZDEP(ROOTLY,ITYP(ChNo))
      VGZDEX(RECHLY,ChNo) = VGZDEP(RECHLY,ITYP(ChNo))
      VGSLOX(ChNo) = VGSLOP(ITYP(ChNo))
      VGPH1X(ChNo) = VGPH1(ITYP(ChNo))
      VGPH2X(ChNo) = VGPH2(ITYP(ChNo))
      VGRPLX(ChNo) = VGRPLN(ITYP(ChNo))
  50  CONTINUE

C****
C**** Pre-process input arrays as necessary:

      DO 100 ChNo = 1, NCH

      DEDQA(CHNO)  = MAX(  DEDQA(CHNO), 500. _d 0/ALHE )
      DEDTC(CHNO)  = MAX(  DEDTC(CHNO),   0. _d 0)
      DHSDQA(CHNO) = MAX( DHSDQA(CHNO),   0. _d 0)
      DHSDTC(CHNO) = MAX( DHSDTC(CHNO), -10. _d 0)

      EM(CHNO)     = QM(CHNO) * PSUR(CHNO) / EPSILON
      EA(CHNO)     = QA(CHNO) * PSUR(CHNO) / EPSILON
      ESATTC(CHNO) = QSATTC(CHNO) * PSUR(CHNO) / EPSILON
      DESDTC(CHNO) = DQSDTC(CHNO) * PSUR(CHNO) / EPSILON
C      DEDEA(CHNO)  = DEDQA(CHNO) * EPSILON / ( PSUR(CHNO) * ALHX(CHNO) )
      DEDEA(CHNO)  = DEDQA(CHNO) * EPSILON / PSUR(CHNO)
      DHSDEA(CHNO) = DHSDQA(CHNO) * EPSILON / PSUR(CHNO)
C      DEDTC(CHNO)  = DEDTC(CHNO) / ALHX(CHNO)
C      ETURB(CHNO)  = ETURB(CHNO) / ALHX(CHNO)
      PAR(CHNO)    = (PARDIR(CHNO) + PARDIF(CHNO) + 1.E-20)
      PDIR(CHNO)   = PARDIR(CHNO) / PAR(CHNO)
      RA(CHNO)     = ONE / ( CD(CHNO) * UM(CHNO) )
      SATCAP(ChNo) = 0.1 * ZLAI(ChNo)
      CSOIL(CHNO)  = CSOIL0(ITYP(ChNo))
      SWET(ChNo,SFCLY ) = max( min(SWET1(ChNo),1. _d 0), 0. _d 0)
      SWET(ChNo,ROOTLY) = max( min(SWET2(ChNo),1. _d 0), 0. _d 0)
      SWET(ChNo,RECHLY) = max( min(SWET3(ChNo),1. _d 0), 0. _d 0)
      CAPAC(CHNO)       = max( min(CAPAC(ChNo),SATCAP(CHNO)), 0. _d 0)
C****

      SNWFRC(CHNO) = SNOW(CHNO) / ( SNOW(CHNO) + SNWMID(ITYP(CHNO)) )
      FCAN(CHNO) = MIN( 1. _d 0, MAX(0. _d 0,CAPAC(ChNo)/SATCAP(ChNo)) )
      POTFRC(CHNO)=1.-(1.-SNWFRC(CHNO))*(1.-FCAN(CHNO))


      WSOI12=SWET(ChNo,SFCLY ) * VGWMAX(SFCLY ,ITYP(ChNo)) +
     &       SWET(ChNo,ROOTLY) * VGWMAX(ROOTLY,ITYP(ChNo))
      SWET12(ChNo) = WSOI12 /
     &          (VGWMAX(SFCLY,ITYP(ChNo)) + VGWMAX(ROOTLY,ITYP(ChNo)))

      EMAXRT(CHNO) = ( SNOW(CHNO) + CAPAC(CHNO) + WSOI12 ) / DTSTEP

C****
      RUNOFF(CHNO) = 0.
      RUNSRF(CHNO) = 0.
C**** (SMELT is zeroed in FLUXES)
C      SMELT(CHNO)  = 0.
      FWSOIL(CHNO) = 0.
 100  CONTINUE

C****
C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C****       STEP 1: COMPUTE EFFECTIVE RESISTANCE RC FOR ENERGY BALANCE.
C****          (RCUNST  computes the unstressed resistance,
C****           VPDFAC  computes the vapor pressure deficit stress term,
C****           TMPFAC  computes the temperature stress term,
C****           SOIL    computes soil moisture potentials and conds.,
C****           SMFAC   computes rc given a leaf water potential stress,
C****           RSURFP  computes rc given a parallel resist. from the
C****                   surface,
C****           RCANOP  computes rc corrected for snow and interception.)
C****

      CALL RCUNST (
     I             NCH, ITYP, SUNANG, SQSCAT, PDIR,
     I             PAR, ZLAI, GREEN,
     O             RCUN
     &            )

      CALL VPDFAC (
     I             NCH,  ITYP,  ESATTC, EA,
     O             VPDSTR
     &            )

      CALL TMPFAC (
     I             NCH,  ITYP, TC,
     O             FTEMP
     &            )

      CALL SOIL (
     I           NCH, ITYP, SWET12, VGBEEX, VGPSAX, VGCSAX, DELZ12,
     O           PHR, SOILCO, DUMMY
     &          )

      cmpbug=0.

      CALL SMFAC (
     I            NCH,    ITYP,   ESATTC, EA,   PHR, SOILCO, RCUN,
     I            VPDSTR, FTEMP,  TC,     PSUR, Z2,  RSOIL1, RSOIL2,
     I            VGPH1X, VGPH2X, VGRPLX,
     O            RC
     &           )

      CALL RSURFP (
     I             NCH, UM, U2FAC, Z2, RDC, SWET(1,SFCLY),
     I             ESATTC, EA,
     U             RC,
     O             RX1, RX2
     &            )

      CALL RCANOP (
     I             NCH, CAPAC, SNOW, SATCAP, RA, ETURB, SNWFRC,
     I             POTFRC,
     U             RC
     &            )

C****
C**** -    -    -    -    -    -    -    -    -    -    -    -    -    -
C****
C**** Compute DRC/DT and DRC/DEA using temperature, v.p. perturbations:
C****

      DO 120 ChNo = 1, NCH
      TX(ChNo) = TC(ChNo) + DELTC
      ESATTX(ChNo) = ESATTC(ChNo) + DESDTC(CHNO) * DELTC
      EAX(ChNo) = EA(ChNo) + DELEA
  120 CONTINUE
C****
C**** temperature:
      CALL VPDFAC (NCH, ITYP, ESATTX, EA, VPDSTX)
      CALL TMPFAC (NCH, ITYP, TX, FTEMPX)
      CALL SMFAC (
     I            NCH,    ITYP,   ESATTX, EA,   PHR, SOILCO, RCUN,
     I            VPDSTX, FTEMPX, TX,     PSUR, Z2,  RSOIL1, RSOIL2,
     I            VGPH1X, VGPH2X, VGRPLX,
     O            RCX
     &           )
      CALL RSURFP (NCH, UM, U2FAC, Z2, RDC, SWET(1,SFCLY),
     I             ESATTX, EA,
     &             RCX,
     O             DUMMY,DUMMY
     &            )
      CALL RCANOP (NCH, CAPAC, SNOW, SATCAP, RA, ETURB, SNWFRC,
     &               POTFRC, RCX)
C****
      DO 125 ChNo = 1, NCH
      DRCDTC(ChNo) = (RCX(ChNo) - RC(ChNo)) / DELTC
  125 CONTINUE
C****
C**** vapor pressure:
      CALL VPDFAC (NCH, ITYP, ESATTC, EAX, VPDSTX)
      CALL SMFAC (
     I            NCH,    ITYP,  ESATTC, EAX,  PHR, SOILCO, RCUN,
     I            VPDSTX, FTEMP, TC,     PSUR, Z2,  RSOIL1, RSOIL2,
     I            VGPH1X, VGPH2X, VGRPLX,
     O            RCX
     &           )
      CALL RSURFP (NCH, UM, U2FAC, Z2, RDC, SWET(1,SFCLY),
     I             ESATTC, EAX,
     &             RCX,
     O             DUMMY,DUMMY
     &            )
      CALL RCANOP (NCH, CAPAC, SNOW, SATCAP, RA, ETURB, SNWFRC,
     &               POTFRC, RCX)
C****
      DO 150 ChNo = 1, NCH
      DRCDEA(ChNo) = (RCX(ChNo) - RC(ChNo)) / DELEA
  150 CONTINUE
C****

C****
C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C****       STEP 2: Solve the energy balance at the surface.
C****
C****
C**** Determine effective latent heat of vaporization based on
C**** assumed fractional coverage of snow.  This requires the
C**** determination of the fractions of moisture taken from the various
C**** reservoirs:
C****         ESNFRC=fraction of total evap. coming from snow
C****         EIRFRC=fraction of total evap. coming from interception res.

      DO 200 CHNO=1,NCH
      RTBS=RX1(CHNO)*RX2(CHNO)/(RX1(CHNO)+RX2(CHNO))
      EPFRC=POTFRC(CHNO) * ( RA(CHNO) + RTBS ) /
     &                    ( RA(CHNO) + POTFRC(CHNO)*RTBS )
      ESNFRC(CHNO)=EPFRC*SNWFRC(CHNO)/
     &                      (SNWFRC(CHNO)+FCAN(CHNO)+1.E-20)
      EIRFRC(CHNO)=EPFRC*FCAN(CHNO)/(SNWFRC(CHNO)+FCAN(CHNO)+1.E-20)
      ALHX(CHNO) = (1.-ESNFRC(CHNO))*ALHE + ESNFRC(CHNO)*ALHS
  200 CONTINUE

      CALL FLUXES (
     I                NCH,   ITYP, DTSTEP, ESATTC, DESDTC,   ALHX,
     I              ETURB,  DEDEA,  DEDTC, HSTURB, DHSDEA, DHSDTC,
     I                 RC, DRCDEA, DRCDTC,
     I              SWNET, HLWDWN, ALWRAD, BLWRAD, ESNFRC,
     I                 TM,     EM,  CSOIL,   PSUR, EMAXRT, VGWMAX,
     U                 TC,     TD,     EA,   SWET,   SNOW,
     O             RUNOFF,   EVAP, SHFLUX,  SMELT,  HLWUP, BOMB,STRDG1,
     O             STRDG2, STRDG3, STRDG4, STRDG5, STRDG6, STRDG7,
     O             STRDG8, STRDG9
     &            )
c****
C****
C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C****       STEP 3: Update the water quantities in the soil and in the
C****          interception reservoir.
C****          (WUPDAT  reduces moisture contents using calculated
C****           evap.,
C****           GWATER  computes darcian flux of water between soil
C****           layers.)
C****

      CALL WUPDAT (
     I               NCH,   ITYP, DTSTEP,
     I               EVAP, SATCAP, VGWMAX, TC, RA, RC,
     I               RX1, RX2,ESNFRC,EIRFRC,
     U               CAPAC, SNOW, SWET, RUNOFF,
     O               EINT, ESOI, EVEG, ESNO, RUNSRF,FWSOIL
     &              )

C****
C**** Correct any energy balance inconsistency.
C****
      DO 300 CHNO=1,NCH
      IF(EVAP(CHNO) .GT. 0.) THEN
        EADJST=(ESNO(CHNO)/DTSTEP) - ESNFRC(CHNO)*EVAP(CHNO)
        SHFLUX(CHNO)=SHFLUX(CHNO)-EADJST*(ALHS-ALHE)
        ENDIF
  300 CONTINUE



      CALL SOIL (
     I             NCH,          ITYP,        SWET (1, SFCLY), VGBEEX,
     I             VGPSAX,       VGCSAX,      DELZ12,
     O             PHLAY(1,SFCLY), AKLAY(1,SFCLY),  DUMMY
     &            )
      CALL SOIL (
     I             NCH,          ITYP,        SWET(1,ROOTLY), VGBEEX,
     I             VGPSAX,       VGCSAX,      DELZ12,
     O             PHLAY(1,ROOTLY), AKLAY(1,ROOTLY),   WETEQ1
     &            )
      CALL SOIL (
     I             NCH,          ITYP,        SWET(1,RECHLY), VGBEEX,
     I             VGPSAX,       VGCSAX,      DELZ23,
     O             PHLAY(1,RECHLY), AKLAY(1,RECHLY),   WETEQ2
     &            )

      CALL GWATER (
     I             NCH, ITYP, VGWMAX, PHLAY, AKLAY, TC, DTSTEP,
     I             VGZDEX, VGSLOX, WETEQ1, WETEQ2,
     I             VGPSAX, VGCSAX,
     U             SWET, RUNOFF, GDRAIN
     &            )

C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C****
C****       STEP 4: Allow precipitation to fill interception reservoir
C****          and top soil layer
C****

      CALL SOIL (
     I             NCH,          ITYP,        SWET(1,ROOTLY), VGBEEX,
     I             VGPSAX,       VGCSAX,      DELZ12,
     O             PHLAY(1,ROOTLY), AKLAY(1,ROOTLY),   WETEQ1
     &            )

      CALL INTERC (
     I             NCH, ITYP, DTSTEP, TRAINL, TRAINC, TSNOW, SATCAP,
     I             VGWMAX, CSOIL, WETEQ1,
     U             TC, CAPAC, SNOW, SWET, RUNOFF, RUNSRF,
     &             SMELT, FWSOIL
     &            )


C****
C****
C**** Process data for return to GCM:

      DO 2000 ChNo = 1, NCH
      QA(CHNO) = EA(CHNO) * EPSILON / PSUR(CHNO)
C      DEDTC(CHNO) = DEDTC(CHNO) * ALHX(CHNO)
C      ETURB(CHNO) = ETURB(CHNO) * ALHX(CHNO)
      HLATN (CHNO) = EVAP (CHNO) * ALHX(CHNO)
      SWET1(ChNo) = SWET(ChNo,SFCLY)
      SWET2(ChNo) = SWET(ChNo,ROOTLY)
      SWET3(ChNo) = SWET(ChNo,RECHLY)

      IF(SWET1(ChNo).LT.1.E-10) SWET1(CHNO) = 0.0
      IF(SWET2(ChNo).LT.1.E-10) SWET2(CHNO) = 0.0
      IF(SWET3(ChNo).LT.1.E-10) SWET3(CHNO) = 0.0
      IF(CAPAC(ChNo).LT.1.E-10) CAPAC(CHNO) = 0.0
      IF(SNOW (ChNo).LT.1.E-10) SNOW (CHNO) = 0.0
      IF(RUNOFF(ChNo).LT.1.E-10) RUNOFF(CHNO) = 0.0

      EINT(CHNO) = EINT(CHNO) * ALHE / DTSTEP
      ESOI(CHNO) = ESOI(CHNO) * ALHE / DTSTEP
      EVEG(CHNO) = EVEG(CHNO) * ALHE / DTSTEP
      ESNO(CHNO) = ESNO(CHNO) * ALHS / DTSTEP

      DEFCIT = EVAP(CHNO) * ( RC(CHNO) + RA(CHNO) )
CCOOMMSTRDG1(CHNO) = DEFCIT / RA(CHNO)
CCOOMMSTRDG2(CHNO) = DEFCIT / ( RA(CHNO) + RCUN(CHNO) )
CCOOMMSTRDG3(CHNO) = DEFCIT / ( RA(CHNO) + RCUN(CHNO)/VPDSTR(CHNO) )
CCOOMMSTRDG4(CHNO) = DEFCIT / ( RA(CHNO) +
CCOOMM                   RCUN(CHNO)/(VPDSTR(CHNO)*FTEMP(CHNO)) )

 2000 CONTINUE
C****

      RETURN
      END
C****
C**** [ END CHIP ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN INTERC ]
C****
      SUBROUTINE INTERC (
     I                   NCH, ITYP, DTSTEP, TRAINL, TRAINC,
     I                   TSNOW, SATCAP, WMAX, CSOIL, WETEQ1,
     U                   TC, CAPAC, SNOW,  SWET1,  RUNOFF, RUNSRF,
     U                   SMELT, FWSOIL
     &                  )
C****
C**** This routine uses the precipitation forcing to determine
C**** changes in interception, snowcover, and soil moisture storage.
C****
C**** Note:  In this formulation, rain that falls on frozen ground
C**** runs-off, rather than freezes.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo
      _RL  TRAINL(NCH), TRAINC(NCH), TSNOW(NCH),  SATCAP(NCH),
     &      WMAX(NLAY,NTYPS), TC(NCH),   CSOIL(NCH),   CAPAC(NCH),
     &      SNOW(NCH), SWET1(NCH),  RUNOFF(NCH),  SMELT(NCH),
     &     RUNSRF(NCH), FWSOIL(NCH), WETEQ1(NCH), WETINT
      _RL DTSTEP, SNOWM, WATADD, CAVAIL, THRUC, WRUNC, WRUNL,
     &     TIMFRL, TIMFRC, FWETL, FWETC, THRU1, THRU2, THRUL, XTCORR,
     &     WETFRC
C****
C      DATA FWET0  /0.30/
      DATA FWETL /1.00/, FWETC /0.20/, TIMFRL/1.00/, TIMFRC/0.125/
C****
C**** ------------------------------------------------------------------
C**** Loop over chips:
      DO 100 ChNo = 1, NCH
C****
C**** Add to snow cover.  Melt snow if necessary.
      SNOW(ChNo) = SNOW(ChNo) + TSNOW(ChNo)*DTSTEP
      SNOWM = 0.
      IF( SNOW(CHNO).GT.0. .AND. TC(CHNO).GT.TF ) THEN
        SNOWM = MIN( SNOW(ChNo),
     &       MAX( 0. _d 0, (TC(ChNo)-TF)*CSOIL(ChNo)/ALHM )  )
        IF( SNOWM .EQ. SNOW(CHNO) ) THEN
            TC(ChNo) = TC(ChNo) - SNOWM * ALHM / CSOIL(ChNo)
            SNOW(CHNO)=0.
          ELSE
            TC(CHNO)=TF
            SNOW(ChNo) = SNOW(ChNo) - SNOWM
          ENDIF
        SMELT(CHNO)=SMELT(CHNO)+SNOWM/DTSTEP
        ENDIF
C****
C**** =======================================================
C****
C**** Load interception reservoir.  STEP 1: Large scale condensation.
C****
C**** Determine XTCORR, the fraction of a storm that falls on a previously
C**** wet surface due to the time correlation of precipitation position.
C**** (Time scale TIMFRL for large scale storms set to one for FWETL=1
C**** to reflect the effective loss of "position memory" when storm
C**** covers entire grid square.)

        XTCORR= (1.-TIMFRL) * MIN( 1. _d 0,(CAPAC(CHNO)/SATCAP(CHNO))/
     $       FWETL )

C****
C**** Fill interception reservoir with precipitation.
C**** THRU1 is first calculated as the amount falling through the
C****    canopy under the assumption that all rain falls randomly.
C****    only a fraction 1-XTCORR falls randomly, though, so the result
C****    is multiplied by 1-XTCORR.
C****
      WATADD = TRAINL(ChNo)*DTSTEP + SMELT(CHNO)*DTSTEP
      CAVAIL = ( SATCAP(CHNO) - CAPAC(CHNO) ) * FWETL
      WETINT = CAPAC(CHNO)/SATCAP(CHNO)
      IF( WATADD*(1.-WETINT) .LT. CAVAIL ) THEN
          THRU1 = WATADD*WETINT
        ELSE
          THRU1 = (WATADD - CAVAIL)
        ENDIF
      THRU1=THRU1*(1.-XTCORR)

C**** THRU2 is the amount that falls immediately through the canopy due
C**** to 'position memory'.

      THRU2=XTCORR*WATADD

      THRUL=THRU1+THRU2
      CAPAC(CHNO)=CAPAC(CHNO)+WATADD-THRU1-THRU2
C****
C**** ---------------------------------------------------
C****
C**** STEP 2: Moist convective precipitation.
C****
C**** Determine XTCORR, the fraction of a storm that falls on a previously
C**** wet surface due to the time correlation of precipitation position.

      XTCORR= (1.-TIMFRC) * MIN( 1. _d 0,(CAPAC(CHNO)/SATCAP(CHNO))/
     $     FWETC )

C****
C**** Fill interception reservoir with precipitation.
C**** THRU1 is first calculated as the amount falling through the
C****    canopy under the assumption that all rain falls randomly.
C****    only a fraction 1-XTCORR falls randomly, though, so the result
C****    is multiplied by 1-XTCORR.
C****
      WATADD = TRAINC(ChNo)*DTSTEP
      CAVAIL = ( SATCAP(CHNO) - CAPAC(CHNO) ) * FWETC
      WETINT = CAPAC(CHNO)/SATCAP(CHNO)
      IF( WATADD*(1.-WETINT) .LT. CAVAIL ) THEN
          THRU1 = WATADD*WETINT
        ELSE
          THRU1 = (WATADD - CAVAIL)
        ENDIF
      THRU1=THRU1*(1.-XTCORR)

C**** THRU2 is the amount that falls immediately through the canopy due
C**** to 'position memory'.

      THRU2=XTCORR*WATADD

      THRUC=THRU1+THRU2
      CAPAC(CHNO)=CAPAC(CHNO)+WATADD-THRU1-THRU2
C****
C***** =================================================================
C****
C**** Add precipitation moisture to soil, generate runoff.  if
C**** temperature is below freezing, all precipitation runs off.
C****
C**** STEP 1: Large scale condensation:
C****
      IF(SWET1(CHNO).GT.WETEQ1(CHNO) .AND. WETEQ1(CHNO).NE.1.) THEN
          WETFRC=(SWET1(CHNO)-WETEQ1(CHNO))/(1.-WETEQ1(CHNO))
        ELSE
          WETFRC=0.
        ENDIF

      CAVAIL = ( 1.-WETFRC)*FWETL*WMAX(1,ITYP(ChNo))*(1-WETEQ1(CHNO))
      IF ( THRUL * (1-WETFRC) .LT. CAVAIL ) THEN
          WRUNL = THRUL * WETFRC
          SWET1(ChNo) = SWET1(ChNo)
     *    + THRUL * ( 1. - WETFRC) / WMAX(1,ITYP(ChNo))
        ELSE
          WRUNL = THRUL - CAVAIL
          SWET1(CHNO) = SWET1(CHNO) + CAVAIL / WMAX(1,ITYP(ChNo))
        ENDIF
C****

C**** STEP 2: Moist convective precipitation:
C****
      IF(SWET1(CHNO).GT.WETEQ1(CHNO) .AND. WETEQ1(CHNO).NE.1.) THEN
          WETFRC=(SWET1(CHNO)-WETEQ1(CHNO))/(1.-WETEQ1(CHNO))
        ELSE
          WETFRC=0.
        ENDIF

      CAVAIL = (1.-WETFRC)*FWETC*WMAX(1,ITYP(ChNo))*(1-WETEQ1(CHNO))
      IF ( THRUC * (1-WETFRC) .LT. CAVAIL ) THEN
          WRUNC = THRUC * WETFRC
          SWET1(ChNo) = SWET1(ChNo)
     *    + THRUC * ( 1. - WETFRC) / WMAX(1,ITYP(ChNo))
        ELSE
          WRUNC = THRUC - CAVAIL
          SWET1(CHNO) = SWET1(CHNO) + CAVAIL / WMAX(1,ITYP(ChNo))
        ENDIF
C****
      RUNOFF(ChNo) = RUNOFF(ChNo) + (WRUNC+WRUNL)/DTSTEP
      RUNSRF(ChNo) = RUNSRF(ChNo) + (WRUNC+WRUNL)/DTSTEP
      FWSOIL(CHNO) = FWSOIL(CHNO) + (THRUC+THRUL-WRUNC-WRUNL)/DTSTEP
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END INTERC ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN RCUNST ]
C****
      SUBROUTINE RCUNST (
     I                   NCH, ITYP, SUNANG, SQSCAT, PDIR,
     I                   PAR, ZLAI, GREEN,
     O                   RCUN
     &                  )
C****
C****     This subroutine calculates the unstressed canopy resistance.
C**** (p. 1353, Sellers 1985.)  Extinction coefficients are computed first.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo

      _RL  SUNANG(NCH),   PDIR(NCH),   PAR(NCH),   ZLAI(NCH),
     &      SQSCAT(NCH),  GREEN(NCH),  RCUN(NCH)

      _RL   VGCHIL(NTYPS), VGZMEW(NTYPS),
     &       VGRST1(NTYPS), VGRST2(NTYPS), VGRST3(NTYPS)

      _RL            RHO4,         EXTK1,         EXTK2,
     &               RCINV,         GAMMA,          EKAT,    DUM1,
     &                DUM2,          DUM3,            AA,      BB,
     &                  ZK,            CC


      DATA VGCHIL /        0.1,        0.25,        0.01,        -0.3,
     5                    0.01,        0.20,         0.0,         0.0,
     9                     0.0,        0.0 /

      DATA VGZMEW/      0.9809,      0.9638,      0.9980,      1.0773,
     5                  0.9980,      0.9676,       1.000,       1.000,
     9                   1.000,      1.0000 /

      DATA VGRST1 /     2335.9,      9802.2,      2869.7,      2582.0,
     5                 93989.4,      9802.2,         0.0,         0.0,
     9                     0.0 ,        0.0/

      DATA VGRST2 /        0.0,        10.6,         3.7,         1.1,
     5                    0.01,        10.6,         0.0,         0.0,
     9                     0.0 ,        0.0/

      DATA VGRST3 /      153.5,       180.0,       233.0,       110.0,
     5                   855.0,       180.0,         1.0,         1.0,
     9                     1.0,         1.0 /




      DO 100 ChNo = 1, NCH

C**** First compute optical parameters.
C**** (Note: CHIL is constrained to be >= 0.01, as in SiB calcs.)

      AA = 0.5 - (0.633 + 0.330*VGCHIL(ITYP(ChNo)))*VGCHIL(ITYP(ChNo))
      BB = 0.877 * ( ONE - 2.*AA )
      CC =  ( AA + BB*SUNANG(ChNo) ) / SUNANG(ChNo)

      EXTK1 =  CC * SQSCAT(ChNo)
      EXTK2 = (ONE / VGZMEW(ITYP(ChNo))) * SQSCAT(ChNo)

      DUM1 =      PDIR(ChNo)  *   CC
      DUM2 = (ONE-PDIR(ChNo)) * ( BB*(ONE/3.+PIE/4.) + AA*1.5 )

C**** Bound extinction coefficient by 50./ZLAI:

      ZK =     PDIR(ChNo) *MIN( EXTK1, 50. _d 0/ZLAI(ChNo) ) +
     &    (ONE-PDIR(ChNo))*MIN( EXTK2, 50. _d 0/ZLAI(ChNo) )

C**** Now compute unstressed canopy resistance:

      GAMMA = VGRST1(ITYP(ChNo)) / VGRST3(ITYP(ChNo)) +
     &        VGRST2(ITYP(ChNo))

      EKAT = EXP( ZK*ZLAI(ChNo) )
      RHO4 = GAMMA / (PAR(ChNo) * (DUM1 + DUM2))

      DUM1 = (VGRST2(ITYP(ChNo)) - GAMMA) / (GAMMA + 1.E-20)
      DUM2 = (RHO4 * EKAT + ONE) / (RHO4 + ONE)
      DUM3 = ZK * VGRST3(ITYP(ChNo))
      RCINV = ( DUM1*LOG(DUM2) + ZK*ZLAI(ChNo) ) / DUM3

      RCUN(ChNo) = ONE / (RCINV * GREEN(ChNo) + 1.E-10)

 100  CONTINUE


      RETURN
      END
C****
C**** [ END RCUNST ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN SOIL ]
C****
      SUBROUTINE SOIL (
     I         NCH, ITYP,  WET, VGBEEX, VGPSAX, VGCSAX, DELZ,
     O         PHR, SOILCO, WETEQ
     &                )
C****
C**** This subroutine returns soil moisture potential and conductivity.

      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo

      _RL    WET(NCH),    PHR(NCH), SOILCO(NCH), VGPSAX(NCH),
     &     VGCSAX(NCH), VGBEEX(NCH),  DELZ(NTYPS),  WETEQ(NCH),
     &     WEXPB,  WET0,  PHEQ

      DO 100 ChNo = 1, NCH

      WET0  = MAX(WET(CHNO),0.01 _d 0)
      WEXPB = WET0**VGBEEX(ChNo)

      PHR(ChNo)    = VGPSAX(ChNo) / WEXPB
      SOILCO(ChNo) = VGCSAX(ChNo) * WEXPB * WEXPB * WET0 * WET0 * WET0

      PHEQ = PHR(CHNO) - DELZ(ITYP(CHNO))
      WETEQ(CHNO) = ( PHEQ/VGPSAX(CHNO) ) ** ( -1/VGBEEX(CHNO) )

 100  CONTINUE

      RETURN
      END
C****
C**** [ END SOIL ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN FLUXES ]
C****
      SUBROUTINE FLUXES (
     I            NCH,   ITYP, DTSTEP, ESATTC, DESDTC,   ALHX,
     I          ETURB,  DEDEA,  DEDTC, HSTURB, DHSDEA, DHSDTC,
     I             RC, DRCDEA, DRCDTC,
     I          SWNET, HLWDWN, ALWRAD, BLWRAD, ESNFRC,
     I             TM,     EM,  CSOIL,   PSUR, EMAXRT,   WMAX,
     U             TC,     TD,     EA,  SWET1,   SNOW,
     O         RUNOFF,   EVAP, SHFLUX,  SMELT,  HLWUP, BOMB, STRDG1,
     O         STRDG2, STRDG3, STRDG4, STRDG5, STRDG6, STRDG7,
     O         STRDG8, STRDG9
     &                  )
C****
C**** This subroutine computes the fluxes of latent and sensible heat
C**** from the surface through an energy balance calculation.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo

      _RL       DTSTEP, ESATTC(NCH), DESDTC(NCH),   ALHX(NCH),
     &       ETURB(NCH),  DEDEA(NCH),  DEDTC(NCH),
     &      HSTURB(NCH), DHSDEA(NCH), DHSDTC(NCH),
     &          RC(NCH), DRCDEA(NCH), DRCDTC(NCH),
     &       SWNET(NCH), HLWDWN(NCH), ALWRAD(NCH), BLWRAD(NCH),
     &          TM(NCH),     EM(NCH),  CSOIL(NCH),   PSUR(NCH),
     &      EMAXRT(NCH),         WMAX(NLAY,NTYPS),
     &          TC(NCH),     TD(NCH),     EA(NCH), ESNFRC(NCH),
     &               SWET1(NCH,NLAY),   SNOW(NCH),
     &      RUNOFF(NCH),   EVAP(NCH), SHFLUX(NCH),  SMELT(NCH),
     &       HLWUP(NCH),   BOMB(NCH)
      _RL STRDG1(NCH),STRDG2(NCH),STRDG3(NCH),STRDG4(NCH)
      _RL STRDG5(NCH),STRDG6(NCH),STRDG7(NCH),STRDG8(NCH)
      _RL STRDG9(NCH)

      _RL     HLWTC,  CDEEPS,      Q0,  RHOAIR,   CONST,  DHLWTC,
     &        EPLANT,     A11,     A12,     A21,     A22,      F0,
     &           DEA,     DTC,  SNLEFT,     Q0X,  Q0SNOW,
     &         EANEW,  ESATNW,  EHARMN, DETERM, DENOM

      LOGICAL CHOKE
      _RL deepfac(ntyps)
      DATA deepfac /1.,1.,1.,1.,1.,1.,1.,1.,1.,1./
C****
C**** -------------------------------------------------------------------

      DO 200 ChNo = 1, NCH
C****
      HLWTC = ALWRAD(CHNO) + BLWRAD(CHNO) * TC(CHNO)
      CDEEPS = PIE * CSOIL(ChNo) * 2. / 86400.
      RHOAIR = PSUR(ChNo) * 100. / (RGAS * TC(ChNo))
      CONST = RHOAIR * EPSILON / PSUR(ChNo)
      DHLWTC = BLWRAD(CHNO)
C****
C**** Compute matrix elements A11, A22, AND Q0 (energy balance equation).
C****

      A11 = CSOIL(ChNo)/DTSTEP +
     &        DHLWTC +
     &        DHSDTC(ChNo) +
     &        ALHX(CHNO)*DEDTC(ChNo) +
     &        CDEEPS
      A12 = DHSDEA(ChNo) + ALHX(CHNO) * DEDEA(ChNo)
      Q0 =  SWNET(ChNo) +
     &        HLWDWN(ChNo) -
     &        HLWTC -
     &        HSTURB(ChNo) -
     &        ALHX(CHNO) * ETURB(ChNo) -
     &        CDEEPS * (TC(ChNo) - TD(ChNo))

          STRDG3(ChNo)=Q0/A11
          STRDG4(ChNo)=(SWNET(ChNo)+HLWDWN(ChNo)-HLWTC)/A11
          STRDG5(ChNo)=HSTURB(ChNo)/A11
          STRDG6(ChNo)=ALHX(CHNO)*ETURB(ChNo)/A11
          STRDG7(ChNo)=CDEEPS*(TC(ChNo) - TD(ChNo))/A11
          STRDG9(ChNo)=A11

C****
C**** Compute matrix elements A21, A22, and F0 (canopy water budget
C**** equation) and solve for fluxes.  Three cases are considered:
C****
C**** 1. Standard case: RC>0.
C**** 2. RC = 0.  Can only occur if CIR is full or ETURB is negative.
C****
      CHOKE = .TRUE.

      IF( RC(CHNO) .GT. 0.) THEN
          EPLANT = CONST * (ESATTC(ChNo) - EA(ChNo)) / RC(ChNo)
          IF(EPLANT*ETURB(ChNo).GE.0.) THEN
              EHARMN = 2.*EPLANT*ETURB(CHNO) / (EPLANT + ETURB(ChNo))
            ELSE
              EHARMN=0.
            ENDIF
C****
C****            Some limitations to A21 and A22 are applied:
C****            we assume that the increase in plant evaporation
C****            due to an increase in either TC or EA balances
C****            or outweighs any decrease due to RC changes.
C****

          A21 =  -DEDTC(ChNo)*RC(ChNo) +
     &      max(0. _d 0, CONST*DESDTC(ChNo) - EHARMN*DRCDTC(ChNo) )
          A22 = -( RC(ChNo)*DEDEA(ChNo) +
     &               max( 0. _d 0, CONST + EHARMN*DRCDEA(ChNo) )   )

          F0 = RC(ChNo) * (ETURB(ChNo) - EPLANT)
          DETERM = MIN( A12*A21/(A11*A22) - 1., -0.1 _d 0)
          DEA = ( Q0*A21 - A11*F0 ) / ( DETERM * A11*A22 )
          DTC = ( Q0 - A12*DEA ) / A11

          STRDG1(ChNo)=DTC
          STRDG2(ChNo)=DEA
          STRDG8(ChNo)=-A12*DEA/A11

          EVAP(ChNo) = ETURB(ChNo) + DEDEA(ChNo)*DEA + DEDTC(ChNo)*DTC
          SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC
          DENOM = DETERM * A11*A22
        ELSE
          CHOKE = .FALSE.
          A21 = -DESDTC(ChNo)
          A22 = 1.
          F0 = ESATTC(ChNo) - EA(ChNo)
          DEA = ( Q0*A21 - A11*F0 ) / ( A12*A21 - A11*A22 )
          DTC = ( Q0 - A12*DEA ) / A11

          STRDG1(ChNo)=DTC
          STRDG2(ChNo)=DEA
          STRDG8(ChNo)=-A12*DEA/A11

          EVAP(ChNo) = ETURB(ChNo) + DEDEA(ChNo)*DEA + DEDTC(ChNo)*DTC
          SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC
          DENOM = A12 * A21 - A11*A22
        ENDIF


C**** - - - - - - - - - - - - - - - - - - - -
C**** Account for snowmelt, if necessary.
C**** - - - - - - - - - - - - - - - - - - - -
      SMELT(CHNO)=0.
      SNLEFT=SNOW(CHNO)-EVAP(CHNO)*DTSTEP*ESNFRC(CHNO)
      IF(TC(CHNO)+DTC .GT. TF .AND. SNLEFT.GT.0.) THEN

C****      First re-calculate energy balance under assumption that all
C****      snow is melted.

        Q0X=Q0-ALHM*SNLEFT/DTSTEP
        SMELT(CHNO)=SNLEFT/DTSTEP

            DEA = ( Q0X*A21 - A11*F0 ) / DENOM
            DTC = ( Q0X - A12*DEA ) / A11

C****      If TC+DTC is now less than TF, too much snow has melted.  Now
C****      solve for balance assuming only some of the snow has melted.
C****      Set TC to TF.

        IF(TC(CHNO)+DTC .LT. TF) THEN
          DTC=TF-TC(CHNO)
          DEA=(F0-A21*DTC)/A22
          Q0SNOW=A11*DTC+A12*DEA
          SMELT(CHNO)=(Q0-Q0SNOW)/ALHM
          ENDIF

          STRDG1(ChNo)=DTC
          STRDG2(ChNo)=DEA
          STRDG8(ChNo)=-A12*DEA/A11

        EVAP(ChNo) = ETURB(ChNo) + DEDEA(ChNo)*DEA +
     &                   DEDTC(ChNo)*DTC
        SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC

        ENDIF

C**** - - - - - - - - - - - - - - - - - - - - - - -
C**** Adjustments

C**** 1. Adjust deltas and fluxes if all available water evaporates
C****    during time step:
C****
      IF( EVAP(CHNO) .GT. EMAXRT(CHNO) ) THEN
        CHOKE = .FALSE.
        DEA = EM(CHNO) - EA(CHNO)
        DTC =
     &   (Q0 + ALHX(CHNO)*(ETURB(ChNo)-EMAXRT(CHNO)) - DHSDEA(CHNO)*DEA)
     &          /  ( A11 - ALHX(CHNO)*DEDTC(ChNo) )

          STRDG1(ChNo)=DTC
          STRDG2(ChNo)=DEA
          STRDG8(ChNo)=0.

        EVAP(CHNO) = EMAXRT(CHNO)
        SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC
        SMELT(CHNO)=0.
        ENDIF
C****
C**** Adjust DEA and DTC if solutions were pathological:
C****
      ESATNW = ESATTC(CHNO)+DESDTC(CHNO)*DTC
      EANEW = EA(CHNO) + DEA



C**** 2. Pathological cases.

C**** Case 1: EVAP is positive in presence of negative gradient.
C**** Case 2: EVAP and ETURB have opposite sign, implying that
C**** "virtual effects" derivatives are meaningless and thus that we
C**** do not know the proper tendency terms.
C**** In both cases, assume zero evaporation for the time step.

C      IF( ( EVAP(CHNO) .LT. 0. .AND. EM(CHNO).LT.ESATNW )
C     &    .OR. ( EVAP(CHNO)*ETURB(CHNO) .LT. 0. ) )  THEN
      IF( EVAP(CHNO) .LT. 0. .AND. EM(CHNO).LT.ESATNW )  THEN
        CHOKE = .FALSE.
        DEA = EM(CHNO) - EA(CHNO)
        DTC = ( Q0 + ALHX(CHNO)*ETURB(ChNo) - DHSDEA(CHNO)*DEA ) /
     &            ( A11 - ALHX(CHNO)*DEDTC(ChNo) )

          STRDG1(ChNo)=DTC
          STRDG2(ChNo)=DEA
          STRDG8(ChNo)=0.

        EVAP(CHNO) = 0.
        SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC

        IF(TC(CHNO)+DTC .GT. TF .AND. SNOW(CHNO).GT.0.) THEN
          Q0X=Q0-ALHM*SNOW(CHNO)/DTSTEP
          SMELT(CHNO)=SNOW(CHNO)/DTSTEP
          DTC = ( Q0X + ALHX(CHNO)*ETURB(ChNo) - DHSDEA(CHNO)*DEA ) /
     &              ( A11 - ALHX(CHNO)*DEDTC(ChNo) )
          IF(TC(CHNO)+DTC .LT. TF) THEN
            DTC=TF-TC(CHNO)
            Q0SNOW=A11*DTC+A12*DEA
            SMELT(CHNO)=(Q0-Q0SNOW)/ALHM
            ENDIF
          SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC
          ENDIF
        ENDIF



C**** 3. Exceesive dea change: apply "choke".

      IF( CHOKE .AND. ABS(DEA) .GT. 0.5*EA(CHNO) ) THEN
        DEA = SIGN(.5*EA(CHNO),DEA)
        DTC = ( Q0 - A12*DEA ) / A11

          STRDG1(ChNo)=DTC
          STRDG2(ChNo)=DEA
          STRDG8(ChNo)=-A12*DEA/A11

        EVAP(ChNo) = ETURB(ChNo) + DEDEA(ChNo)*DEA + DEDTC(ChNo)*DTC
        SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC
        IF(TC(CHNO)+DTC .GT. TF .AND. SNOW(CHNO).GT.0.) THEN
          SNLEFT=SNOW(CHNO)-EVAP(CHNO)*DTSTEP*ESNFRC(CHNO)
          Q0X=Q0-ALHM*SNLEFT/DTSTEP
          SMELT(CHNO)=SNLEFT/DTSTEP
          DTC = ( Q0X - A12*DEA ) / A11
          IF(TC(CHNO)+DTC .LT. TF) THEN
            DTC=TF-TC(CHNO)
            Q0SNOW=A11*DTC+A12*DEA
            SMELT(CHNO)=(Q0-Q0SNOW)/ALHM
            ENDIF
          EVAP(ChNo) = ETURB(ChNo) + DEDEA(ChNo)*DEA + DEDTC(ChNo)*DTC
          SHFLUX(ChNo) = HSTURB(ChNo) + DHSDEA(ChNo)*DEA +
     &                                            DHSDTC(ChNo)*DTC
          ENDIF
        ENDIF

C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      TC(ChNo) = TC(ChNo) + DTC
      EA(ChNo) = EA(ChNo) + DEA
      TD(ChNo) = TD(ChNo) +
c  CHANGED THIS: deep layer 2 times deeper
     &     DTSTEP * CDEEPS * (TC(ChNo) - TD(ChNo)) /
     .       (CSOIL(ChNo)*67.73*deepfac(ityp(ChNo)))
      HLWUP(CHNO) = HLWTC + DHLWTC*DTC

      SNOW(CHNO)=SNOW(CHNO)-SMELT(CHNO)*DTSTEP

C**** Make sure EA remains positive

      EA(CHNO) = MAX(EA(CHNO), 0.0 _d 0)

  200 CONTINUE



      RETURN
      END
C****
C**** [ END FLUXES ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN VPDFAC ]
C****
      SUBROUTINE VPDFAC (
     I                   NCH, ITYP, ESATTC, EA,
     O                   VPDSTR
     &                  )
C****
C**** This subroutine computes the vapor pressure deficit stress.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo
      _RL    ESATTC(NCH), EA(NCH),  VPDSTR(NCH)
c     _RL  VGDFAC(NTYPS)
C****
c     DATA VGDFAC /   .0273,    .0357,    .0310,    .0238,
c    5                .0275,    .0275,       0.,       0.,
c    9                   0.,      0. /
C****
C**** -----------------------------------------------------------------

      DO 100 ChNo = 1, NCH
C****
c      VPDSTR(ChNo) = 1. - (ESATTC(ChNo)-EA(ChNo)) * VGDFAC(ITYP(ChNo))
c      VPDSTR (ChNo) = MIN( 1. _d 0, MAX( VPDSTR(ChNo), 1. _d -10 ) )
      VPDSTR(CHNO) = 1.
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END VPDFAC ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN TMPFAC ]
C****
      SUBROUTINE TMPFAC (
     I                   NCH,  ITYP, TC,
     O                   FTEMP
     &                  )
C****
C**** Compute temperature stress factor.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo, TypPtr
      _RL TC(NCH), FTEMP(NCH)
      _RL    VGTLL(MemFac*NTYPS),    VGTU(MemFac*NTYPS),
     &       VGTCF1(MemFac*NTYPS),  VGTCF2(MemFac*NTYPS),
     &       VGTCF3(MemFac*NTYPS)
C****
      DATA VGTLL /MemFac*273., MemFac*273., MemFac*268., MemFac*283.,
     5            MemFac*283., MemFac*273., MemFac*  0., MemFac*  0.,
     9            MemFac*  0., MemFac*  0. /
      DATA VGTU /MemFac*318., MemFac*318., MemFac*313., MemFac*328.,
     5           MemFac*323., MemFac*323., MemFac*  0., MemFac*  0.,
     9           MemFac*  0. , MemFac*  0./
      DATA VGTCF1 / MemFac*-1.43549E-06,  MemFac*-6.83584E-07,
     3              MemFac* 1.67699E-07,  MemFac*-1.43465E-06,
     5              MemFac*-2.76097E-06,  MemFac*-1.58094E-07,
     7                       MemFac* 0.,           MemFac* 0.,
     9                       MemFac* 0. , MemFac*  0./
      DATA VGTCF2 / MemFac* 7.95859E-04,  MemFac* 3.72064E-04,
     3              MemFac*-7.65944E-05,  MemFac* 8.24060E-04,
     5              MemFac* 1.57617E-03,  MemFac* 8.44847E-05,
     7                       MemFac* 0.,           MemFac* 0.,
     9                       MemFac* 0. , MemFac*  0./
      DATA VGTCF3 / MemFac*-1.11575E-01,  MemFac*-5.21533E-02,
     3              MemFac* 6.14960E-03,  MemFac*-1.19602E-01,
     5              MemFac*-2.26109E-01,  MemFac*-1.27272E-02,
     7                       MemFac* 0.,           MemFac* 0.,
     9                       MemFac* 0. , MemFac*  0./
C****
C**** ----------------------------------------------------------------

      DO 100 ChNo = 1, NCH
C****
      TypPtr = MOD(ChNo,MemFac) + (ITYP(ChNo)-1)*MemFac + 1
      FTEMP(ChNo) = (TC(ChNo) - VGTLL(TypPtr)) *
     &              (TC(ChNo) - VGTU(TypPtr)) *
     &                    ( VGTCF1(TypPtr)*TC(ChNo)*TC(ChNo) +
     &                      VGTCF2(TypPtr)*TC(ChNo) +
     &                      VGTCF3(TypPtr) )
      IF ( TC(ChNo) .LE. VGTLL(TypPtr) .OR. TC(ChNo) .GE. VGTU(TypPtr) )
     &      FTEMP (ChNo) = 1.E-10
      FTEMP(CHNO) = MIN( 1. _d 0, MAX( FTEMP(ChNo), 1. _d -10 ) )
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END TMPFAC ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN SMFAC ]
C****
      SUBROUTINE SMFAC (
     I                  NCH,    ITYP,   ESATTC, EA, PHR,  SOILCO,
     I                  RCUN,   VPDSTR, FTEMP,  TC, PSUR, Z2,
     I                  RSOIL1, RSOIL2, VGPH1X, VGPH2X, VGRPLX,
     O                  RC
     &                 )
C****
C**** This subroutine estimates RC after computing the
C**** leaf water potential stress.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo
      _RL  ESATTC(NCH),      EA(NCH),     PHR(NCH),  SOILCO(NCH),
     &        RCUN(NCH),  VPDSTR(NCH),   FTEMP(NCH),      TC(NCH),
     &        PSUR(NCH),      Z2(NCH),  RSOIL1(NCH),  RSOIL2(NCH),
     &      VGPH1X(NCH),  VGPH2X(NCH),  VGRPLX(NCH),      RC(NCH)
      _RL   RCUNTD,  RHOAIR,   CONST,     DEF,     D12,     DR2,
     &        RSOIL,      R0,    EEST,  RSTFAC
C****
C**** -----------------------------------------------------------------

      DO 100 ChNo = 1, NCH
C****
      RCUNTD = RCUN(ChNo) / ( VPDSTR(ChNo)*FTEMP(ChNo) )
      RHOAIR = PSUR(ChNo) * 100. / ( RGAS*TC(ChNo) )

C****
      CONST = RHOAIR * EPSILON / PSUR(ChNo)
      DEF = ( ESATTC(ChNo) - EA(ChNo) ) * CONST
      D12 = VGPH1X(ChNo) - VGPH2X(ChNo)
      DR2 = PHR(ChNo) - Z2(ChNo) - VGPH2X(ChNo)
      RSOIL = RSOIL1(ChNo) + RSOIL2(ChNo)/SOILCO(ChNo)
      R0 = ( VGRPLX(ChNo) + RSOIL ) / RHOW
      EEST = DEF*DR2 / ( RCUNTD*D12 + DEF*R0 )
      RSTFAC = ( DR2 - R0*EEST ) / D12
      RSTFAC = MIN( 1. _d 0, MAX( 0.001 _d 0, RSTFAC ) )
      RC(ChNo) = RCUNTD / RSTFAC
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END SMFAC ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN RSURFP ]
C****
      SUBROUTINE RSURFP (
     I                   NCH, UM, U2FAC, Z2, RDC, WET, ESATTC, EA,
     U                   RC,
     O                   RX1, RX2
     &                  )
C****
      IMPLICIT NONE
      INTEGER NCH
      INTEGER ChNo
      _RL     UM(NCH),   U2FAC(NCH),     Z2(NCH),    RDC(NCH),
     &        WET(NCH),  ESATTC(NCH),     EA(NCH),
     &         RC(NCH),     RX1(NCH),    RX2(NCH)
      _RL  U2, RSURF, HESAT
C****
C**** -----------------------------------------------------------------

      DO 100 ChNo = 1, NCH
C****
      U2 = UM(ChNo) * U2FAC(ChNo)
c      RSURF = RDC(ChNo) / U2 + 30. / (1.E-20 + WET(ChNo))
      RSURF = RDC(ChNo) / U2 + 26. + 6. / (1.E-20 + WET(ChNo))**2

C**** Account for subsaturated humidity at soil surface:
C****
      HESAT = ESATTC(CHNO) * MIN( 1. _d 0, WET(CHNO)*2. _d 0)
      IF( EA(CHNO) .LT. HESAT ) THEN
          RSURF=RSURF*( 1. + (ESATTC(CHNO)-HESAT)/(HESAT-EA(CHNO)) )
        ELSE
          RSURF=1.E10
        ENDIF


      RX1(CHNO)=RC(CHNO)
      RX2(CHNO)=RSURF

      RC(ChNo) = RC(CHNO) * RSURF / ( RC(ChNo) + RSURF )
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END RSURFP ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN RCANOP ]
C****
      SUBROUTINE RCANOP (
     I                   NCH, CAPAC, SNOW, SATCAP, RA, ETURB, SNWFRC,
     I                   POTFRC,
     U                   RC
     &                  )
C****
C**** The effective latent heat resistance RC depends on the quantity
C**** of interception reservoir water and the snow cover.  POTFRC
C**** is the fraction of the tile from which potential evaporation
C**** occurs.
C****
      IMPLICIT NONE
      INTEGER NCH
      INTEGER ChNo
      _RL CAPAC(NCH), SNOW(NCH), SATCAP(NCH), RA(NCH), ETURB(NCH),
     &          RC(NCH), SNWFRC(NCH), POTFRC(NCH)
      _RL ETCRIT,RAMPFC

C**** (Note: ETCRIT arbitrarily set to ~-5 W/m2, or -2.e-6 mm/sec.)
      DATA ETCRIT/ -2.E-6 /
C****
C**** -----------------------------------------------------------------

      DO 100 ChNo = 1, NCH

C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C**** Case 1: Vegetation present (SATCAP GT .001).  Potential evap. from
c**** both interception reservoir and snow.

      IF(SATCAP(CHNO).GT..001) THEN
        RC(ChNo)=RC(ChNo)*(1.-POTFRC(CHNO))/
     &                ( 1.+POTFRC(CHNO)*RC(ChNo)/RA(ChNo) )
        ENDIF

C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C**** Case 2: Vegetation absent (SATCAP LE .001).  Potential evap. from
c**** snow only.

      IF(SATCAP(CHNO) .LE. .001) THEN
        RC(ChNo)=RC(ChNo)*(1.-SNWFRC(CHNO))/
     &                  ( 1.+SNWFRC(CHNO)*RC(ChNo)/RA(ChNo) )
        ENDIF

C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C**** Assume RC=0 for condensation (dew).
C**** RAMPFC is used to ensure continuity in RC.

      RAMPFC=ETURB(CHNO)/ETCRIT
      IF ( RAMPFC .GE. 0. ) RC(CHNO) = RC(CHNO)*(1.-RAMPFC)
      IF ( RAMPFC .GT. 1. ) RC(ChNo) = 0.
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END RCANOP ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C***** [ BEGIN WUPDAT ]
C****
      SUBROUTINE WUPDAT (
     I                     NCH,   ITYP, DTSTEP,
     I                     EVAP, SATCAP, VGWMAX, TC, RA, RC,
     I                     RX1, RX2,ESNFRC,EIRFRC,
     U                     CAPAC, SNOW, SWET, RUNOFF,
     O                     EINT, ESOI, EVEG, ESNO, RUNSRF, FWSOIL
     &                    )
C****
C**** This subroutine allows evapotranspiration to adjust the water
C**** contents of the interception reservoir and the soil layers.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo
      _RL    EVAP(NCH),  SATCAP(NCH),  VGWMAX(NLAY,NTYPS),
     &          TC(NCH),      RA(NCH),             RC(NCH),
     &       CAPAC(NCH),    SNOW(NCH),    SWET(nch,NLAY),
     &      RUNOFF(NCH),     RX1(NCH),
     &         RX2(NCH),  RUNSRF(NCH),  FWSOIL(NCH),
     &      ESNFRC(NCH),  EIRFRC(NCH)
      _RL EINT(NCH), ESOI(NCH), EVEG(NCH), ESNO(NCH)
      _RL  DTSTEP, EGRO, FWS, THRU, DEWRUN,
     &     WTOTAL,WLAY1,WLAY2,ELAY1,ELAY2,EGROI
C****
C**** -----------------------------------------------------------------

      DO 100 ChNo = 1, NCH
C****
C**** Partition evap between interception, snow, and ground reservoirs.
C****
      WLAY1 = SWET(ChNo,SFCLY) * VGWMAX(SFCLY,ITYP(ChNo))
      WLAY2 = SWET(ChNo,ROOTLY) * VGWMAX(ROOTLY,ITYP(ChNo))
      WTOTAL = WLAY1 + WLAY2
C****
      ESNO(CHNO)=ESNFRC(CHNO)*EVAP(CHNO)*DTSTEP
      EINT(CHNO)=EIRFRC(CHNO)*EVAP(CHNO)*DTSTEP
      EGRO = EVAP(CHNO)*DTSTEP - ESNO(CHNO) - EINT(CHNO)

C**** Ensure that individual capacities are not exceeded.

      IF(ESNO(CHNO) .GT. SNOW(CHNO)) THEN
        EINT(CHNO)=EINT(CHNO)+(ESNO(CHNO)-SNOW(CHNO))
        ESNO(CHNO)=SNOW(CHNO)
        ENDIF
      EGROI=EGRO+EINT(CHNO)
      IF(EGROI .GT. CAPAC(CHNO)+WTOTAL) THEN
        ESNO(CHNO)=ESNO(CHNO)+EGROI-(CAPAC(CHNO)+WTOTAL)
        EGROI=CAPAC(CHNO)+WTOTAL
        ENDIF

      EINT(CHNO)=EGROI-EGRO
      IF(EINT(CHNO) .GT. CAPAC(CHNO)) THEN
        EGRO=EGRO+EINT(CHNO)-CAPAC(CHNO)
        EINT(CHNO)=CAPAC(CHNO)
        ENDIF
      IF(EGRO .GT. WTOTAL) THEN
        EINT(CHNO)=EINT(CHNO)+EGRO-WTOTAL
        EGRO=WTOTAL
        ENDIF
C****
C**** Separate egro into surface-evaporation/transpiration components:
C****
      IF( RX1(CHNO)+RX2(CHNO) .NE. 0. ) THEN
          ESOI(CHNO)=EGRO*RX1(CHNO)/(RX1(CHNO)+RX2(CHNO))
          EVEG(CHNO)=EGRO - ESOI(CHNO)
        ELSE
          ESOI(CHNO)=EGRO/2.
          EVEG(CHNO)=EGRO/2.
        ENDIF
C****
C**** Translate ESOI and EVEG into evaporation fluxes from layers 1 and 2:
C****
      IF( WTOTAL .GT. 0. ) THEN
          FWS = EVEG(CHNO) / WTOTAL
        ELSE
          FWS = 1.
        ENDIF
      ELAY1 = WLAY1*FWS + ESOI(CHNO)
      ELAY2 = WLAY2*FWS
C****
C**** Ensure that enough soil water is available in each layer:
C****
      IF( ELAY1 .GT. WLAY1 ) THEN
        ELAY2 = ELAY2 + (ELAY1 - WLAY1)
        ELAY1 = WLAY1
        ENDIF
      IF( ELAY2 .GT. WLAY2 ) THEN
        ELAY1 = ELAY1 + (ELAY2 - WLAY2)
        ELAY2 = WLAY2
        ENDIF

C**** Special case for condensation:
      IF(EVAP(CHNO) .LT. 0.) THEN
          EINT(CHNO)=(1.-ESNFRC(CHNO))*EVAP(CHNO)*DTSTEP
          ELAY1=0.
          ELAY2=0.
          ESOI(CHNO)=0.
          EVEG(CHNO)=0.
          ESNO(CHNO)=ESNFRC(CHNO)*EVAP(CHNO)*DTSTEP
        ENDIF

C****
C**** Remove moisture from reservoirs:
C****
      SNOW(ChNo) = SNOW(ChNo) - ESNO(CHNO)
      CAPAC(ChNo) = CAPAC(ChNo) - EINT(CHNO)
      SWET(ChNo,SFCLY) = (WLAY1 - ELAY1) / VGWMAX(SFCLY,ITYP(ChNo))
      SWET(ChNo,ROOTLY) = (WLAY2 - ELAY2) / VGWMAX(ROOTLY,ITYP(CHNO))
C****
C**** Ensure against numerical precision problems:
      SWET(ChNo,SFCLY)  = MIN( 1. _d 0, MAX( 0. _d 0, SWET(ChNo,SFCLY) )
     $     )
      SWET(ChNo,ROOTLY) = MIN( 1. _d 0, MAX( 0. _d 0, SWET(ChNo,ROOTLY)
     $     ) )
C****
C****
C**** -------------------------------------------------
C**** DEWFALL:
C****
C**** If dewfall adds to cir, insure that it does not fill
C**** beyond capacity.  If resulting throughfall adds to top soil layer,
C**** insure that it also does not fill beyond capacity.
C****
      IF( CAPAC(ChNo) .GT. SATCAP(ChNo) ) THEN
        THRU = CAPAC(ChNo) - SATCAP(ChNo)
        CAPAC(ChNo) = SATCAP(ChNo)
        SWET(ChNo,SFCLY) = SWET(ChNo,SFCLY) +
     &                         THRU / VGWMAX(SFCLY,ITYP(ChNo))
        DEWRUN=0.
        IF ( SWET(ChNo,SFCLY) .GT. 1. ) THEN
          DEWRUN = ( SWET(ChNo,SFCLY) - 1. ) * VGWMAX(SFCLY,ITYP(ChNo))
          SWET(ChNo,SFCLY) = 1.
          RUNOFF(ChNo) = RUNOFF(ChNo) + DEWRUN/DTSTEP
          RUNSRF(ChNo) = RUNSRF(ChNo) + DEWRUN/DTSTEP
          ENDIF
        FWSOIL(CHNO) = FWSOIL(CHNO) + (THRU-DEWRUN)/DTSTEP
        ENDIF
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END WUPDAT ]
C****
C**** -----------------------------------------------------------------
C**** /////////////////////////////////////////////////////////////////
C**** -----------------------------------------------------------------
C****
C**** [ BEGIN GWATER ]
C****
      SUBROUTINE GWATER (
     I                     NCH,    ITYP,   WSMAX,  PHLAY, AKLAY, TC,
     I                     DTSTEP, VGZDEX, VGSLOX, WETEQ1, WETEQ2,
     I                     PHSAT,   AKSAT,
     U                     SWET,   RUNOFF,  GDRAIN
     &                    )
C****
C**** This subroutine computes diffusion between soil layers.
C****
      IMPLICIT NONE
C****
#include "sibber.h"
C****
      INTEGER NCH
      INTEGER ITYP(NCH), ChNo
      _RL  VGSLOX(NCH), RUNOFF(NCH), GDRAIN(NCH)
      _RL  ZDEP12,   AKAVE,  GWFLUX,  ZDEP23,  HALFMX,  DHDZ,
     &    FAREA, TFM2, FRAMP

      _RL   WSMAX(NLAY,NTYPS),   PHLAY(nch,NLAY),
     &       AKLAY(nch,NLAY),             TC(NCH),
     &                  DTSTEP,    SWET(nch,NLAY),
     &      VGZDEX(NLAY,nch),  WETEQ1(NCH),  WETEQ2(NCH),
     &      PHSAT(NCH), AKSAT(NCH)

C****
C**** ----------------------------------------------------------------

      DO 100 ChNo = 1, NCH
C****
C**** Diffusion between layer 1 and 2:
      ZDEP12 = VGZDEX(SFCLY,ChNo) + VGZDEX(ROOTLY,ChNo)
      IF( SWET(CHNO,SFCLY) .GT. WETEQ1(CHNO) ) THEN
          FAREA=(SWET(CHNO,SFCLY) - WETEQ1(CHNO)) / (1. - WETEQ1(CHNO))
          DHDZ = 1. + 2.*(PHSAT(CHNO)-PHLAY(CHNO,ROOTLY))/ZDEP12
          AKAVE = AKSAT(CHNO)
        ELSE
          FAREA = 1.
          DHDZ = 1. + 2.*(PHLAY(ChNo,SFCLY)-PHLAY(ChNo,ROOTLY))/ZDEP12
          AKAVE = AKLAY(ChNo,ROOTLY)
        ENDIF
C****
      GWFLUX = 1000. * DTSTEP * AKAVE * DHDZ * FAREA
C****
C**** Test for limits on water holding capacity.
C****
      HALFMX=0.5*ABS( SWET(CHNO,SFCLY)-WETEQ1(CHNO) )
     &                               * WSMAX(SFCLY,ITYP(CHNO))
      IF (GWFLUX .GE. 0.) THEN
          GWFLUX = MIN( GWFLUX,
     &               SWET(ChNo,SFCLY) * WSMAX(SFCLY,ITYP(ChNo)) )
          GWFLUX = MIN( GWFLUX,    WSMAX(ROOTLY,ITYP(ChNo)) -
     &              SWET(ChNo,ROOTLY) * WSMAX(ROOTLY,ITYP(ChNo)) )
          GWFLUX = MIN( GWFLUX, HALFMX )
        ELSE
          GWFLUX = -MIN( ABS(GWFLUX),
     &                SWET(ChNo,ROOTLY) * WSMAX(ROOTLY,ITYP(ChNo)) )
          GWFLUX = -MIN( ABS(GWFLUX),   WSMAX(SFCLY,ITYP(ChNo)) -
     &                SWET(ChNo,SFCLY) * WSMAX(SFCLY,ITYP(ChNo)) )
          GWFLUX = -MIN( ABS(GWFLUX),  HALFMX )
        ENDIF
C****
C**** Prevent diffusion when ground is frozen (INCLUDE RAMPING):
      TFM2=TF-2.
      FRAMP=(TC(CHNO)-TFM2)/2.
      FRAMP=MIN(1. _d 0, MAX(0. _d 0,FRAMP) )
      GWFLUX=GWFLUX*FRAMP
C****
C**** Update water contents
      SWET(ChNo,SFCLY) = SWET(ChNo,SFCLY) -
     &                        GWFLUX / WSMAX(SFCLY,ITYP(ChNo))
      SWET(ChNo,ROOTLY) = SWET(ChNo,ROOTLY) +
     &                        GWFLUX / WSMAX(ROOTLY,ITYP(ChNo))
C****
C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C**** Diffusion between root and recharge layers:
      ZDEP23 = VGZDEX(ROOTLY,ChNo) + VGZDEX(RECHLY,ChNo)
      DHDZ=1. + 2. * (PHLAY(ChNo,ROOTLY) - PHLAY(ChNo,RECHLY)) / ZDEP23
      IF(DHDZ.GE.0.) THEN
          AKAVE = AKLAY(ChNo,ROOTLY)
        ELSE
          AKAVE = AKLAY(ChNo,RECHLY)
        ENDIF
      GWFLUX = 1000. * DTSTEP * AKAVE * DHDZ
C****
C**** Test for limits on water holding capacity:
      HALFMX=0.5*ABS( SWET(CHNO,ROOTLY)-WETEQ2(CHNO) )
     &                               * WSMAX(ROOTLY,ITYP(CHNO))
      IF (GWFLUX .GE. 0.) THEN
          GWFLUX = MIN( GWFLUX,
     &                    SWET(ChNo,ROOTLY) * WSMAX(ROOTLY,ITYP(ChNo)) )
          GWFLUX = MIN( GWFLUX,    WSMAX(RECHLY,ITYP(ChNo)) -
     &                    SWET(ChNo,RECHLY) * WSMAX(RECHLY,ITYP(ChNo)) )
          GWFLUX = MIN( GWFLUX, HALFMX )
        ELSE
          GWFLUX = -MIN( ABS(GWFLUX),
     &                    SWET(ChNo,RECHLY) * WSMAX(RECHLY,ITYP(ChNo)) )
          GWFLUX = -MIN( ABS(GWFLUX), WSMAX(ROOTLY,ITYP(ChNo)) -
     &                    SWET(ChNo,ROOTLY) * WSMAX(ROOTLY,ITYP(ChNo)) )
          GWFLUX = -MIN( ABS(GWFLUX), HALFMX )
        ENDIF
C****
C**** Prevent diffusion when ground is frozen (INCLUDE RAMPING):
      FRAMP=(TC(CHNO)-TFM2)/2.
      FRAMP=MIN(1. _d 0, MAX(0. _d 0,FRAMP) )
      GWFLUX=GWFLUX*FRAMP
C****
C**** Update water contents
      SWET(ChNo,ROOTLY) = SWET(ChNo,ROOTLY) -
     &                            GWFLUX / WSMAX(ROOTLY,ITYP(ChNo))
      SWET(ChNo,RECHLY) = SWET(ChNo,RECHLY) +
     &                            GWFLUX / WSMAX(RECHLY,ITYP(ChNo))
      GDRAIN(CHNO)=GWFLUX/DTSTEP
C****
C**** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C**** Flux of moisture out of lower soil layer to water table
C**** (approximation to SiB)
C****
      GWFLUX = VGSLOX(ChNo) * AKLAY(ChNo,RECHLY) * 1000. * DTSTEP

C**** Prevent diffusion when ground is frozen (INCLUDE RAMPING):
      FRAMP=(TC(CHNO)-TFM2)/2.
      FRAMP=MIN(1. _d 0, MAX(0. _d 0,FRAMP) )
      GWFLUX=GWFLUX*FRAMP

      GWFLUX = MIN( GWFLUX,
     &                 SWET(ChNo,RECHLY) * WSMAX(RECHLY,ITYP(ChNo)) )
      SWET(ChNo,RECHLY) = SWET(ChNo,RECHLY) -
     &                                GWFLUX / WSMAX(RECHLY,ITYP(ChNo))
C****
      RUNOFF (ChNo) = RUNOFF (ChNo) + GWFLUX/DTSTEP
C****
 100  CONTINUE
C****
      RETURN
      END
C****
C**** [ END GWATER ]

