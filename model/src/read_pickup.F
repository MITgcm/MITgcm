#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_PICKUP
C     !INTERFACE:
      SUBROUTINE READ_PICKUP(
     I                        myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine for IO to read restart (or
C     "pickup" or "checkpoint" ) files.  It calls routines from other
C     packages (\textit{eg.} rw and mnc) to do the per-variable
C     reads.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
#include "DYNVARS.h"
#include "NH_VARS.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#if defined(ALLOW_EDDYPSI) && defined(ALLOW_GMREDI)
# include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myIter :: Iteration number
C     myThid :: my Thread Id. number
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     fp          :: pickup-file precision
C     fn          :: Temp. for building file name.
C     suff        :: suffix of pickup file to read
C     filePrec    :: pickup-file precision (read from meta file)
C     nbFields    :: number of fields in pickup file (read from meta file)
C     missFldList :: List of missing fields   (attempted to read but not found)
C     missFldDim  :: Dimension of missing fields list array: missFldList
C     nMissing    :: Number of missing fields (attempted to read but not found)
C     m1, m2      :: 6.th dim index (AB-3) corresponding to time-step N-1 & N-2
C     j           :: loop index
C     nj          :: record number
C     ioUnit      :: temp for writing msg unit
C     msgBuf      :: Informational/error message buffer
      INTEGER fp
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER filePrec, nbFields
      INTEGER missFldDim, nMissing
      PARAMETER( missFldDim = 20 )
      CHARACTER*(8) missFldList(missFldDim)
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER j, m1, m2
#endif
      INTEGER nj, ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifndef ALLOW_GENERIC_ADVDIFF
      LOGICAL AdamsBashforthGt
      LOGICAL AdamsBashforthGs
      LOGICAL AdamsBashforth_T
      LOGICAL AdamsBashforth_S
      PARAMETER ( AdamsBashforthGt = .FALSE. ,
     &            AdamsBashforthGs = .FALSE. ,
     &            AdamsBashforth_T = .FALSE. ,
     &            AdamsBashforth_S = .FALSE. )
#endif

C     Suffix for pickup files
      IF (pickupSuff .EQ. ' ') THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
        ENDIF
      ELSE
        WRITE(suff,'(A10)') pickupSuff
      ENDIF
      WRITE(fn,'(A,A10)') 'pickup.',suff

C     Going to really do some IO. Make everyone except master thread wait.
C     this is done within IO routines => no longer needed
c     _BARRIER

      IF (pickup_read_mdsio) THEN

       fp = precFloat64
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, myIter, myThid )

       _BEGIN_MASTER( myThid )
c      IF ( filePrec.NE.0 .AND. filePrec.NE.fp ) THEN
       IF ( nbFields.GE.0 .AND. filePrec.NE.fp ) THEN
         WRITE(msgBuf,'(2A,I4)') 'READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', fp
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R READ_PICKUP (data-prec Pb)'
       ENDIF
       _END_MASTER( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        ioUnit = errorMessageUnit
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
c         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(4A)') 'READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          STOP 'ABNORMAL END: S/R READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'WARNING >> READ_PICKUP: ',
     &      ' no field-list found'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         IF ( nbFields.EQ.-1 ) THEN
C-      No meta-file
          WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' try to read pickup as currently written'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ELSE
C-      Old meta-file without List of Fields
          WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' try to read pickup as it used to be written'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' until checkpoint59i (2007 Oct 22)'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDIF
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---   Old way to read model fields:
       IF ( nbFields.EQ.0 ) THEN
        IF ( usePickupBeforeC54 ) THEN
#ifndef ALLOW_ADAMSBASHFORTH_3
          CALL READ_REC_3D_RL( fn, fp, Nr, uVel,  1, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gU,    2, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, guNm1, 3, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, vVel,  4, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gV,    5, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gvNm1, 6, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, theta, 7, myIter,myThid )
c         CALL READ_REC_3D_RL( fn, fp, Nr, gT,    8, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gtNm1, 9, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, salt, 10, myIter,myThid )
c         CALL READ_REC_3D_RL( fn, fp, Nr, gS,   11, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gsNm1,12, myIter,myThid )
#endif /*  ALLOW_ADAMSBASHFORTH_3 */
          CALL READ_REC_3D_RL( fn, fp,  1, etaN,
     &                                      12*Nr+1, myIter,myThid )
#ifdef NONLIN_FRSURF
          IF (nonlinFreeSurf .GE. 0) THEN
           CALL READ_REC_3D_RL(fn, fp,  1, etaH,
     &                                      12*Nr+2, myIter,myThid )
          ENDIF
#endif
        ELSE
#ifdef ALLOW_ADAMSBASHFORTH_3
          j = 3
          IF ( startFromPickupAB2 ) j = 2
          nj = 0
          CALL READ_REC_3D_RL( fn, fp, Nr, uVel, nj+1, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, guNm(1-OLx,1-OLy,1,1,1,1),
     &                                           nj+2, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, guNm(1-OLx,1-OLy,1,1,1,2),
     &                                           nj+j, myIter,myThid )
          nj = j
          CALL READ_REC_3D_RL( fn, fp, Nr, vVel, nj+1, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gvNm(1-OLx,1-OLy,1,1,1,1),
     &                                           nj+2, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gvNm(1-OLx,1-OLy,1,1,1,2),
     &                                           nj+j, myIter,myThid )
          nj = 2*j
          CALL READ_REC_3D_RL( fn, fp, Nr, theta,nj+1, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gtNm(1-OLx,1-OLy,1,1,1,1),
     &                                           nj+2, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gtNm(1-OLx,1-OLy,1,1,1,2),
     &                                           nj+j, myIter,myThid )
          nj = 3*j
          CALL READ_REC_3D_RL( fn, fp, Nr, salt, nj+1, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gsNm(1-OLx,1-OLy,1,1,1,1),
     &                                           nj+2, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gsNm(1-OLx,1-OLy,1,1,1,2),
     &                                           nj+j, myIter,myThid )
          nj = 4*j
#else /*  ALLOW_ADAMSBASHFORTH_3 */
          CALL READ_REC_3D_RL( fn, fp, Nr, uVel,  1, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, guNm1, 2, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, vVel,  3, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gvNm1, 4, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, theta, 5, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gtNm1, 6, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, salt,  7, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gsNm1, 8, myIter,myThid )
          nj = 8
#endif /*  ALLOW_ADAMSBASHFORTH_3 */
          CALL READ_REC_3D_RL( fn,fp,1, etaN, nj*Nr+1, myIter,myThid )
#ifdef EXACT_CONSERV
          IF ( exactConserv ) THEN
           CALL READ_REC_3D_RL(fn,fp,1,dEtaHdt,nj*Nr+2,myIter,myThid )
          ENDIF
          IF ( nonlinFreeSurf.GT.0 ) THEN
           CALL READ_REC_3D_RL(fn,fp,1, etaH, nj*Nr+3, myIter,myThid )
          ENDIF
#endif
        ENDIF

        IF ( storePhiHyd4Phys ) THEN
          WRITE(fn,'(A,A10)') 'pickup_ph.',suff
          CALL READ_REC_3D_RL( fn, fp, Nr, totPhiHyd,1,myIter,myThid )
         ENDIF
#ifdef ALLOW_NONHYDROSTATIC
         IF ( use3Dsolver ) THEN
          WRITE(fn,'(A,A10)') 'pickup_nh.',suff
          CALL READ_REC_3D_RL( fn, fp, Nr, phi_nh,  1, myIter,myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
          CALL READ_REC_3D_RL( fn, fp, Nr, gwNm(1-OLx,1-OLy,1,1,1,1),
     &                                              2, myIter,myThid )
          CALL READ_REC_3D_RL( fn, fp, Nr, gwNm(1-OLx,1-OLy,1,1,1,2),
     &                                              2, myIter,myThid )
#else /*  ALLOW_ADAMSBASHFORTH_3 */
          CALL READ_REC_3D_RL( fn, fp, Nr, gwNm1,   2, myIter,myThid )
#endif /*  ALLOW_ADAMSBASHFORTH_3 */
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
       ELSE
C---   New way to read model fields:
          nj = 0
C---    read State 3-D fields for restart
          CALL READ_MFLDS_3D_RL( 'Uvel    ', uVel,
     &                                     nj, fp, Nr, myIter, myThid )
          CALL READ_MFLDS_3D_RL( 'Vvel    ', vVel,
     &                                     nj, fp, Nr, myIter, myThid )
#if defined(ALLOW_EDDYPSI) && defined(ALLOW_GMREDI)
          IF (GM_InMomAsStress) THEN
            CALL READ_MFLDS_3D_RL( 'UEulerM ', uEulerMean,
     &                                      nj, fp, Nr, myIter, myThid )
            CALL READ_MFLDS_3D_RL( 'VEulerM ', vEulerMean,
     &                                      nj, fp, Nr, myIter, myThid )
          ENDIF
#endif
          CALL READ_MFLDS_3D_RL( 'Theta   ', theta,
     &                                     nj, fp, Nr, myIter, myThid )
          CALL READ_MFLDS_3D_RL( 'Salt    ', salt,
     &                                     nj, fp, Nr, myIter, myThid )
C---    read 3-D fields for AB-restart
#ifdef ALLOW_ADAMSBASHFORTH_3

         m1 = 1 + MOD(myIter+1,2)
         m2 = 1 + MOD( myIter ,2)
        IF ( momStepping ) THEN
C--     U velocity:
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GuNm1   ',guNm(1-OLx,1-OLy,1,1,1,m1),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GuNm2   ',guNm(1-OLx,1-OLy,1,1,1,m2),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
C--     V velocity:
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GvNm1   ',gvNm(1-OLx,1-OLy,1,1,1,m1),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GvNm2   ',gvNm(1-OLx,1-OLy,1,1,1,m2),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
        ENDIF
C--     Temperature:
        IF ( AdamsBashforthGt ) THEN
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GtNm1   ',gtNm(1-OLx,1-OLy,1,1,1,m1),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GtNm2   ',gtNm(1-OLx,1-OLy,1,1,1,m2),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
        ELSEIF ( AdamsBashforth_T ) THEN
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'TempNm1 ',gtNm(1-OLx,1-OLy,1,1,1,m1),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'TempNm2 ',gtNm(1-OLx,1-OLy,1,1,1,m2),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
        ENDIF
C--     Salinity:
        IF ( AdamsBashforthGs ) THEN
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GsNm1   ',gsNm(1-OLx,1-OLy,1,1,1,m1),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GsNm2   ',gsNm(1-OLx,1-OLy,1,1,1,m2),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
        ELSEIF ( AdamsBashforth_S ) THEN
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'SaltNm1 ',gsNm(1-OLx,1-OLy,1,1,1,m1),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'SaltNm2 ',gsNm(1-OLx,1-OLy,1,1,1,m2),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
C--     W velocity:
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GwNm1   ',gwNm(1-OLx,1-OLy,1,1,1,m1),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'GwNm2   ',gwNm(1-OLx,1-OLy,1,1,1,m2),
     &                                     nj, fp, Nr, myIter, myThid )
         ENDIF
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_QHYD_STAGGER_TS
C--     Quasi-Hydrostatic Adams-Bashforth variables:
        IF ( quasiHydrostatic .AND. staggerTimeStep ) THEN
         IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'QH_GwNm1',
     O                           QHydGwNm(1-OLx,1-OLy,1,1,1,m1),
     I                                     nj, fp, Nr, myIter, myThid )
         ENDIF
         IF ( beta_AB.NE.0. ) THEN
          CALL READ_MFLDS_3D_RL( 'QH_GwNm2',
     O                           QHydGwNm(1-OLx,1-OLy,1,1,1,m2),
     I                                     nj, fp, Nr, myIter, myThid )
         ENDIF
        ENDIF
#endif /* ALLOW_QHYD_STAGGER_TS */

#else /*  ALLOW_ADAMSBASHFORTH_3 */
        IF ( momStepping ) THEN
C--     U velocity:
          CALL READ_MFLDS_3D_RL( 'GuNm1   ', guNm1,
     &                                     nj, fp, Nr, myIter, myThid )
C--     V velocity:
          CALL READ_MFLDS_3D_RL( 'GvNm1   ', gvNm1,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
C--     Temperature
        IF ( AdamsBashforthGt ) THEN
          CALL READ_MFLDS_3D_RL( 'GtNm1   ', gtNm1,
     &                                     nj, fp, Nr, myIter, myThid )
        ELSEIF ( AdamsBashforth_T ) THEN
          CALL READ_MFLDS_3D_RL( 'TempNm1 ', gtNm1,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
C--     Salinity
        IF ( AdamsBashforthGs ) THEN
          CALL READ_MFLDS_3D_RL( 'GsNm1   ', gsNm1,
     &                                     nj, fp, Nr, myIter, myThid )
        ELSEIF ( AdamsBashforth_S ) THEN
          CALL READ_MFLDS_3D_RL( 'SaltNm1 ', gsNm1,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
          CALL READ_MFLDS_3D_RL( 'GwNm1   ', gwNm1,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_QHYD_STAGGER_TS
C--     Quasi-Hydrostatic Adams-Bashforth variables:
        IF ( quasiHydrostatic .AND. staggerTimeStep ) THEN
          CALL READ_MFLDS_3D_RL( 'QH_GwNm1', QHydGwNm,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
#endif /* ALLOW_QHYD_STAGGER_TS */

#endif /*  ALLOW_ADAMSBASHFORTH_3 */

C-      read Full Pressure for EOS in pressure:
        IF ( storePhiHyd4Phys ) THEN
          CALL READ_MFLDS_3D_RL( 'PhiHyd  ', totPhiHyd,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( use3Dsolver ) THEN
          CALL READ_MFLDS_3D_RL( 'Phi_NHyd', phi_nh,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
C-    With synchronous time-stepping, Smag-3D diffusivity is lagging by 1 time-step
C     (i.e., used before being updated meaning field is from previous time-step calc)
C     --> needs to be read-in from pickup file
        IF ( smag3D_diffCoeff.GT.zeroRL
     &       .AND. .NOT.staggerTimeStep ) THEN
          CALL READ_MFLDS_3D_RL( 'SmagDiff', smag3D_diffK,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
#endif /* ALLOW_SMAG_3D_DIFFUSIVITY */
#ifdef ALLOW_ADDFLUID
C-    read mass source/sink of fluid
        IF ( selectAddFluid.GE.1 ) THEN
          CALL READ_MFLDS_3D_RL( 'AddMass ', addMass,
     &                                     nj, fp, Nr, myIter, myThid )
        ENDIF
#endif /* ALLOW_ADDFLUID */
#ifdef ALLOW_FRICTION_HEATING
C-    needs frictional heating when using synchronous time-stepping
        IF ( addFrictionHeating .AND. .NOT.staggerTimeStep ) THEN
          CALL READ_MFLDS_LEV_RS( 'FricHeat', frictionHeating,
     &                              nj, fp, Nr, 1, Nr, myIter, myThid )
        ENDIF
#endif /* ALLOW_FRICTION_HEATING */

C---    read 2-D fields, starting with Eta:
          nj = nj*Nr
          CALL READ_MFLDS_3D_RL( 'EtaN    ', etaN,
     &                                     nj, fp, 1 , myIter, myThid )
        IF ( usingPCoords .AND. useSEAICE ) THEN
          CALL READ_MFLDS_3D_RL( 'Phi_rLow', phiHydLow,
     &                                     nj, fp, 1 , myIter, myThid )
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( selectNHfreeSurf.GE.1 ) THEN
          CALL READ_MFLDS_3D_RL( 'dPhiNH  ', dPhiNH,
     &                                     nj, fp, 1 , myIter, myThid )
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef EXACT_CONSERV
        IF ( exactConserv ) THEN
          CALL READ_MFLDS_3D_RL( 'dEtaHdt ', dEtaHdt,
     &                                     nj, fp, 1 , myIter, myThid )
        ENDIF
        IF ( nonlinFreeSurf.GT.0 ) THEN
          CALL READ_MFLDS_3D_RL( 'EtaH    ', etaH,
     &                                     nj, fp, 1 , myIter, myThid )
        ENDIF
#endif /* EXACT_CONSERV */
C--    end: new way to read pickup file
       ENDIF

C--    Check for missing fields:
       nMissing = missFldDim
       CALL READ_MFLDS_CHECK(
     O                    missFldList,
     U                    nMissing,
     I                    myIter, myThid )
       IF ( nMissing.GT.missFldDim ) THEN
         WRITE(msgBuf,'(2A,I4)') 'READ_PICKUP: ',
     &     'missing fields list has been truncated to', missFldDim
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R READ_PICKUP (list-size Pb)'
       ENDIF
       CALL CHECK_PICKUP(
     I                    missFldList,
     I                    nMissing, nbFields,
     I                    myIter, myThid )

C--   end: pickup_read_mdsio
      ENDIF

#ifdef ALLOW_MNC
      IF (useMNC .AND. pickup_read_mnc) THEN
        WRITE(fn,'(A)') 'pickup'
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, -1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'U',uVel, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'V',vVel, myThid)
#if defined(ALLOW_EDDYPSI) && defined(ALLOW_GMREDI)
        IF (GM_InMomAsStress) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'UEulerM',uEulerMean, myThid)
          CALL MNC_CW_RL_R('D',fn,0,0,'VEulerM',vEulerMean, myThid)
        ENDIF
#endif
        CALL MNC_CW_RL_R('D',fn,0,0,'Temp',theta, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'S',salt, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Eta',etaN, myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
        CALL MNC_CW_RL_R('D',fn,0,0,'gUnm1',guNm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'gVnm1',gvNm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'gTnm1',gtNm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'gSnm1',gsNm1, myThid)
#endif /* ALLOW_ADAMSBASHFORTH_3 */
C#ifdef NONLIN_FRSURF
C        IF ( nonlinFreeSurf.GE.0 .AND. usePickupBeforeC54 )
C     &    CALL MNC_CW_RL_R('D',fn,0,0,'EtaH', etaH, myThid)
C#endif
#ifdef EXACT_CONSERV
        IF (exactConserv) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'dEtaHdt',dEtaHdt,myThid)
        ENDIF
        IF (nonlinFreeSurf .GT. 0) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'EtaH', etaH, myThid)
        ENDIF
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF (use3Dsolver) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'phi_nh', phi_nh, myThid)
c         CALL MNC_CW_RL_R('D',fn,0,0,'gW', gW, myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
          CALL MNC_CW_RL_R('D',fn,0,0,'gWnm1', gwNm1, myThid)
#endif
        ENDIF
#endif
        IF ( storePhiHyd4Phys ) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'phiHyd',totPhiHyd,myThid)
        ENDIF
      ENDIF
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Fill in edge regions
      CALL EXCH_UV_3D_RL( uVel, vVel, .TRUE., Nr, myThid )
#if defined(ALLOW_EDDYPSI) && defined(ALLOW_GMREDI)
      IF (GM_InMomAsStress) THEN
        CALL EXCH_UV_3D_RL( uEulerMean, vEulerMean, .TRUE., Nr, myThid )
      ENDIF
#endif
      CALL EXCH_3D_RL( theta, Nr, myThid )
      CALL EXCH_3D_RL( salt,  Nr, myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
      CALL EXCH_UV_3D_RL( guNm(1-OLx,1-OLy,1,1,1,1),
     &                    gvNm(1-OLx,1-OLy,1,1,1,1),.TRUE.,Nr,myThid )
      CALL EXCH_UV_3D_RL( guNm(1-OLx,1-OLy,1,1,1,2),
     &                    gvNm(1-OLx,1-OLy,1,1,1,2),.TRUE.,Nr,myThid )
      CALL EXCH_3D_RL( gtNm(1-OLx,1-OLy,1,1,1,1), Nr, myThid )
      CALL EXCH_3D_RL( gtNm(1-OLx,1-OLy,1,1,1,2), Nr, myThid )
      CALL EXCH_3D_RL( gsNm(1-OLx,1-OLy,1,1,1,1), Nr, myThid )
      CALL EXCH_3D_RL( gsNm(1-OLx,1-OLy,1,1,1,2), Nr, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
      CALL EXCH_UV_3D_RL( guNm1, gvNm1, .TRUE., Nr, myThid )
      CALL EXCH_3D_RL( gtNm1, Nr, myThid )
      CALL EXCH_3D_RL( gsNm1, Nr, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      CALL EXCH_XY_RL( etaN, myThid )
      CALL EXCH_XY_RL( etaH, myThid )
#ifdef EXACT_CONSERV
      CALL EXCH_XY_RL( detaHdt, myThid )
#endif

      IF ( storePhiHyd4Phys )
     &  CALL EXCH_3D_RL( totPhiHyd, Nr, myThid )
      IF ( usingPCoords .AND. useSEAICE )
     &  CALL EXCH_XY_RL( phiHydLow, myThid )

#ifdef ALLOW_NONHYDROSTATIC
      IF ( use3Dsolver ) THEN
        CALL EXCH_3D_RL( phi_nh, Nr, myThid )
      ENDIF
      IF ( nonHydrostatic ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL EXCH_3D_RL( gwNm(1-OLx,1-OLy,1,1,1,1), Nr, myThid )
        CALL EXCH_3D_RL( gwNm(1-OLx,1-OLy,1,1,1,2), Nr, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL EXCH_3D_RL( gwNm1,  Nr, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      ENDIF
      IF ( selectNHfreeSurf.GE.1 ) THEN
        CALL EXCH_XY_RL( dPhiNH, myThid )
      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_QHYD_STAGGER_TS
      IF ( quasiHydrostatic .AND. staggerTimeStep ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL EXCH_3D_RL( QHydGwNm(1-OLx,1-OLy,1,1,1,1), Nr, myThid )
        CALL EXCH_3D_RL( QHydGwNm(1-OLx,1-OLy,1,1,1,2), Nr, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL EXCH_3D_RL( QHydGwNm, Nr, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      ENDIF
#endif /* ALLOW_QHYD_STAGGER_TS */

      RETURN
      END
