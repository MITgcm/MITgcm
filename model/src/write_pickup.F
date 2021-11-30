#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE WRITE_PICKUP(
     I                 permPickup, suffix,
     I                 myTime, myIter, myThid )

C     !DESCRIPTION:
C     Write the main-model pickup-file and do it NOW.
C     It writes both "rolling-pickup" files (ckptA,ckptB) and
C     permanent pickup files (with iteration number in the file name).
C     It calls routines from other packages (\textit{eg.} rw and mnc)
C     to do the per-variable writes.

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

C     !INPUT PARAMETERS:
C     permPickup      :: Is or is not a permanent pickup.
C     suffix          :: pickup-name suffix
C     myTime          :: Current time of simulation ( s )
C     myIter          :: Iteration number
C     myThid          :: Thread number for this instance of the routine.
      LOGICAL permPickup
      CHARACTER*(*) suffix
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     fp          :: pickup-file precision
C     glf         :: local flag for "globalFiles"
C     fn          :: Temp. for building file name.
C     nWrFlds     :: number of fields being written
C     n3D         :: number of 3-D fields being written
C     listDim     :: dimension of "wrFldList" local array
C     wrFldList   :: list of written fields
C     m1,m2       :: 6.th dim index (AB-3) corresponding to time-step N-1 & N-2
C     j           :: loop index / field number
C     nj          :: record number
C     msgBuf      :: Informational/error message buffer
      INTEGER fp
      LOGICAL  glf
      _RL      timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds, n3D
      PARAMETER( listDim = 20 )
      CHARACTER*(8) wrFldList(listDim)
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m1, m2
#endif
      INTEGER j, nj
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

C-    Initialise:
      DO j=1,listDim
        wrFldList(j) = ' '
      ENDDO

C     Write model fields

C     Going to really do some IO. Make everyone except master thread wait.
C     this is done within IO routines => no longer needed
c     _BARRIER

      IF (pickup_write_mdsio) THEN

        WRITE(fn,'(A,A)') 'pickup.', suffix
        fp = precFloat64
        j  = 0
C     record number < 0 : a hack not to write meta files now:

C---  write State 3-D fields for restart
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, uVel,   -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Uvel    '
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, vVel,   -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Vvel    '

#if defined(ALLOW_EDDYPSI) && defined(ALLOW_GMREDI)
        IF (GM_InMomAsStress) THEN
          j = j + 1
          CALL WRITE_REC_3D_RL( fn, fp, Nr, uEulerMean,
     &                                            -j, myIter, myThid )
          IF (j.LE.listDim) wrFldList(j) = 'UEulerM '
          j = j + 1
          CALL WRITE_REC_3D_RL( fn, fp, Nr, vEulerMean,
     &                                            -j, myIter, myThid )
          IF (j.LE.listDim) wrFldList(j) = 'VEulerM '
      ENDIF
#endif

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, theta,  -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Theta   '
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, salt,   -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Salt    '
C---  write 3-D fields for AB-restart
#ifdef ALLOW_ADAMSBASHFORTH_3

        m1 = 1 + MOD(myIter+1,2)
        m2 = 1 + MOD( myIter ,2)
      IF ( momStepping ) THEN
C--   U velocity:
       IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, guNm(1-OLx,1-OLy,1,1,1,m1),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GuNm1   '
       ENDIF
       IF ( beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, guNm(1-OLx,1-OLy,1,1,1,m2),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GuNm2   '
       ENDIF
C--   V velocity:
       IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gvNm(1-OLx,1-OLy,1,1,1,m1),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GvNm1   '
       ENDIF
       IF ( beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gvNm(1-OLx,1-OLy,1,1,1,m2),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GvNm2   '
       ENDIF
      ENDIF
C--   Temperature:
      IF ( AdamsBashforthGt.OR.AdamsBashforth_T ) THEN
       IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gtNm(1-OLx,1-OLy,1,1,1,m1),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) THEN
         IF ( AdamsBashforthGt ) wrFldList(j) = 'GtNm1   '
         IF ( AdamsBashforth_T ) wrFldList(j) = 'TempNm1 '
        ENDIF
       ENDIF
       IF ( beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gtNm(1-OLx,1-OLy,1,1,1,m2),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) THEN
         IF ( AdamsBashforthGt ) wrFldList(j) = 'GtNm2   '
         IF ( AdamsBashforth_T ) wrFldList(j) = 'TempNm2 '
        ENDIF
       ENDIF
      ENDIF
C--   Salinity:
      IF ( AdamsBashforthGs.OR.AdamsBashforth_S ) THEN
       IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gsNm(1-OLx,1-OLy,1,1,1,m1),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) THEN
         IF ( AdamsBashforthGs ) wrFldList(j) = 'GsNm1   '
         IF ( AdamsBashforth_S ) wrFldList(j) = 'SaltNm1 '
        ENDIF
       ENDIF
       IF ( beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gsNm(1-OLx,1-OLy,1,1,1,m2),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) THEN
         IF ( AdamsBashforthGs ) wrFldList(j) = 'GsNm2   '
         IF ( AdamsBashforth_S ) wrFldList(j) = 'SaltNm2 '
        ENDIF
       ENDIF
      ENDIF
#ifdef ALLOW_NONHYDROSTATIC
C--   W velocity:
      IF ( nonHydrostatic ) THEN
       IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gwNm(1-OLx,1-OLy,1,1,1,m1),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GwNm1   '
       ENDIF
       IF ( beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gwNm(1-OLx,1-OLy,1,1,1,m2),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GwNm2   '
       ENDIF
      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_QHYD_STAGGER_TS
C--   Quasi-Hydrostatic Adams-Bashforth variables:
      IF ( quasiHydrostatic .AND. staggerTimeStep ) THEN
       IF ( alph_AB.NE.0. .OR. beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn,fp, Nr, QHydGwNm(1-OLx,1-OLy,1,1,1,m1),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'QH_GwNm1'
       ENDIF
       IF ( beta_AB.NE.0. ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn,fp, Nr, QHydGwNm(1-OLx,1-OLy,1,1,1,m2),
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'QH_GwNm2'
       ENDIF
      ENDIF
#endif /* ALLOW_QHYD_STAGGER_TS */

#else /*  ALLOW_ADAMSBASHFORTH_3 */

       IF ( momStepping ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, guNm1,  -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GuNm1   '
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gvNm1,  -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GvNm1   '
       ENDIF
       IF ( AdamsBashforthGt.OR.AdamsBashforth_T ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gtNm1,  -j, myIter, myThid )
        IF (j.LE.listDim) THEN
         IF ( AdamsBashforthGt ) wrFldList(j) = 'GtNm1   '
         IF ( AdamsBashforth_T ) wrFldList(j) = 'TempNm1 '
        ENDIF
       ENDIF
       IF ( AdamsBashforthGs.OR.AdamsBashforth_S ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gsNm1,  -j, myIter, myThid )
        IF (j.LE.listDim) THEN
         IF ( AdamsBashforthGs ) wrFldList(j) = 'GsNm1   '
         IF ( AdamsBashforth_S ) wrFldList(j) = 'SaltNm1 '
        ENDIF
       ENDIF
#ifdef ALLOW_NONHYDROSTATIC
       IF ( nonHydrostatic ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gwNm1,  -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'GwNm1   '
       ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_QHYD_STAGGER_TS
C--   Quasi-Hydrostatic Adams-Bashforth variables:
      IF ( quasiHydrostatic .AND. staggerTimeStep ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn,fp, Nr, QHydGwNm, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'QH_GwNm1'
      ENDIF
#endif /* ALLOW_QHYD_STAGGER_TS */

#endif /*  ALLOW_ADAMSBASHFORTH_3 */

C-    write Full Pressure for EOS in pressure:
       IF ( storePhiHyd4Phys ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr,totPhiHyd,-j,myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'PhiHyd  '
       ENDIF
#ifdef ALLOW_NONHYDROSTATIC
       IF ( use3Dsolver ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, phi_nh, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Phi_NHyd'
       ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
C-    With synchronous time-stepping, Smag-3D diffusivity is lagging by 1 time-step
C     (i.e., used before being updated meaning field is from previous time-step calc)
C     --> needs to be stored in pickup file
       IF ( smag3D_diffCoeff.GT.zeroRL .AND. .NOT.staggerTimeStep ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, smag3D_diffK,
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'SmagDiff'
       ENDIF
#endif /* ALLOW_SMAG_3D_DIFFUSIVITY */
#ifdef ALLOW_ADDFLUID
C-    write mass source/sink of fluid (but not needed if selectAddFluid=-1)
       IF ( selectAddFluid.NE.0 ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, Nr, addMass,-j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'AddMass '
       ENDIF
#endif /* ALLOW_ADDFLUID */
#ifdef ALLOW_FRICTION_HEATING
C-    needs frictional heating when using synchronous time-stepping
       IF ( addFrictionHeating .AND. .NOT.staggerTimeStep ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RS( fn, fp, Nr, frictionHeating,
     &                                            -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'FricHeat'
       ENDIF
#endif /* ALLOW_FRICTION_HEATING */

        n3D = j
C---  Write 2-D fields, starting with Eta:
        j = j + 1
        nj = -( n3D*(Nr-1) + j )
        CALL WRITE_REC_3D_RL( fn, fp, 1 , etaN,   nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'EtaN    '
       IF ( usingPCoords .AND. useSEAICE ) THEN
        j = j + 1
        nj = -( n3D*(Nr-1) + j )
        CALL WRITE_REC_3D_RL( fn, fp, 1, phiHydLow, nj,myIter,myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Phi_rLow'
       ENDIF
#ifdef ALLOW_NONHYDROSTATIC
       IF ( selectNHfreeSurf.GE.1 ) THEN
        j = j + 1
        nj = -( n3D*(Nr-1) + j )
        CALL WRITE_REC_3D_RL( fn, fp, 1, dPhiNH,  nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'dPhiNH  '
       ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef EXACT_CONSERV
c      IF ( exactConserv ) THEN
        j = j + 1
        nj = -( n3D*(Nr-1) + j )
        CALL WRITE_REC_3D_RL( fn, fp, 1, dEtaHdt, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'dEtaHdt '
c      ENDIF
C- note: always write dEtaHdt & EtaH but read only if exactConserv & nonlinFreeSurf
C        this works only because nonlinFreeSurf > 0 => exactConserv=T
c      IF ( nonlinFreeSurf.GT.0 ) THEN
        j = j + 1
        nj = -( n3D*(Nr-1) + j )
        CALL WRITE_REC_3D_RL( fn, fp, 1, etaHnm1, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'EtaH    '
c      ENDIF
#endif /* EXACT_CONSERV */
C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R WRITE_PICKUP (list-size Pb)'
        ENDIF
#ifdef ALLOW_MDSIO
C-    Note: temporary: since it is a pain to add more arguments to
C     all MDSIO S/R, uses instead this specific S/R to write only
C     meta files but with more informations in it.
        nj = ABS(nj)
        glf  = globalFiles
        timList(1) = myTime
        CALL MDS_WR_METAFILES( fn, fp, glf, .FALSE.,
     &                         0, 0, 1, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         nj, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------
      ENDIF

#ifdef ALLOW_MNC
      IF (useMNC .AND. pickup_write_mnc) THEN
        IF ( permPickup ) THEN
          WRITE(fn,'(A)') 'pickup'
        ELSE
          WRITE(fn,'(A,A)') 'pickup.',checkPtSuff(nCheckLev)
        ENDIF
C       First ***define*** the file group name
        CALL MNC_CW_SET_UDIM(fn, 0, myThid)
        IF ( permPickup ) THEN
          CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
        ELSE
          CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
        ENDIF
C       Then set the actual unlimited dimension
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'U', uVel, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'V', vVel, myThid)
#if defined(ALLOW_EDDYPSI) && defined(ALLOW_GMREDI)
        IF (GM_InMomAsStress) THEN
          CALL MNC_CW_RL_W('D',fn,0,0,'UEulerM', uEulerMean, myThid)
          CALL MNC_CW_RL_W('D',fn,0,0,'VEulerM', vEulerMean, myThid)
        ENDIF
#endif
        CALL MNC_CW_RL_W('D',fn,0,0,'Temp', theta, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'S', salt, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Eta', etaN, myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
        CALL MNC_CW_RL_W('D',fn,0,0,'gUnm1', guNm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'gVnm1', gvNm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'gTnm1', gtNm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'gSnm1', gsNm1, myThid)
#endif /* ALLOW_ADAMSBASHFORTH_3 */
#ifdef EXACT_CONSERV
        CALL MNC_CW_RL_W('D',fn,0,0,'dEtaHdt', dEtaHdt, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'EtaH', etaHnm1, myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( use3Dsolver ) THEN
          CALL MNC_CW_RL_W('D',fn,0,0,'phi_nh', phi_nh, myThid)
c         CALL MNC_CW_RL_W('D',fn,0,0,'gW', gW, myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
          CALL MNC_CW_RL_W('D',fn,0,0,'gWnm1', gwNm1, myThid)
#endif
        ENDIF
#endif
        IF ( storePhiHyd4Phys ) THEN
          CALL MNC_CW_RL_W('D',fn,0,0,'phiHyd', totPhiHyd, myThid)
        ENDIF
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
      ENDIF
#endif /*  ALLOW_MNC  */

C--   Every one else must wait until writing is done.
C     this is done within IO routines => no longer needed
c     _BARRIER

      RETURN
      END
