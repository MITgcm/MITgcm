C $Header: /u/gcmpack/MITgcm/model/src/write_pickup.F,v 1.1 2006/08/24 01:14:19 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE WRITE_PICKUP(
     I                 permPickup,
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
#include "DYNVARS.h"
#include "SURFACE.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
C     permPickup      :: Is or is not a permanent pickup.
C     myTime          :: Current time of simulation ( s )
C     myIter          :: Iteration number
C     myThid          :: Thread number for this instance of the routine.
      LOGICAL permPickup
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     fn     :: Temp. for building file name string.
C     fp     :: file precision
      INTEGER fp
      INTEGER i, nj
      CHARACTER*(MAX_LEN_FNAM) fn

C     Write model fields
      DO i = 1,MAX_LEN_FNAM
        fn(i:i) = ' '
      ENDDO
      IF ( permPickup ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup.',myIter
      ELSE
        WRITE(fn,'(A,A)') 'pickup.',checkPtSuff(nCheckLev)
      ENDIF

C     Going to really do some IO. Make everyone except master thread wait.
      _BARRIER

      IF (pickup_write_mdsio) THEN

        fp = precFloat64

#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL WRITE_REC_3D_RL( fn, fp, Nr, uVel,  1, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, guNm(1-Olx,1-Oly,1,1,1,1),
     &                                           2, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, guNm(1-Olx,1-Oly,1,1,1,2),
     &                                           3, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, vVel,  4, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gvNm(1-Olx,1-Oly,1,1,1,1),
     &                                           5, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gvNm(1-Olx,1-Oly,1,1,1,2),
     &                                           6, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, theta, 7, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gtNm(1-Olx,1-Oly,1,1,1,1),
     &                                           8, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gtNm(1-Olx,1-Oly,1,1,1,2),
     &                                           9, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, salt, 10, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gsNm(1-Olx,1-Oly,1,1,1,1),
     &                                          11, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gsNm(1-Olx,1-Oly,1,1,1,2),
     &                                          12, myIter, myThid )
        nj = 12
#else /*  ALLOW_ADAMSBASHFORTH_3 */
        CALL WRITE_REC_3D_RL( fn, fp, Nr, uVel,  1, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, guNm1, 2, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, vVel,  3, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gvNm1, 4, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, theta, 5, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gtNm1, 6, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, salt,  7, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, fp, Nr, gsNm1, 8, myIter, myThid )
        nj = 8
#endif /*  ALLOW_ADAMSBASHFORTH_3 */
        CALL WRITE_REC_3D_RL( fn, fp, 1,etaN, nj*Nr+1, myIter, myThid )
#ifdef EXACT_CONSERV
        CALL WRITE_REC_3D_RL( fn, fp, 1,dEtaHdt,nj*Nr+2,myIter,myThid )
        CALL WRITE_REC_3D_RL( fn, fp, 1,etaHnm1,nj*Nr+3,myIter,myThid )
#endif /* EXACT_CONSERV */
        IF ( useDynP_inEos_Zc ) THEN
          IF ( permPickup ) THEN
            WRITE(fn,'(A,I10.10)') 'pickup_ph.',myIter
          ELSE
            WRITE(fn,'(A,A)') 'pickup_ph.',checkPtSuff(nCheckLev)
          ENDIF
          CALL WRITE_REC_3D_RL( fn,fp,Nr, totPhiHyd,1, myIter,myThid )
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( use3Dsolver ) THEN
          IF ( permPickup ) THEN
            WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
          ELSE
            WRITE(fn,'(A,A)') 'pickup_nh.',checkPtSuff(nCheckLev)
          ENDIF
          CALL WRITE_REC_3D_RL( fn,fp,Nr, phi_nh, 1, myIter, myThid )
          CALL WRITE_REC_3D_RL( fn,fp,Nr, gwNm1,  2, myIter, myThid )
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

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
          CALL MNC_CW_RL_W('D',fn,0,0,'gWnm1', gwNm1, myThid)
        ENDIF
#endif
        IF ( useDynP_inEos_Zc ) THEN
          CALL MNC_CW_RL_W('D',fn,0,0,'phiHyd', totPhiHyd, myThid)
        ENDIF
      ENDIF
#endif /*  ALLOW_MNC  */

C--   Every one else must wait until writing is done.
      _BARRIER

      RETURN
      END
