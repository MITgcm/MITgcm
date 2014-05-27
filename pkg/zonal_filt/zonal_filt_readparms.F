C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_readparms.F,v 1.3 2014/05/27 23:41:30 jmc Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

      SUBROUTINE ZONAL_FILT_READPARMS( myThid )
C     *==========================================================*
C     | SUBROUTINE ZONAL_FILT_READPARMS                          |
C     | o Routine to initialize Zonal Filter (=FFT) parameters   |
C     *==========================================================*

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ZONAL_FILT.h"

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_ZONAL_FILT
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /ZONFILT_PARM01/
     &          zonal_filt_uvStar, zonal_filt_TrStagg,
     &          zonal_filt_lat,
     &          zonal_filt_cospow, zonal_filt_sinpow,
     &          zonal_filt_mode2dx

      IF ( .NOT.useZONAL_FILT ) THEN
C-    pkg ZONAL_FILT is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
c        ZONFILTisOn = .FALSE.
C     print a (weak) warning if data.zonfilt is found
          CALL PACKAGES_UNUSED_MSG( 'useZONAL_FILT', ' ', 'zonfilt' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C--   ZONAL_FILT_READPARMS has been called so we know that
C     the package is active.
c     ZONFILTisOn = .TRUE.

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)')' ZONAL_FILT_READPARMS: opening data.zonfilt'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                         'data.zonfilt', 'ZONAL_FILT_READPARMS',
     O                         iUnit,
     I                         myThid )

C--   Default flags and values for Zonal Filter
      zonal_filt_uvStar   = .TRUE.
      zonal_filt_TrStagg  = .TRUE.
      zonal_filt_lat      = 90.
      zonal_filt_sinpow   = 2
      zonal_filt_cospow   = 2
      zonal_filt_mode2dx  = 0

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=ZONFILT_PARM01)

      WRITE(msgBuf,'(A)')
     &   ' ZONAL_FILT_READPARMS: finished reading data.zonfilt'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

c     write(*,*) 'Shap_funct, nShap_Tr,UV _Phys=',
c    &    Shap_funct, nShapT, nShapUV, nShapTrPhys, nShapUVPhys
c     write(*,*) 'Shap_Trtau,Shap_uvtau=',Shap_Trtau,Shap_uvtau

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C--   Check the parameters :

       IF ( .NOT.zonal_filt_uvStar ) THEN

C- Notes: applying the filter at the end of the time step (after SOLVE_FOR_P)
C    affects the barotropic flow divergence ; this might not be consistent
C    with some option of the code.

        IF ( rigidLid ) THEN
         WRITE(msgBuf,'(2A)') 'ZONAL_FILT with rigidLid ',
     &                         'needs zonal_filt_uvStar=.true.'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R ZONAL_FILT_READPARMS'
        ELSEIF ( .NOT.exactConserv ) THEN
         WRITE(msgBuf,'(2A)') 'S/R ZONAL_FILT_READPARMS: WARNING <<< ',
     &    'applying Filter after SOLVE_FOR_P (zonal_filt_uvStar=FALSE)'
         CALL PRINT_MESSAGE(msgBuf, errorMessageUnit, SQUEEZE_RIGHT,1)
         WRITE(msgBuf,'(2A)') 'S/R ZONAL_FILT_READPARMS: WARNING <<< ',
     &    'requires to recompute Eta after ==> turn on exactConserv !'
         CALL PRINT_MESSAGE(msgBuf, errorMessageUnit, SQUEEZE_RIGHT,1)
        ENDIF

       ENDIF

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
