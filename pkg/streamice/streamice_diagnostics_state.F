

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_DIAGNOSTICS_STATE( myTime, myIter, myThid )

C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"

      _RL mytime
      INTEGER myIter, myThid

#ifdef ALLOW_STREAMICE

      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_DIAGNOSTICS


      

      IF ( useDiagnostics ) THEN
       WRITE(msgBuf,'(A)') 'CALLING FILL DIAGNOSTICS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
     
       CALL DIAGNOSTICS_FILL(U_streamice,'SI_Uvel ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(V_streamice,'SI_Vvel ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(H_streamice,'SI_Thick',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(area_shelf_streamice,'SI_area ',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(float_frac_streamice,'SI_float',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(STREAMICE_hmask,'SI_hmask',
     &      0,1,0,1,1,myThid)
#ifdef STREAMICE_HYBRID_STRESS
       CALL DIAGNOSTICS_FILL(streamice_u_surf,'SI_usurf',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(STREAMICE_v_surf,'SI_vsurf',
     &      0,1,0,1,1,myThid)
#endif
       CALL DIAGNOSTICS_FILL(surf_el_streamice,'SI_selev',
     &      0,1,0,1,1,myThid)
#ifdef ALLOW_STREAMICE_2DTRACER
       CALL DIAGNOSTICS_FILL(trac2d,'SI2Dtrac',
     &      0,1,0,1,1,myThid)
#endif

      ENDIF


#endif /* ALLOW_DIAGNOSTICS */
#endif
      RETURN
      END
