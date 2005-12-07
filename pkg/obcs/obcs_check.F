C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_check.F,v 1.9 2005/12/07 19:58:35 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_CHECK                                    |
C     | o Check dependances with other packages                  |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

#ifdef ALLOW_OBCS

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,bi,bj

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_OBCS'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#ifdef ALLOW_ORLANSKI
      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_ORLANSKI'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)
#else
      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: #undef OBCS_RADIATE_ORLANSKI  and'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: one of useOrlanski* logicals is true'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF 
#endif /* ALLOW_ORLANSKI */

      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
       IF (nonlinFreeSurf.GT.0) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: nonlinFreeSurf not yet implemented'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
#ifdef ALLOW_PTRACERS
       IF (usePTracers) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: pTracers not yet implemented'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
#endif ALLOW_PTRACERS
      ENDIF

#ifndef ALLOW_OBCS_PRESCRIBE
        IF (useOBCSprescribe) THEN
         WRITE(msgBuf,'(A)')
     &   'OBCS_CHECK: ERROR: useOBCSprescribe = .TRUE. for'
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(A)')
     &   'OBCS_CHECK: undef ALLOW_OBCS_PRESCRIBE'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
        ENDIF
#endif

      IF ( debugLevel.GE.debLevB ) THEN
       _BEGIN_MASTER( myThid )
       DO bj = 1,nSy
        DO bi = 1,nSx
         write(*,*) 'bi,bj:',bi,bj,' OB_Jn=',(OB_Jn(i,bi,bj),i=1,sNx)
         write(*,*) 'bi,bj:',bi,bj,' OB_Js=',(OB_Js(i,bi,bj),i=1,sNx)
         write(*,*) 'bi,bj:',bi,bj,' OB_Ie=',(OB_Ie(j,bi,bj),j=1,sNy)
         write(*,*) 'bi,bj:',bi,bj,' OB_Iw=',(OB_Iw(j,bi,bj),j=1,sNy)
        ENDDO
       ENDDO
       _END_MASTER(myThid)
      ENDIF

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: OK'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#endif /* ALLOW_OBCS */
      RETURN
      END
