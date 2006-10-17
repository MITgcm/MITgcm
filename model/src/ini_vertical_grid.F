C $Header: /u/gcmpack/MITgcm/model/src/ini_vertical_grid.F,v 1.15 2006/10/17 18:52:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_VERTICAL_GRID
C     !INTERFACE:
      SUBROUTINE INI_VERTICAL_GRID( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_VERTICAL_GRID                              
C     | o Initialise vertical gridding arrays                     
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     K        :: loop index
C     msgBuf   :: Informational/error meesage buffer  
      INTEGER K
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER(myThid)

      IF (setCenterDr) THEN
C-- Interface at middle between 2 centers :

C-    Check that all thickness are > 0 :
       DO K=1,Nr+1
        IF (delRc(K).LE.0.) THEN
         WRITE(msgBuf,'(A,I4,A,E16.8)')
     &  'S/R INI_VERTICAL_GRID: delRc(K=',K,' )=',delRc(K)
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(A)')
     &  'S/R INI_VERTICAL_GRID: Vert. grid spacing MUST BE > 0'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID'
        ENDIF
       ENDDO

C-    Calculate depths of centers and interfaces
        rF(1)  = Ro_SeaLevel
        rC(1)  = rF(1) + rkSign*delRc(1)
        drC(1) = delRc(1)
        drF(1) = delRc(1)
       DO K=2,Nr
        drC(K)   = delRc(K)
        drF(K-1) =  drF(K-1) + 0.5 _d 0*delRc(K)
        drF(K)   = 0.5 _d 0*delRc(K)
        rC(K)    = rC(K-1) + rkSign*drC(K)
        rF(K)    = rF(K-1) + rkSign*drF(K-1)
       ENDDO
        drF(Nr)  = drF(Nr) + delRc(Nr+1)
        rF(Nr+1) = rF(Nr) + rkSign*drF(Nr)

      ELSE
C-- Center at middle between 2 interfaces :

C-    Check that all thickness are > 0 :
       DO K=1,Nr
        IF (delR(K).LE.0.) THEN
         WRITE(msgBuf,'(A,I4,A,E16.8)')
     &  'S/R INI_VERTICAL_GRID: delR(K=',K,' )=',delR(K)
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(A)')
     &  'S/R INI_VERTICAL_GRID: Vert. grid spacing MUST BE > 0'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R INI_VERTICAL_GRID'
        ENDIF
       ENDDO

C-    Calculate depths of interfaces and centers 
      rF(1) = Ro_SeaLevel
      DO K=1,Nr
       drF(K)     = delR(K)
       rF(K+1) = rF(K) + rkSign*delR(K)
      ENDDO
      drC(1)      = delR(1) * 0.5 _d 0
      rC(1)       = rf(1) + rkSign*delR(1) * 0.5 _d 0
      DO K=2,Nr
       drC(K)     = 0.5 _d 0 *(delR(K-1)+delR(K))
       rC(K)      = rC(K-1) + rkSign*drC(K)
      ENDDO

C--
      ENDIF

C-    Calculate reciprol vertical grid spacing :
      DO K=1,Nr
       saFac(K)  = 1. _d 0
       recip_drC(K)   = 1. _d 0/drC(K)
       recip_drF(K)   = 1. _d 0/drF(K)
      ENDDO

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
