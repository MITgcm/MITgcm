C $Header: /u/gcmpack/MITgcm/model/inc/Attic/TR1.h,v 1.1 2001/07/13 14:15:29 heimbach Exp $
C $Name:  $
C
C     /==========================================================\
C     | TR1.h                                                    |
C     | o Tracer 1 prognostic state.                             |
C     |==========================================================|
C     | The value and two levels of time tendency are held for   |
C     | each prognostic variable.                                |
C     \==========================================================/
C
C     TR1     - tracer concentration (tr per unit volume).
C     GTR1    - tracer concentration (TR1 per second).
C     GTR1NM1 - tracer concentration (TR1 per second).

      COMMON /TR1_R/ 
     &                   tr1, gTr1, gTr1NM1
      _RL  tr1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTr1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTr1NM1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
