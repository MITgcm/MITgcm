C     /==========================================================\
C     | CPLIDS.h Declare symbolic constants holding strings that |
C     |          are used to identify this component and the     |
C     |          fields it exchanges with other components.      |
C     |          Note - the name 'Coupler' has a special meaning.|
C     |          It is used in the MIT Coupler library to denote |
C     |          the process that coordinates the communication  |
C     |          between processes.                              |
C     \==========================================================/
      CHARACTER*(*) cplCompName 
      PARAMETER( cplCompName  = 'Coupler'        )
