C $Header: /u/gcmpack/MITgcm/model/src/do_the_model_io.F,v 1.3 1998/05/21 18:25:49 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE DO_THE_MODEL_IO(myCurrentTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE DO_THE_MODEL_IO                               |
C     | o Controlling routine for IO in model main time-stepping |
C     |   loop.                                                  |
C     |==========================================================|
C     | Many systems do not have thread safe IO so it is easier  |
C     | to lump everything together and do dumping of fields     |
C     | and updating of forcing terms in a single place.         |
C     | The approach to IO used here is that writes are only     |
C     | performed by thread 1 and that a process only writes out |
C     | its data ( since it doen't know about anyone elses data!)|
C     | Reading on the other hand is assumed to be from a file   |
C     | containing all the data for all the processes. Only the  |
C     | portion of data of interest to this process is actually  |
C     | loaded. To work well this assumes the existence of some  |
C     | reliable tool to join datasets together at the end of a  |
C     | run.                                                     |
C     | Notes                                                    |
C     | =====                                                    |
C     | We allow thread 2-nThreads to continue whilst thread 1   |
C     | does IO. The assumption is that the other threads won't  |
C     | do anything to update their interior regions before      |
C     | thread 1 has finished writing them out.                  |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      REAL    myCurrentTime

C     == Local variables ==
C     suff - Hold suffix part of a filename
      CHARACTER*(MAX_LEN_FNAM) suff
      

C--   Generaly only thread 1 does IO here. It can't start until everyone's
C--   fields are ready.
      IF ( MOD(myIter,1000) .EQ. 0 ) THEN    
       _BARRIER

C--    Write "text-plots" of certain fields
       CALL PLOT_FIELD_XYZR8( uVel , 'uVel  ' , Nz, myIter, myThid )
       CALL PLOT_FIELD_XYZR8( vVel , 'vVel  ' , Nz, myIter, myThid )
       CALL PLOT_FIELD_XYZR8( theta, 'Theta ' , Nz, myIter, myThid )
       CALL PLOT_FIELD_XYZR8( rho, 'rho ' , Nz, myIter, myThid )
       CALL PLOT_FIELD_XYR8( cg2d_x, 'cg2d_x ' , myIter, myThid )

      ENDIF

C--   Write model state to binary file
       CALL WRITE_STATE( myCurrentTime, myIter, myThid )
C      WRITE(suff,'(I10.10)') myIter
C      CALL DFILE_SET_RW
C      CALL DFILE_SET_CONT_ON_ERROR
C      CALL WRITE_FLD_XYZ(  'U.',suff,      uVel)
C      CALL WRITE_FLD_XYZ(  'V.',suff,      vVel)
C      CALL WRITE_FLD_XYZ(  'T.',suff,     theta)
C      CALL WRITE_FLD_XYZ(  'S.',suff,      salt)
C      CALL WRITE_FLD_XYZ(  'W.',suff,  IO_tmp3d)
C      CALL WRITE_FLD_XYZ('RHO.',suff,  IO_tmp3d)
C      CALL WRITE_FLD_XYZ( 'PH.',suff,  IO_tmp3d)
C      CALL WRITE_FLD_XY ( 'PS.',suff,  IO_tmp2d)

C--   Write model checkpoint files
C     nCheck = 1,2,...nCheckLev, 1,2,...nCheckLev, etc...
C     CALL WRITE_CHECKPOINT( myIter, myThid )
C     nCheck = MOD(nCheck,nCheckLev)+1
C     suff   = checkPtSuff(nCheck)
C     CALL DFILE_SET_RW
C     CALL DFILE_SET_CONT_ON_ERROR
C     CALL WRITE_FLD_XYZ(  'uVel.',suff,uVel  )
C     CALL WRITE_FLD_XYZ(  'vVel.',suff,vVel  )
C     CALL WRITE_FLD_XYZ( 'theta.',suff,theta )
C     CALL WRITE_FLD_XYZ(  'salt.',suff,salt  )
C     CALL WRITE_FLD_XY ('cg2d_x.',suff,cg2d_x)
C     CALL WRITE_FLD_XYZ(    'gu.',suff,gu    )
C     CALL WRITE_FLD_XYZ(    'gv.',suff,gv    )
C     CALL WRITE_FLD_XYZ( 'gunm1.',suff,gunm1 )
C     CALL WRITE_FLD_XYZ( 'gvnm1.',suff,gvnm1 )
 
      _BARRIER
C
      RETURN
      END
 
