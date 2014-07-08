C $Header: /u/gcmpack/MITgcm/pkg/openad/openad_checkpointInit.F,v 1.2 2014/07/08 19:05:12 jmc Exp $
C $Name:  $

      SUBROUTINE openad_checkpointinit(uCheckLev1,
     +     uCheckLev2,
     +     uCheckLev3,
     +     uCheckLev4 )

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      integer iloop
      logical haveRemainder, allStepsCovered
      integer  uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4
      integer  nCheckLev1, nCheckLev2, nCheckLev3,nCheckLev4

      uCheckLev1=0
      uCheckLev2=0
      uCheckLev3=0
      uCheckLev4=0

      open(2,action='read',file='ncheckLev.conf')
      read(2,'(I5,/,I5,/I5,/I5)') nCheckLev1, nCheckLev2, nCheckLev3,
     +nCheckLev4
      close(2)

      write(standardmessageunit,'(A,I4,A,I4,A,I4,A,I8)')
     +'OAD: the_main_loop checkpoints allowed 1st:' ,nCheckLev1,
     +', 2nd:', nCheckLev2,
     +', 3rd:', nCheckLev3,
     +', 4th:', nCheckLev4

cju in this current scheme
cju there is no point in  specifying 1 as the number of checkpoints
cju because for now don't avoid the incidental store of
cju the same checkpoint across multiple levels.
      if (nCheckLev4 .eq. 1 ) then
         uCheckLev4=0
         write(standardmessageunit,'(A)')
     +'OAD: NOTE: resetting level 4 to 0'
      else
         uCheckLev4=nCheckLev4
      end if
      if (nCheckLev3 .eq. 1 ) then
         uCheckLev3=0
         write(standardmessageunit,'(A)')
     +'OAD: NOTE: resetting level 3 to 0'
      else
         uCheckLev3=nCheckLev3
      end if
      if (nCheckLev2 .eq. 1 ) then
         uCheckLev2=0
         write(standardmessageunit,'(A)')
     +'OAD: NOTE: resetting level 2 to 0'
      else
         uCheckLev2=nCheckLev2
      end if
      if (nCheckLev1 .eq. 1 ) then
         uCheckLev1=0
         write(standardmessageunit,'(A)')
     +'OAD: NOTE: resetting level 1 to 0'
      else
         uCheckLev1=nCheckLev1
      end if
cju check sanity
      if (uCheckLev4 .gt. 0 ) then
        if ( uCheckLev3*uCheckLev2*uCheckLev1 .eq. 0) then
           write(errormessageunit,'(A)')
     +'OAD: ERROR: uCheckLev4 > 0 implies ',
     +'all lower levels must be nonzero too.'
        end if
      end if
      if (uCheckLev3 .gt. 0 ) then
        if ( uCheckLev2*uCheckLev1 .eq. 0) then
           write(errormessageunit,'(A)')
     +'OAD: ERROR: uCheckLev3 > 0 implies ',
     +'all lower levels must be nonzero too.'
        end if
      end if
      if (uCheckLev2 .gt. 0 ) then
        if ( uCheckLev1 .eq. 0) then
           write(errormessageunit,'(A)')
     +'OAD: ERROR: uCheckLev2 > 0 implies ',
     +'all lower levels must be nonzero too.'
        end if
      end if
      if (uCheckLev1 .eq. 0 ) then
         write(errormessageunit,'(A)')
     +'OAD: ERROR: uCheckLev1 must be nonzero '
      end if

cju uCheckLev1
      allStepsCovered = .false.
      if (nTimeSteps .lt. uCheckLev1 ) then
         uCheckLev1=nTimeSteps
         allStepsCovered =.true.
      end if

cju uCheckLev2
      if (.NOT. allStepsCovered )  then
         if (MOD(nTimeSteps,uCheckLev1).gt.0)  then
            if((nTimeSteps/uCheckLev1)+1 .le. uCheckLev2 ) then
               uCheckLev2=(nTimeSteps/uCheckLev1)+1
               allStepsCovered =.true.
            end if
         else
            if((nTimeSteps/uCheckLev1) .le. uCheckLev2 ) then
               uCheckLev2=(nTimeSteps/uCheckLev1)
               allStepsCovered =.true.
            end if
         end if
      else
         uCheckLev2 = 0
      end if

cju uCheckLev3
      if (.NOT. allStepsCovered )  then
         if (MOD(nTimeSteps,uCheckLev1*uCheckLev2).gt.0)  then
            if((nTimeSteps/(uCheckLev1*uCheckLev2))+1
     +           .le. uCheckLev3 ) then
               uCheckLev3=(nTimeSteps/(uCheckLev1*uCheckLev2))+1
               allStepsCovered =.true.
            end if
         else
           if((nTimeSteps/(uCheckLev1*uCheckLev2)) .le. uCheckLev3 )then
               uCheckLev3=(nTimeSteps/(uCheckLev1*uCheckLev2))
               allStepsCovered =.true.
            end if
         end if
      else
         uCheckLev3 = 0
      end if

cju uCheckLev4
      if (.NOT. allStepsCovered )  then
         if (MOD(nTimeSteps,uCheckLev1*uCheckLev2*uCheckLev3)
     +        .gt.0)  then
            if((nTimeSteps/(uCheckLev1*uCheckLev2*uCheckLev3))+1
     +           .le. uCheckLev4 ) then
               uCheckLev4=
     +              (nTimeSteps/(uCheckLev1*uCheckLev2*uCheckLev3))+1
               allStepsCovered =.true.
            end if
         else
            if((nTimeSteps/(uCheckLev1*uCheckLev2*uCheckLev3))
     +           .le. uCheckLev4 ) then
               uCheckLev4=
     +              (nTimeSteps/(uCheckLev1*uCheckLev2*uCheckLev3))
               allStepsCovered =.true.
            end if
         end if
      else
         uCheckLev4 = 0
      end if

      if (.NOT. allStepsCovered) then
         write(errormessageunit,'(A)')
     +'OAD: ERROR: not enough checkpoints allowed'
      end if

      write(standardmessageunit,'(A,I6)')
     +'OAD: timesteps required:',ntimesteps
      write(standardmessageunit,'(4(A,I4))')
     +'OAD: the_main_loop checkpoints used    1st:',uCheckLev1,
     +', 2nd:', uCheckLev2,
     +', 3rd:', uCheckLev3,
     +', 4th:', uCheckLev4

      END

