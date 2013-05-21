!> \mainpage
!! This is a Fortran9X adaptation of the functionality of Revolve; see Alg. 799 published as \cite Griewank2000ARA .
!! The interface of the routines differs from the cited revolve implementation
!! found in Adol-C and has been designed to be more in line with the
!! Fortran 9X language features. A minor extension is the  optional `bundle` parameter that allows to treat as many loop
!! iterations in one tape/adjoint sweep. If `bundle` is 1, the default, then the behavior is that of Alg. 799.
!!
!! The implementation (written by J. Utke)  is contained in revolve.f90, the use is illustrated in the `Examples` directory.
!!
!! The mercurial repository with the latest version can be found at:
!! <a href="http://mercurial.mcs.anl.gov/ad/RevolveF9X">http://mercurial.mcs.anl.gov/ad/RevolveF9X</a>
!!


!> the module containing the revolve implementation
!!
MODULE revolve
  IMPLICIT NONE

  PUBLIC :: rvInit, rvVerbose, rvNextAction, &
rvGuess, rvFactor, & 
rvStore, rvRestore, &
rvForward, rvFirstUTurn, rvUTurn, rvDone, &
rvError, rvAdjust

  PRIVATE :: & 
ourSteps, ourACP, ourCStart, ourCEnd, ourVerbosity, &
ourNumFwd , ourNumInv, ourNumStore, ourRWCP, ourPrevCEnd, &
ourFirstUTurned, chkRange, forwdCount

  !> store a checkpoint now
  !! equivalent to TAKESHOT in Alg. 799
  INTEGER, PARAMETER :: rvStore      =1 

  !> restore a checkpoint now
  !! equivalent to RESTORE in Alg. 799
  INTEGER, PARAMETER :: rvRestore    =2

  !> execute iteration(s) forward
  !! equivalent to ADVANCE in Alg. 799
  INTEGER, PARAMETER :: rvForward    =3

  !> tape iteration(s); optionally leave to return later;  and (upon return) do the adjoint(s)
  !! equivalent to FIRSTTURN in Alg. 799
  INTEGER, PARAMETER :: rvFirstUTurn =4

  !> tape iteration(s) and do the adjoint(s)
  !! equivalent to YOUTURN in Alg. 799
  INTEGER, PARAMETER :: rvUTurn      =5

  !> we are done with adjoining the loop
  !! equivalent to the `terminate` enum value in Alg. 799
  INTEGER, PARAMETER :: rvDone       =6

  !> an error has occurred
  !! equivalent to the `error` enum value in Alg. 799;
  !! see also `errorMsg` in \ref rvAction
  INTEGER, PARAMETER :: rvError      =7

  !> this encapsulates all the information needed to perfrom the correct action
  !! an instance is returned from \ref rvNextAction
  TYPE rvAction
     !> the action that is to be implemented, termination, or error;
     !! the value must be one of:
     !! `rvStore`, `rvRestore`, `rvForward`,
     !! `rvFirstUTurn`, `rvUTurn`, `rvDone`, `rvError`
     INTEGER :: actionFlag = 0

     !> assumptions:
     !! - the loop iterations are numbered in range [0,`ourSteps`-1]
     !! - the model state is the input to the iteration numbered `startIteration`
     !!
     !! the interpretation is as follows based on the value of `actionFlag`:
     !! - `rvForward`: execute iterations as the loop: `do currentIteration=startIteration, iteration-1`
     !! - `rvRestore`: restores model state at `iteration` (here it has the same value as `startIteration`)
     !! - `rvFirstUTurn`/`rvUTurn`: tape iterations in loop: do currentIteration=startIteration, iteration-1`
     !!    followed by adjoint sweep over iterations in loop: do currentIteration=iteration-1,startIteration,-1
     !!
     !! for all other values of `actionFlag` the value of `iteration` is meaningless
     INTEGER :: iteration  = 0

     !> assuming the loop iterations are in [0,ourSteps-1] and `currentIteration` variable is maintained,
     !! the interpretation is as follows based on the value of `actionFlag`:
     !! - `rvForward`: execute iterations as the loop: `do currentIteration, iteration-1`
     !! - `rvRestore`: set `currentIteration=iteration`
     !!
     !! for all other values of `actionFlag` the value of `iteration` is meaningless
     INTEGER :: startIteration = 0

     !> the checkpoint number to be stored to restored
     !! the value is meaninfull only if `actionFlag` is set to `rvStore` or `rvRestore`;
     !!
     !! This is approximately equivalent to `checks` in Alg. 799.
     INTEGER :: cpNum      = 0

     !> if an error has occurred `actionFlag` will be set to `rvError` and this will contain an error message
     CHARACTER(80) :: errorMsg
  END TYPE rvAction
  
  !> the number of iteration steps; set by calling \ref rvInit; not supposed to be set/used directly by the user;
  !! note that the iterations are expected to range in [0, ourSteps-1];
  !!
  !! equivalent to `steps` in Alg. 799
  INTEGER :: ourSteps    = 0 ! number of steps

  !> the number of iterations that may be bundled for a taping/adjoining sweep;
  !! set by calling \ref rvInit; not supposed to be set/used directly by the user;
  !!
  !! the default is 1 loop iteration which makes it equivalent to Alg. 799
  INTEGER :: ourBundle   = 1

  !> the number of iterations in the last bundle
  !! set by calling \ref rvInit; not supposed to be set/used directly by the user;
  !!
  !! the default is 1 (for `ourBundle` = 1)  which makes it equivalent to Alg. 799
  INTEGER :: ourTail   = 1

  !> the number of checkpoints (ACP=AllowedCheckPoints) that can be stored at any time during the loop execution
  !! set by calling \ref rvInit; not supposed to be set/used directly by the user
  !!
  !! equivalent to `snaps` in Alg. 799
  INTEGER :: ourACP      = 0

  !> current subrange start;
  !! not to be set/referemced directly by the user
  !!
  !! approximately equivalent to `capo` in Alg. 799
  INTEGER :: ourCStart   = 0

  !> current subrange end;
  !! not to be set/referemced directly by the user
  !!
  !! approximately equivalent to `fine` in Alg. 799
  INTEGER :: ourCEnd     = 0

  !> count of the forward steps; diagnostic only
  INTEGER :: ourNumFwd   = 0

  !> count of invocations to \ref rvNextAction ;  diagnostic only
  INTEGER :: ourNumInv   = 0

  !> count of checkpoint stores; diagnostic only
  INTEGER :: ourNumStore = 0

  !> checkpoint currently (re)stored - the first checkpoint is numbered 0;
  !! not to be set/referemced directly by the user
  INTEGER :: ourRWCP     = -1

  !> previous subrange end;
  !! not to be set/referemced directly by the user
  INTEGER :: ourPrevCEnd = 0

  !> have we first uturned already?;
  !! not to be set/referemced directly by the user
  LOGICAL :: ourFirstUturned = .FALSE.

  !> vector of step numbers indexed by checkpoint;
  !! not to be set/referemced directly by the user
  INTEGER, DIMENSION(:), ALLOCATABLE :: ourStepOf

  !> for debugging purposes; values imply:
  !! - 0 includes errors
  !! - 1 includes summary info
  !! - 2 includes iterations with checkpoints stored
  !! - 3 includes all action results
  !!
  !! set via \ref rvVerbose
  INTEGER :: ourVerbosity = 0

CONTAINS

!--------------------------------------------------------------------*

  !> method to initialize the internal state; must be called before any call to \ref rvNextAction
  !! @param steps  the total number of steps in the iteration; equivalent to `steps` in Alg. 799
  !! @param checkpoints the total number of checkpoints allowed to be stored at any time; equivalent to `snaps` in Alg. 799
  !! @param errorMsg set when an error condition occurs; else set to `"none"`
  !! @param anActionInstance  if supplied initializes its contents
  !! @param bundle if supplied initializes `ourBundle`
  !! @return `.true.` if successfull, else `.false.` ansd `errorMsg` will be set
  FUNCTION rvInit(steps,checkpoints,errorMsg,anActionInstance,bundle)
    IMPLICIT NONE
    LOGICAL :: rvInit
    INTEGER, INTENT(IN) :: steps
    INTEGER, INTENT(IN) :: checkpoints
    CHARACTER(*), INTENT(OUT) :: errorMsg
    type(rvAction), optional :: anActionInstance
    INTEGER, INTENT(IN), optional :: bundle
    INTEGER :: predFwdCnt ! predicted forward count 
    rvInit = .TRUE.
    errorMsg ='none'
    IF (present(anActionInstance)) THEN
       ! same as default init above
       anActionInstance%actionFlag = 0
       anActionInstance%iteration  = 0
       anActionInstance%cpNum      = 0
    END IF
    IF (present(bundle)) THEN
       ourBundle = bundle
    END IF
    IF (ourBundle<1 .OR. ourBundle>steps) THEN
       rvInit=.FALSE.
       errorMsg = "revolve::rvInit: bundle parameter out of range [1,steps]"
    ELSEIF (steps<0) THEN
       rvInit=.FALSE.
       errorMsg = 'revolve::rvInit: negative steps'
    ELSEIF (checkpoints<0) THEN
       rvInit=.FALSE.
       errorMsg = 'revolve::rvInit: negative checkpoints'
    ELSE 
       ourCStart       = 0
       ourSteps        = steps
       IF (ourBundle .gt. 1) THEN
          ourTail=modulo(ourSteps,ourBundle)
          ourSteps=ourSteps/ourBundle
          IF (ourTail>0) THEN
             ourSteps=ourSteps+1
          ELSE
             ourTail=ourBundle
          END IF
       END IF
       ourCEnd         = ourSteps
       ourACP          = checkpoints
       ourNumFwd       = 0 
       ourNumInv       = 0 
       ourNumStore     = 0 
       ourRWCP         = -1 
       ourPrevCEnd     = 0 
       ourFirstUTurned = .FALSE.

       IF (ALLOCATED(ourStepOf)) THEN
          DEALLOCATE(ourStepOf)
       END IF
       IF(.NOT.ALLOCATED(ourStepOf)) THEN
          ALLOCATE(ourStepOf(0:ourACP))
       END IF

       IF (ourVerbosity>0) THEN
          predFwdCnt = forwdCount(steps,ourACP,ourBundle)
          IF (predFwdCnt==-1) THEN
             errorMsg='revolve::rvInit: error returned by  revolve::forwdCount'
             rvInit=.FALSE.
             RETURN
          ELSE
             WRITE (*,'(A)') 'prediction:'
             WRITE (*,'(A,I7)')   ' overhead forward steps : ', predFwdCnt
             WRITE (*,'(A,F8.4)') ' overhead factor        : ', dble(predFwdCnt)/(steps)
          END IF
       END IF
    END IF
  END FUNCTION rvInit

!--------------------------------------------------------------------*

  !> method to change the internal state for the total number of steps/checkpoints; must be called after \ref rvInit
  !! @param steps  the total number of steps in the iteration; equivalent to `steps` in Alg. 799
  !! @param errorMsg set when an error condition occurs; else set to `"none"`
  !! @return `.true.` if successfull, else `.false.` ansd `errorMsg` will be set
  FUNCTION rvAdjust(steps,checkpoints,errorMsg)
    IMPLICIT NONE
    LOGICAL :: rvAdjust
    INTEGER, INTENT(IN) :: steps
    INTEGER, INTENT(IN) :: checkpoints
    CHARACTER(*), INTENT(OUT) :: errorMsg
    rvAdjust=.false.
  END FUNCTION

!--------------------------------------------------------------------*

  !> method to set the verbosity to a level in [0-3] as described for `ourVerbosity`
  SUBROUTINE rvVerbose(level)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: level 
    ourVerbosity=level
  END SUBROUTINE rvVerbose

!--------------------------------------------------------------------*
  !> the method to determine the next action; to be called in an unbound loop after \ref rvInit
  !! @return an instance of `rvAction` set to describe the next action (see the member documentation);
  !!
  !! this method modifies the internal state; it is approximately equivalent to the method `revolve` in Alg. 799
  FUNCTION rvNextAction()
    IMPLICIT NONE
    REAL :: bino1, bino2, bino3, bino4, bino5

    !> available checkpoint slots
    INTEGER :: availCP

    !> local copy of previous subrange start
    INTEGER :: prevCStart

    INTEGER :: range
    INTEGER :: reps
    INTEGER :: i 
    LOGICAL :: rwcpTest
    type(rvAction) :: rvNextAction
    IF (ourNumInv==0) THEN
       ! first invocation
       DO i = 0, ourACP
          ourStepOf(i) = 0
       END DO
       ourStepOf(0) = ourCStart - 1
    END IF
    prevCStart = ourCStart
    ourNumInv = ourNumInv + 1
    rwcpTest=(ourRWCP==(-1))
    IF (.not. rwcpTest) THEN 
       rwcpTest=(ourStepOf(ourRWCP)/=ourCStart)
    END IF
    IF ((ourCEnd-ourCStart)==0) THEN
       ! nothing in current subrange
       IF ((ourRWCP==(-1)) .OR. (ourCStart==ourStepOf(0))) THEN
          ! we are done
          ourRWCP = ourRWCP - 1
          IF (ourVerbosity>2) THEN
             WRITE (*,FMT='(A)') ' done'
          END IF
          IF (ourVerbosity>0) THEN
             WRITE (*,'(A)') 'summary:'
             WRITE (*,'(A,I8)') ' overhead forward steps:', ourNumFwd
             WRITE (*,'(A,I8)') ' CP stores             :', ourNumStore
             WRITE (*,'(A,I8)') ' rvNextAction calls    :', ourNumInv
          END IF
          rvNextAction%actionFlag = rvDone
        ELSE
           ourCStart = ourStepOf(ourRWCP)
           ourPrevCEnd = ourCEnd
           rvNextAction%actionFlag = rvRestore 
        END IF
     ELSE IF ((ourCEnd-ourCStart)==1) THEN
        ourCEnd = ourCEnd - 1
        ourPrevCEnd = ourCEnd
        IF ((ourRWCP>=0) .AND. (ourStepOf(ourRWCP)==ourCStart)) ourRWCP = ourRWCP - 1
        IF (.NOT.ourFirstUTurned) THEN
           rvNextAction%actionFlag = rvFirstUTurn
           ourFirstUTurned = .TRUE.
        ELSE
           rvNextAction%actionFlag = rvUTurn
        END IF
     ELSE IF (rwcpTest) THEN
        ourRWCP = ourRWCP + 1
        IF (ourRWCP+1>ourACP) THEN
           rvNextAction%actionFlag = rvError
           rvNextAction%errorMsg='revolve::rvNextAction: insufficient allowed checkpoints'
           RETURN
        ELSE
           ourStepOf(ourRWCP) = ourCStart
           ourNumStore = ourNumStore + 1
           ourPrevCEnd = ourCEnd
           rvNextAction%actionFlag = rvStore
        END IF
     ELSE IF ((ourPrevCEnd<ourCEnd) .AND. (ourACP==ourRWCP+1)) THEN
        rvNextAction%actionFlag = rvError
        rvNextAction%errorMsg='revolve::rvNextAction: insufficient allowed checkpoints'
     ELSE
        availCP = ourACP - ourRWCP
        IF (availCP<1) THEN
           rvNextAction%actionFlag = rvError
           rvNextAction%errorMsg='revolve::rvNextAction: insufficient allowed checkpoints'
        ELSE
           reps = 0
           range = 1
           DO WHILE (range<ourCEnd-ourCStart)
              reps = reps + 1
              range = range*(reps+availCP)/reps
           END DO
           bino1 = range*reps/(availCP+reps)
           IF (availCP>1) THEN
            bino2 = bino1*availCP/(availCP+reps-1)
          ELSE
            bino2 = 1
          END IF
          IF (availCP==1) THEN
            bino3 = 0
          ELSE IF (availCP>2) THEN
            bino3 = bino2*(availCP-1)/(availCP+reps-2)
          ELSE
            bino3 = 1
          END IF
          bino4 = bino2*(reps-1)/availCP
          IF (availCP<3) THEN
            bino5 = 0
          ELSE IF (availCP>3) THEN
            bino5 = bino3*(availCP-1)/reps
          ELSE
            bino5 = 1
          END IF
          IF (ourCEnd-ourCStart<=bino1+bino3) THEN
            ourCStart = int(ourCStart + bino4)
          ELSE IF (ourCEnd-ourCStart>=range-bino5) THEN
            ourCStart = int(ourCStart + bino1)
          ELSE
            ourCStart = int(ourCEnd - bino2 - bino3)
          END IF
          IF (ourCStart==prevCStart) THEN
            ourCStart = prevCStart + 1
          END IF
          IF (ourCStart==ourSteps) THEN
             ourNumFwd = ourNumFwd + ((ourCStart-1) - prevCStart)*ourBundle + ourTail
          ELSE
             ourNumFwd = ourNumFwd + (ourCStart - prevCStart)*ourBundle
          END IF
          rvNextAction%actionFlag = rvForward
        END IF
      END IF
      rvNextAction%startIteration=prevCStart*ourBundle
      IF (rvNextAction%actionFlag==rvFirstUTurn) THEN
         rvNextAction%iteration=(ourCStart)*ourBundle+ourTail
      ELSE IF (rvNextAction%actionFlag==rvUTurn) THEN
         rvNextAction%iteration=(ourCStart+1)*ourBundle
      ELSE
         rvNextAction%iteration=(ourCStart)*ourBundle
      END IF
      IF (rvNextAction%actionFlag /= rvError) THEN
         IF (ourVerbosity>2) THEN
            SELECT CASE( rvNextAction%actionFlag)
            CASE (rvForward)
               WRITE (*,FMT='(A,I8,A,I8,A)') ' run forward iterations    [', &
               rvNextAction%startIteration, ',', rvNextAction%iteration-1,']'
            CASE (rvRestore)
               WRITE (*,FMT='(A,I8)')        ' restore input of iteration ',&
               rvNextAction%iteration
            CASE (rvFirstUTurn)
               WRITE (*,FMT='(A,I8,A,I8,A)') ' 1st uturn for iterations  [',&
               rvNextAction%startIteration, ',', rvNextAction%iteration-1,']'
            CASE(rvUTurn)      
               WRITE (*,FMT='(A,I8,A,I8,A)') ' uturn for iterations      [',&
               rvNextAction%startIteration, ',', rvNextAction%iteration-1,']'
            END SELECT
         END IF
         IF ((ourVerbosity>1) .AND. (rvNextAction%actionFlag == rvStore)) THEN
                WRITE (*,FMT='(A,I8)')        ' store input of iteration   ',&
                rvNextAction%iteration
         END IF
      END IF
      rvNextAction%cpNum=ourRWCP
    END FUNCTION rvNextAction

!--------------------------------------------------------------------*
    !> estimates the number of checkpoints required; equivalent to `adjust` in Alg. 799
    !! @param steps is the number of iterations
    !! @param bundle is optional; detaults to 1, if specified indicates the number of iterations bundled in one tape/adjoint sweep
    !! @return the number of checkpoints such that the growth in spatial complexity is balanced with the  growth in temporal complexity
    !!
    !! this method does not change the internal state and does not require \ref rvInit
    FUNCTION rvGuess(steps,bundle)
    IMPLICIT NONE
      INTEGER, INTENT(IN) :: steps, bundle
      OPTIONAL :: bundle
      INTEGER :: reps, s, checkpoints, b, tail, bSteps
      INTEGER :: rvGuess
      b=1
      bSteps=steps
      IF (present(bundle)) THEN
         b=bundle
      END IF
      IF (steps<1) THEN
        WRITE (*,fmt=*) 'revolve::rvGuess: error: steps < 1'
        rvGuess = -1
      ELSE IF (b<1) THEN
        WRITE (*,fmt=*) 'revolve::rvGuess: error: bundle < 1'
        rvGuess = -1
      ELSE
        IF (b .gt. 1) THEN
          tail=modulo(bSteps,b)
          bSteps=bSteps/b
          IF (tail>0) THEN
            bSteps=bSteps+1
          END IF
        END IF
        IF (bSteps==1) THEN
          rvGuess=0
        ELSE
          checkpoints = 1
          reps = 1
          s = 0
          DO WHILE (chkRange(checkpoints+s,reps+s)>bSteps)
            s = s - 1
          END DO
          DO WHILE (chkRange(checkpoints+s,reps+s)<bSteps)
            s = s + 1
          END DO
          checkpoints = checkpoints + s
          reps = reps + s
          s = -1
          DO WHILE (chkRange(checkpoints,reps)>=bSteps)
            IF (checkpoints>reps) THEN
              checkpoints = checkpoints - 1
              s = 0
            ELSE
              reps = reps - 1
              s = 1
            END IF
          END DO
          IF (s==0) THEN
            checkpoints = checkpoints + 1
          END IF
          IF (s==1) reps = reps + 1
          rvGuess = checkpoints
        END IF
      END IF
    END FUNCTION rvGuess

!--------------------------------------------------------------------*
    !> computes the run time overhead factor; equivalent to `expense` in Alg. 799
    !! @param steps is the number of iterations
    !! @param checkpoints is the number of allowed checkpoints
    !! @param bundle is optional; detaults to 1, if specified indicates the number of iterations bundled in one tape/adjoint sweep
    !! @return the estimated runtime overhead factor (does not account for the time needed to write checkpoints)
    !!
    !! this method does not change the internal state and does not require \ref rvInit
    FUNCTION rvFactor(steps,checkpoints,bundle)
    IMPLICIT NONE
      INTEGER, INTENT(IN) :: checkpoints, steps, bundle
      OPTIONAL :: bundle
      INTEGER :: b, f
      DOUBLE PRECISION :: rvFactor
      b=1
      IF (present(bundle)) THEN
         b=bundle
      END IF
      f=forwdCount(steps,checkpoints,b)
      IF (f==-1)  THEN
        WRITE (*,fmt=*) 'revolve::rvFactor: error returned by  revolve::forwdCount'
        rvFactor=-1
      ELSE
        rvFactor = dble(f)/steps
      END IF
    END FUNCTION rvFactor

!--------------------------------------------------------------------*
    !> internal method not to be referenced by the user
    FUNCTION chkRange(ss,tt)
    IMPLICIT NONE
      INTEGER :: ss, tt
      DOUBLE PRECISION :: res
      INTEGER :: i
      INTEGER :: chkRange
      res = 1.
      IF (tt<0 .OR. ss<0) THEN
        WRITE (*,fmt=*) 'revolve::chkRange: error: negative parameter '
        chkRange = -1
      ELSE
        DO i = 1, tt
          res = res*(ss+i)
          res = res/i
          IF (res>huge(chkrange)) EXIT
        END DO
        IF (res<huge(chkrange)) THEN
          chkRange = int(res)
        ELSE
          chkRange = huge(chkrange)
          WRITE (*,fmt=*) 'revolve::chkRange: warning: returning maximal integer ',&
          chkRange
        END IF
      END IF
    END FUNCTION chkRange

!--------------------------------------------------------------------*

    !> internal method not to be referenced by the user;
    !> predicts the  number of recomputation-from-checkpoint forwards steps (overhead)
    FUNCTION forwdCount(steps,checkpoints,bundle)
    IMPLICIT NONE
      INTEGER, INTENT(IN) :: checkpoints, steps, bundle
      INTEGER :: range, reps,s,tail
      INTEGER :: forwdCount
      IF (checkpoints<0) THEN
        WRITE (*,fmt=*) 'revolve::forwdCount: error: checkpoints < 0'
        forwdCount = -1
      ELSE IF (steps<1) THEN
        WRITE (*,fmt=*) 'revolve::forwdCount: error: steps < 1'
        forwdCount = -1
      ELSE IF (bundle<1) THEN
        WRITE (*,fmt=*) 'revolve::forwdCount: error: bundle < 1'
        forwdCount = -1
      ELSE
        s=steps
        IF (bundle .gt. 1) THEN
          tail=modulo(s,bundle)
          s=s/bundle
          IF (tail>0) THEN
            s=s+1
          END IF
        END IF
        IF (s==1) THEN
          forwdCount = 0
        ELSE IF (checkpoints==0) THEN
          WRITE (*,fmt=*) &
          'revolve::forwdCount: error: given inputs require checkpoints>0'
          forwdCount = -1
        ELSE
          reps = 0
          range = 1
          DO WHILE (range<s)
            reps = reps + 1
            range = range*(reps+checkpoints)/reps
          END DO
          forwdCount = (reps*s - range*reps/(checkpoints+1))*bundle
        END IF
      END IF
    END FUNCTION forwdCount

!--------------------------------------------------------------------*

END MODULE revolve
