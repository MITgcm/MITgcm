MODULE revolve
  IMPLICIT NONE

  PUBLIC :: rvInit, rvVerbose, rvNextAction, rvGuess, rvFactor, & 
rvStore, rvRestore, rvForward, rvFirstUTurn, rvUTurn, rvDone, rvError

  PRIVATE :: & 
ourSteps, ourACP, ourCStart, ourCEnd, ourVerbosity, &
ourNumFwd , ourNumInv, ourNumStore, ourRWCP, ourPrevCEnd, ourFirstUTurned, &  
chkRange, forwdCount

  ! possible actions
  INTEGER, PARAMETER :: rvStore      =1 
  INTEGER, PARAMETER :: rvRestore    =2
  INTEGER, PARAMETER :: rvForward    =3
  INTEGER, PARAMETER :: rvFirstUTurn =4
  INTEGER, PARAMETER :: rvUTurn      =5
  INTEGER, PARAMETER :: rvDone       =6
  INTEGER, PARAMETER :: rvError      =7

  TYPE rvAction
     INTEGER :: actionFlag = 0
     INTEGER :: iteration  = 0
     INTEGER :: cpNum      = 0
     CHARACTER, dimension(80) :: errorMsg 
  END TYPE rvAction
  
  INTEGER :: ourSteps    = 0 ! number of steps
  INTEGER :: ourACP      = 0 ! allowed number of checkpoints
  INTEGER :: ourCStart   = 0 ! current subrange start
  INTEGER :: ourCEnd     = 0 ! current subrange end
  INTEGER :: ourNumFwd   = 0 ! count of forward steps 
  INTEGER :: ourNumInv   = 0 ! count of invocations of rvNextAction 
  INTEGER :: ourNumStore = 0 ! number of stored checkpoints
  INTEGER :: ourRWCP     = -1! checkpoint currently (re)stored (first checkpoint is 0)
  INTEGER :: ourPrevCEnd = 0 ! previous subrange end
  LOGICAL :: ourFirstUturned = .FALSE. ! have we first for the first time 
  ! vector of step numbers indexed by checkpoint 
  INTEGER, DIMENSION(:), ALLOCATABLE :: ourStepOf

  ! for debugging purposes:
  ! 0 includes errors 
  ! 1 includes summary info
  ! 2 includes iterations with checkpoints stored 
  ! 3 includes all action results
  INTEGER :: ourVerbosity = 0

CONTAINS

!--------------------------------------------------------------------*

  FUNCTION rvInit(steps,checkpoints,errorMsg,anActionInstance)
    IMPLICIT NONE
    LOGICAL :: rvInit
    INTEGER, INTENT(IN) :: steps
    INTEGER, INTENT(IN) :: checkpoints
    CHARACTER ,dimension(:), INTENT(OUT) :: errorMsg
    type(rvAction), optional :: anActionInstance
    INTEGER :: predFwdCnt ! predicted forward count 
    rvInit = .TRUE.
    errorMsg ='none'
    IF (present(anActionInstance)) THEN
       ! same as default init above
       anActionInstance%actionFlag = 0
       anActionInstance%iteration  = 0
       anActionInstance%cpNum      = 0
    END IF
    IF (steps<0 .OR. checkpoints<0) THEN
       rvInit=.FALSE.
       errorMsg = 'revolve::rvInit: negative steps or checkpoints'
    ELSE 
       ourCStart       = 0
       ourSteps        = steps
       ourCEnd         = steps
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
          predFwdCnt = forwdCount(ourCEnd-ourCStart,ourACP)
          IF (predFwdCnt==-1) THEN
             errorMsg='error in forwdCount'
             RETURN
          ELSE
             WRITE (*,'(A)') 'prediction:'
             WRITE (*,'(A,I7)') ' needed forward steps: ', predFwdCnt
             WRITE (*,'(A,F8.4)') ' slowdown factor     : ', dble(predFwdCnt)/(ourCEnd-ourCStart)
          END IF
       END IF
    END IF
  END FUNCTION rvInit

!--------------------------------------------------------------------*

  SUBROUTINE rvVerbose(level)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: level 
    ourVerbosity=level
  END SUBROUTINE rvVerbose

!--------------------------------------------------------------------*

  FUNCTION rvNextAction()
    IMPLICIT NONE
    REAL :: bino1, bino2, bino3, bino4, bino5
    INTEGER :: availCP    ! available checkpoint slots 
    INTEGER :: prevCStart ! previous subrange start
    INTEGER :: range      ! 
    INTEGER :: reps
    INTEGER :: i 
    type(rvAction) :: rvNextAction
    IF (ourNumInv==0) THEN
       ! first invocation
       DO i = 0, ourACP
          ourStepOf(i) = 0
       END DO
       ourStepOf(0) = ourCStart - 1
    END IF
    ourNumInv = ourNumInv + 1
    IF ((ourCEnd-ourCStart)==0) THEN
       ! nothing in current subrange
       IF ((ourRWCP==(-1)) .OR. (ourCStart==ourStepOf(0))) THEN
          ! we are done
          ourRWCP = ourRWCP - 1
          IF (ourVerbosity>0) THEN
             WRITE (*,'(A)') 'summary:'
             WRITE (*,'(A,I8)') ' forward steps:', ourNumFwd
             WRITE (*,'(A,I8)') ' CP stores    :', ourNumStore
             WRITE (*,'(A,I8)') ' invocations  :', ourNumInv
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
     ELSE IF ((ourRWCP==(-1)) .OR. (ourStepOf(ourRWCP)/=ourCStart)) THEN
        ourRWCP = ourRWCP + 1
        IF (ourRWCP+1>ourACP) THEN
           rvNextAction%actionFlag = rvError
           rvNextAction%errorMsg='insufficient allowed checkpoints'
           RETURN
        ELSE
           ourStepOf(ourRWCP) = ourCStart
           ourNumStore = ourNumStore + 1
           ourPrevCEnd = ourCEnd
           rvNextAction%actionFlag = rvStore
        END IF
     ELSE IF ((ourPrevCEnd<ourCEnd) .AND. (ourACP==ourRWCP+1)) THEN
        rvNextAction%actionFlag = rvError
        rvNextAction%errorMsg='insufficient allowed checkpoints'
     ELSE
        prevCStart = ourCStart
        availCP = ourACP - ourRWCP
        IF (availCP<1) THEN
           rvNextAction%actionFlag = rvError
           rvNextAction%errorMsg='insufficient allowed checkpoints'
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
            ourCStart = ourCStart + bino4
          ELSE IF (ourCEnd-ourCStart>=range-bino5) THEN
            ourCStart = ourCStart + bino1
          ELSE
            ourCStart = ourCEnd - bino2 - bino3
          END IF
          IF (ourCStart==prevCStart) THEN
            ourCStart = prevCStart + 1
          END IF
          ourNumFwd = ourNumFwd + ourCStart - prevCStart
          rvNextAction%actionFlag = rvForward
        END IF
      END IF
      rvNextAction%iteration=ourCStart 
      IF (rvNextAction%actionFlag /= rvError .AND. rvNextAction%actionFlag /= rvDone) THEN
         IF (ourVerbosity>2) THEN
            SELECT CASE( rvNextAction%actionFlag)
            CASE (rvForward)
               WRITE (*,FMT='(A)',ADVANCE='NO') ' forward to  :'
            CASE (rvRestore)
               WRITE (*,FMT='(A)',ADVANCE='NO') ' restore at  :'
            CASE (rvFirstUTurn)
               WRITE (*,FMT='(A)',ADVANCE='NO') ' 1st uturn at:'
            CASE(rvUTurn)      
               WRITE (*,FMT='(A)',ADVANCE='NO') ' uturn at    :'
            END SELECT
         END IF
         IF (ourVerbosity>1) THEN
            IF (rvNextAction%actionFlag == rvStore) THEN 
               WRITE (*,FMT='(A)',ADVANCE='NO') ' store at    :'
            END IF
            WRITE (*,'(I8)') rvNextAction%iteration
         END IF
      END IF
      rvNextAction%cpNum=ourRWCP
    END FUNCTION rvNextAction

!--------------------------------------------------------------------*

    FUNCTION rvGuess(steps)
    IMPLICIT NONE
      INTEGER :: steps
      INTEGER :: reps, s, checkpoints
      INTEGER :: rvGuess
      checkpoints = 1
      reps = 1
      s = 0
      DO WHILE (chkRange(checkpoints+s,reps+s)>steps)
        s = s - 1
      END DO
      DO WHILE (chkRange(checkpoints+s,reps+s)<steps)
        s = s + 1
      END DO
      checkpoints = checkpoints + s
      reps = reps + s
      s = -1
      DO WHILE (chkRange(checkpoints,reps)>=steps)
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
    END FUNCTION rvGuess

!--------------------------------------------------------------------*

    FUNCTION rvFactor(steps,checkpoints)
    IMPLICIT NONE
      INTEGER :: checkpoints, steps
      DOUBLE PRECISION :: rvFactor
      IF (checkpoints<1) THEN
        WRITE (*,fmt=*) 'error occurs in RVFACTOR: CHECKPOINTS < 1'
        rvFactor = -1
      ELSE IF (checkpoints<1) THEN
        WRITE (*,fmt=*) 'error occurs in RVFACTOR: CHECKPOINTS < 1'
        rvFactor = -1
      ELSE
        rvFactor = dble(forwdCount(steps,checkpoints))
        IF (rvFactor/=-1) rvFactor = rvFactor/steps
      END IF
    END FUNCTION rvFactor

!--------------------------------------------------------------------*

    FUNCTION chkRange(ss,tt)
    IMPLICIT NONE
      INTEGER :: ss, tt
      DOUBLE PRECISION :: res
      INTEGER :: i
      INTEGER :: chkRange
      res = 1.
      IF (tt<0 .OR. ss<0) THEN
        WRITE (*,fmt=*) 'error in MAXRANGE: negative parameter '
        chkRange = -1
      ELSE
        DO i = 1, tt
          res = res*(ss+i)
          res = res/i
          IF (res>=2.0D0**31) EXIT
        END DO
        IF (res<2.0D0**31-2) THEN
          chkRange = res
        ELSE
          chkRange = 2.0D0**31 - 3
          WRITE (*,fmt=*) 'warning from  MAXRANGE: returned maximal integer'
          WRITE (*,fmt=*) chkRange
        END IF
      END IF
    END FUNCTION chkRange

!--------------------------------------------------------------------*

    FUNCTION forwdCount(steps,checkpoints)
    IMPLICIT NONE
      INTEGER :: checkpoints, steps
      INTEGER :: range, reps
      INTEGER :: forwdCount
      IF (checkpoints<1) THEN
        forwdCount = -1
      ELSE
        reps = 0
        range = 1
        DO WHILE (range<steps)
          reps = reps + 1
          range = range*(reps+checkpoints)/reps
        END DO
        forwdCount = reps*steps - range*reps/(checkpoints+1)
      END IF
    END FUNCTION forwdCount

!--------------------------------------------------------------------*

END MODULE revolve
