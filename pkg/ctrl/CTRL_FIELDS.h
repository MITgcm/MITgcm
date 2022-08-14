CBOP
C     !ROUTINE: CTRL_FIELDS.h
C     !INTERFACE:
C     include "CTRL_FIELDS.h"
C     !DESCRIPTION:
C     \bv
C     *==============================================================*
C     | CTRL_FIELDS.h
C     | o Additional control fields (from main model or pkgs)
C     |   which are only used when specific CTRL options are defined.
C     *==============================================================*
C     | Note: Other model or pkg variables which can also be used
C     | independently from CTRL options remain in their respective
C     | header files.
C     *==============================================================*
C     \ev
CEOP

#ifdef ALLOW_BOTTOMDRAG_CONTROL
      COMMON /CTRL_FIELDS_BOTTOMDRAG/
     &                       bottomDragFld
      _RL  bottomDragFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
