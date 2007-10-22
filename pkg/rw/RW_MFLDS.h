C $Header: /u/gcmpack/MITgcm/pkg/rw/RW_MFLDS.h,v 1.1 2007/10/22 13:20:07 jmc Exp $
C $Name:  $

C     !ROUTINE: RW_MFLDS.h
C     !INTERFACE:
C     include "RW_MFLDS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | RW_MFLDS.h
C     | o Variables used for reading Multi-Fields files (+ meta-file)
C     *==========================================================*
C     | This block allows to shared information across threads
C     |  and between READ_MFLDS subroutines
C     *==========================================================*
C     \ev

C     sizFldList :: dimension of field-list arrays
      INTEGER  sizFldList
      PARAMETER( sizFldList = 100 )

C--   COMMON / RW_MFLDS_I / Integer valued MFLDS variables.
C     thirdDim  :: 3rd dimension of fields in current MFLDS file
C     nFl3D     :: Number of 3-D fields in current MFLDS file
C     nFlds     :: Number of fields (3D+2D) in current MFLDS file
C     nMissFld  :: Number of missing fields (attempted to read but not found)
      COMMON / RW_MFLDS_I /
     &        thirdDim, nFl3D, nFlds, nMissFld

      INTEGER thirdDim
      INTEGER nFl3D
      INTEGER nFlds
      INTEGER nMissFld

C--   COMMON / RW_MFLDS_C / Character valued MFLDS variables.
C     mFldsFile :: current MFLDS file name
C     fldList   :: list of fields in current MFLDS file
C     fldMiss   :: list of missing fields (attempted to read but not found)
      COMMON / RW_MFLDS_C /
     &        mFldsFile, fldList, fldMiss
      CHARACTER*(MAX_LEN_FNAM) mFldsFile
      CHARACTER*(8) fldList(sizFldList)
      CHARACTER*(8) fldMiss(sizFldList)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
