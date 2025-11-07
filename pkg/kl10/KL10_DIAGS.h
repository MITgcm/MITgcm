#ifdef ALLOW_KL10

C     *==========================================================*
C     | KL10_DIAGS.h                                             |
C     | o Header for KL10 diagnostic output                      |
C     *==========================================================*

C----------------------------------------------------------------
C     kl_drctrec     - next record to dump for KL files
C----------------------------------------------------------------
      INTEGER kl_drctrec
      COMMON /KL10_RECORDNUM1/ kl_drctrec

#endif /* ALLOW_KL10 */
