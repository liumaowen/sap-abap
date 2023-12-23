FUNCTION zfm_gp_sd_esb_crmxmfc.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      INTAB STRUCTURE  ZTSD231
*"----------------------------------------------------------------------
  TABLES:ztsd231.
  zfmdatasave1 'ZFM_GP_SD_ESB_CRMXMFC'.
  zfmdatasave2 'B'.
  COMMIT WORK.
**********************************************************************
  IF intab[] IS INITIAL.
    fillmsgr 'E' '参数intab为空'.
  ENDIF.

  LOOP AT intab.
    IF intab-zbh IS INITIAL.
      DATA(msg) = '第' && sy-tabix && '行明细CRM编号ZBH为空'.
      fillmsgr 'E' msg.
    ENDIF.
  ENDLOOP.

  MODIFY ztsd231 FROM TABLE intab[].
  IF sy-subrc NE 0.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.

**********************************************************************

  fillmsgr 'S' '同步成功'.
  zfmdatasav