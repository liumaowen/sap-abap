FUNCTION zfm_gp_sd_esb_crmxmfc.
*"----------------------------------------------------------------------
*"*"���ؽӿڣ�
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
    fillmsgr 'E' '����intabΪ��'.
  ENDIF.

  LOOP AT intab.
    IF intab-zbh IS INITIAL.
      DATA(msg) = '��' && sy-tabix && '����ϸCRM���ZBHΪ��'.
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

  fillmsgr 'S' 'ͬ���ɹ�'.
  zfmdatasav