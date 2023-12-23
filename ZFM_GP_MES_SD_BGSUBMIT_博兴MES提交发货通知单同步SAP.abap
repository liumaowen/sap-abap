FUNCTION zfm_gp_mes_sd_bgsubmit.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(FLAG) TYPE  BAPI_MTYPE
*"     VALUE(MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      IN_TAB STRUCTURE  ZSSD279
*"----------------------------------------------------------------------

  zfmdatasave1 'ZFM_GP_MES_SD_BGSUBMIT'.
  zfmdatasave2 'B'.
  COMMIT WORK.

**********************************************************************
  DATA: it_vbak TYPE TABLE OF vbak.
  IF in_tab[] IS INITIAL.
    fill_flag 'E' '导入参数不能为空'.
  ENDIF.
  LOOP AT in_tab.
    IF in_tab-vbeln IS INITIAL OR in_tab-sign IS INITIAL.
      fill_flag 'E' '入参不能为空'.
    ENDIF.
    SELECT SINGLE * INTO @DATA(wa_vbak) FROM vbak WHERE vbeln = @in_tab-vbeln.
    IF sy-subrc EQ 0.
      IF wa_vbak-zttzt NE 'D'.
        DATA(msg) = |当前发货通知单的状态不是物流提交，保管无法签收，vbeln：{ in_tab-vbeln }|.
        fill_flag 'E' msg.
      ENDIF.
      IF wa_vbak-zttzt EQ 'D' AND in_tab-sign EQ 'F'.
        wa_vbak-zttzt = 'F'.
        APPEND wa_vbak TO it_vbak.
      ENDIF.
    ENDIF.
  ENDLOOP.

  UPDATE vbak FROM TABLE it_vbak.
  fill_flag 'S' '提交成功'.
***********************************************************