FUNCTION zfm_gp_mes_sd_bgsubmit.
*"----------------------------------------------------------------------
*"*"���ؽӿڣ�
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
    fill_flag 'E' '�����������Ϊ��'.
  ENDIF.
  LOOP AT in_tab.
    IF in_tab-vbeln IS INITIAL OR in_tab-sign IS INITIAL.
      fill_flag 'E' '��β���Ϊ��'.
    ENDIF.
    SELECT SINGLE * INTO @DATA(wa_vbak) FROM vbak WHERE vbeln = @in_tab-vbeln.
    IF sy-subrc EQ 0.
      IF wa_vbak-zttzt NE 'D'.
        DATA(msg) = |��ǰ����֪ͨ����״̬���������ύ�������޷�ǩ�գ�vbeln��{ in_tab-vbeln }|.
        fill_flag 'E' msg.
      ENDIF.
      IF wa_vbak-zttzt EQ 'D' AND in_tab-sign EQ 'F'.
        wa_vbak-zttzt = 'F'.
        APPEND wa_vbak TO it_vbak.
      ENDIF.
    ENDIF.
  ENDLOOP.

  UPDATE vbak FROM TABLE it_vbak.
  fill_flag 'S' '�ύ�ɹ�'.
***********************************************************