FUNCTION zfm_gp_sd_erp_xmwgxg.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      IN_TAB STRUCTURE  ZSFI223
*"----------------------------------------------------------------------

  zfmdatasave1 'ZFM_GP_SD_ERP_XMWGXG'.
  zfmdatasave2 'B'.
  COMMIT WORK.
***********************************************
  DATA:gt_z215 TYPE TABLE OF ztfi215 WITH HEADER LINE.
  DATA:gt_z223 TYPE TABLE OF zsfi223 WITH EMPTY KEY,
       gs_z223 LIKE LINE OF gt_z223.
  IF in_tab[] IS INITIAL.
    rtype = 'E'.
    rtmsg = '参数IN_TAB为空'.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.
  gt_z223 = in_tab[].
  SELECT
    z215~*
  FROM ztfi215 AS z215
  INNER JOIN @gt_z223 AS z2 ON  z2~pspid = z215~pspid
                            AND z2~matnr = z215~matnr
  INTO TABLE @gt_z215.
  LOOP AT gt_z215.
    READ TABLE gt_z223 INTO gs_z223 WITH KEY pspid = gt_z215-pspid matnr = gt_z215-matnr.
    IF sy-subrc = 0.
      gt_z215-zdzbl = gs_z223-zdzbl.
      gt_z215-zdzkhdj = gt_z215-zkhdj * ( gt_z215-zdzbl / 100 ).
      gt_z215-zdzje = gt_z215-zdzkhdj * gt_z215-prlab.
      MODIFY gt_z215.
    ENDIF.
  ENDLOOP.
  MODIFY ztfi215 FROM TABLE gt_z215.
  COMMIT WORK.
  rtype = 'S'.
  rtmsg = '更新成功'.

***********************************************
  zfmdatasave2 'R'.