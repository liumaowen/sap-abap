FUNCTION zfm_gp_mm_ocp_db.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(WERKS) TYPE  WERKS_D
*"     VALUE(ZDBDH) TYPE  ZTMM220-ZDBDH OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      OUT_TAB STRUCTURE  ZSMM_230
*"----------------------------------------------------------------------

  zfmdatasave1 'ZFM_GP_MM_OCP_DB'.
  zfmdatasave2 'B'.
  COMMIT WORK.
*************************************
  DATA wherestr TYPE string.
  IF werks EQ ''.
    fillmsgr 'E' '参数werks为空'.
  ENDIF.
*调拨类型ZDBLX是工厂内调拨（CNDB）且调拨单据状态ZDJZT是入库中（4RKZ）
  wherestr = `a~werks = @werks  AND a~zdjzt = '4RKZ' AND a~zdblx = 'CNDB'`.
  IF zdbdh IS NOT INITIAL.
    wherestr = wherestr && ` AND a~zdbdh = @zdbdh`.
  ENDIF.

  SELECT FROM ztmm221 AS b
    JOIN ztmm220 AS a ON a~zdbdh = b~zdbdh
    LEFT JOIN mara ON mara~matnr = b~matnr
    LEFT JOIN makt ON makt~matnr = b~matnr AND makt~spras = @sy-langu
    LEFT JOIN t001l AS dcms ON dcms~werks = a~werks  AND dcms~lgort = a~zdckcd
    LEFT JOIN t001l AS drms ON drms~werks = a~werks  AND drms~lgort = a~lgort
    LEFT JOIN usr21 ON usr21~bname = a~zcjz
    LEFT JOIN adrp ON adrp~persnumber = usr21~persnumber
    FIELDS b~zdbdh,
    a~zdckcd,
    dcms~lgobe AS zdcms,
    a~lgort AS zdrkcd,
    drms~lgobe AS zdrms,
    a~zjsbg,
    a~zzz,
    a~zzl,
    a~zdbrq,
    a~zcjz,
    adrp~name_text AS zcjzms,
    b~zhh AS zdbhh,
    mara~zzl1 AS zpm,
    makt~maktx AS zwlms,
    b~zzbh,b~charg AS zpch,
    b~zdbsl_in AS zsl,
    b~meins_in AS zdw
    WHERE (wherestr)
    INTO TABLE @out_tab.




*************************************
  rtype = 'S'.
  rtmsg = '取数成�