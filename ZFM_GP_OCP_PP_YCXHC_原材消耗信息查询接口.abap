FUNCTION zfm_gp_ocp_pp_ycxhc.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(WERKS) TYPE  WERKS_D
*"     VALUE(AUFNR) TYPE  AUFNR OPTIONAL
*"     VALUE(ZPCDH) TYPE  ZEPP_ZPCDH OPTIONAL
*"     VALUE(ARBPL) TYPE  ARBPL OPTIONAL
*"     VALUE(ZSDATE) TYPE  CO_GSTRS OPTIONAL
*"     VALUE(ZEDATE) TYPE  CO_GSTRS OPTIONAL
*"     VALUE(POST1) TYPE  PS_POST1 OPTIONAL
*"     VALUE(KUNNR) TYPE  NAME1_GP OPTIONAL
*"     VALUE(Z01) TYPE  ATWRT OPTIONAL
*"     VALUE(Z02) TYPE  ATWRT OPTIONAL
*"     VALUE(CHARG) TYPE  CHARG_D OPTIONAL
*"     VALUE(VBELN) TYPE  VBELN_VA OPTIONAL
*"     VALUE(ZZL1) TYPE  ZE_ZPM2 OPTIONAL
*"     VALUE(C_E02) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      OUT_TAB STRUCTURE  ZSPP218
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_GP_OCP_PP_YCXHC'.
  zfmdatasave2 'B'.
  COMMIT WORK.
***********************************************
  DATA:s_werks  TYPE RANGE OF t001l-werks WITH HEADER LINE,
       s_aufnr  TYPE RANGE OF afko-aufnr WITH HEADER LINE,
       s_zpcdh  TYPE RANGE OF ztpp_205-zpcdh WITH HEADER LINE,
       s_arbpl  TYPE RANGE OF rc68a-arbpl WITH HEADER LINE,
       s_zdate  TYPE RANGE OF afko-gstrs WITH HEADER LINE,
       s_post1  TYPE RANGE OF prps-post1 WITH HEADER LINE,
       s_kunnr  TYPE RANGE OF kna1-kunnr WITH HEADER LINE,
       s_z01    TYPE RANGE OF ausp-atwrt WITH HEADER LINE,
       s_z02    TYPE RANGE OF ausp-atwrt WITH HEADER LINE,
       s_charg1 TYPE RANGE OF mch1-charg WITH HEADER LINE,
       s_vbeln  TYPE RANGE OF vbak-vbeln WITH HEADER LINE,
       s_zzl1   TYPE RANGE OF mara-zzl1 WITH HEADER LINE.
  FIELD-SYMBOLS: <gt_outtab> TYPE ANY TABLE,
                 <lt_data>   TYPE ANY TABLE.
  DATA:go_data TYPE REF TO data,
       ls_data TYPE REF TO data.

  IF werks IS INITIAL.
    fillmsgr 'E' '参数werks为空'.
  ENDIF.
  s_werks-sign = 'I'.
  s_werks-option = 'EQ'.
  s_werks-low = werks.
  APPEND s_werks.
  IF aufnr IS NOT INITIAL.
    s_aufnr-sign = 'I'.
    s_aufnr-option = 'EQ'.
    s_aufnr-low = aufnr.
    APPEND s_aufnr.
  ENDIF.
  IF zpcdh IS NOT INITIAL.
    s_zpcdh-sign = 'I'.
    s_zpcdh-option = 'EQ'.
    s_zpcdh-low = zpcdh.
    APPEND s_zpcdh.
  ENDIF.
  IF arbpl IS NOT INITIAL.
    s_arbpl-sign = 'I'.
    s_arbpl-option = 'EQ'.
    s_arbpl-low = arbpl.
    APPEND s_arbpl.
  ENDIF.
  IF zsdate IS NOT INITIAL.
    s_zdate-sign = 'I'.
    IF zedate IS NOT INITIAL.
      s_zdate-option = 'BT'.
      s_zdate-high = zedate.
    ELSE.
      s_zdate-option = 'EQ'.
    ENDIF.
    s_zdate-low = zsdate.
    APPEND s_zdate.
  ENDIF.
  IF post1 IS NOT INITIAL.
    s_post1-sign = 'I'.
    s_post1-option = 'EQ'.
    s_post1-low = post1.
    APPEND s_post1.
  ENDIF.
  IF kunnr IS NOT INITIAL.
    s_kunnr-sign = 'I'.
    s_kunnr-option = 'EQ'.
    s_kunnr-low = kunnr.
    APPEND s_kunnr.
  ENDIF.
  IF z01 IS NOT INITIAL.
    s_z01-sign = 'I'.
    s_z01-option = 'EQ'.
    s_z01-low = z01.
    APPEND s_z01.
  ENDIF.
  IF z02 IS NOT INITIAL.
    s_z02-sign = 'I'.
    s_z02-option = 'EQ'.
    s_z02-low = z02.
    APPEND s_z02.
  ENDIF.
  IF charg IS NOT INITIAL.
    s_charg1-sign = 'I'.
    s_charg1-option = 'EQ'.
    s_charg1-low = charg.
    APPEND s_charg1.
  ENDIF.
  IF vbeln IS NOT INITIAL.
    s_vbeln-sign = 'I'.
    s_vbeln-option = 'EQ'.
    s_vbeln-low = vbeln.
    APPEND s_vbeln.
  ENDIF.
  IF zzl1 IS NOT INITIAL.
    s_zzl1-sign = 'I'.
    s_zzl1-option = 'EQ'.
    s_zzl1-low = zzl1.
    APPEND s_zzl1.
  ENDIF.
*设置执行时不显示，只获取alv数据
  cl_salv_bs_runtime_info=>set(
    display  = abap_false
    metadata = abap_false
    data     = abap_true ).
  SUBMIT zppr225d
  WITH s_werks IN s_werks
  WITH s_aufnr IN s_aufnr
  WITH s_zpcdh IN s_zpcdh
  WITH s_arbpl IN s_arbpl
  WITH s_zdate IN s_zdate
  WITH s_post1 IN s_post1
  WITH s_kunnr IN s_kunnr
  WITH s_z01 IN s_z01
  WITH s_z02 IN s_z02
  WITH s_charg1 IN s_charg1
  WITH s_vbeln IN s_vbeln
  WITH s_zzl1 IN s_zzl1
  WITH c_e02 = c_e02
  AND RETURN.
  TRY.
      "取得运行数据
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data = go_data
      ).
*   数据赋值
      ASSIGN go_data->* TO <gt_outtab>.  "结构必须和被调程序的ALV 结构一样

      IF sy-subrc EQ 0.
*        MOVE <gt_outtab> TO out_tab[].
        IF <gt_outtab> IS NOT INITIAL.
          LOOP AT <gt_outtab> ASSIGNING FIELD-SYMBOL(<fs_outtab>).
            MOVE-CORRESPONDING <fs_outtab> TO out_tab.
            APPEND out_tab.
          ENDLOOP.
        ENDIF.
      ENDIF.
    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE '无法获取ALV数据'  TYPE  'E'.
  ENDTRY.
  CALL METHOD cl_salv_bs_runtime_info=>clear_all.
  fillmsgr 'S' '查询成功'.

*************************