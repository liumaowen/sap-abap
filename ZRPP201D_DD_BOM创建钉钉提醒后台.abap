*&---------------------------------------------------------------------*
*& Report ZRPP201D_DD
*&---------------------------------------------------------------------*
*& BOM创建钉钉提醒后台
*&---------------------------------------------------------------------*
REPORT zrpp201d_dd.

TABLES:sscrfields,t001l,mara,ztpp_216a.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_out,
        werks TYPE ztpp_216a-werks,
        matnr TYPE ztpp_216a-matnr,
        zxfrq TYPE ztpp_216a-zxfrq,
        zxfsj TYPE ztpp_216a-zxfsj,
        zxfr  TYPE ztpp_216a-zxfr,
      END OF ty_out.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE TYPE ty_out.
DATA:  vbeln TYPE vbeln,
       posnr TYPE posnr,
       matkl TYPE mara-matkl,
       maktx TYPE makt-maktx,
       zzl1  TYPE mara-zzl1,
       zisxs TYPE ztpp_216a-zisxs,
       cha   TYPE i,
       sel,
     END OF itab.
DATA date TYPE sy-datum.
DATA: tel     TYPE kna1-telf1 VALUE '18354366404', "正式电话号码
      teltest TYPE kna1-telf1 VALUE '13280679865'. "测试电话号码

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS:p_werks TYPE t001l-werks OBLIGATORY DEFAULT '3000'.
  SELECT-OPTIONS:s_matkl FOR mara-matkl      ,
                 s_zzl1 FOR mara-zzl1        ,
                 s_matnr FOR mara-matnr      .
  PARAMETERS:p_day TYPE i OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '功能选择'.
  %_p_werks_%_app_%-text = '工厂'.
  %_s_matkl_%_app_%-text = '成品物料组'.
  %_s_zzl1_%_app_%-text = '品名'.
  %_s_matnr_%_app_%-text = '成品编码'.
  %_p_day_%_app_%-text = '天数'.


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  IF sy-batch EQ 'X'.
    PERFORM postdd.
  ELSE.
    PERFORM alvshow.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form getdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdata .
  date = sy-datlo.
  SELECT
      ztpp_216a~werks,
      ztpp_216a~vbeln,
      ztpp_216a~posnr,
      mara~matkl,
      ztpp_216a~matnr,
      makt~maktx,
      mara~zzl1,
      ztpp_216a~zxfrq,
      ztpp_216a~zxfsj,
      ztpp_216a~zxfr,
      ztpp_216a~zisxs
  FROM ztpp_216a
  INNER JOIN mara ON mara~matnr = ztpp_216a~matnr
  LEFT JOIN makt  ON makt~matnr = ztpp_216a~matnr
  WHERE ztpp_216a~werks = @p_werks
    AND ztpp_216a~matnr IN @s_matnr
    AND mara~matkl IN @s_matkl
    AND mara~zzl1 IN @s_zzl1
    AND NOT EXISTS ( SELECT 'X' FROM mast WHERE werks = ztpp_216a~werks
                                            AND matnr = ztpp_216a~matnr )
  INTO CORRESPONDING FIELDS OF TABLE @itab.
  LOOP AT itab.
    "后台自动运行日期-下发日期 >= 屏幕填写天数
    CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
      EXPORTING
        begda = itab-zxfrq      "开始日期
        endda = sy-datlo        "截止日期
      IMPORTING
        days  = itab-cha.        "返回天数
    IF itab-cha >= p_day.
      MODIFY itab TRANSPORTING cha.
    ELSE.
      DELETE itab.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alvshow
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alvshow .
  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局

  PERFORM catlg_set TABLES fldct
                    USING:
   'WERKS '  '工厂    '  'ZTPP_216A'  'WERKS',
   'VBELN '  '合同编号'  'ZTPP_216A'  'VBELN',
   'POSNR '  '合同行号'  'ZTPP_216A'  'POSNR',
   'MATKL '  '物料组  '  'MARA     '  'MATKL',
   'MATNR '  '物料编码'  'ZTPP_216A'  'MATNR',
   'MAKTX '  '物料描述'  'MAKT     '  'MAKTX',
   'ZZL1  '  '品名    '  'MARA     '  'ZZL1 ',
   'ZXFRQ '  '下发日期'  'ZTPP_216A'  'ZXFRQ',
   'ZXFSJ '  '下发时间'  'ZTPP_216A'  'ZXFSJ',
   'CHA   '  '差值    '  '         '  '     ',
   'ZXFR  '  '下发人  '  'ZTPP_216A'  'ZXFR ',
   'ZISXS '  '是否不处理'  'ZTPP_216A'  'ZISXS'.

  i_title               = lines( itab ) .
  CONDENSE i_title.
  CONCATENATE '条目数:' i_title INTO i_title.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      it_fieldcat              = fldct[]
      i_save                   = 'A'
      is_variant               = varnt
      is_layout                = slayt
      i_grid_title             = i_title
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
*     IT_EVENTS                = GT_EVENTS
    TABLES
      t_outtab                 = itab[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form catlg_set
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> FLDCT
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM catlg_set  TABLES fldcattab
                USING p_field p_text p_reftab p_reffld .
  DATA: ls_fldct TYPE slis_fieldcat_alv.

  ls_fldct-fieldname     =  p_field.
  ls_fldct-seltext_l     =  p_text.
  ls_fldct-ddictxt       =  'L'.
  ls_fldct-ref_fieldname =  p_reffld.
  ls_fldct-ref_tabname   =  p_reftab.
  IF p_reffld = 'MENGE'.
    ls_fldct-qfieldname = 'MEINS'.
    ls_fldct-no_zero = 'X'.
  ENDIF.
  IF p_reffld = 'DATS'.
    ls_fldct-no_zero = 'X'.
  ENDIF.
  CASE ls_fldct-fieldname.
    WHEN 'MENGE'.
      ls_fldct-qfieldname = 'MEINS'.
      ls_fldct-no_zero = 'X'.
    WHEN 'DMBTR' .
      ls_fldct-cfieldname = 'WAERB'.
    WHEN 'WRBTR' OR 'DMBTR1' OR 'DMBTR2' .
      ls_fldct-cfieldname = 'WAERS'.
      ls_fldct-no_zero = 'X'.
    WHEN 'KUNNR' OR 'EBELN' OR 'BANFN'.
      ls_fldct-edit_mask = '==ALPHA'.
    WHEN 'MATNR' .
      ls_fldct-edit_mask = '==MATN1'.
      ls_fldct-intlen = 18.
    WHEN 'BSTME' OR 'MEINS' .
      ls_fldct-edit_mask = '==CUNIT'.
    WHEN 'ICON'.
      ls_fldct-icon = abap_true.
    WHEN 'EBELN' OR 'RTYPE' OR 'RTMSG' OR
         'MBLPO' OR 'FRGKE'.
      ls_fldct-emphasize = 'C110'.
    WHEN 'CB'.
      ls_fldct-checkbox = 'X'.
      ls_fldct-edit = 'X'.
    WHEN 'FISC_YEAR' OR 'ZHDRQ' OR 'ZZDRQ'.
      ls_fldct-no_zero = 'X'.
    WHEN OTHERS.
  ENDCASE.

  APPEND ls_fldct TO fldcattab .
  CLEAR ls_fldct .
ENDFORM.

FORM set_status USING rt_extab TYPE slis_t_extab.
  CLEAR rt_extab.
  REFRESH rt_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab .

ENDFORM.

FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lv_rtmsg TYPE bapi_msg.
  DATA wa LIKE LINE OF itab.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CHECK rs_selfield-tabindex <> 0 . "小计行总计行什么的忽略
      READ TABLE itab INTO wa INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN ''.

        WHEN OTHERS.
      ENDCASE.
    WHEN 'POSTDD'.
      PERFORM postdd.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.
*推送钉钉消息
FORM postdd.
  DATA: count    TYPE i,
        text     TYPE string,
        rtype    TYPE bapi_mtype,
        rtmsg    TYPE bapi_msg,
        telphone TYPE ad_tlnmbr,
        ret      TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA:itab1 LIKE itab OCCURS 0 WITH HEADER LINE.
  IF itab[] IS NOT INITIAL.
    LOOP AT itab.
      IF itab-zisxs NE 'X'.
        APPEND itab TO itab1.
      ENDIF.
    ENDLOOP.
    count = lines( itab1 ).
    text = |工厂：{ itab[ 1 ]-werks }，您有{ count }条未创建BOM，请及时处理|.
    CASE sy-mandt.
      WHEN '900'.
        telphone = tel.
      WHEN OTHERS.
        telphone = teltest.
    ENDCASE.
    CALL FUNCTION 'ZFM_GP_SD_DD_FHTZTX_POST'
      EXPORTING
        ddstr  = text
        tlnmbr = telphone
*       msgtype = 'markdown'
      IMPORTING
        rtype  = rtype
        rtmsg  = rtmsg.
    PERFORM inmsg(zpubform) TABLES ret USING 'OO' rtype '000' telphone rtmsg+0(50)