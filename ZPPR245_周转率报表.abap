*&---------------------------------------------------------------------*
*& Report ZPPR245
*& 周转率报表
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr245 MESSAGE-ID zgp_msg.
TABLES:eban,mchbh,mara.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_itab.
        INCLUDE TYPE zjxc.
TYPES:  werksname  TYPE t001w-name1,   "工厂描述
        mat_pspnr  TYPE mat_pspnr,     "WBS要素
        ps_psp_pnr TYPE mseg-ps_psp_pnr,
        wlcms      TYPE string,        "物料长描述
        zqh        TYPE char6,         "期号
        zzzl       TYPE string,        "周转率
        zzzts      TYPE string,        "周转天数
        vprsv      TYPE mseg-vprsv,
        dmbtr      TYPE mseg-dmbtr,
        menge      TYPE mseg-menge,
        kunnr      TYPE mseg-kunnr,
        lifnr      TYPE mseg-lifnr,
        sel,
      END OF ty_itab.
DATA:itab    TYPE TABLE OF ty_itab WITH EMPTY KEY,
     wa_itab LIKE LINE OF itab.
DATA:BEGIN OF itab2 OCCURS 0,
       mtart                 TYPE mtart,
       mtbez                 TYPE T134t-mtbez,   "物料分类描述
       issue_value           TYPE zjxc-issue_value,    "出库金额
       value_begin_of_period TYPE zjxc-value_begin_of_period,   "期初金额
       value_end_of_period   TYPE zjxc-value_end_of_period,   "期末金额
       zqh                   TYPE char6,
       zzzl                  TYPE p DECIMALS 2,  "周转率
       zzzts                 TYPE p DECIMALS 2,  "周转天数
     END OF itab2.
DATA:BEGIN OF itab3 OCCURS 0,
       mtart                 TYPE mtart,
       mtbez                 TYPE T134t-mtbez,   "物料分类描述
       zzl1                  TYPE mara-zzl1,     "品名
       issue_value           TYPE zjxc-issue_value,    "出库金额
       value_begin_of_period TYPE zjxc-value_begin_of_period,   "期初金额
       value_end_of_period   TYPE zjxc-value_end_of_period,   "期末金额
       zqh                   TYPE char6,
       zzzl                  TYPE p DECIMALS 2,  "周转率
       zzzts                 TYPE p DECIMALS 2,  "周转天数
     END OF itab3.
DATA: zckjesum TYPE mseg-dmbtr."总的出库金额
DATA: par_days TYPE t009b-butag.

DATA: date_st TYPE date,
      date_ed TYPE date,
      t_werks TYPE TABLE OF ztwerks,
      t_lgort TYPE TABLE OF ztlgort WITH HEADER LINE,
      t_matnr TYPE TABLE OF ztmatnr WITH HEADER LINE,
      t_mtart TYPE TABLE OF ztmtart,
      out_tab TYPE TABLE OF zjxc WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY.
  SELECT-OPTIONS: s_lgort FOR eban-lgort NO INTERVALS,
                  s_zzl1 FOR mara-zzl1 NO INTERVALS.
  PARAMETERS:s_lfgja LIKE mchbh-lfgja OBLIGATORY DEFAULT sy-datum+0(4),
             s_lfmon LIKE mchbh-lfmon OBLIGATORY DEFAULT sy-datum+4(2).
  PARAMETERS:p1 RADIOBUTTON GROUP grd1 USER-COMMAND singleclick DEFAULT 'X',
             p2 RADIOBUTTON GROUP grd1,
             p3 RADIOBUTTON GROUP grd1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t2.
  PARAMETERS:c1 RADIOBUTTON GROUP grd2 USER-COMMAND singleclick DEFAULT 'X',
             c2 RADIOBUTTON GROUP grd2,
             c3 RADIOBUTTON GROUP grd2.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  t1 = '选择条件'.
  t2 = '功能选择'.
  %_p_werks_%_app_%-text = '工厂'.
  %_s_lgort_%_app_%-text = '仓库'.
  %_s_zzl1_%_app_%-text  = '品名'.
  %_s_lfgja_%_app_%-text = '会计年度'.
  %_s_lfmon_%_app_%-text = '会计期间'.
  %_p1_%_app_%-text      = '成品周转率'.
  %_p2_%_app_%-text      = '原材周转率'.
  %_p3_%_app_%-text      = '全部'.
  %_c1_%_app_%-text      = '按分类汇总'.
  %_c2_%_app_%-text      = '按品名汇总'.
  %_c3_%_app_%-text      = '明细详情'.


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-low.
  PERFORM zf4_lgort.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-high.
  PERFORM zf4_lgort.

START-OF-SELECTION.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  IF p1 EQ 'X'.
    t_mtart = VALUE #( ( mtart = 'Z001' )
                       ( mtart = 'Z002' ) ).
  ENDIF.

  IF p2 EQ 'X'.
    t_mtart = VALUE #( ( mtart = 'Z003' )
                       ( mtart = 'Z005' )
                       ( mtart = 'Z008' )
                       ( mtart = 'Z011' ) ).
  ENDIF.
  IF p3 EQ 'X'.
    REFRESH t_mtart.
  ENDIF.

  DATA:s_mtart  TYPE mara-mtart.
  REFRESH t_matnr.
  IF s_zzl1[] IS NOT INITIAL .
    SELECT
      mara~matnr
      FROM mara
      FOR ALL ENTRIES IN @s_zzl1
      WHERE mara~zzl1 = @s_zzl1-low
      INTO TABLE @t_matnr.
  ENDIF.
  IF s_lfmon < 1 OR s_lfmon > 12.
    MESSAGE '请输入正确的期间' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
    EXPORTING
      par_month = s_lfmon
      par_year  = s_lfgja
    IMPORTING
      par_days  = par_days.
  date_st = s_lfgja && s_lfmon && '01'.
  date_ed = s_lfgja && s_lfmon && par_days.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  PERFORM alvshow.
*&---------------------------------------------------------------------*
*& Form getdata
*&---------------------------------------------------------------------*
*& 取值
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdata .
  t_werks = VALUE #( ( werks = p_werks ) ).
  IF s_lgort[] IS NOT INITIAL.
    REFRESH: t_lgort.
    LOOP AT s_lgort.
      t_lgort-lgort = s_lgort-low.
      COLLECT t_lgort.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'ZFM_GP_MM_SAP_JXC'
    EXPORTING
      date_st = date_st
      date_ed = date_ed
    TABLES
      t_werks = t_werks
      t_lgort = t_lgort
      t_matnr = t_matnr
      t_mtart = t_mtart
      out_tab = out_tab.

  IF out_tab[] IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH itab.
  LOOP AT out_tab.
    MOVE-CORRESPONDING out_tab TO wa_itab.
    wa_itab-zqh = s_lfgja && s_lfmon.
    wa_itab-issue_value = wa_itab-issue_value * -1.
    APPEND wa_itab TO itab.
  ENDLOOP.
  IF c1 EQ 'X' OR c2 EQ 'X'.
    DELETE itab WHERE sobkz = 'T'.
  ENDIF.
  "最终赋值

  CASE 'X'.
    WHEN c1.
      REFRESH itab2.
      LOOP AT itab INTO wa_itab.
        CLEAR itab2.
        itab2-mtart = wa_itab-mtart.
        itab2-mtbez = wa_itab-mtbez.
        itab2-issue_value = wa_itab-issue_value.
        itab2-value_begin_of_period = wa_itab-value_begin_of_period.
        itab2-value_end_of_period = wa_itab-value_end_of_period.
        itab2-zqh   = wa_itab-zqh.
        COLLECT itab2.
      ENDLOOP.
      REFRESH itab.
      LOOP AT itab2.
        CLEAR wa_itab.
        DATA(jine) = itab2-value_begin_of_period + itab2-value_end_of_period.
        IF jine NE 0.
          itab2-zzzl = itab2-issue_value / ( jine / 2 ).
        ENDIF.
        IF itab2-zzzl NE 0.
          itab2-zzzts = par_days / itab2-zzzl.
        ENDIF.
        MOVE-CORRESPONDING itab2 TO wa_itab.
        APPEND wa_itab TO itab.
      ENDLOOP.
    WHEN c2.
      REFRESH itab3.
      LOOP AT itab INTO wa_itab.
        CLEAR itab3.
        itab3-mtart = wa_itab-mtart.
        itab3-mtbez = wa_itab-mtbez.
        itab3-zzl1 = wa_itab-zzl1.
        itab3-issue_value = wa_itab-issue_value.
        itab3-value_end_of_period = wa_itab-value_end_of_period.
        itab3-value_begin_of_period = wa_itab-value_begin_of_period.
        itab3-zqh   = wa_itab-zqh.
        COLLECT itab3.
      ENDLOOP.
      REFRESH itab.
      LOOP AT itab3.
        CLEAR wa_itab.
        DATA(jine1) = itab3-value_begin_of_period + itab3-value_end_of_period.
        IF jine1 NE 0.
          itab3-zzzl = itab3-issue_value / ( jine1 / 2 ).
        ENDIF.
        IF itab3-zzzl NE 0.
          itab3-zzzts = par_days / itab3-zzzl.
        ENDIF.
        MOVE-CORRESPONDING itab3 TO wa_itab.
        APPEND wa_itab TO itab.
      ENDLOOP.

  ENDCASE.
  CALL METHOD cl_salv_bs_runtime_info=>set
    EXPORTING
      display  = abap_true
      metadata = abap_true
      data     = abap_true.
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

  PERFORM catlg_set TABLES fldct USING:
    'MTART '  '物料分类    '      'MARA '   'MTART'      ,
    'MTBEZ '  '物料分类描述'      'T134T'   'MTBEZ'      ,
    'ISSUE_VALUE '  '出库金额    '      'ZJXC '   'ISSUE_VALUE'      ,
    'VALUE_BEGIN_OF_PERIOD '  '期初金额    '      'ZJXC'        'VALUE_BEGIN_OF_PERIOD',
    'VALUE_END_OF_PERIOD '  '期末金额    '      'ZJXC'        'VALUE_END_OF_PERIOD',
    'ZQH   '  '期号        '      ''        ''           .
  CASE 'X'.
    WHEN c1.
      PERFORM catlg_set TABLES fldct USING:
            'ZZZL '   '周转率'       ''   '',
            'ZZZTS '  '周转天数'      ''  ''.
    WHEN c2.
      PERFORM catlg_set TABLES fldct USING:
          'ZZL1 '   '品名'     'MARA'  'ZZL1',
          'ZZZL '   '周转率'    ''      ''   ,
          'ZZZTS '  '周转天数'  ''      ''   .
    WHEN OTHERS.
      PERFORM catlg_set TABLES fldct USING:
        'MATNR '      '物料编码'      'MARA'   'MATNR'     ,
        'ZZL1 '       '品名'          'MARA'   'ZZL1'      ,
*        'CHARG '      '批次号'        'MSEG'   'CHARG'     ,
        'LGORT '      '仓库'          'MSEG'   'LGORT'     ,
        'LGOBE '      '仓库描述'       ''       'LGOBE'     ,
        'WNAME1 '     '工厂描述'      'T001W'  'NAME1'     ,
        'POSID '      'WBS要素'       'MSEG'   'POSID'    ,
        'POST1 '      '项目描述'      'PRPS'   'POST1'     ,
        'MEINS '      '基本单位'      'MSEG'   'MEINS'     ,
        'MAKTX '      '物料描述'       ''      'MAKTX'      .
  ENDCASE.


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
  CASE 'X'.
    WHEN p1.
      SET TITLEBAR 'TB01' WITH '成品周转率报表'.
    WHEN p2.
      SET TITLEBAR 'TB01' WITH '原材周转率报表'.
  ENDCASE.
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
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf4_lgort
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf4_lgort.
  DATA: lt_dynpread TYPE STANDARD TABLE OF dynpread WITH HEADER LINE.
  DATA: lw_dynpread TYPE dynpread .
  lw_dynpread-fieldname = 'P_WERKS'.
  APPEND lw_dynpread TO lt_dynpread .
  DATA(dynnr) = '1000'.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = dynnr
    TABLES
      dynpfields           = lt_dynpread
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  READ TABLE lt_dynpread INTO lw_dynpread INDEX 1.
  DATA(werks) = lw_dynpread-fieldvalue.

  DATA: return_tab TYPE ddshretval OCCURS 0 .
  REFRESH return_tab[].
  SELECT t001l~werks, t001l~lgort,t001l~lgobe
    INTO TABLE @DATA(lt_t001l)
    FROM t001l
    WHERE werks = @werks.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'LGORT'
      dynpprog         = sy-repid
      dynpnr           = sy-dynnr
      dynprofield      = 'P_WERKS'
      window_title     = '成品品名'
      value_org        = 'S' "Structure
      callback_program = sy-repid
*     callback_form    = 'CB_FORM'
    TABLES
      value_tab        = lt_t001l
*     field_tab        = l_dfies[]
      return_tab       = return_tab[]
*     dynpfld_mapping  = l_dselc[