*&---------------------------------------------------------------------*
*& Report ZRMM214H
*&---------------------------------------------------------------------*
*& 寄售库存表
*&---------------------------------------------------------------------*
REPORT zrmm214h MESSAGE-ID zgp_msg.

TABLES:t001l,mkol.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA:wa_col    TYPE lvc_s_scol,
     wa_coltab TYPE lvc_t_scol,
     wa_color  TYPE lvc_s_colo.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0,
       werks TYPE mkol-werks,
       matnr TYPE mkol-matnr,
       lgort TYPE mkol-lgort,
       charg TYPE mkol-charg,
       slabs TYPE mkol-slabs, "库存量
       eisbe TYPE marc-eisbe, "安全库存量
       chay  TYPE mkol-slabs, "差异
       color TYPE lvc_t_scol, "颜色
       sel,
     END OF itab.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR t001l-werks NO INTERVALS NO-EXTENSION,
                 s_lgort FOR mkol-lgort,
                 s_matnr FOR mkol-matnr,
                 s_charg FOR mkol-charg.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '功能选择'.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_matnr_%_app_%-text = '物料号'.
  %_s_lgort_%_app_%-text = '库存地'.
  %_s_charg_%_app_%-text = '批次号'.


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-low.
  PERFORM zf4_lgort.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-high.
  PERFORM zf4_lgort.

START-OF-SELECTION.

  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  PERFORM alvshow.
*&---------------------------------------------------------------------*
*& Form getdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdata .
  SELECT
      mkol~*,
      marc~eisbe,
      ( mkol~slabs - marc~eisbe ) AS chay
  FROM mkol
  LEFT JOIN marc ON mkol~matnr = marc~matnr AND mkol~werks = marc~werks
  WHERE mkol~sobkz = 'K'
    AND mkol~werks IN @s_werks
    AND mkol~matnr IN @s_matnr
    AND mkol~lgort IN @s_lgort
    AND mkol~charg IN @s_charg
  INTO CORRESPONDING FIELDS OF TABLE @itab.

  IF itab[] IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "设定颜色
  wa_color-col = 6.
  wa_color-int = 0.
  wa_color-inv = 0.

  LOOP AT itab.
    IF itab-chay < 0.
      CLEAR wa_col.
      wa_col-fname = 'CHAY'.
      wa_col-color = wa_color.
      APPEND wa_col TO wa_coltab.
      itab-color = wa_coltab.
      MODIFY itab.
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
  slayt-coltab_fieldname = 'COLOR'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局

  PERFORM catlg_set TABLES fldct
                    USING:
'WERKS '  '工厂'        'MKOL'  'WERKS',
'MATNR '  '物料号'      'MKOL'  'MATNR',
'LGORT '  '库存地'      'MKOL'  'LGORT',
'CHARG '  '批次号'      'MKOL'  'CHARG',
'SLABS '  '库存量'      'MKOL'  'SLABS',
'EISBE '  '完全库存量'  'MARC'  'EISBE',
'CHAY '   '差异'        'MKOL'  'SLABS'.

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
FORM zf4_lgort .
  DATA: lt_dynpread TYPE STANDARD TABLE OF dynpread WITH HEADER LINE.
  DATA: lw_dynpread TYPE dynpread .
  lw_dynpread-fieldname = 'S_WERKS-LOW'.
  APPEND lw_dynpread TO lt_dynpread .
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
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
      dynprofield      = 'S_WERKS'
      window_title     = '库存地'
      value_org        = 'S' "Structure
      callback_program = sy-repid
*     callback_form    = 'CB_FORM'
    TABLES
      value_tab        = lt_t001l
*     field_tab        = l_dfies[]
      return_tab       = return_tab[]
*     dynpfld_mapping  = l_dselc[]
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO