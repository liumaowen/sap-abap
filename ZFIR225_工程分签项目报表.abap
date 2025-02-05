*&---------------------------------------------------------------------*
*& Report ZFIR225
*&---------------------------------------------------------------------*
*& 工程分签项目未实现报表
*&---------------------------------------------------------------------*
REPORT zfir225.

TABLES:sscrfields,t001l,afko,acdoca.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA: BEGIN OF itab OCCURS 0.
        INCLUDE TYPE ztfi_mml_data.
DATA:   maktx TYPE char200,
        sel   TYPE char1,
      END OF itab.
DATA:gt_mml_data TYPE TABLE OF ztfi_mml_data WITH HEADER LINE.
CONSTANTS: zmlb_num TYPE ztfi_mml_data-zmlb_num VALUE 'KHHBBB', "KHHBBB
           curtp    TYPE ztfi_mml_data-curtp    VALUE '32'.
DATA: BEGIN OF it_wlcms OCCURS 0,
        matnr TYPE matnr,
        wlcms TYPE string,
      END OF it_wlcms.
DATA:r_posid TYPE RANGE OF prps-posid WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS : p_gjahr TYPE ztfi_mml_data-gjahr DEFAULT sy-datum(4) OBLIGATORY.
  PARAMETERS : p_poper TYPE ztfi_mml_data-poper DEFAULT sy-datum+4(2) OBLIGATORY.
  SELECT-OPTIONS:s_segm  FOR acdoca-segment.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t2.
  PARAMETERS:p1 RADIOBUTTON GROUP grd1 USER-COMMAND click DEFAULT 'X',
             p2 RADIOBUTTON GROUP grd1,
             p3 RADIOBUTTON GROUP grd1.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN FUNCTION KEY:1.

INITIALIZATION.
  t1 = '年度期间'.
  t2 = '功能选择'.
  %_p_gjahr_%_app_%-text = '年度'.
  %_p_poper_%_app_%-text = '期间'.
  %_s_segm_%_app_%-text = '产业公司'.
  %_p1_%_app_%-text = '往年项目未实现明细表'.
  %_p2_%_app_%-text = '本年项目未实现明细表'.
  %_p3_%_app_%-text = '本年项目已实现明细表'.
  sscrfields-functxt_01  = '@0J@分签项目维护表'.


AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM callprog(zpubform) USING 'ZTFI218' 'V'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

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
  DATA:lv_sdat TYPE sy-datum.
  DATA:z218_where TYPE string,
       zmml_where TYPE string.
  REFRESH: itab,it_wlcms,r_posid.
  lv_sdat = p_gjahr && p_poper+1(2) && '01'.
  CASE 'X'.
    WHEN p1.
      z218_where = | SUBSTRING( ZXMSDAT,1,4 ) < @P_GJAHR AND ( ZXMEDAT IS INITIAL OR ZXMEDAT >= @LV_SDAT )  |.
      zmml_where = ` ZMLB_NUM = @ZMLB_NUM `     &&
                   ` AND  GJAHR    < @P_GJAHR ` &&
                   ` AND  CURTP    = @CURTP `   &&
                   ` AND  SEGMENT IN @S_SEGM `  &&
                   ` AND  POSID   IN @R_POSID `.
    WHEN p2.

    WHEN p3.


  ENDCASE.
  SELECT
    *
  FROM ztfi218
  WHERE (z218_where)
  INTO TABLE @DATA(lt_z218).
  LOOP AT lt_z218 INTO DATA(ls_z218).
    r_posid-sign = 'I'.
    r_posid-option = 'EQ'.
    r_posid-low = ls_z218-posid.
    COLLECT r_posid.
  ENDLOOP.
  IF r_posid[] IS NOT INITIAL.
    SELECT
        *
      FROM ztfi_mml_data
      WHERE (zmml_where)
      ORDER BY curtp,rbukrs,segment,werks,gjahr,poper
      INTO TABLE @itab.
  ENDIF.
  LOOP AT itab.
    it_wlcms-matnr = itab-matnr.
    COLLECT it_wlcms.
  ENDLOOP.
  PERFORM getlongtextpl(zpubform) TABLES it_wlcms.
  SORT it_wlcms BY matnr.
  LOOP AT itab.
    READ TABLE it_wlcms WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      itab-maktx = it_wlcms-wlcms.
      MODIFY itab TRANSPORTING maktx.
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

  SELECT
     tabname,position,fieldname
  FROM dd03l
  WHERE tabname   = 'ZTFI_MML_DATA'
    AND  as4local  =  'A'
    AND  fieldname <> '.INCLUDE'
  ORDER BY tabname,position,fieldname
  INTO TABLE @DATA(it_dd03l).
  IF sy-subrc NE 0 .
    RETURN.
  ENDIF.
  SELECT
   tabname,fieldname,ddtext
  FROM dd03m
  WHERE tabname  = 'ZTFI_MML_DATA'
   AND  rollstat = 'A'
   AND  fldstat  = 'A'
   AND  ddlanguage = @sy-langu
  ORDER BY tabname,fieldname
  INTO TABLE @DATA(it_dd03m).

  SELECT
     tabname,fieldname,ddtext
  FROM dd03t
  WHERE tabname  = 'ZTFI_MML_DATA'
    AND  as4local = 'A'
    AND  ddlanguage = @sy-langu
  ORDER BY tabname,fieldname
  INTO TABLE @DATA(it_dd03t).
  CLEAR:fldct[].
  LOOP AT it_dd03l INTO DATA(dd03l).
    READ TABLE it_dd03m INTO DATA(dd03m) WITH KEY tabname = dd03l-tabname  fieldname = dd03l-fieldname BINARY SEARCH.
    IF sy-subrc = 0.
      PERFORM catlg_set TABLES fldct USING : dd03l-fieldname dd03m-ddtext dd03l-tabname dd03l-fieldname.
    ELSE.
      READ TABLE it_dd03t INTO DATA(dd03t) WITH KEY tabname = dd03l-tabname  fieldname = dd03l-fieldname BINARY SEARCH.
      IF sy-subrc = 0.
        PERFORM catlg_set TABLES fldct USING : dd03l-fieldname dd03t-ddtext dd03l-tabname dd03l-fieldname.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM catlg_set TABLES fldct
                    USING:
  'MAKTX' '物料长描述'  'MAKT'  'MAKTX'.

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
      SET TITLEBAR 'TIT' WITH '工程分签项目报表-往年未实现'.
    WHEN p2.
      SET TITLEBAR 'TIT' WITH '工程分签项目报表-本年未实现'.
    WHEN p3.
      SET TITLEBAR 'TIT' WITH '工程分签项目报表-本年已实现'.
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
     