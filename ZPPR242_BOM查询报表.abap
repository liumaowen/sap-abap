*&---------------------------------------------------------------------*
*& Report ZPPR242
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr242 MESSAGE-ID zgp_msg.

TABLES:sscrfields,t001l,afko,mast,t023.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0,
       werks     TYPE mast-werks,
       matnr     TYPE mast-matnr,
       cpwlcms   TYPE string,
       stlal     TYPE mast-stlal,
       idnrk     TYPE stpo-idnrk,
       menge     TYPE stpo-menge,
       meins     TYPE stpo-meins,
       zjcpwlcms TYPE string,
       zgylx     TYPE char1,
       zscbb     TYPE char1,
       andat     TYPE mast-andat,
     END OF itab.
DATA:BEGIN OF lt_mapl OCCURS 0,
       werks TYPE mapl-werks,
       matnr TYPE mapl-matnr,
     END OF lt_mapl.
DATA:BEGIN OF lt_mkal OCCURS 0,
       werks TYPE mkal-werks,
       matnr TYPE mkal-matnr,
     END OF lt_mkal.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR mast-werks OBLIGATORY NO INTERVALS NO-EXTENSION,
                 s_andat FOR mast-andat,
                 s_matnr FOR mast-matnr.
*                 s_matkl FOR t023-matkl NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_andat_%_app_%-text = '创建日期'.
  %_s_matnr_%_app_%-text = '物料'.
*  %_s_matkl_%_app_%-text = '物料组'.


AT SELECTION-SCREEN.

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
  SELECT
    mast~werks,
    mast~matnr,
    mast~stlal,
    mast~andat,
    stpo~idnrk,
    stpo~menge,
    stpo~meins
    INTO CORRESPONDING FIELDS OF TABLE @itab
    FROM mast
    JOIN stas ON stas~stlnr = mast~stlnr AND stas~stlal = mast~stlal
    JOIN stpo ON stpo~stlkn = stas~stlkn AND stpo~stlnr = stas~stlnr
    WHERE mast~werks IN @s_werks
    AND mast~matnr IN @s_matnr
    AND mast~andat IN @s_andat.
  IF itab[] IS INITIAL.
    MESSAGE s004 WITH '无数据' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  SELECT werks,matnr INTO TABLE @lt_mapl FROM mapl WHERE werks IN @s_werks AND matnr IN @s_matnr.
  SORT lt_mapl BY matnr.
  SELECT werks,matnr INTO TABLE @lt_mkal FROM mkal WHERE werks IN @s_werks AND matnr IN @s_matnr.
  SORT lt_mkal BY matnr.
  LOOP AT itab.
    PERFORM getlongtext(zpubform) USING 'GRUN' itab-matnr 'MATERIAL' CHANGING itab-cpwlcms.
    PERFORM getlongtext(zpubform) USING 'GRUN' itab-idnrk 'MATERIAL' CHANGING itab-zjcpwlcms.
    READ TABLE lt_mapl WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zgylx = 'X'.
    ENDIF.
    READ TABLE lt_mkal WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zscbb = 'X'.
    ENDIF.
    MODIFY itab.
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
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局

  PERFORM catlg_set TABLES fldct
                    USING:
'WERKS '  '工厂'  'MAST'  'WERKS',
'MATNR '  '成品物料号'  'MAST'  'MATNR',
'CPWLCMS '  '成品物料描述'  ''  '',
'STLAL '  '备选'  'MAST'  'STLAL',
'IDNRK '  '组件物料号'  'MAST'  'MATNR',
'ZJCPWLCMS '  '组件物料描述'  ''  '',
'MENGE '  '数量'  ''  '',
'MEINS '  '单位'  ''  '',
'ZGYLX '  '工艺路线'  ''  '',
'ZSCBB '  '生产版本'  ''  '',
'ANDAT '  'BOM创建时间'  'MAST'  'ANDAT'.

  i_title               = lines( itab ) .
  CONDENSE i_title.
  CONCATENATE '条目数:' i_title INTO i_title.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = repid
      it_fieldcat        = fldct[]
      i_save             = 'A'
      is_variant         = varnt
      is_layout          = slayt
      i_grid_title       = i_title
*     IT_EVENTS          = GT_EVENTS
    TABLES
      t_outtab           = itab[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
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