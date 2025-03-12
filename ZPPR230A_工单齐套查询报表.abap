*&---------------------------------------------------------------------*
*& Report ZPPR230A
*&---------------------------------------------------------------------*
*& 工单齐套查询报表
*&---------------------------------------------------------------------*
REPORT zppr230a MESSAGE-ID zmsg_gp.

TABLES:sscrfields,t001l,ztpp_206,ztpp_206q,ztpp_206q1.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA: gt_zqtzt TYPE TABLE OF dd07v WITH HEADER LINE.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE TYPE ztpp_206q1.
DATA:  erdat   TYPE ztpp_206q-erdat,
       ertim   TYPE ztpp_206q-ertim,
       zqtztms TYPE ddtext,            "齐套状态描述
       zpcdh   TYPE ztpp_206-zpcdh,
       psttr   TYPE ztpp_206-psttr,    "需求日期
       name1   TYPE ztpp_206-name1,    "客户名称
       post1   TYPE ztpp_206-post1,    "项目名称
       matnr   TYPE ztpp_206-matnr,    "
       zzl1    TYPE mara-zzl1,         "品名
       zmlcms  TYPE string,            "物料长描述
       zgdzl   TYPE ztpp_206-gsmng,    "工单总量
       sel,
     END OF itab.
DATA:itab1 LIKE itab[] WITH HEADER LINE.
DATA:BEGIN OF gt_gdzl OCCURS 0,
       aufnr TYPE ztpp_206-aufnr,
       zgdzl TYPE ztpp_206-gsmng,
     END OF gt_gdzl.
DATA: BEGIN OF it_wlcms OCCURS 0,
        matnr TYPE matnr,
        wlcms TYPE string,
      END OF it_wlcms.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS: p_werks TYPE t001l-werks OBLIGATORY.
  SELECT-OPTIONS:s_zqth  FOR ztpp_206q1-zqth,
                 s_aufnr FOR ztpp_206-aufnr,
                 s_post1 FOR ztpp_206-post1,
                 s_erdat FOR ztpp_206q-erdat.
SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.
  PERFORM getdomain(zpubform) TABLES gt_zqtzt USING 'ZD_ZQTZT'.

INITIALIZATION.
  t1 = '功能选择'.
  %_p_werks_%_app_%-text = '工厂'.
  %_s_zqth_%_app_%-text = '齐套号'.
  %_s_aufnr_%_app_%-text = '工单编码'.
  %_s_post1_%_app_%-text = '项目名称'.
  %_s_erdat_%_app_%-text = '齐套日期'.


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
  REFRESH:itab,itab1,it_wlcms.

  SELECT
      206q~erdat,
      206q~ertim,
      206q1~zqtzt,
      206q1~zqth,
      206q1~zqthh,
      206q1~aufnr,
      206q1~werks,
      z206~psttr,
      z206~zpcdh,
      z206~name1,
      z206~post1,
      ma~zzl1,
      z206~zms * z206~zks AS zgdzl
  FROM ztpp_206q AS 206q
  INNER JOIN ztpp_206q1 AS 206q1 ON 206q~zqth = 206q1~zqth
  INNER JOIN ztpp_206 AS z206 ON z206~aufnr = 206q1~aufnr
  INNER JOIN mara     AS ma   ON ma~matnr = z206~matnr
  WHERE 206q~werks  = @p_werks
    AND 206q~zqth   IN @s_zqth
    AND 206q1~aufnr IN @s_aufnr
    AND z206~post1  IN @s_post1
    AND 206q~erdat  IN @s_erdat
    AND 206q~zdelbs IS INITIAL
    AND 206q1~zdelbs IS INITIAL
  INTO CORRESPONDING FIELDS OF TABLE @itab1.
  REFRESH gt_gdzl.
  LOOP AT itab1.
    IF itab1-aufnr IS NOT INITIAL.
      gt_gdzl-aufnr = itab1-aufnr.
      gt_gdzl-zgdzl = itab1-zgdzl.
      COLLECT gt_gdzl.
    ENDIF.
    IF itab1-matnr IS NOT INITIAL.
      it_wlcms-matnr = itab1-matnr.
      COLLECT it_wlcms.
    ENDIF.
  ENDLOOP.
  SORT gt_gdzl BY aufnr.
  SORT itab1 BY zqth zqthh aufnr.
  DELETE ADJACENT DUPLICATES FROM itab1 COMPARING zqth zqthh aufnr.
  PERFORM getlongtextpl(zpubform) TABLES it_wlcms.
  SORT it_wlcms BY matnr.
  LOOP AT itab1.
    CLEAR:itab.
    itab-zqtzt = itab1-zqtzt.
    READ TABLE gt_zqtzt WITH KEY domvalue_l = itab1-zqtzt BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zqtztms = gt_zqtzt-ddtext.
    ENDIF.
    itab-zqth = itab1-zqth.
    itab-zqthh = itab1-zqthh.
    itab-psttr = itab1-psttr.
    itab-aufnr = itab1-aufnr.
    itab-zpcdh = itab1-zpcdh.
    itab-werks = itab1-werks.
    itab-name1 = itab1-name1.
    itab-post1 = itab1-post1.
    itab-zzl1 = itab1-zzl1.
    itab-matnr = itab1-matnr.
    READ TABLE it_wlcms WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zmlcms = it_wlcms-wlcms.
    ENDIF.
    READ TABLE gt_gdzl WITH KEY aufnr = itab-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zgdzl = gt_gdzl-zgdzl.
    ENDIF.
    itab-erdat = itab1-erdat.
    itab-ertim = itab1-ertim.
    APPEND itab.
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
'ZQTZTMS '  '齐套状态'    '       '      '    ',
'ZQTH  '    '齐套号'      'ZTPP_206Q1'   'ZQTH  ',
'ZQTHH '    '齐套行号'    'ZTPP_206Q1'   'ZQTHH ',
'PSTTR '    '需求日期'    'ZTPP_206'     'PSTTR ',
'AUFNR '    '工单号'      'ZTPP_206Q1'   'AUFNR ',
'ZPCDH '    '排产单号'    'ZTPP_206'     'ZPCDH ',
'WERKS '    '纵剪工厂'    'ZTPP_206Q1'   'WERKS ',
'NAME1 '    '客户名称'    'ZTPP_206'     'NAME1 ',
'POST1 '    '项目名称'    'ZTPP_206'     'POST1 ',
'ZZL1  '    '产品名称'    'MARA'         'ZZL1  ',
'ZWLCMS'    '产品长描述'  '    '         '      ',
'ZGDZL '    '工单总量'    'ZTPP_206'     'GSMNG ',
'ERDAT '    '创建日期'    'ZTPP_206Q'    'ERDAT ',
'ERTIM'     '创建时间'    'ZTPP_206Q'    'ERTIM'.

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
    WHEN 'ZPCDH' OR 'AUFNR'.
      ls_fldct-hotspot = 'X'.
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
        WHEN 'ZPCDH'.
          SUBMIT zppd201_v4 WITH p_submit = 'X'
                            WITH p_werks = wa-werks
                            WITH p_zpcdh  = wa-zpcdh
                            AND RETURN.
        WHEN 'AUFNR'.
          IF wa-aufnr IS NOT INITIAL.
            PERFORM co03(zpubform) USING wa-aufnr.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZJWH'.
      PERFORM zjwh.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.

**********************************************************************
*& 创建纵剪
**********************************************************************
FORM zjwh.
  DATA:r_aufnr TYPE RANGE OF ztpp_206-aufnr WITH HEADER LINE,
       r_charg TYPE RANGE OF mch1-charg WITH HEADER LINE.
  TYPES:BEGIN OF ty_zqth,
          zqth TYPE ztpp_206q-zqth,
        END OF ty_zqth.
  DATA:lt_zqth TYPE TABLE OF ty_zqth WITH KEY zqth,
       ls_zqth LIKE LINE OF lt_zqth.
  DATA:num TYPE i.

  LOOP AT itab WHERE sel = 'X'.
    num += 1.
    r_aufnr-sign = 'I'.
    r_aufnr-option = 'EQ'.
    r_aufnr-low = itab-aufnr.
    COLLECT:r_aufnr.
    ls_zqth-zqth = itab-zqth.
    COLLECT ls_zqth INTO lt_zqth.
  ENDLOOP.
  IF num < 1.
    MESSAGE s003 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  SELECT
    charg
  FROM ztpp210 AS a
  INNER JOIN @lt_zqth AS q ON q~zqth = a~zqth
  WHERE charg IS NOT INITIAL
  GROUP BY charg
  INTO TABLE @DATA(lt_charg).
  LOOP AT lt_charg INTO DATA(ls_charg).
    r_charg-sign = 'I'.
    r_charg-option = 'EQ'.
    r_charg-low = ls_charg-charg.
    COLLECT:r_charg.
  ENDLOOP.
  SUBMIT 