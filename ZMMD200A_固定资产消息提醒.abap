*&---------------------------------------------------------------------*
*& Report ZMMD200A
*&---------------------------------------------------------------------*
*& 固定资产消息提醒
*&---------------------------------------------------------------------*
REPORT zmmd200a.

TABLES:acdoca,ekko.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_seg,
        segment TYPE fagl_segm-segment,
      END OF ty_seg.
DATA:gt_seg TYPE TABLE OF ty_seg WITH KEY segment,
     gs_seg LIKE LINE OF gt_seg.
DATA:BEGIN OF gt_ekpo OCCURS 0,
       ebeln TYPE ekpo-ebeln,
       ebelp TYPE ekpo-ebelp,
       lifnr TYPE ekko-lifnr,
       name1 TYPE lfa1-name1,
       txz01 TYPE ekpo-txz01,
       zyjdh TYPE ekpo-zyjdh,
       ekgrp TYPE ekko-ekgrp,
       eknam TYPE t024-eknam,
       days  TYPE i,
     END OF gt_ekpo.
DATA:BEGIN OF gt_fk OCCURS 0,
       ebeln   TYPE ekpo-ebeln,
       lifnr   TYPE ekko-lifnr,
       name1   TYPE lfa1-name1,
       zfktj   TYPE ztmm204-zfktj,
       zfktjms TYPE ddtext,         "付款条件描述
       zfkje   TYPE ztmm204-zfkje,
       zfkrq   TYPE ztmm204-zfkrq,
       ekgrp   TYPE ekko-ekgrp,
       eknam   TYPE t024-eknam,
       days    TYPE i,
     END OF gt_fk.
DATA:BEGIN OF gt_dh OCCURS 0,
       ebeln TYPE ekpo-ebeln,
       ebelp TYPE ekpo-ebelp,
       lifnr TYPE ekko-lifnr,
       name1 TYPE lfa1-name1,
       txz01 TYPE ekpo-txz01,
       budat TYPE ztmm201-budat,
       ekgrp TYPE ekko-ekgrp,
       eknam TYPE t024-eknam,
       days  TYPE i,
     END OF gt_dh.
DATA: gt_zfktj TYPE TABLE OF dd07v WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK ss1 WITH FRAME TITLE t2.
  PARAMETERS:p_ek RADIOBUTTON GROUP grd1 USER-COMMAND singleclick DEFAULT 'X',
             p_fk RADIOBUTTON GROUP grd1,
             p_dh RADIOBUTTON GROUP grd1.
SELECTION-SCREEN END OF BLOCK ss1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS:
    p_ekorg TYPE ekko-ekorg DEFAULT '3000',
    p_bsart TYPE ekko-bsart DEFAULT 'Z06'.
  SELECT-OPTIONS:s_ebeln FOR ekko-ebeln,
                 s_lifnr FOR ekko-lifnr.
SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.
  PERFORM getdomain(zpubform) TABLES gt_zfktj USING 'ZD_ZFKTJ'.

INITIALIZATION.
  t1 = '功能选择'.
  t2 = '条件选择'.
  %_p_ekorg_%_app_%-text = '采购组织'.
  %_p_bsart_%_app_%-text = '采购类型'.
  %_s_ebeln_%_app_%-text = '采购订单号'.
  %_s_lifnr_%_app_%-text = '供应商'.
  %_p_ek_%_app_%-text = '采购订单'.
  %_p_fk_%_app_%-text = '付款计划'.
  %_p_dh_%_app_%-text = '到货通知'.


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.
*  CASE 'X'.
*    WHEN p_ek.
*      p_bsart = 'Z06'.
*    WHEN p_fk.
*      p_bsart = 'Z05'.
*  ENDCASE.
  LOOP AT SCREEN .
    IF screen-name = 'P_EKORG' OR screen-name = 'P_BSART'. "不可编辑
      screen-input = '0'.
      MODIFY SCREEN .
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  IF sy-batch = 'X'.
    PERFORM send.
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
  CASE 'X'.
    WHEN p_ek.
      PERFORM getek.
    WHEN p_fk.
      PERFORM getfk.
    WHEN p_dh.
      PERFORM getdh.
  ENDCASE.
ENDFORM.
FORM getek.
  DATA:lv_dat TYPE sy-datlo.
  SELECT
      ekko~lifnr,
      lfa1~name1,
      ekpo~ebeln,
      ekpo~ebelp,
      ekpo~txz01,
      ekpo~zyjdh,
      ekko~ekgrp,
      t024~eknam
  FROM ekko
  INNER JOIN ekpo ON ekko~ebeln = ekpo~ebeln
  LEFT JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
  LEFT JOIN t024 ON t024~ekgrp = ekko~ekgrp
  WHERE ekko~ekorg = @p_ekorg
    AND ekko~bsart = @p_bsart
    AND ekko~ebeln IN @s_ebeln
    AND ekko~lifnr IN @s_lifnr
    AND ekpo~zyjdh IS NOT INITIAL
  INTO CORRESPONDING FIELDS OF TABLE @gt_ekpo.
  lv_dat = sy-datlo.
  LOOP AT gt_ekpo.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = lv_dat
        i_date_to   = gt_ekpo-zyjdh
      IMPORTING
        e_days      = gt_ekpo-days.
    MODIFY gt_ekpo.
  ENDLOOP.
ENDFORM.
FORM getfk.
  DATA:lv_dat TYPE sy-datlo.
  SELECT
      ekko~lifnr,
      lfa1~name1,
      ztmm204~ebeln,
      ztmm204~zfktj,
      ztmm204~zfkje,
      ztmm204~zfkrq,
      ekko~ekgrp,
      t024~eknam
  FROM ztmm204
  INNER JOIN ekko ON ekko~ebeln = ztmm204~ebeln
  LEFT JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
  LEFT JOIN t024 ON t024~ekgrp = ekko~ekgrp
  WHERE ekko~ekorg = @p_ekorg
    AND ekko~bsart = @p_bsart
    AND ekko~ebeln IN @s_ebeln
    AND ekko~lifnr IN @s_lifnr
    AND ztmm204~zfkrq IS NOT INITIAL
  INTO CORRESPONDING FIELDS OF TABLE @gt_fk.
  lv_dat = sy-datlo.
  LOOP AT gt_fk.
    READ TABLE gt_zfktj WITH KEY domvalue_l = gt_fk-zfktj BINARY SEARCH.
    IF sy-subrc = 0.
      gt_fk-zfktjms = gt_zfktj-ddtext.
    ENDIF.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = lv_dat
        i_date_to   = gt_fk-zfkrq
      IMPORTING
        e_days      = gt_fk-days.
    MODIFY gt_fk.
  ENDLOOP.
ENDFORM.
FORM getdh.
  DATA:lv_dat TYPE sy-datlo.
  SELECT
      ekko~lifnr,
      lfa1~name1,
      ekpo~ebeln,
      ekpo~ebelp,
      ekpo~txz01,
      ztmm201~budat,
      ekko~ekgrp,
      t024~eknam
  FROM ztmm201
  INNER JOIN ztmm202 ON ztmm201~zdhdh = ztmm202~zdhdh
  INNER JOIN ekpo ON ekpo~ebeln = ztmm202~ebeln AND ekpo~ebelp = ztmm202~ebelp
  INNER JOIN ekko    ON ekko~ebeln = ekpo~ebeln
  LEFT JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
  LEFT JOIN t024 ON t024~ekgrp = ekko~ekgrp
  WHERE ekko~ekorg = @p_ekorg
    AND ekko~bsart = @p_bsart
    AND ekko~ebeln IN @s_ebeln
    AND ekko~lifnr IN @s_lifnr
    AND ztmm201~budat IS NOT INITIAL
  INTO CORRESPONDING FIELDS OF TABLE @gt_dh.
  SORT gt_dh BY ebeln ebelp budat.
  DELETE ADJACENT DUPLICATES FROM gt_dh COMPARING ebeln ebelp budat.
  lv_dat = sy-datlo.
  LOOP AT gt_dh.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = gt_dh-budat
        i_date_to   = lv_dat
      IMPORTING
        e_days      = gt_dh-days.
    MODIFY gt_dh.
  ENDLOOP.
ENDFORM.
FORM alvshow.
  CASE 'X'.
    WHEN p_ek.
      PERFORM alvshow_ek.
    WHEN p_fk.
      PERFORM alvshow_fk.
    WHEN p_dh.
      PERFORM alvshow_dh.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form alvshow_ek
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alvshow_ek .
  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
*  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局
  REFRESH:fldct.
  PERFORM catlg_set TABLES fldct
                    USING:
   'LIFNR   '  '供应商     '      'EKKO  '    'LIFNR   ',
   'NAME1   '  '供应商描述  '      'LFA1    '    'NAME1     ',
   'EBELN   '  '采购订单号  '      'EKPO'    'EBELN',
   'EBELP  '  '采购订单行号'      'EKPO'    'EBELP    ',
   'TXZ01  '  '物料短文本 '      'EKPO'    'TXZ01',
   'ZYJDH '  '预计到货日期'       'EKPO'    'ZYJDH    ',
   'DAYS  '  '差'       ''    '    ',
   'EKGRP  '  '采购组'      'EKKO    '    'EKGRP     ',
   'EKNAM  '  '采购组描述'   'T024    '    'EKNAM     '.

  i_title               = lines( gt_ekpo ) .
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
      t_outtab                 = gt_ekpo[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alvshow_fk
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alvshow_fk .
  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
*  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局
  REFRESH:fldct.
  PERFORM catlg_set TABLES fldct
                    USING:
   'LIFNR   '  '供应商     '      'EKKO  '    'LIFNR   ',
   'NAME1   '  '供应商描述  '      'LFA1    '    'NAME1     ',
   'EBELN   '  '采购订单号  '      'EKPO'    'EBELN',
   'ZFKTJMS  '  '付款条件'      ''    '    ',
   'ZFKJE  '   '付款金额 '      'ZTMM204'    'ZFKJE',
   'ZFKRQ '    '付款日期'       'ZTMM204'    'ZFKRQ    ',
   'DAYS  '    '差'            ''    '    ',
   'EKGRP  '   '采购组'        'EKKO    '    'EKGRP     ',
   'EKNAM  '   '采购组描述'    'T024    '    'EKNAM     '.

  i_title               = lines( gt_fk ) .
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
      t_outtab                 = gt_fk[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alvshow_dh
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alvshow_dh .
  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
*  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局
  REFRESH:fldct.
  PERFORM catlg_set TABLES fldct
                    USING:
   'LIFNR   '  '供应商     '      'EKKO  '    'LIFNR   ',
   'NAME1   '  '供应商描述  '      'LFA1    '    'NAME1     ',
   'EBELN   '  '采购订单号  '      'EKPO'    'EBELN',
   'EBELP  '  '采购订单行号'      'EKPO'    'EBELP    ',
   'TXZ01  '  '物料短文本 '      'EKPO'    'TXZ01',
   'BUDAT '  '入库日期'       'ZTMM201'    'BUDAT    ',
   'DAYS  '  '差'       ''    '    ',
   'EKGRP  '  '采购组'      'EKKO    '    'EKGRP     ',
   'EKNAM  '  '采购组描述'   'T024    '    'EKNAM     '.

  i_title               = lines( gt_dh ) .
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
      t_outtab                 = gt_dh[]
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
  DATA wa LIKE LINE OF gt_ekpo.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CHECK rs_selfield-tabindex <> 0 . "小计行总计行什么的忽略
      READ TABLE gt_ekpo INTO wa INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN ''.

        WHEN OTHERS.
      ENDCASE.
    WHEN 'SEND'.
      PERFORM send.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.
FORM send.
  CASE 'X'.
    WHEN p_ek.
      PERFORM send_ek.
    WHEN p_fk.
      PERFORM send_fk.
    WHEN p_dh.
      PERFORM send_dh.
  ENDCASE.
ENDFORM.
FORM send_ek.
  DATA:lt_ek LIKE TABLE OF gt_ekpo WITH HEADER LINE.
  DATA:BEGIN OF lt_eknam OCCURS 0,
         ekgrp TYPE ekko-ekgrp,
         eknam TYPE t024-eknam,
       END OF lt_eknam.
  DATA:lv_xh TYPE char6.
  DATA: token   TYPE string,
        keyword TYPE string.
  DATA:ddstr TYPE string.

  LOOP AT gt_ekpo WHERE days = 10.
    MOVE-CORRESPONDING gt_ekpo TO lt_ek.
    APPEND lt_ek.
    lt_eknam-ekgrp = gt_ekpo-ekgrp.
    lt_eknam-eknam = gt_ekpo-eknam.
    COLLECT lt_eknam.
  ENDLOOP.
  SELECT SINGLE
  *
  INTO @DATA(ztsd230)
  FROM ztsd230
  WHERE zmokuai = 'MM'
  AND  zleixing = 'ZMM205B_EKPO'.
  token = ztsd230-token.
  keyword = ztsd230-keyword.

  SORT lt_ek BY ekgrp lifnr ebeln ebelp.
  LOOP AT lt_eknam.
    CLEAR:ddstr.
    LOOP AT lt_ek WHERE ekgrp = lt_eknam-ekgrp.
      CLEAR:lv_xh.
      lv_xh = |{ lt_ek-ebelp ALPHA = OUT }|.
      ddstr = ddstr && |供应商：{ lt_ek-name1 } \n\n|
                    && |采购单号：{ lt_ek-ebeln }  |
                    && |行号：{ lv_xh } \n\n|
                    && |物料：{ lt_ek-txz01 } \n\n|
                    && |到货日期：{ lt_ek-zyjdh+0(4) }-{ lt_ek-zyjdh+4(2) }-{ lt_ek-zyjdh+6(2) } \n\n|.
    ENDLOOP.
    IF ddstr IS NOT INITIAL AND token IS NOT INITIAL.
      ddstr = ddstr && '采购员：**'  && lt_ek-eknam && '**'.
      CALL FUNCTION 'ZFM_GP_DD_QXXTX'
        EXPORTING
          token   = token
          keyword = keyword
          mess    = ddstr
          msgtype = 'markdown'.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM send_fk.
  DATA:lt_fk LIKE TABLE OF gt_fk WITH HEADER LINE.
  DATA:BEGIN OF lt_eknam OCCURS 0,
         ekgrp TYPE ekko-ekgrp,
         eknam TYPE t024-eknam,
       END OF lt_eknam.
  DATA: token   TYPE string,
        keyword TYPE string.
  DATA:ddstr TYPE string.

  LOOP AT gt_fk WHERE days = 5.
    MOVE-CORRESPONDING gt_fk TO lt_fk.
    APPEND lt_fk.
    lt_eknam-ekgrp = gt_fk-ekgrp.
    lt_eknam-eknam = gt_fk-eknam.
    COLLECT lt_eknam.
  ENDLOOP.
  SELECT SINGLE
    *
  INTO @DATA(ztsd230)
  FROM ztsd230
  WHERE zmokuai = 'MM'
  AND  zleixing = 'ZMM205B_FK'.
  token = ztsd230-token.
  keyword = ztsd230-keyword.

  SORT lt_fk BY ekgrp lifnr ebeln.
  LOOP AT lt_eknam.
    CLEAR:ddstr.
    LOOP AT lt_fk WHERE ekgrp = lt_eknam-ekgrp.
      ddstr = ddstr && |供应商：{ lt_fk-name1 } \n\n|
                    && |采购单号：{ lt_fk-ebeln }  |
                    && |付款条件：{ lt_fk-zfktjms } \n\n|
                    && |付款金额：{ lt_fk-zfkje } \n\n|
                    && |付款日期：{ lt_fk-zfkrq+0(4) }-{ lt_fk-zfkrq+4(2) }-{ lt_fk-zfkrq+6(2) } \n\n|.
    ENDLOOP.
    IF ddstr IS NOT INITIAL AND token IS NOT INITIAL.
      ddstr = ddstr && '采购员：**'  && lt_fk-eknam && '**'.
      CALL FUNCTION 'ZFM_GP_DD_QXXTX'
        EXPORTING
          token   = token
          keyword = keyword
          mess    = ddstr
          msgtype = 'markdown'.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM send_dh.
  DATA:lt_dh LIKE TABLE OF gt_dh WITH HEADER LINE.
  DATA:BEGIN OF lt_eknam OCCURS 0,
         ekgrp TYPE ekko-ekgrp,
         eknam TYPE t024-eknam,
       END OF lt_eknam.
  DATA:lv_xh TYPE char6.
  DATA: token   TYPE string,
        keyword TYPE string.
  DATA:ddstr TYPE string.

  LOOP AT gt_dh WHERE days = 20.
    MOVE-CORRESPONDING gt_dh TO lt_dh.
    APPEND lt_dh.
    lt_eknam-ekgrp = gt_dh-ekgrp.
    lt_eknam-eknam = gt_dh-eknam.
    COLLECT lt_eknam.
  ENDLOOP.
  SELECT SINGLE
  *
  INTO @DATA(ztsd230)
  FROM ztsd230
  WHERE zmokuai = 'MM'
  AND  zleixing = 'ZMM205B_DHTZ'.
  token = ztsd230-token.
  keyword = ztsd230-keyword.

  SORT lt_dh BY ekgrp lifnr ebeln ebelp.
  LOOP AT lt_eknam.
    CLEAR:ddstr.
    LOOP AT lt_dh WHERE ekgrp = lt_eknam-ekgrp.
      CLEAR:lv_xh.
      lv_xh = |{ lt_dh-ebelp ALPHA = OUT }|.
      ddstr = ddstr && |供应商：{ lt_dh-name1 } \n\n|
                    && |采购单号：{ lt_dh-ebeln }  |
                    && |行号：{ lv_xh } \n\n|
                    && |物料：{ lt_dh-txz01 } \n\n|
                    && |入库日期：{ lt_dh-budat+0(4) }-{ lt_dh-budat+4(2) }-{ lt_dh-budat+6(2) } \n\n|.
    ENDLOOP.
    IF ddstr IS NOT INITIAL AND token IS NOT INITIAL.
      dds