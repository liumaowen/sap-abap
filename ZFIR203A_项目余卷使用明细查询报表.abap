*&---------------------------------------------------------------------*
*& Report ZFIR203A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir203a MESSAGE-ID zgp_msg.

TABLES:t001w,aufm.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_itab,
        z01       TYPE  atwrt,         "自编号
        clabs     TYPE mchb-clabs,     "库存数量
        zysje     TYPE ztfi215-zkhje,  "原始金额
        post1     TYPE proj-post1,     "项目名称
        syposid   TYPE prps-posid,     "使用项目
        sypost1   TYPE prps-post1,     "使用项目名称
        maktx     TYPE makt-maktx.
        INCLUDE   TYPE ztfi215.
TYPES:  werksname TYPE t001w-name1,
        sel,
      END OF ty_itab.
DATA: itab    TYPE TABLE OF ty_itab WITH EMPTY KEY,
      itab1   TYPE TABLE OF ty_itab WITH EMPTY KEY,
      wa_itab LIKE LINE OF itab.
DATA:it_mcha TYPE TABLE OF mcha WITH HEADER LINE,
     o_pctx  TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
TYPES:BEGIN OF ty_matc,
        werks TYPE t001w-werks,
        matnr TYPE matnr,
        charg TYPE charg_d,
        zdate TYPE ztfi215-zdate,
      END OF ty_matc.
DATA:gt_matc TYPE TABLE OF ty_matc WITH NON-UNIQUE KEY werks matnr charg zdate,
     gs_matc LIKE LINE OF gt_matc.
DATA:s_budat_lt TYPE RANGE OF aufm-budat WITH HEADER LINE.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:
                 s_werks FOR t001w-werks OBLIGATORY NO-EXTENSION NO INTERVALS,
                 s_budat FOR aufm-budat.
SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.

INITIALIZATION.
  t1 = '功能选择'.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_budat_%_app_%-text = '过账日期'.


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
  REFRESH:itab,itab1,s_budat_lt.
  LOOP AT s_budat WHERE high IS NOT INITIAL.
    s_budat_lt-low = s_budat-high.
    s_budat_lt-sign = 'I'.
    s_budat_lt-option = 'LT'.
    APPEND s_budat_lt.
  ENDLOOP.

  SELECT
    z215~werks,
    t001w~name1 AS werksname,
    z215~kcmatnr,
    makt~maktx,
    z215~charg,
    z215~meins,
    z215~zkhdj,
    z215~zdzbl,
    z215~zdzkhdj,
    proj~pspid,
    proj~post1,
    z215~zdate
  FROM ztfi215 AS z215
  INNER JOIN t001w ON t001w~werks = z215~werks
  INNER JOIN makt  ON makt~matnr = z215~kcmatnr AND makt~spras = @sy-langu
  INNER JOIN proj  ON proj~pspid = z215~zpspid
  WHERE z215~werks IN @s_werks
    AND z215~zdate IN @s_budat
  INTO CORRESPONDING FIELDS OF TABLE @itab1.

  IF itab1 IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH: it_mcha,gt_matc.
  LOOP AT itab1 INTO wa_itab.
    it_mcha-matnr = wa_itab-kcmatnr.
    it_mcha-charg = wa_itab-charg.
    gs_matc-werks = wa_itab-werks.
    gs_matc-matnr = wa_itab-kcmatnr.
    gs_matc-charg = wa_itab-charg.
    gs_matc-charg = wa_itab-charg.
    gs_matc-zdate = wa_itab-zdate.
    COLLECT it_mcha.
    COLLECT gs_matc INTO gt_matc.
  ENDLOOP.
*获取批次特征值
  CALL FUNCTION 'ZFMS_05_GETPCTX'
    EXPORTING
      atnam  = 'Z01'
    TABLES
      intab  = it_mcha
      outtab = o_pctx
    EXCEPTIONS
      OTHERS = 1.
  SORT o_pctx BY matnr charg atnam.

  "使用数量
  SELECT
    aufm~werks,
    aufm~matnr,
    aufm~charg,
    aufm~aufnr,
    pr~posid,
    pr~post1,
    SUM( CASE WHEN aufm~shkzg = 'S' THEN 0 - aufm~menge ELSE aufm~menge END ) AS prlab
  FROM aufm
  INNER JOIN @gt_matc AS matc ON  matc~werks = aufm~werks
                              AND matc~matnr = aufm~matnr
                              AND matc~charg = aufm~charg
                              AND aufm~budat > matc~zdate
  LEFT JOIN afpo     AS ao   ON  ao~aufnr   = aufm~aufnr
  LEFT JOIN prps     AS pr   ON  ao~projn   = pr~pspnr
  WHERE aufm~budat IN @s_budat
  GROUP BY aufm~werks,aufm~matnr,aufm~charg,aufm~aufnr,pr~posid,pr~post1
  ORDER BY aufm~werks, aufm~matnr, aufm~charg
  INTO TABLE @DATA(lt_aufm).

*赋值alv内表
  LOOP AT itab1 ASSIGNING FIELD-SYMBOL(<fs_itab>).
    READ TABLE o_pctx WITH KEY matnr = <fs_itab>-kcmatnr charg = <fs_itab>-charg atnam = 'Z01' BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_itab>-z01 = o_pctx-atwrt.
    ENDIF.
    READ TABLE lt_aufm INTO DATA(ls_aufm) WITH KEY werks = <fs_itab>-werks
                                                   matnr = <fs_itab>-kcmatnr
                                                   charg = <fs_itab>-charg BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT lt_aufm INTO ls_aufm FROM sy-tabix.
        IF  ls_aufm-werks <> <fs_itab>-werks OR ls_aufm-matnr <> <fs_itab>-kcmatnr OR ls_aufm-charg <> <fs_itab>-charg.
          EXIT.
        ENDIF.
        <fs_itab>-clabs = ls_aufm-prlab.
        <fs_itab>-syposid = ls_aufm-posid.
        <fs_itab>-sypost1 = ls_aufm-post1.
        <fs_itab>-zysje = <fs_itab>-clabs * <fs_itab>-zkhdj.
        <fs_itab>-zdzje = <fs_itab>-clabs * <fs_itab>-zdzkhdj.
        IF <fs_itab>-clabs > 0 AND <fs_itab>-syposid+0(8) <> <fs_itab>-pspid.
          APPEND <fs_itab> TO itab.
        ENDIF.
      ENDLOOP.
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
   'WERKS       '  '工厂        '  'ZTFI215'  'WERKS',
   'WERKSNAME   '  '工厂描述     '  'T001W'    'NAME1',
   'KCMATNR     '  '物料号       '  'ZTFI215'  'KCMATNR',
   'MAKTX       '  '物料描述     '  'MAKT'      'MAKTX',
   'CHARG       '  '批次         '  'ZTFI215'  'CHARG',
   'Z01         '  '自编号       '  ''          '',
   'CLABS       '  '使用数量     '  'ZTFI215'  'PRLAB',
   'SYPOSID     '  '使用项目     '  'PRPS   '  'POSID',
   'SYPOST1     '  '使用项目描述  '  'PRPS   '  'POST1',
   'MEINS       '  '单位         '  'ZTFI215'  'MEINS',
   'ZKHDJ       '  '原始单价     '  'ZTFI215'  'ZKHDJ',
   'ZYSJE       '  '原始金额     '  'ZTFI215'  'ZKHJE',
   'ZDZBL       '  '打折比例(%)  '  'ZTFI215'  'ZDZBL',
   'ZDZKHDJ     '  '打折单价     '  'ZTFI215'  'ZDZKHDJ',
   'ZDZJE       '  '打折金额     '  'ZTFI215'  'ZDZJE',
   'PSPID       '  '原项目       '  'ZTFI215'  'PSPID',
   'POST1       '  '原项目描述   '   'PROJ'     'POST1'.

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
      t_outtab                 = itab
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