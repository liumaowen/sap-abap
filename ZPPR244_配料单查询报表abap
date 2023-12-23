*&---------------------------------------------------------------------*
*& Report ZPPR244
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr244 MESSAGE-ID zgp_msg.

TABLES:lips,ztpp210,ausp.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA: BEGIN OF itab OCCURS 0,
        werks    TYPE ztpp210-werks,
        zpldh    TYPE ztpp210-zpldh,
        zdate    TYPE ztpp210-zdate,
        aufnr    TYPE ztpp210-aufnr,
        zpcdh    TYPE ztpp210-zpcdh,
        matnr    TYPE ztpp210-matnr,
        zzl1     TYPE mara-zzl1,
        zwlcms   TYPE string,
        matnrc   TYPE ztpp210-matnr, "成品物料号
        zzl1c    TYPE ztpp210-zzl1, "成品物料品名
        zwlcmsc  TYPE string, "成品物料长描述
        kunnr    TYPE ztpp_205-kunnr,
        name1    TYPE ztpp_205-name1,
        posid    TYPE ztpp210-posid,
        post1    TYPE ztpp210-post1,
        charg    TYPE ztpp210-charg,
        z01      TYPE ausp-atwrt,
        zpssl_in TYPE ztpp210-zpssl_in,
        zpssl    TYPE ztpp210-zpssl,
        prlab    TYPE labst,
        lgort    TYPE mspr-lgort,
        zdbdh    TYPE ztpp210-zdbdh,
        sel,
      END OF itab.
DATA: itab1 LIKE TABLE OF itab WITH HEADER LINE.
DATA:it_zsmm206 TYPE TABLE OF zsmm206 WITH HEADER LINE,
     o_zsmm206  TYPE TABLE OF zsmm206.
DATA: it_mchas TYPE TABLE OF mcha WITH HEADER LINE,
      it_pctx  TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
*物料长描述表
DATA: BEGIN OF it_cms OCCURS 0,
        matnr TYPE matnr,
        wlcms TYPE string,
      END OF it_cms.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR ztpp210-werks OBLIGATORY NO INTERVALS NO-EXTENSION,
                 s_zdate FOR ztpp210-zdate,
                 s_matnr FOR ztpp210-matnr,
                 s_z01 FOR ausp-atwrt,
                 s_zpldh FOR ztpp210-zpldh,
                 s_zdbdh FOR ztpp210-zdbdh,
                 s_aufnr FOR ztpp210-aufnr,
                 s_zpcdh FOR ztpp210-zpcdh.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_zdate_%_app_%-text = '创建日期'.
  %_s_matnr_%_app_%-text = '物料号'.
  %_s_z01_%_app_%-text = '自编号'.
  %_s_zpldh_%_app_%-text = '配料单号'.
  %_s_zdbdh_%_app_%-text = '调拨单号'.
  %_s_aufnr_%_app_%-text = '订单号'.
  %_s_zpcdh_%_app_%-text = '排产单号'.

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
  IF s_z01 IS NOT INITIAL.
    CALL FUNCTION 'ZFMS_16_GETPCTX'
      EXPORTING
        atnam  = 'Z01'
      TABLES
        r_tab  = s_z01[]
        outtab = it_zsmm206.
    APPEND LINES OF it_zsmm206[] TO o_zsmm206.

    IF o_zsmm206 IS NOT INITIAL.
      SELECT
        data~*
        FROM @o_zsmm206 AS data
        WHERE data~matnr IN @s_matnr
        AND data~werks IN @s_werks
        INTO TABLE @DATA(it_matnr1).
      IF sy-subrc EQ 0.
        CLEAR:s_matnr[].
        LOOP AT it_matnr1 INTO DATA(wa_matnr1).
          CLEAR:s_matnr.
          s_matnr+0(3) = 'IEQ'.
          s_matnr-low = wa_matnr1-matnr.
          COLLECT:s_matnr.
        ENDLOOP.
      ENDIF.
      IF s_matnr[] IS INITIAL .
        MESSAGE s009 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSE.
      MESSAGE s009 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  SELECT
  a~werks,
  a~zpldh,
  a~zdate,
  a~aufnr,
  a~zpcdh,
  a~matnr,
  c~zzl1,
  a~zzl1 AS zzl1c,
  a~matnrc,
  b~kunnr,
  b~name1,
  a~posid,
  a~post1,
  a~charg,
  a~zpssl_in,
  a~zpssl,
  a~zdbdh,
  a~lgort
  FROM ztpp210 AS a
  JOIN ztpp_205 AS b ON b~zpcdh = a~zpcdh
  JOIN mara AS c ON c~matnr = a~matnr
  WHERE a~werks IN @s_werks
  AND a~zdate IN @s_zdate
  AND a~matnr IN @s_matnr
  AND a~zpldh IN @s_zpldh
  AND a~zdbdh IN @s_zdbdh
  AND a~aufnr IN @s_aufnr
  AND a~zpcdh IN @s_zpcdh
  INTO CORRESPONDING FIELDS OF TABLE  @itab.

  IF itab[] IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH it_cms.
  LOOP AT itab INTO DATA(lw_itab).
    CLEAR: it_mchas.
    it_mchas-matnr = lw_itab-matnr.
    it_mchas-charg = lw_itab-charg.
    it_cms-matnr = lw_itab-matnr.
    COLLECT: it_mchas,it_cms.
    CLEAR it_cms.
    it_cms-matnr = lw_itab-matnrc.
    COLLECT it_cms.
  ENDLOOP.
  DELETE it_cms WHERE matnr IS INITIAL.
  PERFORM getlongtextpl(zpubform) TABLES it_cms.
  SORT it_cms BY matnr.
  CALL FUNCTION 'ZFMS_05_GETPCTX'
    EXPORTING
      atnam  = 'Z01'
    TABLES
      intab  = it_mchas
      outtab = it_pctx.
  SORT it_pctx BY matnr charg atnam.

  SELECT mspr~werks,
    mspr~matnr,
    mspr~lgort,
    mspr~charg,
    mspr~sobkz,
    mspr~pspnr,
    mspr~prlab
    INTO TABLE @DATA(it_mspr)
  FROM mspr
  FOR ALL ENTRIES IN @it_mchas
  WHERE mspr~prlab NE 0 AND mspr~werks IN @s_werks
  AND mspr~matnr = @it_mchas-matnr AND mspr~charg = @it_mchas-charg.
  SORT it_mspr BY matnr charg.

  SELECT mchb~werks,
   mchb~matnr,
   mchb~lgort,
   mchb~charg,
   mchb~clabs
   INTO TABLE @DATA(it_mchb)
   FROM mchb
   FOR ALL ENTRIES IN @it_mchas
   WHERE mchb~clabs NE 0 AND mchb~werks IN @s_werks
   AND mchb~matnr = @it_mchas-matnr AND mchb~charg = @it_mchas-charg.
  SORT it_mchb BY matnr charg.

  REFRESH itab1.
  LOOP AT itab.
    CLEAR itab1.
    READ TABLE it_cms WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zwlcms = it_cms-wlcms.
    ENDIF.
    READ TABLE it_cms WITH KEY matnr = itab-matnrc BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zwlcmsc = it_cms-wlcms.
    ENDIF.
    READ TABLE it_pctx WITH KEY matnr = itab-matnr charg = itab-charg atnam = 'Z01' BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-z01 = it_pctx-atwrt.
    ENDIF.
    "库存实际吨位 现在仓库
    READ TABLE it_mspr INTO DATA(wa_mspr) WITH KEY matnr = itab-matnr charg = itab-charg BINARY SEARCH.
    IF sy-subrc EQ 0.
      DATA(tabix) = sy-tabix.
      LOOP AT it_mspr INTO wa_mspr FROM tabix.
        IF wa_mspr-matnr NE itab-matnr OR wa_mspr-charg NE itab-charg.
          EXIT.
        ENDIF.
        itab-prlab = wa_mspr-prlab.
        itab-lgort = wa_mspr-lgort.
        MOVE itab TO itab1.
        APPEND itab1.
      ENDLOOP.
    ELSE.
      READ TABLE it_mchb INTO DATA(wa_mchb) WITH KEY matnr = itab-matnr charg = itab-charg BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(tabix1) = sy-tabix.
        LOOP AT it_mchb INTO wa_mchb FROM tabix1.
          IF wa_mchb-matnr NE itab-matnr OR wa_mchb-charg NE itab-charg.
            EXIT.
          ENDIF.
          itab-prlab = wa_mchb-clabs.
          itab-lgort = wa_mchb-lgort.
          MOVE itab TO itab1.
          APPEND itab1.
        ENDLOOP.
      ELSE.
        MOVE itab TO itab1.
        APPEND itab1.
      ENDIF.
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
  slayt-zebra             = 'X'. " 斑马线
  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局
  i_title               = lines( itab1 ) .
  CONDENSE i_title.
  CONCATENATE '条目数:' i_title INTO i_title.
  PERFORM catlg_set TABLES fldct USING:
    'WERKS '     '工厂'          'ZTPP210'  'WERKS',
    'ZPLDH '     '配料单号'      'ZTPP210'  'ZPLDH',
    'ZDATE '     '配料时间'      'ZTPP210'  'ZDATE',
    'AUFNR '     '订单号'        'ZTPP210'  'AUFNR',
    'ZPCDH '     '排产单号'      'ZTPP210'  'ZPCDH',
    'MATNR '     '物料号'        'ZTPP210'  'MATNR',
    'ZZL1 '      '物料品名'      'ZTPP210'  'ZZL1' ,
    'ZWLCMS '    '物料长描述'    ''         ''     ,
    'MATNRC '    '成品物料号'    'ZTPP210'  'MATNR',
    'ZZL1C '     '成品物料品名'  'ZTPP210'  'ZZL1' ,
    'ZWLCMSC '   '成品物料长描述'  ''       ''     ,
    'KUNNR '     '客户编码'      'ZTPP_205' 'KUNNR',
    'NAME1 '     '客户名称'      'ZTPP_205' 'NAME1',
    'POSID '     'WBS号'         'ZTPP210'  'POSID',
    'POST1 '     '项目名称'      'ZTPP210'  'POST1',
    'CHARG '     '批次号'        'ZTPP210'  'CHARG',
    'Z01 '       '自编号'        ''         ''     ,
    'ZPSSL_IN '  '配料米数'      'ZTPP210'  'ZPSSL_IN',
    'ZPPSSL '    '配料吨位'      'ZTPP210'  'ZPPSSL',
    'PRLAB '     '库存实际吨位'  'MSPR'     'PRLAB' ,
    'LGORT '     '现在仓库'      'MSPR'     'LGORT' ,
    'ZDBDH '     '调拨单号'      'ZTPP210'  'ZDBDH' .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = repid
      it_fieldcat        = fldct[]
      i_save             = 'A'
      is_variant         = varnt
      is_layout          = slayt
      i_grid_title       = i_title
*     i_callback_user_command  = 'USER_COMMAND'
*     i_callback_pf_status_set = 'SET_STATUS'
*     IT_EVENTS          = GT_EVENTS
    TABLES
      t_outtab           = itab1[]
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
    ls_fldct-no_zero = 'X'. " 去除前导零
*    ls_fldct-no_out = 'X'. " 不显示列
*    ls_fldct-do_sum = 'X'. " 汇总
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
      ls_fldct-checkbox = 'X'. "复选框
      ls_fldct-edit = 'X'. "