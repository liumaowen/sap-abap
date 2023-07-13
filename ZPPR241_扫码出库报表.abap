*&---------------------------------------------------------------------*
*& Report ZPPR241
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr241 MESSAGE-ID zgp_msg.

TABLES:lips.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES : BEGIN OF zsckfsxx,
          domvalue_l TYPE  domvalue_l,
          ddtext     TYPE  ddtext,
        END OF zsckfsxx.
DATA: gt_ckfs TYPE TABLE OF zsckfsxx WITH HEADER LINE.
TYPES: BEGIN OF zsernamxx,
         name_text TYPE adrp-name_text,
         bname     TYPE usr21-bname,
       END OF zsernamxx.
DATA gt_ernam TYPE SORTED TABLE OF zsernamxx WITH HEADER LINE WITH UNIQUE KEY bname.
TYPES: BEGIN OF zscusxx,
         name1 TYPE kna1-name1,
         vbeln TYPE likp-vbeln,
       END OF zscusxx.
DATA gt_cus TYPE SORTED TABLE OF zscusxx WITH HEADER LINE WITH UNIQUE KEY vbeln.
DATA: BEGIN OF itab OCCURS 0,
        zckdh   TYPE lips-vbeln, "出库单号
        erdat   TYPE likp-erdat,
        erzet   TYPE Likp-erzet,
        ernam   TYPE lips-ernam, "制单人
        ernamms TYPE adrp-name_text, "制单人描述
        zcus    TYPE kna1-name1, "客户
        zproj   TYPE prps-post1, "项目
        zrkck   TYPE t001l-lgobe, "入库仓库
        zckfs   TYPE lips-zckfs,
        zckfsms TYPE ddtext, "出库方式
        zerdat  TYPE lips-zerdat,
        zertim  TYPE lips-zertim,
        zsmsj   TYPE char15, "扫码时间
        zczr    TYPE but000-name_org1, "操作人
        zbaoh   TYPE lips-zbaoh, "包号
        zchehao TYPE vbak-zchehao, "车号
        zzl1    TYPE mara-zzl1, "品名
      END OF itab.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR lips-werks OBLIGATORY NO INTERVALS NO-EXTENSION,
                 s_erdat FOR lips-erdat OBLIGATORY MODIF ID m1,
                 s_zckfs FOR lips-zckfs NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_erdat_%_app_%-text = '出库日期'.
  %_s_zckfs_%_app_%-text = '出库方式'.

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
    a~vbeln AS zckdh,
    likp~ernam,
    likp~erdat,
    likp~erzet,
    a~zerdat,
    a~zertim,
    prps~post1 AS zproj,
    t001l~lgobe AS zrkck,
    a~zckfs,
    but000~name_org1 AS zczr,
    a~zbaoh,
    vbak~zchehao,
    mara~zzl1
*    adrp~name_text AS ernamms,
*    kna1~name1 AS zcus
    INTO CORRESPONDING FIELDS OF TABLE @itab
    FROM lips AS a
    JOIN mara ON mara~matnr = a~matnr
    LEFT JOIN likp ON likp~vbeln = a~vbeln
    LEFT JOIN prps ON prps~pspnr = a~ps_psp_pnr
    LEFT JOIN t001l ON t001l~werks = a~werks AND t001l~lgort = a~zlgort
    LEFT JOIN but000 ON but000~partner = a~zczr
    LEFT JOIN vbak ON vbak~vbeln = a~vgbel
*    LEFT JOIN usr21 ON usr21~bname = a~ernam
*    LEFT JOIN adrp ON adrp~persnumber = usr21~persnumber
*    LEFT JOIN likp ON likp~vbeln = a~vbeln
*    LEFT JOIN kna1 ON kna1~kunnr = likp~kunag
    WHERE a~werks IN @s_werks
    AND a~erdat IN @s_erdat
    AND a~zckfs IN @s_zckfs.
  IF itab[] IS INITIAL.
    MESSAGE s004 WITH '无数据' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  SELECT
     domvalue_l
     ddtext
     INTO TABLE GT_ckfs
     FROM dd07t
     WHERE domname    = 'ZDSD_ZCKFS' AND
           ddlanguage = sy-langu AND
           as4local   = 'A'.
  SELECT
    adrp~name_text
    usr21~bname
    INTO TABLE gt_ernam
    FROM adrp JOIN usr21 ON adrp~persnumber = usr21~persnumber.
  SELECT
    kna1~name1
    likp~vbeln
    INTO TABLE gt_cus
    FROM kna1 JOIN likp ON kna1~kunnr = likp~kunag.

  LOOP AT itab.
    "制单人
    READ TABLE gt_ernam WITH KEY bname = itab-ernam BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-ernamms = gt_ernam-name_text.
    ENDIF.
    "客户
    READ TABLE gt_cus WITH KEY vbeln = itab-zckdh BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zcus = gt_cus-name1.
    ENDIF.
    "制单人
*    SELECT
*      SINGLE adrp~name_text
*      INTO itab-ernamms
*      FROM adrp JOIN usr21 ON adrp~persnumber = usr21~persnumber WHERE usr21~bname = itab-ernam.
*    "客户
*    SELECT
*       SINGLE kna1~name1
*       INTO itab-zcus
*       FROM kna1
*      JOIN likp ON kna1~kunnr = likp~kunag WHERE likp~vbeln = itab-zckdh.
    "出库方式
    CLEAR gt_ckfs.
    READ TABLE gt_ckfs WITH KEY domvalue_l = itab-zckfs.
    itab-zckfsms = gt_ckfs-ddtext.
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
  slayt-box_fieldname     = ''.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局
  i_title               = lines( itab ) .
  CONDENSE i_title.
  CONCATENATE '条目数:' i_title INTO i_title.
  PERFORM catlg_set TABLES fldct USING:
    'ZCKDH '  '出库单号'  'LIPS'  'ZCKDH',
    'ERDAT '  '制单日期'  ''  'ERDAT',
    'ERZET '  '制单时间'  ''  'ERZET',
    'ERNAMMS '  '制单人'  ''  'ERNAMMS',
    'ZCUS '  '客户'  ''  'ZCUS',
    'ZPROJ '  '项目'  ''  'ZPROJ',
    'ZRKCK '  '入库仓库'  ''  'ZRKCK',
    'ZCKFSMS '  '出库方式'  ''  'ZCKFSMS',
    'ZERDAT '  '扫码日期'  ''  'ZERDAT',
    'ZERTIM '  '扫码时间'  ''  'ZERTIM',
    'ZCZR '  '操作人'  'BUT000'  'ZCZR',
    'ZBAOH '  '包号'  'LIPS'  'ZBAOH',
     'ZCHEHAO '  '车号'  'VBAK'  'ZCHEHAO',
     'ZZL1 '  '品名'  'MARA'  'ZZL1'.

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
      ls_fldct-edit = 'X'. "可编辑
    WHEN 'FISC_YEAR' OR 'ZHDRQ' O