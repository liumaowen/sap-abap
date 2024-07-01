*&---------------------------------------------------------------------*
*& Report ZPPD204A
*&---------------------------------------------------------------------*
*& 新增钢卷配料使用情况报表
*&---------------------------------------------------------------------*
REPORT zppd204a MESSAGE-ID zgp_msg.

TABLES:sscrfields,t001l,mseg,rc68a.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0,
       werks      TYPE mseg-werks,     "工厂
       arbpl      TYPE ztpp_206-arbpl, "产线编码
       lgort      TYPE mseg-lgort,     "存储地
       ktext      TYPE crtx-ktext,     "产线名称
       aufnr      TYPE ztpp_206-aufnr, "工单编号
       cpmatnr    TYPE ztpp_206-matnr, "成品编号
       cpcms      TYPE ze_cpwlcms,     "成品长描述
       gsmng      TYPE ztpp_206-gsmng, "订单量
       budat_mkpf TYPE mseg-budat_mkpf, "减账日期
       matnr      TYPE mseg-matnr,     "物料编号
       wlcms      TYPE ze_cpwlcms,     "物料长描述
       charg      TYPE mseg-charg,     "批次号
       z01        TYPE atwrt,          "自编号
       ztm        TYPE atwrt,          "出米率
       posid      TYPE prps-posid,     "WBS元素
       zpldh      TYPE ztpp210-zpldh,  "配料单号
       zdate      TYPE ztpp210-zdate,  "配料日期
       kclto      TYPE menge_d,        "库存量（吨）
       kclm       TYPE menge_d,        "库存量（米）
       jz_aufnr   TYPE mseg-aufnr,     "最后减账工单号
       jz_arbpl   TYPE ztpp_206-arbpl, "最后减账班组
       jz_gdjzl   TYPE mseg-menge,     "工单减账量(吨)
       jz_qtjzl   TYPE mseg-menge,     "其他减账量(吨)
       jz_ktext   TYPE crtx-ktext,     "最后减账班组
       zdbsl      TYPE ztpp210-zsl,    "调拨量
       zsl_3d     TYPE mseg-menge,     "减账3日内总量
       zslto_3d   TYPE mseg-menge,     "减账3日后库存（吨）
       zslm_3d    TYPE mseg-menge,     "减账3日后库存（米）
       sel,
     END OF itab.
DATA:BEGIN OF gt_006 OCCURS 0, "查ZMM006库存量
       werks TYPE werks_d,
       charg TYPE charg_d,
       clabs TYPE mchb-clabs, "库存量
       zkcms TYPE labst,      "库存量（使用单位）
     END OF gt_006.
DATA:gt_charg TYPE RANGE OF charg_d WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR t001l-werks NO INTERVALS NO-EXTENSION,
                 s_mkpf  FOR mseg-budat_mkpf,
                 s_arbpl FOR rc68a-arbpl,
                 s_charg FOR mseg-charg.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '功能选择'.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_mkpf_%_app_%-text = '减账日期'.
  %_s_arbpl_%_app_%-text = '产线编码'.
  %_s_charg_%_app_%-text = '批次号'.


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
  TYPES:BEGIN OF ty_aufnr,
          aufnr TYPE ztpp_206-aufnr,
        END OF ty_aufnr.
  DATA:gt_aufnr TYPE TABLE OF ty_aufnr WITH KEY aufnr,
       gv_aufnr LIKE LINE OF gt_aufnr.
  TYPES:BEGIN OF ty_wercharmat,
          werks TYPE werks_d,
          charg TYPE charg_d,
          matnr TYPE mseg-matnr,
        END OF ty_wercharmat.
  DATA:gt_wercharmat TYPE TABLE OF ty_wercharmat WITH KEY werks charg matnr,
       gv_wercharmat LIKE LINE OF gt_wercharmat.
  TYPES:BEGIN OF ty_wercharmat1.
          INCLUDE TYPE ty_wercharmat.
  TYPES:  zdate TYPE ztpp210-zdate,
        END OF ty_wercharmat1.
  DATA:gt_mseg_3d TYPE TABLE OF ty_wercharmat1 WITH KEY werks charg matnr zdate,
       gv_mseg_3d LIKE LINE OF gt_mseg_3d.
  DATA: BEGIN OF lt_jz3d OCCURS 0.
          INCLUDE TYPE ty_wercharmat.
  DATA:   zsl_3d_sum TYPE mseg-menge,
        END OF lt_jz3d.
  DATA: BEGIN OF it_wlcms OCCURS 0,
          matnr TYPE matnr,
          wlcms TYPE string,
        END OF it_wlcms.
  DATA:it_mcha TYPE TABLE OF mcha WITH HEADER LINE,
       o_pctx  TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
  DATA:lv_3d    TYPE mseg-cpudt_mkpf,
       lv_menge TYPE mseg-menge,
       lv_tabix TYPE sy-tabix.

  SELECT
     mseg~werks,
     mseg~matnr,
     mseg~charg,
     mseg~lgort,
     mseg~budat_mkpf,
     pr~posid,
     mseg~aufnr,
     z210~zpldh,
     z210~zdate,
     z210~zsl AS zdbsl
  FROM mseg
  INNER JOIN prps     AS pr   ON pr~pspnr = mseg~mat_pspnr
  INNER JOIN ztpp210  AS z210 ON z210~werks = mseg~werks
                              AND z210~aufnr = mseg~aufnr
                              AND z210~matnr = mseg~matnr
                              AND z210~charg = mseg~charg
                              AND z210~lgort = mseg~lgort
                              AND z210~posid = pr~posid
  INNER JOIN ztmm221 AS z221  ON  z221~zdbdh = z210~zdbdh
                              AND z221~zhh   = z210~zdbdhh
  WHERE mseg~werks IN @s_werks
    AND mseg~charg IN @s_charg
    AND mseg~budat_mkpf IN @s_mkpf
    AND left( mseg~matnr,3 ) = 'E02'
    AND mseg~bwart = '261'
    AND NOT EXISTS ( SELECT * FROM m_mbmps AS ps   "未被冲销
                         WHERE ps~sjahr = mseg~mjahr
                           AND ps~smbln = mseg~mblnr
                           AND ps~smblp = mseg~zeile
                    )
    INTO CORRESPONDING FIELDS OF TABLE @itab.
  IF itab[] IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH:gt_aufnr.
  LOOP AT itab.
    gv_aufnr-aufnr = itab-aufnr.
    COLLECT gv_aufnr INTO gt_aufnr.
  ENDLOOP.
  "查询ztpp_206
  SELECT
    a~aufnr,
    z206~matnr,
    z206~gsmng,
    z206~arbpl,
    tx~ktext
  FROM @gt_aufnr AS a
  INNER JOIN ztpp_206 AS z206 ON z206~aufnr = a~aufnr
  INNER JOIN crhd     AS hd   ON hd~arbpl = z206~arbpl
  INNER JOIN crtx     AS tx   ON tx~objid = hd~objid
                              AND tx~objty = hd~objty
                              AND tx~spras = @sy-langu
  WHERE z206~arbpl IN @s_arbpl
  ORDER BY a~aufnr
  INTO TABLE @DATA(lt_arbpl206).
  ##itab_db_select
  SELECT
    zar~aufnr,
    SUM( zar~gsmng ) AS gsmng
  FROM @lt_arbpl206 AS zar
  GROUP BY zar~aufnr
  ORDER BY zar~aufnr
  INTO TABLE @DATA(lt_arbpl206_sum).

  DELETE ADJACENT DUPLICATES FROM lt_arbpl206 COMPARING aufnr.
  SORT lt_arbpl206 BY aufnr.

  REFRESH:gt_charg.
  LOOP AT itab.
    READ TABLE lt_arbpl206 INTO DATA(lv_arbpl206) WITH KEY aufnr = itab-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      itab-cpmatnr = lv_arbpl206-matnr.
      itab-arbpl = lv_arbpl206-arbpl.
      itab-ktext = lv_arbpl206-ktext.
    ENDIF.
    READ TABLE lt_arbpl206_sum INTO DATA(lv_arbpl206_sum) WITH KEY aufnr = itab-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      itab-gsmng = lv_arbpl206_sum-gsmng.
    ENDIF.
    it_wlcms-matnr = itab-matnr.
    it_mcha-matnr = itab-matnr.
    it_mcha-charg = itab-charg.
    COLLECT: it_wlcms,it_mcha.
    it_wlcms-matnr = itab-cpmatnr.
    gt_charg-low = itab-charg.
    gt_charg-option = 'EQ'.
    gt_charg-sign = 'I'.
    COLLECT:gt_charg,it_wlcms.
    gv_wercharmat-werks = itab-werks.
    gv_wercharmat-matnr = itab-matnr.
    gv_wercharmat-charg = itab-charg.
    COLLECT gv_wercharmat INTO gt_wercharmat.
    gv_mseg_3d-werks = itab-werks.
    gv_mseg_3d-matnr = itab-matnr.
    gv_mseg_3d-charg = itab-charg.
    gv_mseg_3d-zdate = itab-zdate.
    COLLECT gv_mseg_3d INTO gt_mseg_3d.
    MODIFY itab.
  ENDLOOP.
  "删除不符合工作中心的
  IF s_arbpl[] IS NOT INITIAL.
    DELETE itab WHERE arbpl NOT IN s_arbpl.
  ENDIF.
  PERFORM getlongtextpl(zpubform) TABLES it_wlcms.
  SORT it_wlcms BY matnr.

  "查询zmm006库存
  PERFORM getzmm006.
  SORT gt_006 BY werks charg.

*获取批次特征值
  CALL FUNCTION 'ZFMS_05_GETPCTX'
    EXPORTING
      atnam  = 'Z01,ZTM'
    TABLES
      intab  = it_mcha
      outtab = o_pctx
    EXCEPTIONS
      OTHERS = 1.
  SORT o_pctx BY matnr charg atnam.

  "最后减账工单
  SELECT
      mseg~werks,
      mseg~matnr,
      mseg~charg,
      mseg~aufnr,
      mseg~menge,
      mseg~bwart,
      mseg~cpudt_mkpf,
      mseg~cputm_mkpf
  FROM mseg
  INNER JOIN @gt_wercharmat AS b ON  mseg~werks = b~werks
                                 AND mseg~matnr = b~matnr
                                 AND mseg~charg = b~charg
  WHERE mseg~bwart IN ( '261','Z61' )
    AND NOT EXISTS ( SELECT * FROM m_mbmps AS ps   "未被冲销
                           WHERE ps~sjahr = mseg~mjahr
                             AND ps~smbln = mseg~mblnr
                             AND ps~smblp = mseg~zeile
                    )
  ORDER BY mseg~werks,mseg~matnr,mseg~charg,mseg~cpudt_mkpf DESCENDING,mseg~cputm_mkpf DESCENDING
  INTO TABLE @DATA(lt_zhjz).
  ##itab_db_select
  SELECT
    zjz~werks,
    zjz~matnr,
    zjz~charg,
    SUM( CASE WHEN zjz~bwart = '261' THEN zjz~menge END ) AS gdjzl,
    SUM( CASE WHEN zjz~bwart = 'Z61' THEN zjz~menge END ) AS qtjzl
  FROM @lt_zhjz AS zjz
  GROUP BY zjz~werks,zjz~matnr,zjz~charg
  ORDER BY zjz~werks,zjz~matnr,zjz~charg
  INTO TABLE @DATA(lt_zhjz_sum).

  DELETE lt_zhjz WHERE bwart = 'Z61'.
  DELETE ADJACENT DUPLICATES FROM lt_zhjz COMPARING werks matnr charg.
  SORT lt_zhjz BY werks matnr charg.

  "减账3日后库存
  SELECT
      mseg~werks,
      mseg~matnr,
      mseg~charg,
      mseg~aufnr,
      mseg~menge,
      mseg~cpudt_mkpf,
      mseg~cputm_mkpf
  FROM mseg
  INNER JOIN @gt_mseg_3d AS b ON  mseg~werks = b~werks
                              AND mseg~matnr = b~matnr
                              AND mseg~charg = b~charg
                              AND mseg~budat_mkpf >= b~zdate
  WHERE mseg~bwart = '261'
    AND NOT EXISTS ( SELECT * FROM m_mbmps AS ps   "未被冲销
                           WHERE ps~sjahr = mseg~mjahr
                             AND ps~smbln = mseg~mblnr
                             AND ps~smblp = mseg~zeile
                    )
  ORDER BY mseg~werks,mseg~matnr,mseg~charg,mseg~cpudt_mkpf,mseg~cputm_mkpf
  INTO TABLE @DATA(lt_zhjz_3d).
  LOOP AT lt_zhjz_3d INTO DATA(lv_zhjz_3d) GROUP BY ( werks = lv_zhjz_3d-werks matnr = lv_zhjz_3d-matnr charg = lv_zhjz_3d-charg
                                           size = GROUP SIZE
                                           index = GROUP INDEX )
  ASCENDING ASSIGNING FIELD-SYMBOL(<group>).
    CLEAR:lv_3d,lv_menge,lv_tabix.
    LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<fs_3d>).
      lv_tabix += 1.
      IF lv_tabix = 1.
        lv_3d = <fs_3d>-cpudt_mkpf + 3.
      ENDIF.
      IF <fs_3d>-cpudt_mkpf <= lv_3d.
        lv_menge += <fs_3d>-menge.
      ENDIF.
    ENDLOOP.
    lt_jz3d-werks = <fs_3d>-werks.
    lt_jz3d-matnr = <fs_3d>-matnr.
    lt_jz3d-charg = <fs_3d>-charg.
    lt_jz3d-zsl_3d_sum = lv_menge.
    APPEND lt_jz3d.
  ENDLOOP.
  SORT lt_jz3d BY werks matnr charg.

  "查询最后减账班组
  SELECT
    z206~aufnr,
    z206~arbpl,
    tx~ktext
  FROM ztpp_206 AS z206
  INNER JOIN @lt_zhjz AS b ON z206~aufnr = b~aufnr
  INNER JOIN crhd     AS hd   ON hd~arbpl = z206~arbpl
  INNER JOIN crtx     AS tx   ON tx~objid = hd~objid
                              AND tx~objty = hd~objty
                              AND tx~spras = @sy-langu
  GROUP BY z206~aufnr,z206~arbpl,tx~ktext
  ORDER BY z206~aufnr
  INTO TABLE @DATA(lt_arbpl206_last).

  LOOP AT itab.
    READ TABLE it_wlcms WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      itab-wlcms = it_wlcms-wlcms.
    ENDIF.
    READ TABLE it_wlcms WITH KEY matnr = itab-cpmatnr BINARY SEARCH.
    IF sy-subrc = 0.
      itab-cpcms = it_wlcms-wlcms.
    ENDIF.
    READ TABLE o_pctx WITH KEY matnr = itab-matnr charg = itab-charg atnam = 'Z01' BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-z01 = o_pctx-atwrt.
    ENDIF.
    READ TABLE o_pctx WITH KEY matnr = itab-matnr charg = itab-charg atnam = 'ZTM' BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-ztm = o_pctx-atwrt.
    ENDIF.
    READ TABLE gt_006 WITH KEY werks = itab-werks charg = itab-charg BINARY SEARCH.
    IF sy-subrc = 0.
      itab-kclto = gt_006-clabs.
      itab-kclm = gt_006-zkcms.
    ENDIF.
    READ TABLE lt_zhjz INTO DATA(lv_zhjz) WITH KEY werks = itab-werks matnr = itab-matnr charg = itab-charg BINARY SEARCH.
    IF sy-subrc = 0.
      itab-jz_aufnr = lv_zhjz-aufnr.
      READ TABLE lt_arbpl206_last INTO DATA(lv_arbpl206_last) WITH KEY aufnr = itab-jz_aufnr BINARY SEARCH.
      IF sy-subrc = 0.
        itab-jz_arbpl = lv_arbpl206_last-arbpl.
        itab-jz_ktext = lv_arbpl206_last-ktext.
      ENDIF.
      READ TABLE lt_zhjz_sum INTO DATA(lv_zhjz_sum) WITH KEY werks = itab-werks matnr = itab-matnr charg = itab-charg BINARY SEARCH.
      IF sy-subrc = 0.
        itab-jz_gdjzl = lv_zhjz_sum-gdjzl.
        itab-jz_qtjzl = lv_zhjz_sum-qtjzl.
      ENDIF.
    ENDIF.
    READ TABLE lt_jz3d WITH KEY werks = itab-werks matnr = itab-matnr charg = itab-charg BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zsl_3d = lt_jz3d-zsl_3d_sum.
      itab-zslto_3d = itab-zdbsl - itab-zsl_3d.
      itab-zslm_3d = itab-zslto_3d * itab-ztm.
    ENDIF.
    MODIFY itab.
  ENDLOOP.

ENDFORM.

FORM getzmm006.
  DATA ls_data TYPE REF TO data.

  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_false data = abap_true ).

  SUBMIT zmms_stock_pctx_main
        WITH s_werks IN s_werks
        WITH s_charg IN gt_charg
    AND RETURN.
  CLEAR:ls_data.
  TRY .
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data = ls_data ).
    CATCH  cx_salv_bs_sc_runtime_info.
      MESSAGE s000(oo) WITH '无法获取报表的数据' DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.
  ASSIGN ls_data->* TO FIELD-SYMBOL(<fs_tab>).
  cl_salv_bs_runtime_info=>clear_all( ).

  CLEAR: gt_006[],gt_006.
  IF <fs_tab> IS ASSIGNED.
    MOVE-CORRESPONDING <fs_tab> TO gt_006[].
    DELETE gt_006 WHERE clabs = 0.
    UNASSIGN <fs_tab>.
  ENDIF.
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
'WERKS      '  '工厂        '    'MSEG'  'WERKS',
'ARBPL      '  '产线编码      '  'ZTPP_206'  'ARBPL',
'KTEXT      '  '产线名称      '  'CRTX'  'KTEXT',
'AUFNR      '  '工单编号      '  'ZTPP_206'  'AUFNR',
'CPMATNR    '  '成品编号      '  'ZTPP_206'  'MATNR',
'CPCMS      '  '成品长描述    '  ''  '',
'GSMNG      '  '订单量       '   'ZTPP_206'  'GSMNG',
'BUDAT_MKPF '  '减账日期     '   'MSEG'  'BUDAT_MKPF',
'MATNR      '  '物料编号     '   'MSEG'  'MATNR',
'WLCMS      '  '物料长描述   '   ''  '',
'CHARG      '  '批次号       '  'MSEG'  'CHARG',
'Z01        '  '自编号       '  ''  '',
'ZTM        '  '出米率       '  ''  '',
'POSID      '  'WBS元素     '   'PRPS'  'POSID',
'ZPLDH      '  '配料单号      '  'ZTPP210'  'ZPLDH',
'ZDATE      '  '配料日期      '  'ZTPP210'  'ZDATE',
'KCLTO      '  '库存量（吨）  '  'ZTPP_206'  'MENGE',
'KCLM       '  '库存量（米）  '  'ZTPP_206'  'MENGE',
'JZ_AUFNR   '  '最后减账工单号 '  'ZTPP_206'  'AUFNR',
*'JZ_ARBPL   '  '最后减账班组   '  'ZTPP_206'  'ARBPL',
'JZ_KTEXT   '  '最后减账班组   '  'CRTX'  'KTEXT',
'JZ_GDJZL   '  '工单减账量(吨) '  'MSEG'  'MENGE',
'JZ_QTJZL   '  '其他减账量(吨) '  'MSEG'  'MENGE',
'ZDBSL      '  '调拨量       '   'ZTPP210'  'ZSL',
'ZSL_3D     '  '减账3日内总量  '  'ZTPP_206'  'MENGE',
'ZSLTO_3D   '  '减账3日后库存（吨）'  'ZTPP_206'  'MENGE',
'ZSLM_3D    '  '减账3日后库存（米）'  'ZTPP_206'  'MENGE'.

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

  CALL METHOD lr_grid->check_chan