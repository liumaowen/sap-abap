*&---------------------------------------------------------------------*
*& Report ZMMR208
*&---------------------------------------------------------------------*
*&各事业部年度降本分析
*&---------------------------------------------------------------------*
REPORT zmmr208 MESSAGE-ID zgp_msg.

TABLES:sscrfields,t001l,afko,ekko,mara,lfa1.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA: gjahr   TYPE bseg-gjahr.
TYPES:BEGIN OF ty_itab,
        werks   TYPE werks_d,
        matnr   TYPE matnr,
        wlcms   TYPE string,
        lifnr   TYPE lfa1-lifnr,   "供应商
        name1   TYPE lfa1-name1,   "供应商描述
        matkl   TYPE mara-matkl,   "物料组
        wgbez   TYPE t023t-wgbez,  "物料组描述
        werksms TYPE t001w-name1,
        del,
      END OF ty_itab.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE TYPE ty_itab.
DATA:  knumv TYPE zvmmpo-knumv,
       ekotx TYPE t024e-ekotx,  "采购组织描述
       ekorg TYPE zvmmpo-ekorg, "采购组织
       ebelp TYPE zvmmpo-ebelp,
       ebeln TYPE zvmmpo-ebeln,
       zcge  TYPE ze_dmbtr,     "采购额
       menge TYPE menge_d,      "采购数量
       zrq   TYPE char6,
       bstyp TYPE zvmmpo-bstyp, "采购凭证类别
       bsart TYPE zvmmpo-bsart, "采购凭证类型
     END OF itab,
     itab1    LIKE itab[],
     wa_itab1 LIKE LINE OF itab1.
DATA:BEGIN OF gt_tab OCCURS 0.
       INCLUDE TYPE ty_itab.
DATA:  wlname1   TYPE string,
       gp_jg     TYPE kbetr,        "钢品事业部平均价格含税
       gp_cgl    TYPE menge_d,      "钢品事业部采购量
       gp_cge    TYPE ze_dmbtr,     "钢品事业部采购额
       jj_jg     TYPE kbetr,        "洁净事业部平均价格含税
       jj_cgl    TYPE menge_d,      "洁净事业部采购量
       jj_cge    TYPE ze_dmbtr,     "洁净事业部采购额
       ll_jg     TYPE kbetr,        "冷链事业部平均价格含税
       ll_cgl    TYPE menge_d,      "冷链事业部采购量
       ll_cge    TYPE ze_dmbtr,     "冷链事业部采购额
       cj_jg     TYPE kbetr,        "厂景事业部平均价格含税
       cj_cgl    TYPE menge_d,      "厂景事业部采购量
       cj_cge    TYPE ze_dmbtr,     "厂景事业部采购额
       gp_jg_n   TYPE kbetr,        "当月钢品事业部平均价格含税
       gp_cgl_n  TYPE menge_d,      "当月钢品事业部采购量
       gp_cge_n  TYPE ze_dmbtr,     "当月钢品事业部采购额
       gp_cgec_n TYPE ze_dmbtr,     "当月Saving
       jj_jg_n   TYPE kbetr,        "当月洁净事业部平均价格含税
       jj_cgl_n  TYPE menge_d,      "当月洁净事业部采购量
       jj_cge_n  TYPE ze_dmbtr,     "当月洁净事业部采购额
       jj_cgec_n TYPE ze_dmbtr,     "当月Saving
       ll_jg_n   TYPE kbetr,        "当月冷链事业部平均价格含税
       ll_cgl_n  TYPE menge_d,      "当月冷链事业部采购量
       ll_cge_n  TYPE ze_dmbtr,     "当月冷链事业部采购额
       ll_cgec_n TYPE ze_dmbtr,     "当月Saving
       cj_jg_n   TYPE kbetr,        "当月厂景事业部平均价格含税
       cj_cgl_n  TYPE menge_d,      "当月厂景事业部采购量
       cj_cge_n  TYPE ze_dmbtr,     "当月厂景事业部采购额
       cj_cgec_n TYPE ze_dmbtr,     "当月Saving
       sel,
     END OF gt_tab.
DATA:BEGIN OF gt_stab OCCURS 0,
       werks TYPE werks_d,
       matnr TYPE matnr,
       lifnr TYPE lfa1-lifnr,
       ekorg TYPE zvmmpo-ekorg,
       matkl TYPE mara-matkl,   "物料组
       zrq   TYPE char6,
       zcge  TYPE ze_dmbtr,     "采购额
       menge TYPE menge_d,      "采购数量
     END OF gt_stab.
DATA: it_knumv TYPE TABLE OF zsmmknumv WITH HEADER LINE,
      it_prcd  TYPE TABLE OF prcd_elements WITH HEADER LINE.
TYPES:BEGIN OF ty_ebeln,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
      END OF ty_ebeln.
DATA: it_ebeln TYPE TABLE OF ty_ebeln WITH KEY ebeln ebelp,
      wa_ebeln TYPE ty_ebeln.
TYPES:BEGIN OF ty_t024e, "采购组织
        ekorg TYPE t024e-ekorg,
        ekotx TYPE t024e-ekotx,
        bs    TYPE char10,
      END OF ty_t024e.
DATA:gt_t024e TYPE TABLE OF ty_t024e WITH EMPTY KEY.
DATA:gv_comp TYPE string.
DATA: BEGIN OF it_wlcms OCCURS 0,
        matnr TYPE matnr,
        wlcms TYPE string,
      END OF it_wlcms.
FIELD-SYMBOLS:<fs_jg>   TYPE any,
              <fs_jg_n> TYPE any,
              <fs_sav>  TYPE any.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS:p_month LIKE isellist-month DEFAULT sy-datum+0(6) OBLIGATORY.
  SELECT-OPTIONS: s_werks FOR t001l-werks,
                  s_ekorg FOR ekko-ekorg,
                  s_lifnr FOR lfa1-lifnr,
                  s_matnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '功能选择'.
  %_s_werks_%_app_%-text = '工厂'.
  %_p_month_%_app_%-text = '入库月份'.
  %_s_ekorg_%_app_%-text = '采购组织'.
  %_s_lifnr_%_app_%-text = '供应商'.
  %_s_matnr_%_app_%-text = '物料编码'.

  gt_t024e = VALUE #( ( ekorg = '3000' ekotx = '钢品事业部' bs = 'GP'  )
                      ( ekorg = '3020' ekotx = '洁净事业部' bs = 'JJ'  )
                      ( ekorg = '3090' ekotx = '冷链事业部' bs = 'LL'  )
                      ( ekorg = '3080' ekotx = '厂景事业部' bs = 'CJ'  )
                     ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month.
  PERFORM selmonth(zpubform) CHANGING p_month.

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
  gjahr = p_month+0(4) - 1.
*采购订单数据
  SELECT
    ztmm252~zsyb AS ekorg,
    zvmmpo~lifnr,"供应商编码
    lfa1~name1,  "供应商
    zvmmpo~matnr,
    zvmmpo~matkl,
    t023t~wgbez,
    zvmmpo~knumv,
    zvmmpo~ebelp,
    zvmmpo~ebeln,
    zvmmpo~werks,
    t001w~name1 AS werksms,
    zvmmpo~bstyp,
    zvmmpo~bsart
  FROM zvmmpo
  INNER JOIN ztmm252 ON zvmmpo~ekorg = ztmm252~ekorg
  INNER JOIN lfa1 ON zvmmpo~lifnr = lfa1~lifnr
  LEFT JOIN mara ON mara~matnr = zvmmpo~matnr
  LEFT JOIN t023t ON t023t~matkl = zvmmpo~matkl AND t023t~spras = '1'
  INNER JOIN t001w ON t001w~werks = zvmmpo~werks
  WHERE zvmmpo~memory = ''
    AND zvmmpo~loekz NE 'L'
    AND zvmmpo~ekorg IN @s_ekorg
    AND zvmmpo~lifnr IN @s_lifnr
    AND zvmmpo~matnr IN @s_matnr
    AND zvmmpo~werks IN @s_werks
    AND zvmmpo~bsart NOT IN ( 'UB','Z09' )
  INTO CORRESPONDING FIELDS OF TABLE @itab.
  IF itab[] IS INITIAL.
    IF itab IS INITIAL.
      MESSAGE s009 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.
  LOOP AT itab.
    CLEAR:it_knumv,wa_ebeln.
    it_knumv-knumv = itab-knumv.
    it_knumv-posnr = itab-ebelp.
    it_knumv-ebelp = itab-ebelp.
    wa_ebeln-ebeln = itab-ebeln.
    wa_ebeln-ebelp = itab-ebelp.
    COLLECT: it_knumv.
    COLLECT wa_ebeln INTO it_ebeln.
*    IF itab-ekorg EQ '3005' OR itab-ekorg EQ '3009'.
*      itab-ekorg = '3000'.
*      MODIFY itab TRANSPORTING ekorg.
*    ENDIF.
  ENDLOOP.
*取已收货数量(取当月和去年的数据)
  SELECT
  m~ebeln,
  m~ebelp,
  CASE WHEN substring( m~budat_mkpf,1,4 ) = @gjahr THEN @gjahr WHEN substring( m~budat_mkpf,1,6 ) = @p_month THEN @p_month END AS zrq,
  b~menge
  FROM mseg AS m
  INNER JOIN @it_ebeln AS a ON a~ebeln = m~ebeln AND a~ebelp = m~ebelp
  INNER JOIN ekbe AS b ON m~mblnr = b~belnr AND m~mjahr = b~gjahr AND m~zeile = b~buzei
  WHERE m~bwart IN ( '101','161' )
  AND ( substring( m~budat_mkpf,1,4 ) = @gjahr OR substring( m~budat_mkpf,1,6 ) = @p_month )
  AND m~sjahr = ''
  AND NOT EXISTS (
      SELECT 1 FROM m_mbmps
      WHERE m_mbmps~sjahr = m~mjahr AND
      m_mbmps~smbln = m~mblnr AND
      m_mbmps~smblp = m~zeile
  )
  INTO TABLE @DATA(it_menge).

  ##itab_db_select
  SELECT
    a~ebeln,a~ebelp,a~zrq,SUM( a~menge ) AS menge
  FROM @it_menge AS a
  GROUP BY a~ebeln,a~ebelp,a~zrq
  ORDER BY a~ebeln,a~ebelp,a~zrq
  INTO TABLE @DATA(gt_summenge)
  .
  "采购凭证类型为Z02的冲销判断，行项目上的所有的101数量之和-所有的102数量之和
  DATA(itab2) = itab[].
  SORT itab2 BY ebeln ebelp.
  SELECT
   m~ebeln,
   m~ebelp,
   CASE WHEN substring( m~budat_mkpf,1,4 ) = @gjahr THEN @gjahr WHEN substring( m~budat_mkpf,1,6 ) = @p_month THEN @p_month END AS zrq,
   b~menge
  FROM mseg AS m
  INNER JOIN @itab2 AS a ON a~ebeln = m~ebeln AND a~ebelp = m~ebelp
  INNER JOIN ekbe AS b ON m~mblnr = b~belnr AND m~mjahr = b~gjahr AND m~zeile = b~buzei
  WHERE a~bsart EQ 'Z02' AND m~bwart = '102'
  AND ( substring( m~budat_mkpf,1,4 ) = @gjahr OR substring( m~budat_mkpf,1,6 ) = @p_month )
  AND m~sjahr = ''
  INTO TABLE @DATA(it_menge1).
  ##itab_db_select
  SELECT
    a~ebeln,a~ebelp,a~zrq,SUM( a~menge ) AS menge
  FROM @it_menge1 AS a
  GROUP BY a~ebeln,a~ebelp,a~zrq
  ORDER BY a~ebeln,a~ebelp,a~zrq
  INTO TABLE @DATA(gt_summenge1)
  .
  LOOP AT gt_summenge1 INTO DATA(wa_summenge1).
    READ TABLE gt_summenge INTO DATA(wa_summenge) WITH KEY ebeln = wa_summenge1-ebeln ebelp = wa_summenge1-ebelp zrq = wa_summenge1-zrq.
    IF sy-subrc EQ 0.
      wa_summenge-menge = wa_summenge-menge - wa_summenge1-menge.
      MODIFY gt_summenge FROM wa_summenge INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

*取含税单价
  IF it_knumv[] IS NOT INITIAL.
    SORT it_knumv BY knumv posnr.
    SELECT *
      INTO TABLE it_prcd
      FROM prcd_elements
      FOR ALL ENTRIES IN it_knumv
      WHERE knumv = it_knumv-knumv
      AND   kposn = it_knumv-posnr
      AND   kschl = 'PBXX'
      AND   kinak = ''.
    SORT it_prcd BY knumv kposn.
  ENDIF.
  REFRESH itab1.
  LOOP AT gt_summenge INTO DATA(wa).
    READ TABLE itab2 INTO DATA(wa_itab2) WITH KEY ebeln = wa-ebeln ebelp = wa-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING wa_itab2 TO wa_itab1.
      READ TABLE it_prcd WITH KEY knumv = wa_itab1-knumv
                                  kposn = wa_itab1-ebelp
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF it_prcd-kpein IS NOT INITIAL.
          IF wa_itab2-bstyp EQ 'F' AND wa_itab2-bsart EQ 'Z08'. "退货订单金额乘以-1
            wa_itab1-zcge = it_prcd-kbetr / it_prcd-kpein * wa-menge * ( -1 ).
            wa_itab1-menge = wa-menge * ( -1 ).
          ELSE.
            wa_itab1-zcge = it_prcd-kbetr / it_prcd-kpein * wa-menge.
            wa_itab1-menge = wa-menge.
          ENDIF.
        ENDIF.
        wa_itab1-zrq  = wa-zrq.
        APPEND wa_itab1 TO itab1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  REFRESH: gt_stab,gt_tab,it_wlcms.
  LOOP AT itab1 INTO wa_itab1.
    CLEAR: gt_stab,gt_tab.
    MOVE-CORRESPONDING wa_itab1 TO gt_stab.
    COLLECT gt_stab.
    gt_tab-werks = wa_itab1-werks.
    gt_tab-werksms = wa_itab1-werksms.
    gt_tab-matnr = wa_itab1-matnr.
    gt_tab-lifnr = wa_itab1-lifnr.
    gt_tab-name1 = wa_itab1-name1.
    gt_tab-matkl = wa_itab1-matkl.
    gt_tab-wgbez = wa_itab1-wgbez.
    COLLECT gt_tab.
    IF wa_itab1-matnr IS NOT INITIAL.
      it_wlcms-matnr = wa_itab1-matnr.
      COLLECT it_wlcms.
    ENDIF.
  ENDLOOP.
  SORT gt_stab BY werks matnr matkl lifnr ekorg zrq.
  PERFORM getlongtextpl(zpubform) TABLES it_wlcms.
  SORT it_wlcms BY matnr.
  DATA(lv_i) = 0.
  LOOP AT gt_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
    LOOP AT gt_t024e INTO DATA(gs_t024e).
      lv_i = 0.
      READ TABLE gt_stab WITH KEY werks = <fs_tab>-werks matnr = <fs_tab>-matnr matkl = <fs_tab>-matkl lifnr = <fs_tab>-lifnr ekorg = gs_t024e-ekorg zrq = p_month BINARY SEARCH. "当年月份
      IF sy-subrc EQ 0.
        gv_comp = |{ gs_t024e-bs }_CGL_N|.
        ASSIGN COMPONENT gv_comp OF STRUCTURE <fs_tab> TO FIELD-SYMBOL(<fs_cgl_n>).
        <fs_cgl_n> = gt_stab-menge.
        gv_comp = |{ gs_t024e-bs }_CGE_N|.
        ASSIGN COMPONENT gv_comp OF STRUCTURE <fs_tab> TO FIELD-SYMBOL(<fs_cge_n>).
        <fs_cge_n> = gt_stab-zcge.
        IF <fs_cgl_n> IS NOT INITIAL.
          gv_comp = |{ gs_t024e-bs }_JG_N|.
          ASSIGN COMPONENT gv_comp OF STRUCTURE <fs_tab> TO <fs_jg_n>.
          <fs_jg_n> = <fs_cge_n> / <fs_cgl_n>.
          lv_i += 1.
        ENDIF.
      ENDIF.
      READ TABLE gt_stab WITH KEY werks = <fs_tab>-werks matnr = <fs_tab>-matnr matkl = <fs_tab>-matkl lifnr = <fs_tab>-lifnr ekorg = gs_t024e-ekorg zrq = gjahr BINARY SEARCH. "去年
      IF sy-subrc EQ 0.
        gv_comp = |{ gs_t024e-bs }_CGL|.
        ASSIGN COMPONENT gv_comp OF STRUCTURE <fs_tab> TO FIELD-SYMBOL(<fs_cgl>).
        <fs_cgl> = gt_stab-menge.
        gv_comp = |{ gs_t024e-bs }_CGE|.
        ASSIGN COMPONENT gv_comp OF STRUCTURE <fs_tab> TO FIELD-SYMBOL(<fs_cge>).
        <fs_cge> = gt_stab-zcge.
        IF <fs_cgl> IS NOT INITIAL.
          gv_comp = |{ gs_t024e-bs }_JG|.
          ASSIGN COMPONENT gv_comp OF STRUCTURE <fs_tab> TO <fs_jg>.
          <fs_jg> = <fs_cge> / <fs_cgl>.
          gv_comp = |{ gs_t024e-bs }_CGEC_N|.
          ASSIGN COMPONENT gv_comp OF STRUCTURE <fs_tab> TO <fs_sav>.
          IF lv_i > 0.
            <fs_sav> = ( <fs_cge_n> / <fs_cgl_n> - <fs_cge> / <fs_cgl> ) * <fs_cgl_n>. "2024年1月Saving = (当月平均价格-去年平均价格)*当月采购量
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    READ TABLE it_wlcms WITH KEY matnr = <fs_tab>-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_tab>-wlcms = it_wlcms-wlcms.
    ENDIF.
    <fs_tab>-wlname1 = <fs_tab>-matnr && <fs_tab>-name1.
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
  DATA:gv_jg     TYPE string,   "2023年平均价格含税
       gv_cgl    TYPE string,  "2023年采购数量
       gv_cge    TYPE string,  "2023年采购额
       gv_jg_bs  TYPE char10,   "字段标识
       gv_cgl_bs TYPE char10,   "字段标识
       gv_cge_bs TYPE char10.   "字段标识
  DATA  it_sort TYPE slis_t_sortinfo_alv WITH HEADER LINE.
  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局

  PERFORM catlg_set TABLES fldct
                                 USING:
    'WERKSMS '  '工厂'             'T001W'  'NAME1',
    'MATNR '    '物料编码'         'MARA'  'MATNR',
    'WLNAME1 '  '物料编码+供应商'   ''  '',
    'WLCMS '    '物料长描述'         ''  '',
    'LIFNR '    '供应商'              'LFA1'  'LIFNR',
    'NAME1 '    '供应商描述'           'T001W'  'NAME1',
    'MATKL '    '物料组'              'EKKO'  'MATKL',
    'WGBEZ '    '物料组描述'          'T023T'  'WGBEZ'.

  LOOP AT gt_t024e INTO DATA(gs_t024e).
    gv_jg  = |{ gjahr }年平均价格含税|.
    gv_cgl = |{ gjahr }年采购数量|.
    gv_cge = |{ gjahr }年采购额|.
    gv_jg  = |{ gv_jg }-{ gs_t024e-ekotx }|.
    gv_cgl = |{ gv_cgl }-{ gs_t024e-ekotx }|.
    gv_cge = |{ gv_cge }-{ gs_t024e-ekotx }|.
    gv_jg_bs  = |{ gs_t024e-bs }_JG|.
    gv_cgl_bs  = |{ gs_t024e-bs }_CGL|.
    gv_cge_bs  = |{ gs_t024e-bs }_CGE|.
    PERFORM catlg_set TABLES fldct USING:
        gv_jg_bs  gv_jg   'ZTPP_205B'    'ZZJE',
        gv_cgl_bs  gv_cgl  ''       '',
        gv_cge_bs  gv_cge  'ZTPP_205B'  'ZZJE'.
  ENDLOOP.
  LOOP AT gt_t024e INTO gs_t024e.
    gv_jg  = |{ p_month+0(4) }年{ p_month+4(2) }月平均价格|.
    gv_cgl = |{ p_month+0(4) }年{ p_month+4(2) }月采购数量|.
    gv_cge = |{ p_month+0(4) }年{ p_month+4(2) }月Saving|.
    gv_jg  = |{ gv_jg }-{ gs_t024e-ekotx }|.
    gv_cgl = |{ gv_cgl }-{ gs_t024e-ekotx }|.
    gv_cge = |{ gv_cge }-{ gs_t024e-ekotx }|.
    gv_jg_bs  = |{ gs_t024e-bs }_JG_N|.
    gv_cgl_bs  = |{ gs_t024e-bs }_CGL_N|.
    gv_cge_bs  = |{ gs_t024e-bs }_CGEC_N|.
    PERFORM catlg_set TABLES fldct USING:
        gv_jg_bs  gv_jg   'ZTPP_205B'    'ZZJE',
        gv_cgl_bs  gv_cgl  ''       '',
        gv_cge_bs  gv_cge  'ZTPP_205B'  'ZZJE'.
  ENDLOOP.

  i_title               = lines( gt_tab ) .
  CONDENSE i_title.
  CONCATENATE '条目数:' i_title INTO i_title.

  "排序
  it_sort-fieldname = 'MATNR'.
  it_sort-up = 'X'.
  APPEND it_sort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      it_fieldcat              = fldct[]
      it_sort                  = it_sort[]
      i_save                   = 'A'
      is_variant               = varnt
      is_layout                = slayt
      i_grid_title             = i_title
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
*     IT_EVENTS                = GT_EVENTS
    TABLES
      t_outtab                 = gt_tab[]
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
  DA