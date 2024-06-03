*&---------------------------------------------------------------------*
*& Report ZPPR245A
*&---------------------------------------------------------------------*
*&周转率报表
*&---------------------------------------------------------------------*
REPORT zppr245a MESSAGE-ID zgp_msg.
TABLES:sscrfields,t001l,afko,eban,mchbh,mara,t001w,ztmm_011.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_itab,
        mtart      TYPE mtart,         "物料分类
        mtbez      TYPE T134t-mtbez,   "物料分类描述
        matnr      TYPE matnr,         "物料编码
        zzl1       TYPE mara-zzl1,     "品名
        charg      TYPE CHARG_d,       "批次号
        werks      TYPE werks_d,       "工厂
        werksname  TYPE t001w-name1,   "工厂描述
        lgort      TYPE mseg-lgort,    "库存地
        lgobe      TYPE t001l-lgobe,   "库存地描述
        mat_pspnr  TYPE mat_pspnr,     "WBS要素
        ps_psp_pnr TYPE mseg-ps_psp_pnr,
        post1      TYPE prps-post1,    "项目描述
        meins      TYPE mseg-meins,    "基本单位
        wlcms      TYPE string,        "物料长描述
        zckje      TYPE mseg-dmbtr,    "出库金额
        zqcje      TYPE zfi028_curr,   "期初金额
        qcsl       TYPE mseg-menge,    "期初库存数量
        zqmje      TYPE zfi028_curr,   "期末金额
        qmsl       TYPE mseg-menge,    "期末库存数量
        zqh        TYPE char6,         "期号
        zzzl       TYPE p DECIMALS 3,        "周转率
        zzzts      TYPE p DECIMALS 3,        "周转天数
        vprsv      TYPE mseg-vprsv,
        dmbtr      TYPE mseg-dmbtr,
        menge      TYPE mseg-menge,
        kunnr      TYPE mseg-kunnr,
        lifnr      TYPE mseg-lifnr,
        sel,
      END OF ty_itab.
DATA:itab    TYPE TABLE OF ty_itab WITH EMPTY KEY,
     wa_itab LIKE LINE OF itab.
DATA:it_t001w   TYPE TABLE OF t001w WITH HEADER LINE.
DATA:it_ztpp320 TYPE TABLE OF ztpp320 WITH EMPTY KEY,
     wa_ztpp320 LIKE LINE OF it_ztpp320.
DATA:mtart   TYPE RANGE OF mara-mtart.
DATA:gr_lgort TYPE RANGE OF lgort_d,
     gw_lgort LIKE LINE OF gr_lgort.
DATA:BEGIN OF it_wlcms OCCURS 0,
       matnr TYPE matnr,
       wlcms TYPE string,
     END OF it_wlcms.
DATA:BEGIN OF itab3 OCCURS 0,
       mtart TYPE mtart,
       mtbez TYPE T134t-mtbez,   "物料分类描述
       zzl1  TYPE mara-zzl1,     "品名
       zckje TYPE mseg-dmbtr,    "出库金额
       zqcje TYPE zfi028_curr,   "期初金额
       zqmje TYPE zfi028_curr,   "期末金额
       zqh   TYPE char6,
       zzzl  TYPE p DECIMALS 3,  "周转率
       zzzts TYPE p DECIMALS 3,  "周转天数
     END OF itab3.
DATA: BEGIN OF gs_amount,
        matnr  TYPE ckmlhd-matnr,
        bwkey  TYPE ckmlhd-bwkey,
        bwtar  TYPE ckmlhd-bwtar,
        vbeln  TYPE ckmlhd-vbeln,
        posnr  TYPE ckmlhd-posnr,
        pspnr  TYPE ckmlhd-pspnr,
        amount TYPE zfi028_curr,
        lbkum  TYPE ckmlpp-lbkum,
        pvprs  TYPE p DECIMALS 2,
      END OF gs_amount.
DATA: gt_amount   LIKE TABLE OF gs_amount, "出库金额
      gt_qmamount LIKE TABLE OF gs_amount, "期末
      gt_qcamount LIKE TABLE OF gs_amount. "期初
*&物料成本核算号数据
DATA: gt_ckmlhd TYPE TABLE OF ckmlhd,
      gs_ckmlhd TYPE ckmlhd.
DATA: gt_ckmlhdqc TYPE TABLE OF ckmlhd,
      gs_ckmlhdqc TYPE ckmlhd.
DATA:wherestr TYPE string.
DATA: BEGIN OF it_sel2 OCCURS 0,
        werks TYPE werks_d,
        matnr TYPE matnr,
*        bwtar TYPE ckmlhd-bwtar,
        vbeln TYPE vbeln,
        posnr TYPE posnr,
        pspnr TYPE prps-pspnr,
      END OF it_sel2.
TYPES: BEGIN OF ty_sel3,
         matnr TYPE matnr,
         mtart TYPE mtart,
         zzl1  TYPE zzl1,
       END OF ty_sel3.
DATA: it_sel3 TYPE TABLE OF ty_sel3 WITH EMPTY KEY,
      wa_sel3 LIKE LINE OF it_sel3.
DATA: BEGIN OF it_zzl1 OCCURS 0,
        mtart TYPE mtart,
        zzl1  TYPE zzl1,
      END OF it_zzl1.
TYPES: BEGIN OF ty_sel4,
         zzl1  TYPE zzl1,
         zqcje TYPE zfi028_curr,   "期初金额
         zqmje TYPE zfi028_curr,   "期末金额
       END OF ty_sel4.
DATA: it_sel4 TYPE TABLE OF ty_sel4 WITH NON-UNIQUE KEY zzl1,
      wa_sel4 LIKE LINE OF it_sel4.
DATA:curtp TYPE ckmlcr-curtp.
DATA:gv_factory TYPE isoc_factor.
DATA:gv_waers TYPE t001-waers.
DATA: par_days TYPE t009b-butag.
TYPES: gt_ckmlhd_temp TYPE STANDARD TABLE OF ckmlhd.
*&期末库存数量
TYPES: BEGIN OF ty_qmkc,
         werks  TYPE mchbh-werks,
         lgort  TYPE mchbh-lgort,
         matnr  TYPE mchbh-matnr,
         charg  TYPE mchbh-charg,
         sobkz  TYPE mskuh-sobkz,
         kunnr  TYPE mskuh-kunnr,
         lifnr  TYPE mslbh-lifnr,
         vbeln  TYPE mskah-vbeln,
         posnr  TYPE mskah-posnr,
         pspnr  TYPE mspr-pspnr,
         bwtar  TYPE mbew-bwtar,
         clabs  TYPE mchbh-clabs,     "期初库存数量
         clabs1 TYPE mchbh-clabs,     "期初库存数量
         clabs2 TYPE mchbh-clabs,     "期初库存数量
         clabs3 TYPE mchbh-clabs,     "期初库存数量
         clabs4 TYPE mchbh-clabs,     "期初库存数量
         clabs5 TYPE mchbh-clabs,     "期初库存数量
         clabs6 TYPE mchbh-clabs,     "期初库存数量
         clabs7 TYPE mchbh-clabs,     "期初库存数量
         clabs8 TYPE mchbh-clabs,     "期初库存数量
         clabs9 TYPE mchbh-clabs,     "期初库存数量
         kaspe  TYPE mskah-kaspe,
         del    TYPE char1,
       END OF ty_qmkc.
DATA: gt_qmkc     TYPE TABLE OF ty_qmkc WITH HEADER LINE,
      gs_qmkc     TYPE ty_qmkc,
      gt_qmkc_sum TYPE TABLE OF ty_qmkc,
      gs_qmkc_sum TYPE ty_qmkc,
      gt_qckc_sum TYPE TABLE OF ty_qmkc,
      gs_qckc_sum TYPE ty_qmkc,
      it_mara     TYPE TABLE OF mara WITH HEADER LINE,
      it_ztmm011  TYPE TABLE OF ztmm_011 WITH HEADER LINE,
      BEGIN OF it_atnam OCCURS 0,
        atnam TYPE cabn-atnam,
      END OF it_atnam,
      it_mcha    TYPE TABLE OF mcha WITH HEADER LINE,
      it_mcha1   TYPE TABLE OF mcha WITH HEADER LINE,
      it_zsfi216 TYPE TABLE OF zsfi216 WITH HEADER LINE,
      it_kunnr   TYPE TABLE OF kna1_key WITH HEADER LINE,
      BEGIN OF it_pspnr OCCURS 0,
        pspnr TYPE prps-pspnr,
      END OF it_pspnr.
TYPES: BEGIN OF ty_mcha,
         matnr TYPE mcha-matnr,    "公司代码
         werks TYPE mcha-werks,   "工厂
         charg TYPE mcha-charg,   "批次
         bwtar TYPE mcha-bwtar,    "评估类型
       END OF ty_mcha.
DATA:gt_mcha TYPE TABLE OF ty_mcha,
     gs_mcha TYPE ty_mcha.
DATA:lastdate TYPE sy-datum,
     l_lfgja  LIKE mchbh-lfgja, "上个月年份
     l_lfmon  LIKE mchbh-lfmon. "上个月月份

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS: s_werks LIKE t001w-werks OBLIGATORY DEFAULT '3000'.
  PARAMETERS:p1 RADIOBUTTON GROUP grd1 USER-COMMAND singleclick DEFAULT 'X',
             p2 RADIOBUTTON GROUP grd1,
             p3 RADIOBUTTON GROUP grd1.
  SELECT-OPTIONS: s_lgort FOR eban-lgort NO INTERVALS MODIF ID lg1,
                  s_zzl1  FOR mara-zzl1  NO INTERVALS MODIF ID zl1.
  PARAMETERS:s_lfgja LIKE mchbh-lfgja OBLIGATORY DEFAULT sy-datum+0(4),
             s_lfmon LIKE mchbh-lfmon OBLIGATORY DEFAULT sy-datum+4(2).
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.
  t1 = '选择条件'.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_lgort_%_app_%-text = '仓库'.
  %_s_zzl1_%_app_%-text  = '品名'.
  %_s_lfgja_%_app_%-text = '会计年度'.
  %_s_lfmon_%_app_%-text = '会计期间'.
  %_p1_%_app_%-text      = '成品周转率'.
  %_p2_%_app_%-text      = '原材周转率'.
  %_p3_%_app_%-text      = '全部'.


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'LG1'.
      IF p3 EQ 'X'.
        screen-required = 2.
      ELSE.
        screen-active = 0.
      ENDIF.
    ENDIF.
    IF screen-group1 = 'ZL1'.
      IF p1 EQ 'X'.
        screen-required = 2.
      ELSE.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-low.
  PERFORM zf4_lgort.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-high.
  PERFORM zf4_lgort.

START-OF-SELECTION.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.

  IF p2 EQ 'X'.
    mtart = VALUE #( sign = 'I' option = 'EQ'
     ( low = 'Z003' )
     ( low = 'Z005' )
     ( low = 'Z008' )
     ( low = 'Z011' )
     ).
  ENDIF.
  IF s_werks NE '3000' AND s_werks NE '3060'.
    MESSAGE s004 WITH '所选工厂不在报表统计范围内' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF p1 EQ 'X' AND s_zzl1[] IS INITIAL.
    MESSAGE s001 WITH '品名' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF p3 EQ 'X' AND s_lgort[] IS INITIAL.
    MESSAGE s001 WITH '仓库' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  DATA:s_mtart  TYPE mara-mtart.
  IF s_zzl1[] IS NOT INITIAL AND p1 EQ 'X'.
    LOOP AT s_zzl1.
      CLEAR s_mtart.
      SELECT SINGLE mtart INTO s_mtart FROM mara WHERE zzl1 = s_zzl1-low.
      IF s_mtart NOT IN mtart.
        DATA(rtmsg) =  '所填品名' && s_zzl1-low && '不在本报表查询范围内'.
        MESSAGE ID 'ZGP_MSG' TYPE 'S' NUMBER '004' DISPLAY LIKE 'E' WITH rtmsg RAISING e.
        STOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF s_lfmon < 1 OR s_lfmon > 12.
    MESSAGE '请输入正确的期间' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  curtp = '10'.
  SELECT SINGLE waers
  INTO gv_waers
  FROM t001
  WHERE bukrs = s_werks.
*&取公司本币
  CALL FUNCTION 'CURRENCY_CONVERTING_FACTOR'
    EXPORTING
      currency = gv_waers
    IMPORTING
      factor   = gv_factory.
  CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
    EXPORTING
      par_month = s_lfmon
      par_year  = s_lfgja
    IMPORTING
      par_days  = par_days.
  CLEAR: lastdate,l_lfgja,l_lfmon.
  lastdate = s_lfgja && s_lfmon && '01'.
  lastdate = lastdate - 1.
  l_lfgja = lastdate+0(4).
  l_lfmon = lastdate+4(2).
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  PERFORM alvshow.
*&---------------------------------------------------------------------*
*& Form getdata
*&---------------------------------------------------------------------*
*& 取值
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdata .
  SELECT * FROM ztpp320 WHERE werks1 = @s_werks INTO TABLE @it_ztpp320.
  SELECT werks FROM @it_ztpp320 AS a GROUP BY werks INTO TABLE @DATA(it_ztpp320_werks).
  CLEAR: wherestr.
  IF p3 NE 'X'.
    IF p1 EQ 'X'.
      wherestr = `  m~zzl1  IN @s_zzl1 `
                && `AND     LEFT( a~budat_mkpf,4 ) = @s_lfgja `
                && `AND     SUBSTRING( a~budat_mkpf,5,2 ) = @s_lfmon`.
    ELSEIF p2 EQ 'X'.
      wherestr = `  m~mtart IN @mtart `
                && `AND   LEFT( a~budat_mkpf,4 ) = @s_lfgja `
                && `AND   SUBSTRING( a~budat_mkpf,5,2 ) = @s_lfmon`.
    ENDIF.
    SELECT
        a~matnr,
        m~mtart,
        m~zzl1,
        a~charg,
        a~werks,
        a~lgort,
        a~mat_pspnr,
        a~meins,
        a~vprsv,
        a~dmbtr,
        a~menge,
        concat( @s_lfgja,@s_lfmon ) AS zqh,
        a~ps_psp_pnr,
        a~kunnr,
        a~lifnr
      FROM mseg AS a
      INNER JOIN mara AS m ON m~matnr = a~matnr
      INNER JOIN @it_ztpp320 AS p ON a~werks = p~werks AND a~lgort = p~lgort
      WHERE (wherestr)
      AND a~bwart IN ( '601','687','Z81' )
      AND a~sobkz EQ 'Q'
      AND NOT EXISTS ( SELECT * FROM m_mbmps AS ps "未被冲销
                          WHERE ps~sjahr = a~mjahr
                            AND ps~smbln = a~mblnr
                            AND ps~smblp = a~zeile
                      )
      AND NOT EXISTS ( SELECT * FROM likp AS p "排除投料消耗
                          WHERE p~vbeln = a~xblnr_mkpf
                            AND p~lfart = 'ZNX1'
                      )
      INTO CORRESPONDING FIELDS OF TABLE @itab.
    SELECT
       a~matnr,
       m~mtart,
       m~zzl1,
       a~charg,
       a~werks,
       a~lgort,
       a~mat_pspnr,
       a~meins,
       a~vprsv,
       a~dmbtr,
       a~menge,
       concat( @s_lfgja,@s_lfmon ) AS zqh,
       a~ps_psp_pnr,
       a~kunnr,
       a~lifnr
     FROM mseg AS a
     INNER JOIN mara AS m ON m~matnr = a~matnr
     INNER JOIN @it_ztpp320 AS p ON a~werks = p~werks AND a~lgort = p~lgort
     WHERE (wherestr)
     AND a~bwart EQ '261'
     AND NOT EXISTS ( SELECT * FROM m_mbmps AS ps "未被冲销
                         WHERE ps~sjahr = a~mjahr
                           AND ps~smbln = a~mblnr
                           AND ps~smblp = a~zeile
                     )
     APPENDING CORRESPONDING FIELDS OF TABLE @itab.
  ELSE.
    wherestr = ` a~werks = @s_werks `
            && `AND   a~lgort IN @s_lgort `
            && `AND   LEFT( a~budat_mkpf,4 ) = @s_lfgja `
            && `AND   SUBSTRING( a~budat_mkpf,5,2 ) = @s_lfmon`.
    SELECT
        a~matnr,
        m~mtart,
        m~zzl1,
        a~charg,
        a~werks,
        a~lgort,
        a~mat_pspnr,
        a~meins,
        a~vprsv,
        a~dmbtr,
        a~menge,
        concat( @s_lfgja,@s_lfmon ) AS zqh,
        a~ps_psp_pnr,
        a~kunnr,
        a~lifnr
      FROM mseg AS a
      INNER JOIN mara AS m ON m~matnr = a~matnr
      WHERE (wherestr)
      AND a~bwart IN ( '601','687','Z81' )
      AND a~sobkz EQ 'Q'
      AND NOT EXISTS ( SELECT * FROM m_mbmps AS ps "未被冲销
                          WHERE ps~sjahr = a~mjahr
                            AND ps~smbln = a~mblnr
                            AND ps~smblp = a~zeile
                      )
      AND NOT EXISTS ( SELECT * FROM likp AS p "排除投料消耗
                          WHERE p~vbeln = a~xblnr_mkpf
                            AND p~lfart = 'ZNX1'
                      )
      INTO CORRESPONDING FIELDS OF TABLE @itab.
    SELECT
       a~matnr,
       m~mtart,
       m~zzl1,
       a~charg,
       a~werks,
       a~lgort,
       a~mat_pspnr,
       a~meins,
       a~vprsv,
       a~dmbtr,
       a~menge,
       concat( @s_lfgja,@s_lfmon ) AS zqh,
       a~ps_psp_pnr,
       a~kunnr,
       a~lifnr
     FROM mseg AS a
     INNER JOIN mara AS m ON m~matnr = a~matnr
     WHERE (wherestr)
     AND a~bwart EQ '261'
     AND NOT EXISTS ( SELECT * FROM m_mbmps AS ps "未被冲销
                         WHERE ps~sjahr = a~mjahr
                           AND ps~smbln = a~mblnr
                           AND ps~smblp = a~zeile
                     )
     APPENDING CORRESPONDING FIELDS OF TABLE @itab.
  ENDIF.

  REFRESH:it_sel2,it_mcha,it_mcha1,gr_lgort,it_zzl1.
  "物料长描述
  LOOP AT itab INTO wa_itab.
    CLEAR: it_wlcms,it_sel2,it_mcha,gw_lgort.
    it_wlcms-matnr = wa_itab-matnr.
    it_sel2-werks = wa_itab-werks.
    it_sel2-matnr = wa_itab-matnr.
    it_sel2-pspnr = wa_itab-mat_pspnr.
    it_mcha-matnr = wa_itab-matnr.
    it_mcha-charg = wa_itab-charg.
    it_mcha1 = it_mcha.
    it_mcha1-werks = wa_itab-werks.
    it_zzl1-zzl1 = wa_itab-zzl1.
    it_zzl1-mtart = wa_itab-mtart.
    gw_lgort-sign   = 'I'.
    gw_lgort-option = 'EQ'.
    gw_lgort-low    = wa_itab-lgort.
    COLLECT gw_lgort INTO gr_lgort.
    COLLECT: it_wlcms,it_sel2,it_mcha,it_mcha1,it_zzl1.
  ENDLOOP.
  IF it_sel2[] IS NOT INITIAL.
    "出库单相关的单独取
    SORT it_sel2 BY werks matnr pspnr.
    "取CKMLHD 数据：
    SELECT *
      INTO TABLE gt_ckmlhd
      FROM ckmlhd
        FOR ALL ENTRIES IN it_sel2
        WHERE bwkey = it_sel2-werks
          AND matnr = it_sel2-matnr
          AND pspnr = it_sel2-pspnr
          AND vbeln = ''.
    SORT gt_ckmlhd BY kalnr.
    DELETE ADJACENT DUPLICATES FROM gt_ckmlhd COMPARING kalnr.
    "出库金额
    REFRESH:gt_amount.
    PERFORM get_amount USING gt_amount s_werks s_lfgja s_lfmon.
    SORT gt_amount BY matnr bwkey pspnr.
*    "期初
*    REFRESH:gl_amount.
*    PERFORM get_amount USING gl_amount s_werks l_lfgja l_lfmon.
*    SORT gl_amount BY matnr bwkey pspnr.
  ENDIF.
  REFRESH it_sel3.
  CASE 'X'.
    WHEN p1.
      SELECT
        matnr,
        mtart,
        zzl1
      FROM mara
      WHERE zzl1 IN @s_zzl1
      GROUP BY matnr,mtart,zzl1
      ORDER BY matnr,mtart,zzl1
      INTO TABLE @it_sel3.
    WHEN p2.
      SELECT
        matnr,
        mtart,
        zzl1
      FROM mara
      WHERE mtart IN @mtart
      GROUP BY matnr,mtart,zzl1
      ORDER BY matnr,mtart,zzl1
      INTO TABLE @it_sel3.
    WHEN p3.
      SELECT
        matnr,
        mtart,
        zzl1
      FROM mara
      GROUP BY matnr,mtart,zzl1
      ORDER BY matnr,mtart,zzl1
      INTO TABLE @it_sel3.

  ENDCASE.
  LOOP AT it_sel3 INTO wa_sel3.
    CLEAR it_zzl1.
    it_zzl1-mtart = wa_sel3-mtart.
    it_zzl1-zzl1 = wa_sel3-zzl1.
    COLLECT it_zzl1.
  ENDLOOP.
  IF it_zzl1[] IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF it_sel3 IS NOT INITIAL.
    SELECT ckmlhd~*
    FROM ckmlhd
    INNER JOIN @it_sel3 AS a ON a~matnr = ckmlhd~matnr
    ORDER BY kalnr
    INTO TABLE @gt_ckmlhdqc
    .
    SORT gt_ckmlhdqc BY kalnr.
    DELETE ADJACENT DUPLICATES FROM gt_ckmlhdqc.
    "期末
    REFRESH:gt_qmamount.
    CASE 'X'.
      WHEN p1.
        LOOP AT it_ztpp320_werks INTO DATA(wa_ztpp320_werks).
          PERFORM get_amountje USING gt_qmamount wa_ztpp320_werks-werks gt_ckmlhdqc s_lfgja s_lfmon.
        ENDLOOP.
      WHEN OTHERS.
        PERFORM get_amountje USING gt_qmamount s_werks gt_ckmlhdqc s_lfgja s_lfmon.
    ENDCASE.
    SORT gt_qmamount BY matnr bwkey.
    "期初
    REFRESH:gt_qcamount.
    CASE 'X'.
      WHEN p1.
        LOOP AT it_ztpp320_werks INTO wa_ztpp320_werks.
          PERFORM get_amountje USING gt_qcamount wa_ztpp320_werks-werks gt_ckmlhdqc l_lfgja l_lfmon.
        ENDLOOP.
      WHEN OTHERS.
        PERFORM get_amountje USING gt_qcamount s_werks gt_ckmlhdqc l_lfgja l_lfmon.
    ENDCASE.
    SORT gt_qcamount BY matnr bwkey .
  ENDIF.
*  PERFORM getlongtextpl(zpubform) TABLES it_wlcms.
*  SORT it_wlcms BY matnr.
  "期末库存
  PERFORM getgmkc USING gt_qmkc_sum s_werks gr_lgort s_lfgja s_lfmon.
  "期初库存
  PERFORM getgmkc USING gt_qckc_sum s_werks gr_lgort l_lfgja l_lfmon.
  REFRESH it_sel4.
  LOOP AT it_sel3 INTO wa_sel3.
    CLEAR wa_sel4.
    wa_sel4-zzl1 = wa_sel3-zzl1.
    CASE 'X'.
      WHEN p1.
        LOOP AT it_ztpp320_werks INTO wa_ztpp320_werks.
*&期初库存数量
          READ TABLE gt_qckc_sum INTO gs_qckc_sum WITH KEY matnr = wa_sel3-matnr werks = wa_ztpp320_werks-werks
                                                           BINARY SEARCH. "非发出商品
          IF sy-subrc = 0.
**&期初库存价值
            READ TABLE gt_qcamount INTO gs_amount WITH KEY matnr = wa_sel3-matnr bwkey = wa_ztpp320_werks-werks BINARY SEARCH.
            IF sy-subrc = 0.
              IF gs_amount-lbkum > 0.
                wa_sel4-zqcje = wa_sel4-zqcje + ( ( gs_amount-amount / gs_amount-lbkum )  * gv_factory * gs_qckc_sum-clabs ).
              ENDIF.
            ENDIF.
          ENDIF.
*&期末库存数量
          READ TABLE gt_qmkc_sum INTO gs_qmkc_sum WITH KEY matnr = wa_sel3-matnr werks = wa_ztpp320_werks-werks
                                                             BINARY SEARCH. "非发出商品
          IF sy-subrc = 0.
**&期末库存价值
            READ TABLE gt_qmamount INTO gs_amount WITH KEY matnr = wa_sel3-matnr bwkey = wa_ztpp320_werks-werks BINARY SEARCH.
            IF sy-subrc = 0.
              IF gs_amount-lbkum > 0.
                wa_sel4-zqmje = wa_sel4-zqmje + ( ( gs_amount-amount / gs_amount-lbkum )  * gv_factory * gs_qmkc_sum-clabs ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      WHEN OTHERS.
*&期初库存数量
        READ TABLE gt_qckc_sum INTO gs_qckc_sum WITH KEY matnr = wa_sel3-matnr werks = s_werks
                                                         BINARY SEARCH. "非发出商品
        IF sy-subrc = 0.
**&期初库存价值
          READ TABLE gt_qcamount INTO gs_amount WITH KEY matnr = wa_sel3-matnr bwkey = s_werks BINARY SEARCH.
          IF sy-subrc = 0.
            IF gs_amount-lbkum > 0.
              wa_sel4-zqcje = wa_sel4-zqcje + ( ( gs_amount-amount / gs_amount-lbkum )  * gv_factory * gs_qckc_sum-clabs ).
            ENDIF.
          ENDIF.
        ENDIF.
*&期末库存数量
        READ TABLE gt_qmkc_sum INTO gs_qmkc_sum WITH KEY matnr = wa_sel3-matnr werks = s_werks
                                                           BINARY SEARCH. "非发出商品
        IF sy-subrc = 0.
**&期末库存价值
          READ TABLE gt_qmamount INTO gs_amount WITH KEY matnr = wa_sel3-matnr bwkey = s_werks BINARY SEARCH.
          IF sy-subrc = 0.
            IF gs_amount-lbkum > 0.
              wa_sel4-zqmje = wa_sel4-zqmje + ( ( gs_amount-amount / gs_amount-lbkum )  * gv_factory * gs_qmkc_sum-clabs ).
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
    COLLECT wa_sel4 INTO it_sel4.
  ENDLOOP.
  SORT it_sel4 BY zzl1.

  "项目描述
*  SELECT
*    b~mat_pspnr,
*    p~post1
*  FROM prps AS p
*  INNER JOIN @itab AS b ON b~mat_pspnr = p~pspnr
*  ORDER BY mat_pspnr
*  INTO TABLE @DATA(it_prps).
  "物料分类描述
  SELECT
    t~spras,
    t~mtart,
    t~mtbez
  FROM t134t AS t
  FOR ALL ENTRIES IN @it_zzl1
  WHERE spras = @sy-langu
  AND t~mtart = @it_zzl1-mtart
  ORDER BY PRIMARY KEY
  INTO TABLE @DATA(it_t134t).
  "工厂描述
  SELECT
    *
  INTO TABLE it_t001w
  FROM t001w
  ORDER BY werks.
  "库存地描述
  SELECT
      t~werks,
      t~lgort,
      t~lgobe
  FROM t001l AS t
  INNER JOIN @itab AS b ON b~lgort = t~lgort AND b~werks = t~werks
  ORDER BY t~werks,t~lgort
  INTO TABLE @DATA(it_t001l).

  REFRESH itab3.
  "汇总
  LOOP AT itab INTO wa_itab.
    READ TABLE it_t001w WITH KEY werks = wa_itab-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-werksname = it_t001w-name1.
    ENDIF.
*    READ TABLE it_wlcms WITH KEY matnr = wa_itab-matnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_itab-wlcms = it_wlcms-wlcms.
*    ENDIF.
*    READ TABLE it_prps INTO DATA(wa_prps) WITH KEY mat_pspnr = wa_itab-mat_pspnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_itab-post1 = wa_prps-post1.
*    ENDIF.
    READ TABLE it_t001l INTO DATA(wa_t001l) WITH KEY lgort = wa_itab-lgort werks = wa_itab-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-lgobe = wa_t001l-lgobe.
    ENDIF.
    IF wa_itab-vprsv EQ 'V'.
      wa_itab-zckje = wa_itab-dmbtr.
    ELSEIF wa_itab-vprsv = 'S'.
      CLEAR wa_itab-dmbtr.
      READ TABLE gt_amount INTO gs_amount WITH  KEY matnr = wa_itab-matnr bwkey = wa_itab-werks pspnr = wa_itab-mat_pspnr BINARY SEARCH.
      IF sy-subrc EQ 0.
*            IF gs_amount-lbkum > 0.
*              itab-dmbtr = itab-menge * ( gs_amount-amount / gs_amount-lbkum ) .
*            ENDIF.
        wa_itab-zckje = gs_amount-pvprs * wa_itab-menge.
      ENDIF.
    ENDIF.
    CLEAR itab3.
    itab3-mtart = wa_itab-mtart.
    itab3-zzl1 = wa_itab-zzl1.
    itab3-zckje = wa_itab-zckje.
    COLLECT itab3.
  ENDLOOP.
  SORT itab3 BY mtart zzl1.
*  REFRESH itab3.
*  LOOP AT itab INTO wa_itab.
*    CLEAR itab3.
*    itab3-mtart = wa_itab-mtart.
*    itab3-mtbez = wa_itab-mtbez.
*    itab3-zzl1 = wa_itab-zzl1.
*    itab3-zckje = wa_itab-zckje.
*    itab3-zqh   = wa_itab-zqh.
*    COLLECT itab3.
*  ENDLOOP.
  REFRESH itab.
  LOOP AT it_zzl1.
    CLEAR wa_itab.
    wa_itab-zzl1 = it_zzl1-zzl1.
    wa_itab-mtart = it_zzl1-mtart.
    READ TABLE it_t134t INTO DATA(wa_t134t) WITH KEY mtart = it_zzl1-mtart BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-mtbez = wa_t134t-mtbez.
    ENDIF.
    wa_itab-zqh = s_lfgja && s_lfmon.
    READ TABLE itab3 WITH KEY mtart = it_zzl1-mtart zzl1 = it_zzl1-zzl1 BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-zckje = itab3-zckje.
    ENDIF.
    READ TABLE it_sel4 INTO wa_sel4 WITH KEY zzl1 = it_zzl1-zzl1 BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-zqcje = wa_sel4-zqcje.
      wa_itab-zqmje = wa_sel4-zqmje.
    ENDIF.
    DATA(jine1) = wa_itab-zqcje + wa_itab-zqmje.
    IF jine1 NE 0.
      wa_itab-zzzl = wa_itab-zckje / ( jine1 / 2 ).
    ENDIF.
    IF wa_itab-zzzl NE 0.
      wa_itab-zzzts = par_days / wa_itab-zzzl.
    ENDIF.
    IF wa_itab-zckje NE 0 OR wa_itab-zqcje NE 0 OR wa_itab-zqmje NE 0.
      APPEND wa_itab TO itab.
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

  PERFORM catlg_set TABLES fldct USING:
    'MTART '  '物料分类    '      'MARA '   'MTART'      ,
    'MTBEZ '  '物料分类描述'      'T134T'   'MTBEZ'      ,
    'ZZL1 '   '品名'     'MARA'  'ZZL1',
    'ZCKJE '  '出库金额    '      'MSEG '   'DMBTR'      ,
    'ZQCJE '  '期初金额    '      ''        'ZFI028_CURR',
    'ZQMJE '  '期末金额    '      ''        'ZFI028_CURR',
    'ZQH   '  '期号        '      ''        ''           ,
    'ZZZL '   '周转率'    ''      ''   ,
    'ZZZTS '  '周转天数'  ''      ''   .


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
      SET TITLEBAR 'TB01' WITH '成品周转率报表'.
    WHEN p2.
      SET TITLEBAR 'TB01' WITH '原材周转率报表'.
    WHEN p3.
      SET TITLEBAR 'TB01' WITH '全部报表'.
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
FORM zf4_lgort.
  DATA: lt_dynpread TYPE STANDARD TABLE OF dynpread WITH HEADER LINE.
  DATA: lw_dynpread TYPE dynpread .
  lw_dynpread-fieldname = 'S_WERKS'.
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
      window_title     = '仓库'
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
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_amount USING p_gt_amount LIKE gt_amount
                      p_werks     LIKE t001w-werks
                      p_lfgja     LIKE mchbh-lfgja
                      p_lfmon     LIKE mchbh-lfmon.
  DATA: ls_periods        TYPE cl_fml_join_ckmlpp_cr_buffer=>ts_gs_period,
        lt_periods        TYPE cl_fml_join_ckmlpp_cr_buffer=>tt_gt_period,
        lt_period_ml_data TYPE cl_fml_join_ckmlpp_cr_buffer=>tt_gt_ml_period_data,
        f_untper          TYPE poper VALUE '000',
        lt_ckmlhd_all     TYPE STANDARD TABLE OF ckmlhd,
        lt_ckmlhd_pack    TYPE STANDARD TABLE OF ckmlhd,
        ls_ckmlhd         TYPE ckmlhd,
        gt_ckmlhd1        TYPE TABLE OF ckmlhd,
        lv_bwkey          TYPE bwkey,
        lv_pack_index     TYPE i,                   " note 1902569
        lv_last_index     TYPE i,                   " note 1902569
        lv_package_size   TYPE i.                   "note 2456791 - this variable is introduced solely for debugging purposes

  DATA:ls_amount LIKE gs_amount,
       lt_amount LIKE TABLE OF gs_amount.

  CONSTANTS: c_package_size TYPE i VALUE 20000. "note 2456791 (to improve performance - 200 was too low)

  CLEAR:lt_ckmlhd_all,lv_bwkey,ls_periods,lt_periods,
  lt_period_ml_data,lt_ckmlhd_all,lt_ckmlhd_pack,ls_ckmlhd,
  lv_pack_index,lv_last_index,lv_package_size,gt_ckmlhd1,
  lt_amount,ls_amount.

  LOOP AT gt_ckmlhd INTO gs_ckmlhd WHERE bwkey = p_werks.
    APPEND gs_ckmlhd TO gt_ckmlhd1.
  ENDLOOP.

  lt_ckmlhd_all  = gt_ckmlhd1.
  lv_bwkey = p_werks.

  DESCRIBE TABLE lt_ckmlhd_all LINES lv_last_index.

  IF lv_last_index = 0.
*  nothing found ...
    EXIT.
  ENDIF.

  CLEAR lv_package_size.
  lv_package_size = c_package_size.

  WHILE ( lv_pack_index <= lv_last_index ).

    PERFORM build_kalnr_package
    TABLES   lt_ckmlhd_all
             lt_ckmlhd_pack
    USING    lv_package_size
             lv_last_index
    CHANGING lv_pack_index..

    CLEAR lt_periods.
    LOOP AT lt_ckmlhd_pack INTO ls_ckmlhd.
      CLEAR ls_periods.
      ls_periods-kalnr = ls_ckmlhd-kalnr.
      ls_periods-idx = sy-tabix.
      MOVE p_lfmon  TO  ls_periods-poper.
      MOVE p_lfgja  TO ls_periods-bdatj .
      APPEND ls_periods TO lt_periods.
    ENDLOOP.

    SORT lt_periods BY bdatj poper kalnr.
    DELETE ADJACENT DUPLICATES FROM lt_periods COMPARING bdatj poper kalnr.
    SORT lt_periods BY idx.


    CLEAR lt_period_ml_data.
    CALL METHOD cl_fml_join_ckmlpp_cr_buffer=>get_most_recent_ml_pd_mass
      EXPORTING
        iv_untper         = f_untper
        iv_bwkey          = lv_bwkey
        it_kalnr          = lt_periods
        iv_poper          = ls_periods-poper
        iv_bdatj          = ls_periods-bdatj
      IMPORTING
        et_ml_period_data = lt_period_ml_data.

    CLEAR:ls_ckmlhd.

    LOOP AT lt_period_ml_data INTO  DATA(wa_period_ml_data) WHERE curtp = curtp.
      CLEAR:ls_amount.
      READ TABLE gt_ckmlhd INTO DATA(wa_ckmlhd) WITH KEY kalnr = wa_period_ml_data-kalnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_amount-matnr = wa_ckmlhd-matnr.
        ls_amount-bwkey = wa_ckmlhd-bwkey.
        ls_amount-bwtar = wa_ckmlhd-bwtar.
        ls_amount-vbeln = wa_ckmlhd-vbeln.
        ls_amount-posnr = wa_ckmlhd-posnr.
        ls_amount-pspnr = wa_ckmlhd-pspnr.
*        IF wa_ckmlhd-vbeln IS NOT INITIAL.
*          READ TABLE it_lips WITH KEY vbeln = wa_ckmlhd-vbeln
*                                      posnr = wa_ckmlhd-posnr
*                                      BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            ls_amount-pspnr = it_lips-ps_psp_pnr.
*          ENDIF.
*        ENDIF.
        ls_amount-amount = wa_period_ml_data-salk3.
        ls_amount-lbkum = wa_period_ml_data-lbkum.
        ls_amount-pvprs = wa_period_ml_data-pvprs.
        COLLECT ls_amount INTO lt_amount.
      ENDIF.
    ENDLOOP.
    CLEAR:
    lt_ckmlhd_pack.
  ENDWHILE.
  APPEND LINES OF lt_amount TO p_gt_amount.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_amountje USING p_gt_amount LIKE gt_amount
                      p_werks     LIKE t001w-werks
                      p_ckmlhd    LIKE gt_ckmlhdqc
                      p_lfgja     LIKE mchbh-lfgja
                      p_lfmon     LIKE mchbh-lfmon.
  DATA: ls_periods        TYPE cl_fml_join_ckmlpp_cr_buffer=>ts_gs_period,
        lt_periods        TYPE cl_fml_join_ckmlpp_cr_buffer=>tt_gt_period,
        lt_period_ml_data TYPE cl_fml_join_ckmlpp_cr_buffer=>tt_gt_ml_period_data,
        f_untper          TYPE poper VALUE '000',
        lt_ckmlhd_all     TYPE STANDARD TABLE OF ckmlhd,
        lt_ckmlhd_pack    TYPE STANDARD TABLE OF ckmlhd,
        ls_ckmlhd         TYPE ckmlhd,
        gt_ckmlhd1        TYPE TABLE OF ckmlhd,
        lv_bwkey          TYPE bwkey,
        lv_pack_index     TYPE i,                   " note 1902569
        lv_last_index     TYPE i,                   " note 1902569
        lv_package_size   TYPE i.                   "note 2456791 - this variable is introduced solely for debugging purposes

  DATA:ls_amount LIKE gs_amount,
       lt_amount LIKE TABLE OF gs_amount.

  CONSTANTS: c_package_size TYPE i VALUE 20000. "note 2456791 (to improve performance - 200 was too low)

  CLEAR:lt_ckmlhd_all,lv_bwkey,ls_periods,lt_periods,
  lt_period_ml_data,lt_ckmlhd_all,lt_ckmlhd_pack,ls_ckmlhd,
  lv_pack_index,lv_last_index,lv_package_size,gt_ckmlhd1,
  lt_amount,ls_amount.

  LOOP AT p_ckmlhd INTO gs_ckmlhd WHERE bwkey = p_werks.
    APPEND gs_ckmlhd TO gt_ckmlhd1.
  ENDLOOP.

  lt_ckmlhd_all  = gt_ckmlhd1.
  lv_bwkey = p_werks.

  DESCRIBE TABLE lt_ckmlhd_all LINES lv_last_index.

  IF lv_last_index = 0.
*  nothing found ...
    EXIT.
  ENDIF.

  CLEAR lv_package_size.
  lv_package_size = c_package_size.

  WHILE ( lv_pack_index <= lv_last_index ).

    PERFORM build_kalnr_package
    TABLES   lt_ckmlhd_all
             lt_ckmlhd_pack
    USING    lv_package_size
             lv_last_index
    CHANGING lv_pack_index..

    CLEAR lt_periods.
    LOOP AT lt_ckmlhd_pack INTO ls_ckmlhd.
      CLEAR ls_periods.
      ls_periods-kalnr = ls_ckmlhd-kalnr.
      ls_periods-idx = sy-tabix.
      MOVE p_lfmon  TO  ls_periods-poper.
      MOVE p_lfgja  TO ls_periods-bdatj .
      APPEND ls_periods TO lt_periods.
    ENDLOOP.

    SORT lt_periods BY bdatj poper kalnr.
    DELETE ADJACENT DUPLICATES FROM lt_periods COMPARING bdatj poper kalnr.
    SORT lt_periods BY idx.


    CLEAR lt_period_ml_data.
    CALL METHOD cl_fml_join_ckmlpp_cr_buffer=>get_most_recent_ml_pd_mass
      EXPORTING
        iv_untper         = f_untper
        iv_bwkey          = lv_bwkey
        it_kalnr          = lt_periods
        iv_poper          = ls_periods-poper
        iv_bdatj          = ls_periods-bdatj
      IMPORTING
        et_ml_period_data = lt_period_ml_data.

    CLEAR:ls_ckmlhd.

    LOOP AT lt_period_ml_data INTO  DATA(wa_period_ml_data) WHERE curtp = curtp.
      IF wa_period_ml_data-salk3 EQ 0.
        CONTINUE.
      ENDIF.
      CLEAR:ls_amount.
      READ TABLE p_ckmlhd INTO DATA(wa_ckmlhd) WITH KEY kalnr = wa_period_ml_data-kalnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_amount-matnr = wa_ckmlhd-matnr.
        ls_amount-bwkey = wa_ckmlhd-bwkey.
*        ls_amount-bwtar = wa_ckmlhd-bwtar.
*        ls_amount-vbeln = wa_ckmlhd-vbeln.
*        ls_amount-posnr = wa_ckmlhd-posnr.
*        ls_amount-pspnr = wa_ckmlhd-pspnr.
        ls_amount-amount = wa_period_ml_data-salk3.
        ls_amount-lbkum = wa_period_ml_data-lbkum.
*        ls_amount-pvprs = wa_period_ml_data-pvprs.
        COLLECT ls_amount INTO lt_amount.
      ENDIF.
    ENDLOOP.
    CLEAR:
    lt_ckmlhd_pack.
  ENDWHILE.
  APPEND LINES OF lt_amount TO p_gt_amount.
ENDFORM.

FORM build_kalnr_package TABLES
                         it_ckmlhd_all    TYPE  gt_ckmlhd_temp
                         et_ckmlhd_pack   TYPE  gt_ckmlhd_temp
                         USING
                         iv_package_size  TYPE i
                         iv_last_index    TYPE i
                         CHANGING
                         cv_pack_index    TYPE i.

  DATA: lv_pck_size    TYPE  i,
        lv_pck_start   TYPE  i,
        ls_ckmlhd_pck2 TYPE  ckmlhd.

  CLEAR et_ckmlhd_pack.
  lv_pck_size   = 0.
  lv_pck_start  = cv_pack_index.

  CHECK lv_pck_start LE iv_last_index.

* build packages
  LOOP AT   it_ckmlhd_all
  INTO ls_ckmlhd_pck2
  FROM lv_pck_start.                                    "#EC CI_NOORDER

    IF ( lv_pck_size LT iv_package_size ).

      lv_pck_size  = lv_pck_size + 1.
      cv_pack_index = sy-tabix + 1.

      APPEND ls_ckmlhd_pck2 TO et_ckmlhd_pack.
    ELSE.
*      Package is finished.
      EXIT.                                             "#EC CI_NOORDER
    ENDIF.

  ENDLOOP.

ENDFORM.                    "build_kalnr_package
*&---------------------------------------------------------------------*
*& Form GETGMKC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        gt_qmkc_sum
*& -->  p2        s_werks
*& -->  p2        s_lfgja
*& -->  p2        s_lfmon
*&---------------------------------------------------------------------*
FORM getgmkc USING p_qmkc_sum LIKE gt_qmkc_sum
                   p_werks    TYPE werks_d
                   p_lgort    LIKE gr_lgort
                   s_lfgja    LIKE mchbh-lfgja
                   s_lfmon    LIKE mchbh-lfmon.
  REFRESH: p_qmkc_sum,gt_qmkc.
  SELECT mardh~werks,
         mardh~matnr,
         mardh~lgort,
         mardh~labst AS clabs,
         mardh~umlme AS clabs1,
         mardh~insme AS clabs2,
         mardh~speme AS clabs3,
         mbew~bwtar
  FROM mardh INNER JOIN mara ON mardh~matnr = mara~matnr
             INNER JOIN mbew ON mardh~matnr = mbew~matnr
                            AND mardh~werks = mbew~bwkey
             INNER JOIN @it_sel3 AS a ON a~matnr = mardh~matnr
  WHERE mardh~lfgja = @s_lfgja
   AND  mardh~lfmon = @s_lfmon
*   AND  mardh~lgort IN @p_lgort
   AND  mara~xchpf NE 'X'
   AND  ( mardh~labst GT 0
    OR    mardh~umlme GT 0
    OR    mardh~insme GT 0
    OR    mardh~speme GT 0 )
  INTO CORRESPONDING FIELDS OF TABLE @gt_qmkc.
  SELECT mara~*
      FROM mara
      INNER JOIN @it_sel3 AS a ON a~matnr = mara~matnr
      INTO TABLE @it_mara
      .
  IF sy-subrc EQ 0.
    SORT it_mara BY matkl.
*取ZTMM_011
    SELECT *
      INTO TABLE @it_ztmm011
      FROM ztmm_011
      FOR ALL ENTRIES IN @it_mara
      WHERE matkl = @it_mara-matkl.
    SORT it_ztmm011 BY werks matkl.
*汇总出米率特征码
    LOOP AT it_ztmm011 INTO ztmm_011.
      CLEAR:it_atnam.
      it_atnam-atnam = ztmm_011-atnam.
      COLLECT it_atnam.
    ENDLOOP.
    SORT it_mara BY matnr.
  ENDIF.
*取折算数量数据
  PERFORM getzsdj IN PROGRAM zrfi217
  TABLES it_mcha1 it_ztmm011 it_zsfi216.
*&取期末库存
*&取表MCHBH库存CLABS
*&取在途库存 CUMLM
*&取退货质检库存 CINSM
*&取限制使用库存 CEINM
*&取已冻结库存  CSPEM
*&取退货库存  CRETM
  SELECT mchbh~lgort,
         mchbh~matnr,
         mchbh~charg,
         mchbh~clabs,
         mchbh~werks,
         mbew~bwtar,
         mchbh~cumlm AS clabs1,
         mchbh~cinsm AS clabs2,
         mchbh~ceinm AS clabs3,
         mchbh~cspem AS clabs4,
         mchbh~cretm AS clabs5
  FROM mchbh INNER JOIN mbew ON mchbh~matnr = mbew~matnr
                            AND mchbh~werks = mbew~bwkey
  INNER JOIN @it_sel3 AS a ON a~matnr = mchbh~matnr
  WHERE mchbh~lfgja = @s_lfgja
    AND   mchbh~lfmon = @s_lfmon
*    AND   mchbh~lgort IN @p_lgort
    AND   ( mchbh~clabs GT 0
     OR     mchbh~cumlm GT 0
     OR     mchbh~cinsm GT 0
     OR     mchbh~ceinm GT 0
     OR     mchbh~cspem GT 0
     OR     mchbh~cretm GT 0 )
  APPENDING CORRESPONDING FIELDS OF TABLE @gt_qmkc.
*&MSKAH - 销售订单库存：历史 （T库存）
  SELECT mskah~werks,
         mskah~lgort,
         mskah~matnr,
         mskah~charg,
         mskah~sobkz,
         mskah~vbeln,
         mskah~posnr,
         mskah~kaspe AS clabs1,
         mskah~kalab AS clabs2,
         mskah~kains AS clabs3,
         lips~ps_psp_pnr AS pspnr,
         likp~kunag AS kunnr,
         mbew~bwtar
  FROM mskah INNER JOIN lips ON mskah~vbeln = lips~vbeln
                            AND mskah~posnr = lips~posnr
             INNER JOIN likp ON lips~vbeln = likp~vbeln
             INNER JOIN mbew ON mskah~matnr = mbew~matnr
                            AND mskah~werks = mbew~bwkey
             INNER JOIN @it_sel3 AS a ON a~matnr = mskah~matnr
  WHERE mskah~lfgja = @s_lfgja
    AND mskah~lfmon = @s_lfmon
*    AND mskah~lgort IN @p_lgort
    AND ( mskah~kaspe GT 0
    OR    mskah~kalab GT 0
    OR    mskah~kains GT 0 )
  APPENDING CORRESPONDING FIELDS OF TABLE @gt_qmkc.
*存在出库单删掉但却在MSKA中有库存的
  SELECT mskah~werks
         mskah~matnr
         mskah~charg
         mskah~lgort
         mskah~vbeln
         mskah~posnr
         mskah~kaspe AS clabs1
         mskah~kalab AS clabs2
         mskah~kains AS clabs3
         mbew~bwtar
  APPENDING CORRESPONDING FIELDS OF TABLE gt_qmkc
  FROM mskah INNER JOIN mara ON mskah~matnr = mara~matnr
             INNER JOIN mbew ON mskah~matnr = mbew~matnr
                            AND mskah~werks = mbew~bwkey
  WHERE mskah~lfgja = s_lfgja
    AND mskah~lfmon = s_lfmon
*    AND mskah~lgort IN p_lgort
    AND ( mskah~kalab GT 0
    OR    mskah~kains GT 0
    OR    mskah~kaspe GT 0 )
    AND NOT EXISTS ( SELECT * FROM lips   "
                                   WHERE lips~vbeln = mskah~vbeln AND
                                         lips~posnr = mskah~posnr ).
*取项目库存
  SELECT msprh~werks,
         msprh~matnr,
         msprh~charg,
         msprh~pspnr,
         msprh~lgort,
         msprh~prlab AS clabs1,
         msprh~prins AS clabs2,
         msprh~prspe AS clabs3,
         proj~usr02 AS kunnr,
         mbew~bwtar
  FROM msprh INNER JOIN mbew ON msprh~matnr = mbew~matnr
                            AND msprh~werks = mbew~bwkey
             INNER JOIN prps ON msprh~pspnr = prps~pspnr
             INNER JOIN proj ON prps~psphi = proj~pspnr
             INNER JOIN @it_sel3 AS a ON a~matnr = msprh~matnr
  WHERE msprh~lfgja = @s_lfgja
    AND msprh~lfmon = @s_lfmon
*    AND msprh~lgort IN @p_lgort
    AND ( msprh~prlab GT 0
    OR    msprh~prins GT 0
    OR    msprh~prspe GT 0 )
  APPENDING CORRESPONDING FIELDS OF TABLE @gt_qmkc.
*&MSLBH - 供应商特殊库存：历史（分包）（O库存）
  SELECT mslbh~werks,
         mslbh~matnr,
         mslbh~charg,
         mslbh~sobkz,
         mslbh~lifnr,
         mslbh~lblab AS clabs1,
         mslbh~lbins AS clabs2,
         mbew~bwtar
  FROM mslbh INNER JOIN mbew ON mslbh~matnr = mbew~matnr
                            AND mslbh~werks = mbew~bwkey
             INNER JOIN @it_sel3 AS a ON a~matnr = mslbh~matnr
  WHERE mslbh~lfgja = @s_lfgja
    AND mslbh~lfmon = @s_lfmon
    AND ( mslbh~lblab GT 0
    OR    mslbh~lbins GT 0 )
  APPENDING CORRESPONDING FIELDS OF TABLE @gt_qmkc.
*&MSKUH - 客户特殊库存：历史 （W库存）
  SELECT mskuh~werks,
         mskuh~matnr,
         mskuh~charg,
         mskuh~sobkz,
         mskuh~kunnr,
         mskuh~kulab AS clabs3,
         mskuh~kuins AS clabs1,
         mskuh~kuuml AS clabs2,
         mbew~bwtar
  FROM mskuh INNER JOIN mbew ON mskuh~matnr = mbew~matnr
                            AND mskuh~werks = mbew~bwkey
            INNER JOIN @it_sel3 AS a ON a~matnr = mskuh~matnr
  WHERE mskuh~lfgja = @s_lfgja
    AND mskuh~lfmon = @s_lfmon
    AND ( mskuh~kulab GT 0
    OR    mskuh~kuins GT 0
    OR    mskuh~kuuml GT 0 )
  APPENDING CORRESPONDING FIELDS OF TABLE @gt_qmkc.
*供应商R库存
  SELECT msrdh~werks
         msrdh~matnr
         msrdh~charg
         msrdh~pspnr
         msrdh~rdlab AS clabs
         msrdh~rdins AS clabs1
         msrdh~lifnr
         msrdh~sobkz
         mbew~bwtar
         proj~usr02 AS kunnr
  APPENDING CORRESPONDING FIELDS OF TABLE gt_qmkc
  FROM msrdh INNER JOIN mara ON msrdh~matnr = mara~matnr
             INNER JOIN mbew ON msrdh~matnr = mbew~matnr
                            AND msrdh~werks = mbew~bwkey
             INNER JOIN prps ON msrdh~pspnr = prps~pspnr
             INNER JOIN proj ON prps~psphi = proj~pspnr
  WHERE msrdh~lfgja = s_lfgja
    AND msrdh~lfmon = s_lfmon
    AND ( msrdh~rdlab GT 0
    OR    msrdh~rdins GT 0 ).
  CLEAR:it_pspnr[].
  CASE 'X'.
    WHEN p1.
      LOOP AT gt_qmkc INTO gs_qmkc.
        READ TABLE it_ztpp320 INTO wa_ztpp320 WITH KEY werks = gs_qmkc-werks lgort = gs_qmkc-lgort.
        IF sy-subrc NE 0.
          gs_qmkc-del = 'X'.
          MODIFY gt_qmkc FROM gs_qmkc TRANSPORTING del.
        ENDIF.
      ENDLOOP.
      DELETE gt_qmkc[] WHERE del = 'X'.
    WHEN p2.
      LOOP AT gt_qmkc INTO gs_qmkc.
        IF gs_qmkc-werks NE s_werks.
          gs_qmkc-del = 'X'.
          MODIFY gt_qmkc FROM gs_qmkc TRANSPORTING del.
        ENDIF.
      ENDLOOP.
      DELETE gt_qmkc[] WHERE del = 'X'.
    WHEN p3.
      LOOP AT gt_qmkc INTO gs_qmkc.
        IF gs_qmkc-werks NE s_werks.
          gs_qmkc-del = 'X'.
        ENDIF.
        IF NOT gs_qmkc-lgort IN s_lgort .
          gs_qmkc-del = 'X'.
        ENDIF.
        MODIFY gt_qmkc FROM gs_qmkc TRANSPORTING del.
      ENDLOOP.
      DELETE gt_qmkc[] WHERE del = 'X'.
  ENDCASE.
  LOOP AT gt_qmkc INTO gs_qmkc.
    CLEAR :gs_mcha,gs_qmkc_sum.
*      READ TABLE GT_MCHA INTO GS_MCHA WITH KEY MATNR = GS_QMKC-MATNR
*                                               CHARG = GS_QMKC-CHARG
*                                               BINARY SEARCH.
    gs_qmkc_sum-werks = gs_qmkc-werks.
    gs_qmkc_sum-matnr = gs_qmkc-matnr.
*    gs_qmkc_sum-charg = gs_qmkc-charg.
*    gs_qmkc_sum-vbeln = gs_qmkc-vbeln.
*    gs_qmkc_sum-posnr = gs_qmkc-posnr.
*    gs_qmkc_sum-pspnr = gs_qmkc-pspnr.
*    gs_qmkc_sum-lgort = gs_qmkc-lgort.
*    gs_qmk