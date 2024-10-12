FUNCTION zfm_gp_sd_erp_xmwgbc.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(PSPID) TYPE  PS_PSPID OPTIONAL
*"  EXPORTING
*"     VALUE(HTQK) TYPE  ZSSD278
*"     VALUE(JQQK) TYPE  ZSSD278A
*"     VALUE(KXQK) TYPE  ZSSD278B
*"     VALUE(FLAG) TYPE  ZEMM_FLAG
*"     VALUE(MESSAGE) TYPE  ZEMM_MESSAGE
*"  TABLES
*"      IN_TAB STRUCTURE  ZSFI223 OPTIONAL
*"      ZXQK STRUCTURE  ZSSD278C OPTIONAL
*"      YCYL STRUCTURE  ZSSD278D OPTIONAL
*"      Z215 STRUCTURE  ZTFI215 OPTIONAL
*"----------------------------------------------------------------------

  zfmdatasave1 'ZFM_GP_SD_ERP_XMWGBC'.
  zfmdatasave2 'B'.
  COMMIT WORK.

*  RANGES:R_HKONT1 FOR BSEG-HKONT,
*         R_HKONT2 FOR BSEG-HKONT,
*         R_HKONT3 FOR BSEG-HKONT,
*         R_HKONT4 FOR BSEG-HKONT,
*         R_HKONT5 FOR BSEG-HKONT,
*         R_HKONT6 FOR BSEG-HKONT,
*         R_HKONT7 FOR BSEG-HKONT,
*         R_HKONT8 FOR BSEG-HKONT,
*         R_HKONT  FOR BSEG-HKONT.
  RANGES:s_matkl  FOR mara-matkl.
*  DATA:ZYYWSR TYPE ZE_DMBTR,
*       QTYWSR TYPE ZE_DMBTR,
*       ZYCB   TYPE ZE_DMBTR,
*       QTCB   TYPE ZE_DMBTR,
*       YF     TYPE ZE_DMBTR,
*       ZCB    TYPE ZE_DMBTR,
*       ZSR    TYPE ZE_DMBTR,
*       ML     TYPE ZE_DMBTR,
*       MC     TYPE ZE_DMBTR,
*       XC     TYPE ZE_DMBTR,
*       HBL    TYPE ZE_DMBTR,
*       FC     TYPE ZE_DMBTR,
*       BPBJ   TYPE ZE_DMBTR,
*       BZCL   TYPE ZE_DMBTR,
*       BCP    TYPE ZE_DMBTR,
*       CCP    TYPE ZE_DMBTR,
*       WXJGF  TYPE ZE_DMBTR,
*       RG     TYPE ZE_DMBTR,
*       RD     TYPE ZE_DMBTR,
*       ZJ     TYPE ZE_DMBTR,
*       JJWLXH TYPE ZE_DMBTR,
*       QT     TYPE ZE_DMBTR,
*       LRZXSR TYPE ZE_DMBTR,
*       LRZXCB TYPE ZE_DMBTR.
  DATA:bl TYPE p DECIMALS 2.
  TYPES:BEGIN OF zsvbeln ,
          vbeln TYPE vbeln,
        END OF zsvbeln.
*  DATA: IT_VBELN1  TYPE TABLE OF ZSVBELN WITH HEADER LINE.
  DATA: it_vbeln2  TYPE TABLE OF zsvbeln WITH HEADER LINE.
*  DATA: BEGIN OF IT_VBELN1,
*          VBELN TYPE VBELN,
*        END OF IT_VBELN1.
  TYPES:BEGIN OF zafko,
          aufnr     TYPE afko-aufnr,
          matnr_261 TYPE matnr,
          matnr_101 TYPE matnr,
          101       TYPE aufm-menge,
          261       TYPE aufm-menge,
          bl        TYPE aufm-menge,
        END OF zafko.
  RANGES:s_matnr FOR mara-matnr.
  RANGES:s_pspnr FOR prps-pspnr.
  RANGES:s_juanmatkl FOR mara-matkl.
*  DATA: IT_MATNR   TYPE TABLE OF MARA     WITH HEADER LINE.
  DATA: it_afko    TYPE TABLE OF zafko    WITH HEADER LINE.
  DATA: it_mcha    TYPE TABLE OF mcha_key WITH HEADER LINE.
  DATA: it_vbeln   TYPE TABLE OF lips_key WITH HEADER LINE.
  DATA: tabix  TYPE sy-tabix,
        tabix1 TYPE sy-tabix,
        tabix2 TYPE sy-tabix,
        tabix3 TYPE sy-tabix.
  DATA: kl     TYPE int2.
  DATA: vbelnposnr TYPE tdobname.
  DATA: tdname     TYPE thead-tdname.
  DATA: intab    TYPE TABLE OF mcha WITH HEADER LINE.
  DATA: outtab   TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
*  DATA: LFIMG    TYPE LIPS-LFIMG.
  TYPES:BEGIN OF zspcxx,
          matnr  TYPE lips-matnr,
          charg  TYPE lips-charg,
          zpcdh  TYPE ztpp_205a-zpcdh,
          zpcdhh TYPE ztpp_205a-zpcdhh,
*          ZCBS   TYPE CHAR1,
        END OF zspcxx.
  DATA:it_pcxx  TYPE TABLE OF zspcxx WITH HEADER LINE.
*  DATA:LT_MSSQ  TYPE TABLE OF MSSQ WITH HEADER LINE.
*  DATA:LT_MCHA  TYPE TABLE OF MCHA WITH HEADER LINE.
  DATA:lt_gjpc  TYPE TABLE OF mcha WITH HEADER LINE.
  DATA:lt_mspr  TYPE TABLE OF mspr WITH HEADER LINE.
  DATA:ksrq     TYPE sy-datum.
  DATA:jsrq     TYPE sy-datum.
  DATA:dysh     TYPE sy-datum."当于10号
  DATA:sgy     TYPE sy-datum. "上个月
  DATA:e_user_status TYPE TABLE OF bapi_user_status WITH HEADER LINE.
  DATA:gt_ZTFI214 TYPE TABLE OF ztfi214 WITH HEADER LINE.

  TYPES:BEGIN OF zspcmx,
          matnr    TYPE mcha-matnr,
          werks    TYPE mcha-werks,
          charg    TYPE mcha-charg,
          menge101 TYPE mseg-menge,
          menge543 TYPE mseg-menge,
        END OF zspcmx.
  DATA:it_543 TYPE TABLE OF zspcmx WITH HEADER LINE.
  DATA:it_101 TYPE TABLE OF zspcmx WITH HEADER LINE.
  DATA:it_wxpc TYPE TABLE OF mcha_key WITH HEADER LINE.
  DATA:it_kunnr TYPE TABLE OF kna1_key WITH HEADER LINE.
  DATA:it_zcbs TYPE TABLE OF mcha WITH HEADER LINE.
  DATA:ot_zcbs TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
  DATA:it_kl TYPE TABLE OF mcha WITH HEADER LINE.
  DATA:ot_kl TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
  DATA:datum TYPE sy-datum.
  DATA:gt_fhtl TYPE TABLE OF zspp221 WITH HEADER LINE. "通过OCP查询到的复核数据

  DATA:it_matnr  TYPE TABLE OF ccvx_matnr WITH HEADER LINE,
       outtab001 TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.

  DATA:wgbg_xh_matnr TYPE string.
  RANGES:s_xhmatnr FOR mara-matnr.

  CLEAR:wgbg_xh_matnr,s_xhmatnr,s_xhmatnr[].
  CLEAR:it_vbeln,it_vbeln2,htqk,jqqk,kxqk,flag,message,zxqk,ycyl,s_matnr,it_afko,s_juanmatkl,z215,gt_z215."IT_VBELN1,
  CLEAR:it_vbeln[],it_vbeln2[],zxqk[],ycyl[],s_matnr[],it_afko[],s_juanmatkl[],lt_mspr[],it_wxpc[],z215[],gt_z215[].",IT_VBELN1[]
*  REFRESH:R_HKONT1,R_HKONT2,R_HKONT3,R_HKONT4,R_HKONT5,R_HKONT6,R_HKONT7,R_HKONT8,R_HKONT,S_MATKL.
*  CLEAR:ZYYWSR,QTYWSR,ZYCB,QTCB,YF,ZCB,ZSR,ML.
*  CLEAR:MC,XC,HBL,FC,BPBJ,BZCL,BCP,CCP,WXJGF,RG,RD,ZJ,JJWLXH,QT,LRZXSR,LRZXCB,KSRQ,JSRQ.

  "半成品物料组
*  APPEND 'IEQA1301' TO S_MATKL. " 夹芯板_外墙板半成品_单料号半成品
*  APPEND 'IEQA1302' TO S_MATKL. " 夹芯板_外墙板半成品_自产小件
*  APPEND 'IEQA1401' TO S_MATKL. " 夹芯板_净化板半成品_单料号半成品
*  APPEND 'IEQA1402' TO S_MATKL. " 夹芯板_净化板半成品_自产小件
*  APPEND 'IEQD0800' TO S_MATKL. " 门窗_门窗半成品

  APPEND 'IEQE0201' TO s_juanmatkl. "     彩涂卷
  APPEND 'IEQE0202' TO s_juanmatkl. "     彩涂铝卷
  APPEND 'IEQE0203' TO s_juanmatkl. "     覆膜卷
  APPEND 'IEQE0204' TO s_juanmatkl. "     镀锌卷
  APPEND 'IEQE0205' TO s_juanmatkl. "     镀铝锌卷
  APPEND 'IEQE0206' TO s_juanmatkl. "     不锈钢卷
  APPEND 'IEQE0212' TO s_juanmatkl. "     镀锌带钢

  SELECT * INTO TABLE gt_ztfi214 FROM ztfi214 ORDER BY ztczl.

  SELECT
     matkl
    INTO TABLE @DATA(it_matkl)
    FROM t023t
    WHERE wgbez LIKE '%半成品%'
     .
  LOOP AT it_matkl INTO DATA(matkl).
    CLEAR s_matkl.
    s_matkl(3) = 'IEQ' .
    s_matkl-low = matkl-matkl.
    COLLECT s_matkl.
  ENDLOOP.

*  APPEND 'IBT60010100006001999999' TO R_HKONT1."主营业务收入
*  APPEND 'IBT64010100006401999999' TO R_HKONT2."主营业务成本
*  APPEND 'IBT60510100006051999999' TO R_HKONT3."其他业务收入
*  APPEND 'IBT64020100006402999999' TO R_HKONT4."其他业务成本
*
*  APPEND 'IEQ9964011110' TO R_HKONT6.
*  APPEND 'IEQ9964011120' TO R_HKONT6.
*  APPEND 'IEQ9964011130' TO R_HKONT6.
*  APPEND 'IEQ9964011140' TO R_HKONT6.
*  APPEND 'IEQ9964011150' TO R_HKONT6.
*  APPEND 'IEQ9964011160' TO R_HKONT6.
*  APPEND 'IEQ9964011170' TO R_HKONT6.
*  APPEND 'IEQ9964011180' TO R_HKONT6.
*  APPEND 'IEQ9964011200' TO R_HKONT6.
*  APPEND 'IEQ9964011210' TO R_HKONT6.
*  APPEND 'IEQ9964011220' TO R_HKONT6.
*  APPEND 'IEQ9964011230' TO R_HKONT6.
*  APPEND 'IEQ9964011250' TO R_HKONT6.
*  APPEND 'IEQ9964011260' TO R_HKONT6."主营/其他收入成本
*
*  APPEND 'IEQ9998698000' TO R_HKONT7."利润中心收入
*  APPEND 'IEQ9998698100' TO R_HKONT8."利润中心成本
*
*
*  APPEND LINES OF R_HKONT1 TO R_HKONT.
*  APPEND LINES OF R_HKONT2 TO R_HKONT.
*  APPEND LINES OF R_HKONT3 TO R_HKONT.
*  APPEND LINES OF R_HKONT4 TO R_HKONT.
*  APPEND LINES OF R_HKONT7 TO R_HKONT.
*  APPEND LINES OF R_HKONT8 TO R_HKONT.
*
*  APPEND LINES OF R_HKONT  TO R_HKONT5.
*
*  APPEND LINES OF R_HKONT6 TO R_HKONT.
* 获取当前日期
  ksrq = sy-datum(6) && '01'.
  dysh = sy-datum(6) && '10'.

* 查询上个月
  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      months  = -1
      olddate = sy-datum   "输入日期
    IMPORTING
      newdate = sgy.        "返回日期：20140401
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = ksrq
    IMPORTING
      last_day_of_month = jsrq
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.


  SELECT DISTINCT
     'I' AS sign,
     'EQ' AS option,
     kunnr AS low,
     kunnr AS high
    FROM ztsd204
    ORDER BY sign,option,low,high
    INTO TABLE @DATA(s_nbkh)
     .

*  WBS
  SELECT
      prps~pspnr AS xmnm,
      proj~pspid AS xmwm,
      proj~post1 AS xmms,
      prps~pspnr AS wbsnm,
      prps~posid AS wbswm,
      prps~post1 AS wbsms
    FROM prps
    INNER JOIN proj ON proj~pspnr  = prps~psphi
    WHERE proj~pspid = @pspid
    ORDER BY wbsnm
    INTO TABLE @DATA(it_prps)
     .

  IF sy-subrc NE 0.
    flag = 'N'.
    message = '查询失败：未查询到项目！'.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.

  CLEAR:e_user_status[].
  CALL FUNCTION 'BAPI_BUS2001_GET_STATUS'
    EXPORTING
      project_definition = pspid
*  IMPORTING
*     RETURN             =
    TABLES
*     E_SYSTEM_STATUS    =
      e_user_status      = e_user_status.

  LOOP AT e_user_status .
    htqk-zxmzt = htqk-zxmzt && e_user_status-user_status && ';' .
  ENDLOOP.

  CLEAR:s_pspnr[].
  LOOP AT it_prps INTO DATA(prps).
    CLEAR:s_pspnr.
    s_pspnr(3) = 'IEQ'.
    s_pspnr-low = prps-wbsnm.
    COLLECT s_pspnr.
  ENDLOOP.
*取会计凭证
*  SELECT
*       ACDOCA~*
*    FROM ACDOCA
*    INNER JOIN @IT_PRPS AS PRPS ON PRPS~WBSNM = ACDOCA~PS_PSP_PNR
*    WHERE RACCT IN @R_HKONT
*    ORDER BY ACDOCA~PS_PSP_PNR
*    INTO TABLE @DATA(IT_ACDOCA)
*      .
*  LOOP AT IT_ACDOCA INTO DATA(ACDOCA).
*    IF ACDOCA-RACCT IN R_HKONT2.
*      ZYCB   = ZYCB + ACDOCA-CSL.
*    ELSEIF ACDOCA-RACCT IN R_HKONT4.
*      QTCB   = QTCB + ACDOCA-CSL.
*    ELSEIF ACDOCA-RACCT IN R_HKONT8.
*      LRZXCB = LRZXCB + ACDOCA-CSL.
*    ELSEIF ACDOCA-RACCT IN R_HKONT1.
*      ZYYWSR = ZYYWSR + ACDOCA-CSL.
*    ELSEIF ACDOCA-RACCT IN R_HKONT3.
*      QTYWSR = QTYWSR + ACDOCA-CSL.
*    ELSEIF ACDOCA-RACCT IN R_HKONT7.
*      LRZXSR = LRZXSR + ACDOCA-CSL.
*    ENDIF.
**匹配各种材料费用
*    CASE ACDOCA-RACCT.
*      WHEN '9964011110'.
*        MC     = MC     + ACDOCA-CSL.
*      WHEN '9964011120'.
*        XC     =  XC    + ACDOCA-CSL.   .
*      WHEN '9964011130'.
*        HBL    = HBL    + ACDOCA-CSL.
*      WHEN '9964011140'.
*        FC     = FC     + ACDOCA-CSL.
*      WHEN '9964011150'.
*        BPBJ   = BPBJ   + ACDOCA-CSL.
*      WHEN '9964011160'.
*        BZCL   = BZCL   + ACDOCA-CSL.
*      WHEN '9964011170'.
*        BCP    = BCP    + ACDOCA-CSL.
*      WHEN '9964011180'.
*        CCP    = CCP    + ACDOCA-CSL.
*      WHEN '9964011200'.
*        WXJGF  = WXJGF  + ACDOCA-CSL.
*      WHEN '9964011210'.
*        RG     = RG     + ACDOCA-CSL.
*      WHEN '9964011220'.
*        RD     = RD     + ACDOCA-CSL.
*      WHEN '9964011230'.
*        ZJ     = ZJ     + ACDOCA-CSL.
*      WHEN '9964011250'.
*        JJWLXH = JJWLXH + ACDOCA-CSL.
*      WHEN '9964011260'.
*        QT     = QT     + ACDOCA-CSL.
*    ENDCASE.
*  ENDLOOP.
* 合同抬头
  SELECT
      vbak~*
    FROM vbak
    INNER JOIN @it_prps AS prps ON prps~wbsnm = vbak~ps_psp_pnr
    WHERE vbak~vbtyp = 'G' "合同
     AND  vbak~auart NE 'ZIC0' "排除内购内销合同
     AND  vbak~zzt1  IN ( 'D' , 'F' , 'G' , 'H' )
    ORDER BY vbak~erdat
    INTO TABLE @DATA(it_htak)
     .
  IF sy-subrc NE 0.
    flag = 'N'.
    message = '查询失败：未查询到合同！'.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.
  SELECT
     vbkd~vbeln
    FROM ztsd225a
    INNER JOIN vbkd             ON vbkd~bstkd = ztsd225a~bstkd
    INNER JOIN @it_htak AS vbak ON vbak~vbeln = vbkd~vbeln
    ORDER BY vbkd~vbeln
    INTO TABLE @DATA(it_ztsd225)
     .

  "合同情况
  READ TABLE it_prps INTO prps INDEX 1.
  htqk-pspid = prps-xmwm.
  htqk-post1 = prps-xmms.

  CLEAR:it_kunnr[].
  LOOP AT it_htak INTO DATA(vbak).
    "禅道 ID 27890  不看批导的合同版本
*    READ TABLE IT_ZTSD225 INTO DATA(ZTSD225A) WITH KEY VBELN = VBAK-VBELN BINARY SEARCH.
*    IF SY-SUBRC = 0.
    it_kunnr-kunnr = vbak-zhtly.
    COLLECT it_kunnr.
*    ENDIF.
  ENDLOOP.
  CLEAR:htqk-zhtly, htqk-zhtlyms.
  IF it_kunnr[] IS NOT INITIAL.
    SELECT
       kna1~kunnr,
       kna1~name1
      INTO TABLE @DATA(it_kna1)
      FROM kna1
      FOR ALL ENTRIES IN @it_kunnr
      WHERE kna1~kunnr = @it_kunnr-kunnr
       .
    LOOP AT it_kna1 INTO DATA(kna1).
      htqk-zhtly   = htqk-zhtly   && kna1-kunnr && ';'.
      htqk-zhtlyms = htqk-zhtlyms && kna1-name1 && ';'.
    ENDLOOP.

  ENDIF.


  READ TABLE it_htak INTO vbak INDEX 1.
  htqk-vbeln = vbak-vbeln.
  htqk-auart = vbak-auart.
  htqk-kunnr = vbak-kunnr.
  SELECT SINGLE
       name1
    INTO htqk-name1
    FROM kna1
    WHERE kunnr = vbak-kunnr
     .
  htqk-zclfw = vbak-zclfw.
  SELECT SINGLE
      ddtext
    INTO htqk-zclfwms
    FROM dd07t
    WHERE domname   IN ( 'ZD_ZCLFW' )
      AND ddlanguage = sy-langu
      AND as4local   = 'A'
      AND domvalue_l = vbak-zclfw
     .
  SELECT
    MIN( zdate )
    INTO @DATA(qdrq)
    FROM ztsdzdlog
    WHERE sapno   = @vbak-vbeln
     AND  tabname = 'VBAK'
     AND  fieldname = 'ZZT1'
     AND  value_o   = 'E'
     AND  value_n   IN ( 'C','I' )
      .
  htqk-qdrq = qdrq.
  SELECT SINGLE
     bezei
   INTO htqk-auartms
   FROM tvakt
   WHERE auart = htqk-auart
    AND  spras = sy-langu
     .
  htqk-zywy = vbak-zywy.
  SELECT SINGLE
     name1
    INTO htqk-zywyms
    FROM kna1
    WHERE kunnr = vbak-zywy
     .
  "交期情况
  jqqk-vbeln = vbak-vbeln.
  SORT it_htak BY zdjscdz .
  READ TABLE it_htak INTO DATA(wa_htak) INDEX 1.
*  JQQK-DJDZRQ = WA_HTAK-ZDJSCDZ.
  SELECT
    MIN( zdate )
    INTO @DATA(htsxrq)
    FROM ztsdzdlog
    WHERE sapno     = @vbak-vbeln
     AND  tabname   = 'VBAK'
     AND  fieldname = 'ZZT1'
     AND  value_o   = 'C'
     AND  value_n   = 'D'
      .
  jqqk-htsxrq = htsxrq.


  "款项情况
  kxqk-vbeln = vbak-vbeln.

* 合同明细
  SELECT
     vbak~vbeln,
     vbak~zgpdjbl,
     vbkd~bstkd,
     vbak~knumv,
     vbap~posnr,
     vbap~matnr,
     vbap~kwmeng,
     vbap~vrkme,
     vbap~kzwi1,
     ztpp_205a~zxishu,
     mara~zzl1
    FROM vbap
    INNER JOIN mara             ON mara~matnr = vbap~matnr
    INNER JOIN vbkd             ON vbap~vbeln = vbkd~vbeln AND vbkd~posnr = '000000'
    INNER JOIN @it_htak AS vbak ON vbak~vbeln = vbap~vbeln
    LEFT JOIN ztpp_205a         ON ztpp_205a~vbeln = vbap~vbeln AND ztpp_205a~posnr = vbap~posnr
    ORDER BY vbap~vbeln,vbap~posnr
    INTO TABLE @DATA(it_htap)
    .
  SORT it_htap BY vbeln posnr.
  DELETE ADJACENT DUPLICATES FROM it_htap COMPARING vbeln posnr.

  SELECT
    vbap~vbeln,
    vbap~posnr,
    vbap~vgbel,
    vbap~vgpos,
    ztpp_205a~zxishu
    FROM vbap
    INNER JOIN @it_htap AS htap ON htap~vbeln = vbap~vgbel AND htap~posnr = vbap~vgpos
    INNER JOIN ztpp_205a        ON ztpp_205a~vbeln = vbap~vbeln AND ztpp_205a~posnr = vbap~posnr
    WHERE vbap~auart_ana = 'ZIC0'
    ORDER BY vbap~vgbel,vbap~vgpos,vbap~vbeln,vbap~posnr
    INTO TABLE @DATA(it_nxht)
     .

  SELECT
     prcd_elements~*
    FROM prcd_elements
    INNER JOIN @it_htap AS vbap ON vbap~knumv = prcd_elements~knumv AND vbap~posnr = prcd_elements~kposn
    WHERE prcd_elements~kinak = ''
     AND  prcd_elements~kschl = 'ZPR0'
    ORDER BY prcd_elements~knumv , prcd_elements~kposn
    INTO TABLE @DATA(it_prcd)
     .
*新建表去重合同号
  DATA(it_htap_p) = it_htap.
  SORT it_htap_p BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_htap_p COMPARING vbeln.

  SELECT
      SUM( ztfi_gtrcont~dmbtr_gtsj )
   FROM ztfi_gtrcont
   INNER JOIN ztfi_gtre_hd     ON ztfi_gtre_hd~kpsq  = ztfi_gtrcont~kpsq
   INNER JOIN @it_htap_p AS htap ON ztfi_gtrcont~vbeln = htap~vbeln
*  AND ZTFI_GTRCONT~POSNR = HTAP~POSNR
   WHERE ztfi_gtre_hd~spzt  = '1'
   INTO @kxqk-kpe
     .
*   = DMBTR_GTSJ.
* 发货 、退货 、依据合同的 期初补差
  SELECT
      vbap~*
    FROM vbap
    INNER JOIN @it_htap AS htap ON  vbap~vgbel = htap~vbeln AND vbap~vgpos = htap~posnr
    WHERE vbap~zckzt NE 'Z'
    AND   vbap~vbtyp_ana IN ( 'C' , 'H' ) " 'K', 'L' ,
    ORDER BY vbap~vgbel,vbap~vgpos
    INTO TABLE @DATA(it_vbap)
     .

  LOOP AT it_vbap INTO DATA(wa_vbap) .
    CLEAR :it_vbeln.
    it_vbeln-vbeln = wa_vbap-vbeln.
    it_vbeln-posnr = wa_vbap-posnr.
    COLLECT it_vbeln.
*    IF WA_VBAP-VBTYP_ANA = 'K' OR WA_VBAP-VBTYP_ANA = 'L'.
*      IT_VBELN1-VBELN = WA_VBAP-VBELN.
*      COLLECT IT_VBELN1.
*    ENDIF.
    IF wa_vbap-vbtyp_ana =  'C' .
      it_vbeln2-vbeln = wa_vbap-vbeln.
      COLLECT it_vbeln2.
    ENDIF.
  ENDLOOP.
  "运费金额
  IF it_vbeln2[] IS NOT INITIAL.

    SELECT
        vbak~vbeln,
        vbak~zyfcdf,
        vbak~zysje,
        vbak~province,
        vbak~city,
        vbak~county,
        vbak~zxxdz
      INTO TABLE @DATA(it_fhak)
      FROM vbak
      FOR ALL ENTRIES IN @it_vbeln2
      WHERE vbeln   =    @it_vbeln2-vbeln
            .
    SORT it_fhak BY vbeln.

    LOOP AT it_fhak INTO DATA(wa_fhak).
      IF wa_fhak-zyfcdf = '1'.
        kxqk-yf = kxqk-yf + wa_fhak-zysje.
      ENDIF.
    ENDLOOP.
  ENDIF.
*  IF IT_VBELN1[] IS NOT INITIAL.
*    "补差
*    SELECT
*      *
*     INTO TABLE @DATA(IT_BCAK)
*     FROM VBAK
*     FOR ALL ENTRIES IN @IT_VBELN1
*     WHERE VBELN = @IT_VBELN1-VBELN
*      .
*    SORT IT_BCAK BY VBELN.
*  ENDIF.

  "期初补差
  SELECT
*     ZTSD_QCBC~VBELN,
*     ZTSD_QCBC~POSNR,
*     ZTSD_QCBC~ZHTH,
*     ZTSD_QCBC~ZHTHH,
*     VBRP~VBELN AS VFBEL,
*     VBRP~POSNR AS VGPOS,
*      SUM( VBRP~KZWI1 )
     ztsd_qcbc~vbeln,
     ztsd_qcbc~posnr,
     ztsd_qcbc~zhth,
     ztsd_qcbc~zhthh,
*     SUBSTRING( VBAP~ERDAT , 1 ,6 ) AS ERDAT,
     vbap~erdat,
     vbrp~kzwi1
    FROM ztsd_qcbc
    INNER JOIN vbap             ON vbap~vbeln = ztsd_qcbc~vbeln AND vbap~posnr = ztsd_qcbc~posnr
    INNER JOIN vbrp             ON vbrp~vgbel = vbap~vbeln      AND vbrp~vgpos = vbap~posnr
    INNER JOIN vbrk             ON vbrp~vbeln = vbrk~vbeln
    INNER JOIN @it_prps AS prps ON prps~wbsnm = vbap~ps_psp_pnr
    WHERE vbrk~fksto = @space
      AND vbrk~sfakn = @space
      AND vbap~kunnr_ana NOT IN @s_nbkh
   INTO TABLE @DATA(it_qcbc)
      .

  SELECT
     SUM( ztfi_gtrcont~dmbtr_gtsj )
  FROM ztfi_gtrcont
  INNER JOIN ztfi_gtre_hd     ON ztfi_gtre_hd~kpsq  = ztfi_gtrcont~kpsq
  INNER JOIN @it_qcbc AS bcap ON ztfi_gtrcont~vbeln = bcap~vbeln
*  AND ZTFI_GTRCONT~POSNR = HTAP~POSNR
  WHERE ztfi_gtre_hd~spzt  = '1'
  INTO @DATA(dmbtr_gtsj)
    .
  SELECT
     SUM( qcbc~kzwi1 )
    FROM @it_qcbc AS qcbc
    INTO @DATA(qcbcje)
    .
*按项目查补差
  SELECT
    vbak~auart AS auart,
    vbap~kzwi1 AS kzwi1,
    vbak~vbeln AS bcdh,
    vbap~erdat
  FROM vbak
  INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
  INNER JOIN vbrp ON vbap~vbeln = vbrp~vgbel AND vbap~posnr = vbrp~vgpos
  INNER JOIN vbrk             ON vbrp~vbeln = vbrk~vbeln
  INNER JOIN @it_prps AS prps ON prps~wbsnm = vbap~ps_psp_pnr
  WHERE vbak~vbtyp IN ( 'K' , 'L' )
    AND vbrk~fksto = @space
    AND vbrk~sfakn = @space
    AND vbak~kunnr NOT IN @s_nbkh
  INTO TABLE @DATA(it_bcxx2)
  .
  LOOP AT it_bcxx2 INTO DATA(xmbc).
    IF xmbc-auart = 'ZCR'.
      kxqk-jsje = kxqk-jsje + xmbc-kzwi1.
    ELSE.
      kxqk-jsje = kxqk-jsje - xmbc-kzwi1.
    ENDIF.
  ENDLOOP.
*补差
  SELECT
       htap~vbeln AS htd,
       htap~posnr AS htdh,
       fhap~vbeln AS fhd,
       fhap~posnr AS fhdh,
       lips~vbeln AS ckd,
       lips~posnr AS ckdh,
       bcap~vbeln AS bcd,
       bcap~posnr AS bcdh,
       bcak~auart,
*       SUBSTRING( BCAK~ERDAT , 1 ,6 ) AS ERDAT,
       bcak~erdat,
       bcap~kwmeng,
       bcap~kzwi1
    FROM vbap AS htap
    INNER JOIN vbap     AS fhap ON fhap~vgbel = htap~vbeln AND fhap~vgpos = htap~posnr
    INNER JOIN lips             ON lips~vgbel = fhap~vbeln AND lips~vgpos = fhap~posnr
    INNER JOIN vbap     AS bcap ON bcap~vgbel = lips~vbeln AND bcap~vgpos = lips~posnr
    INNER JOIN vbak     AS bcak ON bcak~vbeln = bcap~vbeln
    INNER JOIN @it_htap AS vbap ON htap~vbeln = vbap~vbeln AND htap~posnr = vbap~posnr
    WHERE fhap~vbtyp_ana = 'C' "发货
    AND   bcap~vbtyp_ana IN ( 'K' , 'L' ) "补差
    ORDER BY htap~vbeln,htap~posnr
    INTO TABLE @DATA(it_bcxx)
    .
  LOOP AT it_bcxx INTO DATA(wa_bcxx).
    CLEAR :it_vbeln.
    it_vbeln-vbeln = wa_bcxx-bcd.
    it_vbeln-posnr = wa_bcxx-bcdh.
    COLLECT it_vbeln.
  ENDLOOP.

  SELECT
     SUM( ztfi_gtrcont~dmbtr_gtsj )
  FROM ztfi_gtrcont
  INNER JOIN ztfi_gtre_hd     ON ztfi_gtre_hd~kpsq  = ztfi_gtrcont~kpsq
  INNER JOIN @it_bcxx AS bcap ON ztfi_gtrcont~vbeln = bcap~bcd
*  AND ZTFI_GTRCONT~POSNR = HTAP~POSNR
  WHERE ztfi_gtre_hd~spzt  = '1'
  INTO @DATA(dmbtr_gtsj1)
    .

  kxqk-kpe = kxqk-kpe +  dmbtr_gtsj + dmbtr_gtsj1.
*VF01
  IF it_vbeln[] IS NOT INITIAL.

*    SELECT
*       LIPS~*
*      INTO TABLE @DATA(IT_LIPS)
*      FROM LIPS
*      INNER JOIN @IT_VBELN AS VBELN ON  VBELN~VBELN = LIPS~VGBEL AND VBELN~POSNR = LIPS~VGPOS
*      WHERE WBSTA = 'C'.
*    SORT IT_LIPS BY VGBEL VGPOS.
    SELECT
       lips~vbeln,
       lips~posnr,
       likp~wadat_ist,
       likp~vbtyp,
       lips~vgbel,
       lips~vgpos,
       lips~matnr,
       lips~werks,
       lips~charg,
       lips~lfimg,
       lips~lgmng,
       lips~wbsta,
       lips~meins
      INTO TABLE @DATA(it_lips)
      FROM lips
      INNER JOIN likp ON likp~vbeln = lips~vbeln
      FOR ALL ENTRIES IN @it_vbeln
      WHERE vgbel = @it_vbeln-vbeln
      AND   vgpos = @it_vbeln-posnr
      AND   wbsta = 'C'.
    SORT it_lips BY vgbel vgpos.

    SELECT
       vbrp~*
      INTO TABLE @DATA(it_vbrp)
      FROM vbrp INNER JOIN vbrk ON vbrp~vbeln = vbrk~vbeln
      FOR ALL ENTRIES IN @it_vbeln
      WHERE vbrp~aubel = @it_vbeln-vbeln
      AND   vbrp~aupos = @it_vbeln-posnr
      AND   vbrk~fksto = @space
      AND   vbrk~sfakn = @space.
    SORT it_vbrp BY vgbel vgpos.

  ENDIF.

  SELECT
      *
    FROM ztsd226a
    WHERE ztsd226a~spras = @sy-langu
    ORDER BY province
    INTO TABLE @DATA(it_226a)
     .

  SELECT
      *
    FROM ztsd226b
    WHERE ztsd226b~spras = @sy-langu
    ORDER BY city
    INTO TABLE @DATA(it_226b)
     .

  SELECT
      *
    FROM ztsd226c
    WHERE ztsd226c~spras = @sy-langu
    ORDER BY county
    INTO TABLE @DATA(it_226c)
     .

  CLEAR intab[].


  LOOP AT it_htap INTO DATA(wa_htap).
    CLEAR : zxqk .
    MOVE-CORRESPONDING wa_htap TO zxqk.
    zxqk-htl = wa_htap-kwmeng.
    zxqk-dw  = wa_htap-vrkme.
*    IF WA_HTAP-VRKME = 'M2' AND WA_HTAP-ZXISHU NE 0.
*      ZXQK-HTM = WA_HTAP-KWMENG / WA_HTAP-ZXISHU.
*    ELSEIF WA_HTAP-VRKME = 'M'.
*      ZXQK-HTM = WA_HTAP-KWMENG.
*    ENDIF.
*    IF WA_HTAP-ZXISHU EQ 0.
*      "取内购内销合同排产
*      READ TABLE IT_NXHT INTO DATA(NXHT) WITH KEY VGBEL = WA_HTAP-VBELN VGPOS = WA_HTAP-POSNR BINARY SEARCH.
*      IF SY-SUBRC = 0 AND WA_HTAP-VRKME = 'M2' AND NXHT-ZXISHU NE 0.
*        ZXQK-HTM = WA_HTAP-KWMENG / NXHT-ZXISHU.
*      ELSEIF WA_HTAP-VRKME = 'M'.
*        ZXQK-HTM = WA_HTAP-KWMENG.
*      ENDIF.
*    ELSE.
*      IF WA_HTAP-VRKME = 'M2'.
*        ZXQK-HTM = WA_HTAP-KWMENG / WA_HTAP-ZXISHU.
*      ELSEIF WA_HTAP-VRKME = 'M'.
*        ZXQK-HTM = WA_HTAP-KWMENG.
*      ENDIF.
*    ENDIF.
    READ TABLE it_vbap INTO wa_vbap WITH KEY vgbel = wa_htap-vbeln vgpos = wa_htap-posnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      tabix = sy-tabix.
      LOOP AT it_vbap INTO wa_vbap FROM tabix.
        IF wa_vbap-vgbel NE wa_htap-vbeln OR wa_vbap-vgpos NE wa_htap-posnr.
          EXIT.
        ENDIF.
        CASE wa_vbap-vbtyp_ana.
          WHEN 'C'.
            READ TABLE it_fhak INTO DATA(wa_vbak) WITH KEY vbeln = wa_vbap-vbeln BINARY SEARCH.
            READ TABLE it_lips INTO DATA(wa_lips) WITH KEY vgbel = wa_vbap-vbeln vgpos = wa_vbap-posnr BINARY SEARCH.
            IF sy-subrc EQ 0.
              tabix1 = sy-tabix.
              LOOP AT it_lips INTO wa_lips FROM tabix1.
                IF wa_lips-vgbel NE wa_vbap-vbeln OR wa_lips-vgpos NE wa_vbap-posnr .
                  EXIT.
                ENDIF.
                CLEAR intab.
                intab-matnr = wa_lips-matnr.
                intab-werks = wa_lips-werks.
                intab-charg = wa_lips-charg.
                COLLECT intab.

                IF jqqk-spjhrq > wa_lips-wadat_ist OR jqqk-spjhrq IS INITIAL.
                  jqqk-spjhrq = wa_lips-wadat_ist .
                  READ TABLE it_226a INTO DATA(wa_226a) WITH KEY province = wa_vbak-province BINARY SEARCH.
                  READ TABLE it_226b INTO DATA(wa_226b) WITH KEY city     = wa_vbak-city   BINARY SEARCH.
                  READ TABLE it_226c INTO DATA(wa_226c) WITH KEY county   = wa_vbak-county BINARY SEARCH.
                  htqk-jhdd   = wa_226a-text && wa_226b-text && wa_226c-text ."&& WA_VBAK-ZXXDZ .交货地点  只展示省市县  不需要详细地址
                  CLEAR : wa_226a , wa_226b , wa_226c.
                ENDIF.
                IF jqqk-mpjhrq < wa_lips-wadat_ist.
                  jqqk-mpjhrq = wa_lips-wadat_ist .
                ENDIF.
                zxqk-zxl = zxqk-zxl + wa_lips-lfimg.
*                IF WA_HTAP-VRKME = 'M2' AND WA_HTAP-ZXISHU NE 0.
*                  ZXQK-ZXM = ZXQK-ZXL / WA_HTAP-ZXISHU.
*                ELSEIF WA_HTAP-VRKME = 'M'.
*                  ZXQK-ZXM = ZXQK-ZXL.
*                ENDIF.
                "modify hanwq 20240614
                READ TABLE it_vbrp INTO DATA(wa_vbrp) WITH KEY vgbel = wa_lips-vbeln vgpos = wa_lips-posnr BINARY SEARCH.
                IF sy-subrc EQ 0.
                  tabix2 = sy-tabix.
                  LOOP AT it_vbrp INTO wa_vbrp FROM tabix2.
                    IF wa_vbrp-vgbel NE wa_lips-vbeln OR wa_vbrp-vgpos NE wa_lips-posnr.
                      EXIT.
                    ENDIF.
                    kxqk-jsje = kxqk-jsje + wa_vbrp-kzwi1.
                    zxqk-kpje = zxqk-kpje + wa_vbrp-kzwi1.
                  ENDLOOP.
                ENDIF.
              ENDLOOP.
            ENDIF.

          WHEN 'H'.

            READ TABLE it_lips INTO wa_lips WITH KEY vgbel = wa_vbap-vbeln vgpos = wa_vbap-posnr BINARY SEARCH.
            IF sy-subrc EQ 0.
              tabix1 = sy-tabix.
              LOOP AT it_lips INTO wa_lips FROM tabix1.
                IF wa_lips-vgbel NE wa_vbap-vbeln OR wa_lips-vgpos NE wa_vbap-posnr.
                  EXIT.
                ENDIF.
                CLEAR intab.
                intab-matnr = wa_lips-matnr.
                intab-werks = wa_lips-werks.
                intab-charg = wa_lips-charg.
                COLLECT intab.
                IF wa_lips-wbsta NE 'C'.
                  CONTINUE.
                ENDIF.
                zxqk-zxl = zxqk-zxl - wa_lips-lfimg.
*                IF WA_HTAP-VRKME = 'M2'AND WA_HTAP-ZXISHU NE 0.
*                  ZXQK-ZXM = ZXQK-ZXL / WA_HTAP-ZXISHU.
*                ELSEIF WA_HTAP-VRKME = 'M'.
*                  ZXQK-ZXM = ZXQK-ZXL.
*                ENDIF.
                READ TABLE it_vbrp INTO wa_vbrp WITH KEY vgbel = wa_lips-vbeln vgpos = wa_lips-posnr BINARY SEARCH.
                IF sy-subrc EQ 0.
                  tabix2 = sy-tabix.
                  LOOP AT it_vbrp INTO wa_vbrp FROM tabix2.
                    IF wa_vbrp-vgbel NE wa_lips-vbeln OR wa_vbrp-vgpos NE wa_lips-posnr.
                      EXIT.
                    ENDIF.
                    kxqk-jsje = kxqk-jsje - wa_vbrp-kzwi1.
                    zxqk-kpje = zxqk-kpje - wa_vbrp-kzwi1.
                  ENDLOOP.
                ENDIF.
              ENDLOOP.
            ENDIF.
*            "增加 用标准功能 依附于合同的直接补差
*          WHEN 'K' OR 'L'.
*            READ TABLE IT_BCAK INTO DATA(WA_BCAK) WITH KEY VBELN = WA_VBAP-VBELN BINARY SEARCH.
*            IF SY-SUBRC = 0.
*              READ TABLE IT_VBRP INTO WA_VBRP WITH KEY VGBEL = WA_VBAP-VBELN VGPOS = WA_VBAP-POSNR BINARY SEARCH.
*              IF SY-SUBRC EQ 0.
*                TABIX2 = SY-TABIX.
*                LOOP AT IT_VBRP INTO WA_VBRP FROM TABIX2.
*                  IF WA_VBRP-VGBEL NE WA_VBAP-VBELN OR WA_VBRP-VGPOS NE WA_VBAP-POSNR.
*                    EXIT.
*                  ENDIF.
*                  IF WA_BCAK-AUART = 'ZCR'.
*                    KXQK-JSJE = KXQK-JSJE + WA_VBRP-KZWI1.
*                    ZXQK-KPJE = ZXQK-KPJE + WA_VBRP-KZWI1.
*                    KXQK-BCML = KXQK-BCML + WA_VBRP-KZWI1 * '0.59'.
*                  ELSE.
*                    KXQK-JSJE = KXQK-JSJE - WA_VBRP-KZWI1.
*                    ZXQK-KPJE = ZXQK-KPJE - WA_VBRP-KZWI1.
*                    KXQK-BCML = KXQK-BCML - WA_VBRP-KZWI1 * '0.59'.
*                  ENDIF.
*                ENDLOOP.
*              ENDIF.
*            ENDIF.

          WHEN OTHERS.

        ENDCASE.
      ENDLOOP.
    ENDIF.
    "程序补差
    READ TABLE it_bcxx INTO wa_bcxx WITH KEY htd = wa_htap-vbeln htdh = wa_htap-posnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_bcxx INTO wa_bcxx FROM sy-tabix.
        IF wa_bcxx-htd NE wa_htap-vbeln OR wa_bcxx-htdh NE wa_htap-posnr.
          EXIT.
        ENDIF.
        READ TABLE it_vbrp INTO wa_vbrp WITH KEY vgbel = wa_bcxx-bcd vgpos = wa_bcxx-bcdh  BINARY SEARCH.
        IF sy-subrc EQ 0.
          tabix2 = sy-tabix.
          LOOP AT it_vbrp INTO wa_vbrp FROM tabix2.
            IF wa_vbrp-vgbel NE wa_bcxx-bcd OR wa_vbrp-vgpos NE wa_bcxx-bcdh.
              EXIT.
            ENDIF.
            IF wa_bcxx-auart = 'ZCR'.
*              KXQK-JSJE = KXQK-JSJE + WA_VBRP-KZWI1.
              zxqk-kpje = zxqk-kpje + wa_vbrp-kzwi1.
              kxqk-bcml = kxqk-bcml + wa_vbrp-kzwi1 * '0.59'.
            ELSE.
*              KXQK-JSJE = KXQK-JSJE - WA_VBRP-KZWI1.
              zxqk-kpje = zxqk-kpje - wa_vbrp-kzwi1.
              kxqk-bcml = kxqk-bcml - wa_vbrp-kzwi1 * '0.59'.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.
    vbelnposnr = wa_htap-vbeln && wa_htap-posnr.
    PERFORM getlongtext(zpubform) USING 'Z001' vbelnposnr 'VBBP' CHANGING zxqk-guige.

    READ TABLE it_prcd INTO DATA(prcd) WITH KEY knumv = wa_htap-knumv  kposn = wa_htap-posnr BINARY SEARCH.
    IF sy-subrc = 0.
      IF prcd-kpein <> 0  .
        zxqk-dj = prcd-kbetr / prcd-kpein.
      ELSE.
        zxqk-dj = prcd-kbetr.
      ENDIF.
    ENDIF.
    zxqk-zxlc = zxqk-zxl - zxqk-htl.
    IF zxqk-htl NE 0 .
      bl = zxqk-zxlc / zxqk-htl * 100.
      zxqk-pcbl = bl && '%'.
    ENDIF.
    zxqk-pcje = zxqk-dj * zxqk-zxlc.

    "modify hanwq 20240614
    CASE zxqk-dw.
      WHEN 'M'.
        zxqk-htm = zxqk-htl.
        zxqk-zxm = zxqk-zxl.
      WHEN 'M2'.
        IF wa_htap-zxishu NE 0.
          zxqk-htm = zxqk-htl / wa_htap-zxishu.
          zxqk-zxm = zxqk-zxl / wa_htap-zxishu.
        ELSE.
          "取内购内销合同排产
          READ TABLE it_nxht INTO DATA(nxht) WITH KEY vgbel = wa_htap-vbeln vgpos = wa_htap-posnr BINARY SEARCH.
          IF sy-subrc = 0 AND nxht-zxishu NE 0.
            zxqk-htm = zxqk-htl / nxht-zxishu.
            zxqk-zxm = zxqk-zxl / nxht-zxishu.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    APPEND zxqk .
    READ TABLE it_ztsd225 INTO DATA(ztsd225) WITH KEY vbeln = wa_htap-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      kxqk-qdje    = kxqk-qdje    + wa_htap-kzwi1.
      kxqk-htyfkje = kxqk-htyfkje + wa_htap-kzwi1 * wa_htap-zgpdjbl / 100.
    ENDIF.
  ENDLOOP.

  "期初补差
*  KXQK-JSJE = KXQK-JSJE - QCBCJE.
  kxqk-bcml = kxqk-bcml - qcbcje * '0.59'.

  SELECT
     *
    INTO TABLE @DATA(it_ztfi213f)
    FROM ztfi_213f
    WHERE posid = @pspid
    .
  LOOP AT it_ztfi213f INTO DATA(ztfi213f).
    kxqk-zsr = kxqk-zsr + ztfi213f-zsr.
    kxqk-zcb = kxqk-zcb + ztfi213f-zcb.
  ENDLOOP.
  kxqk-cwml  = kxqk-zsr  - kxqk-zcb.


  "原材余量
  SELECT
*     EBAN~BANFN,
*     EBAN~BNFPO,
*     EBAN~MATNR,
*     EBAN~MENGE,
*     MARA~ZZL1
      ebkn~ps_psp_pnr AS pspnr,
      eban~banfn,
      eban~bnfpo,
      eban~matnr,
      eban~menge,
      eban~bmein,
      eban~erdat,
      eban~bsart
    FROM eban
    INNER JOIN ebkn             ON ebkn~banfn = eban~banfn AND ebkn~bnfpo = eban~bnfpo
*    INNER JOIN MARA             ON MARA~MATNR = EBAN~MATNR
    INNER JOIN @it_prps AS prps ON prps~wbsnm = ebkn~ps_psp_pnr
    WHERE eban~loekz NE 'X'
     AND  eban~frgkz EQ 'R'
     AND  eban~statu EQ 'B'
    ORDER BY eban~matnr
    INTO TABLE @DATA(it_eban)
     .
*delete hanwq 20230906 外协
*  SELECT
*      MSEG~MBLNR,
*      MSEG~MJAHR,
*      MSEG~ZEILE,
*      MSEG~BWART,
*      MSEG~MENGE,
*      MSEG~MATNR,
*      MSEG~WERKS,
*      MSEG~CHARG
*    FROM MSEG
*    INNER JOIN EKPO             ON MSEG~EBELN = EKPO~EBELN AND MSEG~EBELP = EKPO~EBELP
**    INNER JOIN EKKO             ON EKKO~EBELN = EKPO~EBELN
*    INNER JOIN @IT_EBAN AS EBAN ON EBAN~BANFN = EKPO~BANFN AND EBAN~BNFPO = EKPO~BNFPO
*    WHERE MSEG~BWART IN ( '541' , '542' )
*     AND  MSEG~XAUTO NE 'X' "541 542 都是两条 取一条做数量累计
*     AND  EBAN~BSART = 'Z07'
**     AND  NOT EXISTS ( SELECT * FROM  M_MBMPS   "未被冲销
**                       WHERE M_MBMPS~SJAHR = MSEG~GJAHR AND
**                             M_MBMPS~SMBLN = MSEG~BELNR AND
**                             M_MBMPS~SMBLP = MSEG~BUZEI )
*    ORDER BY MSEG~MATNR
*    INTO TABLE @DATA(IT_541)
*      .

  DELETE it_eban WHERE bsart = 'Z07'.

  SELECT DISTINCT
     mara~matnr,
     mara~meins,
     mara~zzl1
    FROM mara
    INNER JOIN @it_eban AS eban ON eban~matnr = mara~matnr
    ORDER BY mara~matnr
    INTO TABLE @DATA(it_mara)
     .
  REFRESH it_matnr.
  LOOP AT it_mara INTO DATA(is_mara).
    it_matnr-matnr    = is_mara-matnr.
    COLLECT: it_matnr.
  ENDLOOP.
  "取001属性
  PERFORM get001 IN PROGRAM zpubform TABLES it_matnr outtab001 USING 'ZTCZL'.
  SORT outtab001 BY matnr atnam.
  "单位换算逻辑取值
  SELECT
      marm~matnr,
      marm~meinh,
      marm~umrez,
      marm~umren
    FROM marm
    INNER JOIN @it_mara AS mara ON mara~matnr = marm~matnr
    ORDER BY marm~matnr,marm~meinh
    INTO TABLE @DATA(it_marm)
     .

  "采购订单单价
  SELECT
      ekpo~ebeln,
      ekpo~ebelp,
      prcd_elements~knumv,
      prcd_elements~kposn,
      prcd_elements~stunr,
      prcd_elements~zaehk,
      prcd_elements~kschl,
      prcd_elements~kbetr,
      prcd_elements~kwert,
      prcd_elements~kpein
    FROM ekko
    INNER JOIN ekpo             ON ekpo~ebeln = ekko~ebeln
    INNER JOIN @it_eban AS eban ON eban~banfn = ekpo~banfn AND eban~bnfpo = ekpo~bnfpo
    INNER JOIN ztmmsdhh         ON ztmmsdhh~ebelp = ekpo~ebelp
    INNER JOIN prcd_elements    ON prcd_elements~knumv = ekko~knumv AND ztmmsdhh~posnr = prcd_elements~kposn
    WHERE prcd_elements~kschl = 'PBXX'
     AND  prcd_elements~kinak = ''
     AND  ekpo~pstyp          NE '3'
    ORDER BY ekpo~ebeln , ekpo~ebelp
    INTO TABLE @DATA(it_pojg)
      .

  "采购订单101
  SELECT
      ekpo~banfn,
      ekpo~bnfpo,
      ekpo~ebeln,
      ekpo~ebelp,
      ekbe~gjahr,
      ekbe~belnr,
      ekbe~buzei,
*      EKBE~MENGE,
      ekbe~bamng AS menge,
      ekbe~bwart,
      ekbe~matnr,
      ekbe~werks,
      ekbe~charg,
      ekbe~budat,
      eban~pspnr
    FROM ekbe
    INNER JOIN ekpo             ON ekbe~ebeln = ekpo~ebeln AND ekbe~ebelp = ekpo~ebelp
    INNER JOIN @it_eban AS eban ON eban~banfn = ekpo~banfn AND eban~bnfpo = ekpo~bnfpo
    WHERE ekbe~bwart IN ( '101' )
     AND  NOT EXISTS ( SELECT * FROM  m_mbmps   "未被冲销
                       WHERE m_mbmps~sjahr = ekbe~gjahr AND
                             m_mbmps~smbln = ekbe~belnr AND
                             m_mbmps~smblp = ekbe~buzei )
    ORDER BY ekpo~banfn , ekpo~bnfpo , ekpo~ebeln , ekpo~ebelp
    INTO TABLE @DATA(it_ekbe)
      .
  SELECT
      ztmm249~ebeln,
      ztmm249~ebelp,
      mseg~mblnr,
      mseg~mjahr,
      mseg~zeile,
      mseg~matnr,
      mseg~werks,
      mseg~charg,
      mseg~menge,
      mseg543~mblnr AS mblnr543,
      mseg543~mjahr AS mjahr543,
      mseg543~zeile AS zeile543,
      mseg543~matnr AS matnr543,
      mseg543~werks AS werks543,
      mseg543~charg AS charg543,
      mseg543~menge AS menge543,
      ztmm249~type
    FROM ztmm249
    INNER JOIN mseg     AS mseg    ON mseg~mblnr = ztmm249~mblnr       AND mseg~mjahr = ztmm249~mjahr       AND mseg~zeile = ztmm249~zeile
    INNER JOIN mseg     AS mseg543 ON mseg543~mblnr = ztmm249~mblnr543 AND mseg543~mjahr = ztmm249~mjahr543 AND mseg543~zeile = ztmm249~zeile543
    INNER JOIN @it_ekbe AS ekbe    ON ekbe~matnr    = mseg543~matnr    AND ekbe~werks = mseg543~werks       AND ekbe~charg = mseg543~charg
    WHERE mseg~ps_psp_pnr       IN @s_pspnr  "外协出库控制必须是本项目
     AND  mseg543~ps_psp_pnr    IN @s_pspnr  "外协入库控制必须是本项目
     AND  NOT EXISTS ( SELECT * FROM  m_mbmps   "未被冲销
                                WHERE m_mbmps~sjahr = mseg~mjahr AND
                                      m_mbmps~smbln = mseg~mblnr AND
                                      m_mbmps~smblp = mseg~zeile )
    ORDER BY mseg543~matnr,mseg543~werks,mseg543~charg,ztmm249~type
    INTO TABLE @DATA(it_wcjg)
      .

  CLEAR:it_543[],it_101[].
  LOOP AT it_wcjg INTO DATA(wcjg).
    CLEAR:it_543,it_101.
    CASE wcjg-type.
      WHEN '101'.
        it_543-matnr = wcjg-matnr543.
        it_543-werks = wcjg-werks543.
        it_543-charg = wcjg-charg543.
        it_543-menge101 = wcjg-menge.
        it_543-menge543 = wcjg-menge543.
        COLLECT it_543.

        it_101-matnr    = wcjg-matnr543.
        it_101-werks    = wcjg-werks543.
        it_101-charg    = wcjg-charg543.
        it_101-menge101 = wcjg-menge543."101的总出库量
        COLLECT it_101.

      WHEN '544'.
        it_543-matnr = wcjg-matnr543.
        it_543-werks = wcjg-werks543.
        it_543-charg = wcjg-charg543.
        it_543-menge101 = 0.
        it_543-menge543 = wcjg-menge * '-1'.
        COLLECT it_543.
    ENDCASE.

    CLEAR:lt_mspr.
    lt_mspr-matnr = wcjg-matnr.
    lt_mspr-charg = wcjg-charg.
    COLLECT lt_mspr.

    IF wcjg-matnr(3) = 'E02'.
      CLEAR:it_wxpc.
      it_wxpc-matnr = wcjg-matnr.
      it_wxpc-charg = wcjg-charg.
      it_wxpc-werks = wcjg-werks.
      COLLECT:it_wxpc.
      CLEAR:it_wxpc.
      it_wxpc-matnr = wcjg-matnr543.
      it_wxpc-charg = wcjg-charg543.
      it_wxpc-werks = wcjg-werks543.
      COLLECT:it_wxpc.
    ENDIF.

  ENDLOOP.

  SORT it_wxpc BY matnr werks charg.
  SORT it_543 BY matnr werks charg.
  SORT it_101 BY matnr werks charg.

  SELECT
      ekkn~ps_psp_pnr AS pspnr,
      ekpo~ebeln,
      ekpo~ebelp,
      ekbe~gjahr,
      ekbe~belnr,
      ekbe~buzei,
      ekbe~matnr,
      ekbe~werks,
      ekbe~charg,
*      EKBE~MENGE,
      ekbe~bamng AS menge
    FROM ekko
    INNER JOIN ekpo             ON ekpo~ebeln = ekko~ebeln
    INNER JOIN ekbe             ON ekbe~ebeln = ekpo~ebeln AND ekbe~ebelp = ekpo~ebelp
    INNER JOIN ekkn             ON ekkn~ebeln = ekpo~ebeln AND ekkn~ebelp = ekpo~ebelp
    INNER JOIN @it_prps AS prps ON prps~wbsnm = ekkn~ps_psp_pnr
    WHERE ekbe~bwart IN ( '161' )
     AND  NOT EXISTS ( SELECT * FROM  m_mbmps   "未被冲销
                       WHERE m_mbmps~sjahr = ekbe~gjahr AND
                             m_mbmps~smbln = ekbe~belnr AND
                             m_mbmps~smblp = ekbe~buzei )
    ORDER BY ekbe~matnr
    INTO TABLE @DATA(it_cgth)
     .


  "供应商调拨回厂
*  SELECT
*     ZTMM220~ZDBDH,
*     ZTMM221~ZHH,
*     PRPS~WBSNM AS PSPNR,
*     EKBE~EBELN,
*     EKBE~EBELP,
*     EKBE~ZEKKN,
*     EKBE~VGABE,
*     EKBE~GJAHR,
*     EKBE~BELNR,
*     EKBE~BUZEI,
*     EKBE~MATNR,
*     EKBE~WERKS,
*     EKBE~CHARG
*    FROM ZTMM220
*    INNER JOIN ZTMM221          ON ZTMM221~ZDBDH = ZTMM220~ZDBDH
*    INNER JOIN @IT_PRPS AS PRPS ON PRPS~WBSWM    = ZTMM221~ZXMH
**    INNER JOIN ZTMM220          ON ZTMM220~EBELN = EKPO~EBELN
*    INNER JOIN EKBE             ON EKBE~GJAHR    = ZTMM220~MJAHR101 AND EKBE~BELNR = ZTMM220~MBLNR101
*    WHERE NOT EXISTS ( SELECT * FROM  M_MBMPS   "未被冲销
*                       WHERE M_MBMPS~SJAHR = EKBE~GJAHR AND
*                             M_MBMPS~SMBLN = EKBE~BELNR AND
*                             M_MBMPS~SMBLP = EKBE~BUZEI )
*    ORDER BY EKBE~EBELP
*    INTO TABLE @DATA(IT_ZTMM220)
*     .
  "MODIFY HANWQ 20230920  zentao ID 27408
  SELECT
     ztmm220~zdbdh,
     ztmm221~zhh,
     prps~wbsnm AS pspnr,
     mseg~ebeln,
     mseg~ebelp,
     mseg~mblnr,
     mseg~mjahr,
     mseg~zeile,
     mseg~matnr,
     mseg~werks,
     mseg~charg
    FROM ztmm220
    INNER JOIN ztmm221          ON ztmm221~zdbdh = ztmm220~zdbdh
    INNER JOIN @it_prps AS prps ON prps~wbswm    = ztmm221~zxmh
*    INNER JOIN ZTMM220          ON ZTMM220~EBELN = EKPO~EBELN
    INNER JOIN mseg             ON mseg~mjahr    = ztmm220~mjahr101 AND mseg~mblnr = ztmm220~mblnr101
    WHERE mseg~ps_psp_pnr       IN @s_pspnr
     AND  ztmm220~zdblx         = 'GYSHC'
     AND  NOT EXISTS ( SELECT * FROM  m_mbmps   "未被冲销
                       WHERE m_mbmps~sjahr = mseg~mjahr AND
                             m_mbmps~smbln = mseg~mblnr AND
                             m_mbmps~smblp = mseg~zeile )
    ORDER BY mseg~matnr,mseg~werks,mseg~charg
    INTO TABLE @DATA(it_ztmm220)
     .


  CLEAR:lt_gjpc[].
  LOOP AT it_ekbe INTO DATA(ekbe).
*    CLEAR:LT_MCHA.
*    LT_MCHA-MATNR = EKBE-MATNR.
*    LT_MCHA-WERKS = EKBE-WERKS.
*    LT_MCHA-CHARG = EKBE-CHARG.
*    COLLECT LT_MCHA.

    IF ekbe-matnr(3) = 'E02'.
      CLEAR:lt_gjpc.
      lt_gjpc-matnr = ekbe-matnr.
      lt_gjpc-werks = ekbe-werks.
      lt_gjpc-charg = ekbe-charg.
      COLLECT lt_gjpc.
    ENDIF.

*    IF EKBE-CHARG IS INITIAL.
*      CLEAR:LT_MSSQ.
*      LT_MSSQ-MATNR = EKBE-MATNR.
**      LT_MSSQ-PSPNR = EKBE-PSPNR.
*      LT_MSSQ-WERKS = EKBE-WERKS.
**      LT_MSSQ-SOBKZ = 'Q'.
*      COLLECT LT_MSSQ.
*    ELSE.
    CLEAR:lt_mspr.
    lt_mspr-matnr = ekbe-matnr.
*      LT_MSPR-PSPNR = EKBE-PSPNR.
*      LT_MSPR-WERKS = EKBE-WERKS.
    lt_mspr-charg = ekbe-charg.
*      LT_MSPR-SOBKZ = 'Q' .
    COLLECT lt_mspr.
*    ENDIF.
  ENDLOOP.
*
  IF lt_gjpc[] IS NOT INITIAL.
    SELECT
         aufm~werks,
         aufm~matnr,
         aufm~charg,
         aufm~aufnr
      INTO TABLE @DATA(it_zjgd)
      FROM aufm
      FOR ALL ENTRIES IN @lt_gjpc
      WHERE aufm~werks = @lt_gjpc-werks
       AND  aufm~matnr = @lt_gjpc-matnr
       AND  aufm~charg = @lt_gjpc-charg
       AND  aufm~bwart = '261'
       AND NOT EXISTS ( SELECT * FROM m_mbmps   "未被冲销
                                WHERE m_mbmps~sjahr = aufm~mjahr AND
                                      m_mbmps~smbln = aufm~mblnr AND
                                      m_mbmps~smblp = aufm~zeile )
        .
    IF it_zjgd IS NOT INITIAL AND sy-subrc = 0.
      SELECT
         aufm~aufnr,
         aufm~werks,
         aufm~matnr,
         aufm~charg,
         aufm~ps_psp_pnr
      INTO TABLE @DATA(it_zjcc)
      FROM aufm
      FOR ALL ENTRIES IN @it_zjgd
      WHERE aufm~aufnr = @it_zjgd-aufnr
       AND  aufm~bwart = '101'
       AND  aufm~charg LIKE 'ZJ%'
       AND NOT EXISTS ( SELECT * FROM m_mbmps   "未被冲销
                                WHERE m_mbmps~sjahr = aufm~mjahr AND
                                      m_mbmps~smbln = aufm~mblnr AND
                                      m_mbmps~smblp = aufm~zeile )
        .
      LOOP AT it_zjcc INTO DATA(zjcc).
        CLEAR:lt_mspr.
        lt_mspr-matnr = zjcc-matnr.
*        LT_MSPR-WERKS = ZJCC-WERKS.
        lt_mspr-charg = zjcc-charg.
        COLLECT lt_mspr.

*        CLEAR:LT_MCHA.
*        LT_MCHA-MATNR = ZJCC-MATNR.
*        LT_MCHA-WERKS = ZJCC-WERKS.
*        LT_MCHA-CHARG = ZJCC-CHARG.
*        COLLECT LT_MCHA.

      ENDLOOP.
    ENDIF.
  ENDIF.


  LOOP AT it_ztmm220 INTO DATA(ztmm220).
*    CLEAR:LT_MCHA.
*    LT_MCHA-MATNR = ZTMM220-MATNR.
*    LT_MCHA-WERKS = ZTMM220-WERKS.
*    LT_MCHA-CHARG = ZTMM220-CHARG.
*    COLLECT LT_MCHA.

*    IF ZTMM220-CHARG IS INITIAL.
*      CLEAR:LT_MSSQ.
*      LT_MSSQ-MATNR = ZTMM220-MATNR.
**      LT_MSSQ-PSPNR = ZTMM220-PSPNR.
*      LT_MSSQ-WERKS = ZTMM220-WERKS.
**      LT_MSSQ-SOBKZ = 'Q'.
*      COLLECT LT_MSSQ.
*    ELSE.
    CLEAR:lt_mspr.
    lt_mspr-matnr = ztmm220-matnr.
*      LT_MSPR-PSPNR = ZTMM220-PSPNR.
*      LT_MSPR-WERKS = ZTMM220-WERKS.
    lt_mspr-charg = ztmm220-charg.
*      LT_MSPR-SOBKZ = 'Q'.
    COLLECT lt_mspr.
*    ENDIF.
  ENDLOOP.

*  IF LT_MSSQ[] IS NOT INITIAL.
*    SELECT
*      MSSQ~MATNR,
*      MSSQ~PSPNR,
*      MSSQ~WERKS,
*      MSSQ~SOBKZ,
*      MSSQ~SQLAB
*      INTO TABLE @DATA(IT_MSSQ)
*      FROM MSSQ
*      FOR ALL ENTRIES IN @LT_MSSQ
*      WHERE MSSQ~MATNR = @LT_MSSQ-MATNR
**       AND  MSSQ~PSPNR = @LT_MSSQ-PSPNR
*       AND  MSSQ~WERKS = @LT_MSSQ-WERKS
**       AND  MSSQ~SOBKZ = 'Q'
*       .
*    SORT IT_MSSQ BY MATNR WERKS.
*    SELECT
*      MARD~MATNR,
*      MARD~WERKS,
*      MARD~LGORT,
*      MARD~LABST
*      INTO TABLE @DATA(IT_MARD)
*      FROM MARD
*      FOR ALL ENTRIES IN @LT_MSSQ
*      WHERE MARD~MATNR = @LT_MSSQ-MATNR
*       AND  MARD~WERKS = @LT_MSSQ-WERKS
*       .
*    SORT IT_MARD BY MATNR WERKS.
*  ENDIF.

  IF lt_mspr[] IS NOT INITIAL.
    SELECT
      mspr~matnr,
      mspr~werks,
      mspr~lgort,
      mspr~charg,
      mspr~sobkz,
      mspr~pspnr,
      mspr~prlab,
      mspr~prspe
      INTO TABLE @DATA(it_mspr)
      FROM mspr
      FOR ALL ENTRIES IN @lt_mspr
      WHERE mspr~matnr = @lt_mspr-matnr
*       AND  MSPR~PSPNR = @LT_MSPR-PSPNR
*       AND  MSPR~WERKS = @LT_MSPR-WERKS
       AND  mspr~charg = @lt_mspr-charg
*       AND  MSPR~SOBKZ = 'Q'
      AND ( mspr~prlab NE 0 OR mspr~prspe NE 0 )
      .
    SORT it_mspr BY matnr charg."WERKS
    SELECT
       mchb~matnr,
       mchb~werks,
       mchb~lgort,
       mchb~charg,
       mchb~clabs,
       mchb~cumlm
      INTO TABLE @DATA(it_mchb)
      FROM mchb
      FOR ALL ENTRIES IN @lt_mspr
      WHERE mchb~matnr = @lt_mspr-matnr
*       AND  MCHB~WERKS = @LT_MSPR-WERKS
       AND  mchb~charg = @lt_mspr-charg
       AND ( mchb~clabs NE 0 OR mchb~cumlm NE 0 )
       .
    SORT it_mchb BY matnr charg." WERKS

    SELECT
      msrd~matnr,
      msrd~werks,
      msrd~charg,
      msrd~sobkz,
      msrd~lifnr,
      msrd~pspnr,
      msrd~rdlab
    INTO TABLE @DATA(it_msrd)
    FROM msrd
    FOR ALL ENTRIES IN @lt_mspr
    WHERE msrd~matnr = @lt_mspr-matnr
     AND  msrd~charg = @lt_mspr-charg
     AND  msrd~rdlab NE 0
      .
    SORT it_msrd BY matnr charg.

    SELECT
       mslb~matnr,
       mslb~werks,
       mslb~charg,
       mslb~sobkz,
       mslb~lifnr,
       mslb~lblab
      INTO TABLE @DATA(it_mslb)
      FROM mslb
      FOR ALL ENTRIES IN @lt_mspr
      WHERE mslb~matnr = @lt_mspr-matnr
       AND  mslb~charg = @lt_mspr-charg
       AND  mslb~lblab NE 0
        .
    SORT it_mslb BY matnr charg.

  ENDIF.

*  IF LT_MCHA[] IS NOT INITIAL.
*
*    SELECT
*       MSEG~MBLNR,
*       MSEG~MJAHR,
*       MSEG~ZEILE,
*       MSEG~MATNR,
*       MSEG~WERKS,
*       MSEG~CHARG,
*       MSEG~MENGE,
*       MSEG~SHKZG,
*       MSEG~PS_PSP_PNR
*
*      INTO TABLE @DATA(IT_MSEG)
*      FROM MSEG
*      INNER JOIN MARA ON MARA~MATNR = MSEG~MATNR
*      FOR ALL ENTRIES IN @LT_MCHA
*      WHERE MSEG~MATNR = @LT_MCHA-MATNR
*       AND  MSEG~WERKS = @LT_MCHA-WERKS
*       AND  MSEG~CHARG = @LT_MCHA-CHARG
*       AND  MARA~MATKL IN @S_JUANMATKL
*       AND  MSEG~BWART = '411'
*       AND  MSEG~PS_PSP_PNR NOT IN @S_PSPNR"只取非本项目
*       AND  NOT EXISTS (  SELECT * FROM  M_MBMPS   "未被冲销
*                          WHERE M_MBMPS~SJAHR = MSEG~MJAHR AND
*                                M_MBMPS~SMBLN = MSEG~MBLNR AND
*                                M_MBMPS~SMBLP = MSEG~ZEILE  )
*         .
*
*    SORT IT_MSEG BY MATNR WERKS CHARG.
*
*  ENDIF.


  "生产工单 261
  SELECT
      aufm~*
    FROM aufm
    INNER JOIN afpo             ON afpo~aufnr = aufm~aufnr
    INNER JOIN mara             ON mara~matnr = aufm~matnr
    INNER JOIN @it_prps AS prps ON prps~wbsnm = afpo~projn
    WHERE aufm~bwart IN ( '261','262' )
*     AND  AUFM~CHARG NOT LIKE 'ZJ%'
     AND ( aufm~aufnr NOT LIKE 'ZJ%' AND  aufm~aufnr NOT LIKE 'PC%-001'  ) "排除 纵剪
     AND  mara~matkl NOT IN @s_matkl
*     AND  NOT EXISTS ( SELECT * FROM  M_MBMPS   "未被冲销
*                       WHERE M_MBMPS~SJAHR = AUFM~MJAHR AND
*                             M_MBMPS~SMBLN = AUFM~MBLNR AND
*                             M_MBMPS~SMBLP = AUFM~ZEILE )
    ORDER BY aufm~matnr,aufm~werks,aufm~charg
    INTO  TABLE @DATA(it_aufm)
      .
  " 通过OCP接口获取复核投料数据  2024-03-28 14:41:34 by lmw
  ##itab_db_select
  SELECT aufnr
  FROM @it_aufm AS a
  GROUP BY aufnr
  INTO TABLE @DATA(gt_aufnr).
  CALL FUNCTION 'ZFM_GP_OCP_PP_GETGDFH'
    TABLES
      in_tab  = gt_aufnr
      out_tab = gt_fhtl.
  IF gt_fhtl[] IS NOT INITIAL.
    DATA:wa_aufm LIKE LINE OF it_aufm.
    LOOP AT gt_fhtl.
      CLEAR wa_aufm.
      wa_aufm-werks = gt_fhtl-werks.
      wa_aufm-aufnr = gt_fhtl-aufnr.
      wa_aufm-matnr = gt_fhtl-matnr.
      wa_aufm-charg = gt_fhtl-charg.
      wa_aufm-meins = gt_fhtl-zjbdw.
      wa_aufm-menge = gt_fhtl-zfhsl.
      wa_aufm-bwart = '261'.
      APPEND wa_aufm TO it_aufm.
    ENDLOOP.
  ENDIF.
  SORT it_aufm BY matnr werks charg.
  DATA(it_aufm_261) = it_aufm.

  CLEAR:it_zcbs[],ot_zcbs[],wgbg_xh_matnr.
*折弯件料号
  PERFORM getdata(zpub_data) USING 'WGBG_XH_MATNR' CHANGING wgbg_xh_matnr.
  SPLIT wgbg_xh_matnr AT ';' INTO TABLE DATA(list).
  IF list IS NOT INITIAL.
    LOOP AT list INTO DATA(wa_list).
      s_xhmatnr(3) = 'IEQ'.
      s_xhmatnr-low = wa_list.
      COLLECT s_xhmatnr.
    ENDLOOP.
    MOVE-CORRESPONDING it_lips[] TO it_zcbs[].
    DELETE it_zcbs WHERE matnr NOT IN s_xhmatnr.
    CALL FUNCTION 'ZFMS_05_GETPCTX'
      EXPORTING
        atnam  = 'Z76'
      TABLES
        intab  = it_zcbs
        outtab = ot_zcbs.
    DELETE ot_zcbs WHERE atwrt IS INITIAL.
    SORT ot_zcbs BY matnr charg.
  ENDIF.

  MOVE-CORRESPONDING it_mchb[] TO it_kl[].
  MOVE-CORRESPONDING it_mspr[] TO it_kl[] KEEPING TARGET LINES.

  CALL FUNCTION 'ZFMS_05_GETPCTX'
    EXPORTING
      atnam  = 'Z38'
    TABLES
      intab  = it_kl
      outtab = ot_kl.
  DELETE ot_kl WHERE atwrt IS INITIAL.
  SORT ot_kl BY matnr charg.


  SORT it_lips BY matnr.

  SORT it_zjgd BY matnr werks charg.
  SORT it_zjcc BY aufnr.

  LOOP AT it_mara INTO DATA(mara).
    CLEAR ycyl.
    ycyl-matnr = mara-matnr.
    ycyl-zzl1  = mara-zzl1.

    READ TABLE it_eban INTO DATA(eban) WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_eban INTO eban FROM sy-tabix.
        IF eban-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        IF ( eban-erdat < jqqk-sccgrq OR jqqk-sccgrq IS INITIAL ) AND eban-erdat IS NOT INITIAL.
          jqqk-sccgrq = eban-erdat.
        ENDIF.
*        IF EBAN-BSART NE 'Z07'.
        IF eban-bmein NE mara-meins.
          READ TABLE it_marm INTO DATA(marm) WITH KEY matnr = mara-matnr meinh = eban-bmein BINARY SEARCH.
          IF sy-subrc = 0 .
            ycyl-cgl = ycyl-cgl + eban-menge * marm-umrez / marm-umren .
          ELSE.
            ycyl-cgl = ycyl-cgl + eban-menge.
          ENDIF.
        ELSE.
          ycyl-cgl = ycyl-cgl + eban-menge.
        ENDIF.
*        ENDIF.
        READ TABLE it_ekbe INTO ekbe WITH KEY banfn = eban-banfn bnfpo = eban-bnfpo bwart = '101' BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT it_ekbe INTO ekbe FROM sy-tabix.
            IF ekbe-banfn NE eban-banfn OR ekbe-bnfpo NE eban-bnfpo OR ekbe-bwart NE '101'.
              EXIT.
            ENDIF.
            IF ( ekbe-budat < jqqk-scycdhrq OR jqqk-scycdhrq IS INITIAL ) AND ekbe-budat IS NOT INITIAL.
              jqqk-scycdhrq = ekbe-budat.
            ENDIF.
            ycyl-dhl = ycyl-dhl + ekbe-menge.

            READ TABLE it_zjgd INTO DATA(zjgd) WITH KEY matnr = ekbe-matnr werks = ekbe-werks charg = ekbe-charg BINARY SEARCH.
            IF sy-subrc = 0.
              CLEAR lv_sjshengyl.
              LOOP AT it_zjgd INTO zjgd FROM sy-tabix.
                IF zjgd-matnr NE ekbe-matnr OR zjgd-werks NE ekbe-werks OR zjgd-charg NE ekbe-charg.
                  EXIT.
                ENDIF.
                READ TABLE it_zjcc INTO zjcc WITH KEY aufnr = zjgd-aufnr BINARY SEARCH.
                IF sy-subrc = 0.
                  LOOP AT it_zjcc INTO zjcc FROM sy-tabix.
                    IF zjcc-aufnr NE zjgd-aufnr.
                      EXIT.
                    ENDIF.

                    READ TABLE it_mspr INTO DATA(mspr) WITH KEY matnr = zjcc-matnr charg = zjcc-charg BINARY SEARCH."WERKS = ZJCC-WERKS
                    IF sy-subrc = 0.
                      LOOP AT it_mspr INTO mspr FROM sy-tabix.
                        IF mspr-matnr NE zjcc-matnr OR  mspr-charg NE zjcc-charg."MSPR-WERKS NE ZJCC-WERKS OR
                          EXIT.
                        ENDIF.
                        ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mspr-prlab + mspr-prspe.
                        lv_sjshengyl =  lv_sjshengyl + mspr-prlab + mspr-prspe.

                        READ TABLE ot_kl WITH KEY matnr = zjcc-matnr charg = zjcc-charg BINARY SEARCH.
                        IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
                          datum = ot_kl-atwrt.
                          kl = sy-datum - datum.
                          IF kl > ycyl-kl .
                            ycyl-kl = kl.
                          ENDIF.
                        ENDIF.
                      ENDLOOP.
                    ENDIF.

                    READ TABLE it_mchb INTO DATA(mchb) WITH KEY matnr = zjcc-matnr charg = zjcc-charg BINARY SEARCH."WERKS = ZJCC-WERKS
                    IF sy-subrc = 0.
                      LOOP AT it_mchb INTO mchb FROM sy-tabix.
                        IF mchb-matnr NE zjcc-matnr OR mchb-charg NE zjcc-charg."OR MCHB-WERKS NE ZJCC-WERKS
                          EXIT.
                        ENDIF.
                        ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mchb-clabs + mchb-cumlm.
                        lv_sjshengyl =  lv_sjshengyl + mchb-clabs + mchb-cumlm.
                        READ TABLE ot_kl WITH KEY matnr = zjcc-matnr charg = zjcc-charg BINARY SEARCH.
                        IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
                          datum = ot_kl-atwrt.
                          kl = sy-datum - datum.
                          IF kl > ycyl-kl .
                            ycyl-kl = kl.
                          ENDIF.
                        ENDIF.
                      ENDLOOP.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDLOOP.
              IF lv_sjshengyl > 0.
                PERFORM appendz215 USING ekbe-werks pspid ycyl-matnr ekbe-charg lv_sjshengyl.
              ENDIF.
            ENDIF.

*            READ TABLE IT_MSEG INTO DATA(MSEG) WITH KEY MATNR = EKBE-MATNR WERKS = EKBE-WERKS CHARG = EKBE-CHARG BINARY SEARCH.
*            IF SY-SUBRC = 0.
*              LOOP AT IT_MSEG INTO MSEG FROM SY-TABIX.
*                IF MSEG-MATNR NE EKBE-MATNR OR MSEG-WERKS NE EKBE-WERKS OR MSEG-CHARG NE EKBE-CHARG.
*                  EXIT.
*                ENDIF.
*                CASE MSEG-SHKZG.
*                    "S 移入 H 移出
*                  WHEN 'S'.
*                    YCYL-SJSHENGYL411 = YCYL-SJSHENGYL411 - MSEG-MENGE.
*                  WHEN 'H'.
*                    YCYL-SJSHENGYL411 = YCYL-SJSHENGYL411 + MSEG-MENGE.
*                ENDCASE.
*
*              ENDLOOP.
*            ENDIF.


          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "到货量 - 采购退货
    READ TABLE it_cgth INTO DATA(cgth) WITH KEY matnr = ycyl-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_cgth INTO cgth FROM sy-tabix.
        IF cgth-matnr NE ycyl-matnr.
          EXIT.
        ENDIF.
        ycyl-dhl = ycyl-dhl - cgth-menge.
      ENDLOOP.
    ENDIF.


    READ TABLE it_aufm INTO DATA(aufm) WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_aufm INTO aufm FROM sy-tabix.
        tabix = sy-tabix.
        IF aufm-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        IF aufm-charg(2) = 'ZJ'.
          CONTINUE.
        ENDIF.

        IF aufm-bwart EQ '261'.
          ycyl-syl = ycyl-syl + aufm-menge.
        ELSEIF aufm-bwart EQ '262'.
          ycyl-syl = ycyl-syl - aufm-menge.
        ENDIF.

        aufm-charg = 'DELETE'.
        MODIFY it_aufm FROM aufm INDEX tabix TRANSPORTING charg.

      ENDLOOP.

    ENDIF.

*    READ TABLE IT_541 INTO DATA(WA_541) WITH KEY MATNR = MARA-MATNR BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      LOOP AT IT_541 INTO WA_541 FROM SY-TABIX.
*        IF WA_541-MATNR NE MARA-MATNR.
*          EXIT.
*        ENDIF.
*        CASE WA_541-BWART.
*          WHEN '541' .
*            YCYL-SYL = YCYL-SYL + WA_541-MENGE.
*          WHEN '542' .
*            YCYL-SYL = YCYL-SYL - WA_541-MENGE.
*          WHEN OTHERS.
*        ENDCASE.
*      ENDLOOP.
*    ENDIF.

    READ TABLE it_543 WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_543 FROM sy-tabix.
        IF it_543-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        IF it_543-menge101 EQ 0 OR it_543-menge543 EQ 0.
          CONTINUE.
        ENDIF.
        READ TABLE it_wcjg INTO wcjg WITH KEY matnr543 = it_543-matnr werks543 = it_543-werks charg543 = it_543-charg BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT it_wcjg INTO wcjg FROM sy-tabix.
            IF wcjg-matnr543 NE it_543-matnr OR wcjg-werks543 NE it_543-werks OR wcjg-charg543 NE it_543-charg.
              EXIT.
            ENDIF.

*    ENDLOOP.
*            "库存量 外协 计算
*            READ TABLE IT_MCHB INTO MCHB WITH KEY MATNR = WCJG-MATNR CHARG = WCJG-CHARG BINARY SEARCH.
*            IF SY-SUBRC = 0.
*              LOOP AT IT_MCHB INTO MCHB FROM SY-TABIX.
*                IF MCHB-MATNR NE WCJG-MATNR OR MCHB-CHARG NE WCJG-CHARG .
*                  EXIT.
*                ENDIF.
*                YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MCHB-CLABS + MCHB-CUMLM.
*              ENDLOOP.
*            ENDIF.
*            READ TABLE IT_MSPR INTO MSPR WITH KEY MATNR = WCJG-MATNR CHARG = WCJG-CHARG BINARY SEARCH.
*            IF SY-SUBRC = 0.
*              LOOP AT IT_MSPR INTO MSPR FROM SY-TABIX.
*                IF MSPR-MATNR NE WCJG-MATNR OR MSPR-CHARG NE WCJG-CHARG.
*                  EXIT.
*                ENDIF.
*                YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MSPR-PRLAB + MSPR-PRSPE.
*              ENDLOOP.
*            ENDIF.
*
*            READ TABLE IT_MCHB INTO MCHB WITH KEY MATNR = WCJG-MATNR543 CHARG = WCJG-CHARG543 BINARY SEARCH.
*            IF SY-SUBRC = 0.
*              LOOP AT IT_MCHB INTO MCHB FROM SY-TABIX.
*                IF MCHB-MATNR NE WCJG-MATNR543 OR MCHB-CHARG NE WCJG-CHARG543 .
*                  EXIT.
*                ENDIF.
*                YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MCHB-CLABS + MCHB-CUMLM.
*              ENDLOOP.
*            ENDIF.
*
*            READ TABLE IT_MSPR INTO MSPR WITH KEY MATNR = WCJG-MATNR543 CHARG = WCJG-CHARG543 BINARY SEARCH.
*            IF SY-SUBRC = 0.
*              LOOP AT IT_MSPR INTO MSPR FROM SY-TABIX.
*                IF MSPR-MATNR NE WCJG-MATNR543 OR MSPR-CHARG NE WCJG-CHARG543.
*                  EXIT.
*                ENDIF.
*                YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MSPR-PRLAB + MSPR-PRSPE.
*              ENDLOOP.
*            ENDIF.
            IF wcjg-type NE '101'.
              CONTINUE.
            ENDIF.

            IF wcjg-matnr(3) = 'E02'.
              READ TABLE it_aufm INTO aufm WITH KEY matnr = wcjg-matnr werks = wcjg-werks charg = wcjg-charg." BINARY SEARCH.
              IF sy-subrc = 0.
                LOOP AT it_aufm INTO aufm FROM sy-tabix.
                  tabix = sy-tabix.
                  IF aufm-matnr NE wcjg-matnr OR aufm-werks NE wcjg-werks OR aufm-charg NE wcjg-charg.
                    EXIT.
                  ENDIF.
                  IF aufm-bwart EQ '261'.
                    ycyl-syl = ycyl-syl + aufm-menge *  it_543-menge543 / it_543-menge101 .
                  ELSEIF aufm-bwart EQ '262'.
                    ycyl-syl = ycyl-syl - aufm-menge *  it_543-menge543 / it_543-menge101 .
                  ENDIF.
                  aufm-charg = 'DELETE'.
                  MODIFY it_aufm FROM aufm INDEX tabix TRANSPORTING charg.
                ENDLOOP.
              ENDIF.
            ELSE.
              "MODIFY HANWQ 20240422
              "委外原材拉回来成品的原材消耗消耗量计算的时候按成品的产量比例分摊
*              YCYL-SYL = YCYL-SYL +  IT_543-MENGE543.
              READ TABLE it_101 WITH KEY matnr = wcjg-matnr543 werks = wcjg-werks543 charg = wcjg-charg543 BINARY SEARCH.
              IF sy-subrc = 0 AND it_101-menge101 NE 0.
                "101 对应发库量 * 发货量 - 退卷量 / 总发货量
                ycyl-syl = ycyl-syl +  wcjg-menge543 * it_543-menge543 / it_101-menge101 .
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDLOOP.
    ENDIF.
    "原材余量 使用量 = 生产工单261 + 出库 20230422
    READ TABLE it_lips INTO DATA(lips) WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_lips INTO lips FROM sy-tabix.
        IF lips-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        IF lips-matnr IN s_xhmatnr AND s_xhmatnr[] IS NOT INITIAL.
          READ TABLE ot_zcbs WITH KEY matnr = lips-matnr charg = lips-charg BINARY SEARCH.
          IF sy-subrc = 0 AND ot_zcbs-atwrt IS NOT INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.

        CASE lips-vbtyp.
          WHEN 'J'.
            ycyl-syl = ycyl-syl + lips-lgmng.
          WHEN 'T'.
            ycyl-syl = ycyl-syl - lips-lgmng.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.

*    YCYL-SHENGYL = YCYL-SYL - YCYL-DHL.
*    YCYL-DHPC    = YCYL-DHL - YCYL-CGL.
*    YCYL-SYPC    = YCYL-SYL - YCYL-CGL.

*    READ TABLE IT_MSSQ INTO DATA(MSSQ) WITH KEY MATNR = MARA-MATNR BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      LOOP AT IT_MSSQ INTO MSSQ FROM SY-TABIX.
*        IF MSSQ-MATNR NE MARA-MATNR.
*          EXIT.
*        ENDIF.
*        YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MSSQ-SQLAB.
*      ENDLOOP.
*    ENDIF.

*    READ TABLE IT_MARD INTO DATA(MARD) WITH KEY MATNR = MARA-MATNR BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      LOOP AT IT_MARD INTO MARD FROM SY-TABIX.
*        IF MARD-MATNR NE MARA-MATNR.
*          EXIT.
*        ENDIF.
*        YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MARD-LABST.
*      ENDLOOP.
*    ENDIF.

    READ TABLE it_mspr INTO mspr WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_mspr INTO mspr FROM sy-tabix.
        CLEAR lv_sjshengyl.
        IF mspr-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        IF mspr-charg(2) = 'ZJ'.
          CONTINUE.
        ENDIF.
        READ TABLE it_wxpc WITH KEY matnr = mspr-matnr charg = mspr-charg BINARY SEARCH.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
        ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mspr-prlab + mspr-prspe.
        lv_sjshengyl =  mspr-prlab + mspr-prspe.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING mspr-werks pspid ycyl-matnr mspr-charg lv_sjshengyl.
        ENDIF.
        READ TABLE ot_kl WITH KEY matnr = mspr-matnr charg = mspr-charg BINARY SEARCH.
        IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
          datum = ot_kl-atwrt.
          kl = sy-datum - datum.
          IF kl > ycyl-kl .
            ycyl-kl = kl.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE it_mchb INTO mchb WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_mchb INTO mchb FROM sy-tabix.
        CLEAR lv_sjshengyl.
        IF mchb-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        IF mchb-charg(2) = 'ZJ'.
          CONTINUE.
        ENDIF.
        READ TABLE it_wxpc WITH KEY matnr = mchb-matnr charg = mchb-charg BINARY SEARCH.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
        ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mchb-clabs + mchb-cumlm.
        lv_sjshengyl =  mchb-clabs + mchb-cumlm.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING mchb-werks pspid ycyl-matnr mchb-charg lv_sjshengyl.
        ENDIF.
        READ TABLE ot_kl WITH KEY matnr = mchb-matnr charg = mchb-charg BINARY SEARCH.
        IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
          datum = ot_kl-atwrt.
          kl = sy-datum - datum.
          IF kl > ycyl-kl .
            ycyl-kl = kl.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.


    READ TABLE it_msrd INTO DATA(msrd) WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_msrd INTO msrd FROM sy-tabix.
        CLEAR lv_sjshengyl.
        IF msrd-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        READ TABLE it_wxpc WITH KEY matnr = msrd-matnr charg = msrd-charg BINARY SEARCH.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
        ycyl-sjshengylkcl =  ycyl-sjshengylkcl + msrd-rdlab.
        lv_sjshengyl =  msrd-rdlab.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING msrd-werks pspid ycyl-matnr msrd-charg lv_sjshengyl.
        ENDIF.
        READ TABLE ot_kl WITH KEY matnr = msrd-matnr charg = msrd-charg BINARY SEARCH.
        IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
          datum = ot_kl-atwrt.
          kl = sy-datum - datum.
          IF kl > ycyl-kl .
            ycyl-kl = kl.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE it_mslb INTO DATA(mslb) WITH KEY matnr = mara-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_mslb INTO mslb FROM sy-tabix.
        CLEAR lv_sjshengyl.
        IF mslb-matnr NE mara-matnr.
          EXIT.
        ENDIF.
        READ TABLE it_wxpc WITH KEY matnr = mslb-matnr charg = mslb-charg BINARY SEARCH.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
        ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mslb-lblab.
        lv_sjshengyl =  mslb-lblab.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING mslb-werks pspid ycyl-matnr mslb-charg lv_sjshengyl.
        ENDIF.
        READ TABLE ot_kl WITH KEY matnr = mslb-matnr charg = mslb-charg BINARY SEARCH.
        IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
          datum = ot_kl-atwrt.
          kl = sy-datum - datum.
          IF kl > ycyl-kl .
            ycyl-kl = kl.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " 增加涂层种类打折比例  2024-10-08 16:39:07 by lmw
    READ TABLE outtab001 WITH KEY matnr = mara-matnr atnam = 'ZTCZL' BINARY SEARCH.
    IF sy-subrc = 0.
      ycyl-ztczl = outtab001-atwrt.
      READ TABLE gt_ztfi214 WITH KEY ztczl = ycyl-ztczl BINARY SEARCH.
      IF sy-subrc = 0.
        ycyl-zdzbl = gt_ztfi214-zdzbl.
      ENDIF.
    ENDIF.

*    LOOP AT IT_WXPC INTO DATA(WXPC).
*      READ TABLE IT_MCHB INTO MCHB WITH KEY MATNR = WXPC-MATNR CHARG = WXPC-CHARG BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MCHB-CLABS + MCHB-CUMLM.
*      ENDIF.
*      READ TABLE IT_MSPR INTO MSPR WITH KEY MATNR = WXPC-MATNR CHARG = WXPC-CHARG BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        YCYL-SJSHENGYLKCL =  YCYL-SJSHENGYLKCL + MSPR-PRLAB + MSPR-PRSPE.
*      ENDIF.
*    ENDLOOP.



    ycyl-type = 'CG'.
    APPEND ycyl.

    CLEAR s_matnr.
    s_matnr-sign = 'I'.
    s_matnr-option = 'EQ'.
    s_matnr-low = mara-matnr.
    COLLECT s_matnr.
  ENDLOOP.

  SORT ycyl BY matnr type.

  "排除已汇总过的
*  IF S_MATNR[] IS NOT INITIAL.
*    DELETE IT_AUFM WHERE MATNR IN S_MATNR.
*  ENDIF.
  DELETE it_aufm WHERE charg = 'DELETE'.

  IF it_aufm[] IS NOT INITIAL.

    "纵剪 取 投入产出比
    LOOP AT it_aufm INTO aufm.
      IF aufm-charg(2) = 'ZJ'.
        CLEAR:it_mcha.
        it_mcha-matnr = aufm-matnr.
        it_mcha-werks = aufm-werks.
        it_mcha-charg = aufm-charg.
        COLLECT it_mcha.
      ENDIF.
    ENDLOOP.
    IF it_mcha[] IS NOT INITIAL.
      SELECT DISTINCT
         aufnr
        FROM aufm
        INTO TABLE @DATA(it_aufnr)
        FOR ALL ENTRIES IN @it_mcha
        WHERE matnr = @it_mcha-matnr
         AND  werks = @it_mcha-werks
         AND  charg = @it_mcha-charg
         AND  bwart = '101'
         .

      SORT it_aufnr BY aufnr.

      SELECT
         aufm~mjahr,
         aufm~mblnr,
         aufm~zeile,
         aufm~aufnr,
         aufm~matnr,
         aufm~charg,
         aufm~menge,
         aufm~bwart
        FROM aufm
        INNER JOIN @it_aufnr AS aufnr ON aufnr~aufnr = aufm~aufnr
        WHERE aufm~bwart IN ( '261' , '101' , '262' , '102' )
*         AND  AUFM~MATNR LIKE 'E02%' "纵剪只取卷 的投料和产出
         AND  aufm~matnr LIKE 'E%' "纵剪工单投/产出物料可通过判断E*开头物料排除投入的保护膜物料
*         AND  NOT EXISTS ( SELECT * FROM  M_MBMPS   "未被冲销
*                           WHERE M_MBMPS~SJAHR = AUFM~MJAHR AND
*                                 M_MBMPS~SMBLN = AUFM~MBLNR AND
*                                 M_MBMPS~SMBLP = AUFM~ZEILE )
        ORDER BY aufm~aufnr
        INTO TABLE @DATA(it_aufm1)
           .
      LOOP AT it_aufnr INTO DATA(aufnr1).
        CLEAR : it_afko.
        it_afko-aufnr = aufnr1-aufnr.
        READ TABLE it_aufm1 INTO DATA(aufm1) WITH KEY aufnr = it_afko-aufnr BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT it_aufm1 INTO aufm1 FROM sy-tabix .
            IF aufm1-aufnr NE it_afko-aufnr .
              EXIT.
            ENDIF.
            CASE aufm1-bwart.
              WHEN '101'.
                it_afko-101 = it_afko-101 + aufm1-menge.
                it_afko-matnr_101 = aufm1-matnr.
              WHEN '102'.
                it_afko-101 = it_afko-101 - aufm1-menge.
*                IT_AFKO-MATNR_101 = AUFM1-MATNR.
              WHEN '261'.
                it_afko-261 = it_afko-261 + aufm1-menge.
                it_afko-matnr_261 = aufm1-matnr.
              WHEN '262'.
                it_afko-261 = it_afko-261 - aufm1-menge.
*                IT_AFKO-MATNR_261 = AUFM1-MATNR.
            ENDCASE.
          ENDLOOP.
          IF it_afko-101 NE 0.
            it_afko-bl = it_afko-261 / it_afko-101.
          ENDIF.
          APPEND it_afko.
        ENDIF.

      ENDLOOP.

      SORT it_afko BY matnr_261.
      SORT it_aufm BY matnr.
      SORT it_aufm1 BY aufnr charg bwart.

      LOOP AT ycyl.
        READ TABLE it_afko WITH KEY matnr_261 = ycyl-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT it_afko FROM sy-tabix .
            IF it_afko-matnr_261 NE ycyl-matnr.
              EXIT.
            ENDIF.
            READ TABLE it_aufm INTO aufm WITH KEY matnr = it_afko-matnr_101 BINARY SEARCH.
            IF sy-subrc = 0.
              LOOP AT it_aufm INTO aufm FROM sy-tabix.
                IF aufm-matnr NE it_afko-matnr_101.
                  EXIT.
                ENDIF.
                tabix = sy-tabix.
                READ TABLE it_aufm1 INTO aufm1 WITH KEY aufnr = it_afko-aufnr charg = aufm-charg bwart = '101' BINARY SEARCH.
                IF sy-subrc = 0.
                  IF aufm-bwart EQ '261'.
                    ycyl-syl = ycyl-syl + aufm-menge * it_afko-bl.
                  ELSEIF aufm-bwart EQ '262'.
                    ycyl-syl = ycyl-syl - aufm-menge * it_afko-bl.
                  ENDIF.
                  aufm-charg = 'DELETE'.
                  MODIFY it_aufm FROM aufm INDEX tabix TRANSPORTING charg.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          MODIFY ycyl.
        ENDIF.
      ENDLOOP.
      DELETE it_aufm WHERE charg = 'DELETE'.
    ENDIF.
  ENDIF.

  "排除已汇总过的
*  DELETE IT_AUFM WHERE MATNR IN S_MATNR.
*  CLEAR:IT_MARA.
*
*  IF IT_AUFM IS NOT INITIAL.
*
*    LOOP AT IT_AUFM INTO AUFM.
*      IT_MATNR-MATNR = AUFM-MATNR.
*      COLLECT IT_MATNR.
*      IT_AFKO-AUFNR = AUFM-AUFNR.
*      COLLECT IT_AFKO.
*    ENDLOOP.
*    "取 投入产出比
*    SELECT
*       AUFM~*
*      INTO TABLE @DATA(IT_AUFM1)
*      FROM AUFM
*      FOR ALL ENTRIES IN @IT_AFKO
*      WHERE AUFNR      = @IT_AFKO-AUFNR
*       AND  AUFM~BWART IN ( '261' , '101' )
*       AND  NOT EXISTS ( SELECT * FROM  M_MBMPS   "未被冲销
*                         WHERE M_MBMPS~SJAHR = AUFM~MJAHR AND
*                               M_MBMPS~SMBLN = AUFM~MBLNR AND
*                               M_MBMPS~SMBLP = AUFM~ZEILE )
*         .
*    SORT IT_AFKO BY AUFNR.
*
*    LOOP AT IT_AFKO.
*      READ TABLE IT_AUFM1 INTO DATA(AUFM1) WITH KEY AUFNR = IT_AFKO-AUFNR BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        LOOP AT IT_AUFM1 INTO AUFM1 FROM SY-TABIX .
*          IF AUFM1-AUFNR NE IT_AFKO-AUFNR .
*            EXIT.
*          ENDIF.
*          CASE AUFM1-BWART.
*            WHEN '101'.
*              IT_AFKO-101 = IT_AFKO-101 + AUFM1-MENGE.
*            WHEN '261'.
*              IT_AFKO-261 = IT_AFKO-261 + AUFM1-MENGE.
*          ENDCASE.
*        ENDLOOP.
*        IF IT_AFKO-101 NE 0.
*          IT_AFKO-BL = IT_AFKO-261 / IT_AFKO-101.
*        ENDIF.
*        MODIFY IT_AFKO.
*      ENDIF.
*      CLEAR : IT_AFKO , AUFM1.
*    ENDLOOP.
*
*    SELECT
*      MARA~MATNR,
*      MARA~ZZL1
*     INTO TABLE @IT_MARA
*     FROM MARA
*     FOR ALL ENTRIES IN @IT_MATNR
*     WHERE MATNR = @IT_MATNR-MATNR
*      .
*    SORT IT_MARA  BY MATNR.
*    SORT IT_AFKO  BY AUFNR.
*    SORT IT_EKBE  BY MATNR WERKS CHARG .
*    SORT IT_AUFM1 BY MATNR WERKS CHARG BWART.
*    LOOP AT IT_AUFM INTO AUFM.
*      READ TABLE IT_AFKO WITH KEY AUFNR = AUFM-AUFNR BINARY SEARCH.
*      IF SY-SUBRC = 0 AND AUFM-AUFNR+11(1) = ''.
*        READ TABLE IT_AUFM1 INTO AUFM1 WITH KEY MATNR = AUFM-MATNR WERKS = AUFM-WERKS CHARG = AUFM-CHARG BWART = '101' BINARY SEARCH .
*        IF SY-SUBRC = 0.
*          READ TABLE IT_AUFM1 INTO DATA(AUFM2) WITH KEY MATNR = AUFM1-MATNR WERKS = AUFM1-WERKS CHARG = AUFM1-CHARG BWART = '261' BINARY SEARCH .
*          IF SY-SUBRC = 0.
*            READ TABLE IT_EKBE INTO EKBE WITH KEY MATNR = AUFM2-MATNR WERKS = AUFM2-WERKS CHARG = AUFM2-CHARG  BINARY SEARCH.
*            IF SY-SUBRC = 0.
*              READ TABLE YCYL WITH KEY MATNR = EKBE-MATNR BINARY SEARCH.
*              IF SY-SUBRC = 0.
*                YCYL-SYL     = YCYL-SYL + AUFM-MENGE * IT_AFKO-BL.
*                MODIFY YCYL INDEX SY-TABIX TRANSPORTING SYL.
*              ELSE.
**                ????
*              ENDIF.
*            ELSE.
*              READ TABLE YCYL WITH KEY MATNR = EKBE-MATNR BINARY SEARCH.
*              IF SY-SUBRC = 0.
*                YCYL-SYL     = YCYL-SYL + AUFM-MENGE * IT_AFKO-BL.
*                MODIFY YCYL INDEX SY-TABIX TRANSPORTING SYL.
*              ELSE.
*                YCYL-MATNR = MARA-MATNR.
*                YCYL-ZZL1  = MARA-ZZL1.
*                YCYL-SYL   = YCYL-SYL + AUFM-MENGE * IT_AFKO-BL.
*                APPEND YCYL.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
*
*      ENDIF.
*    ENDLOOP.
*
*    SELECT DISTINCT
*      MARA~MATNR,
*      MARA~ZZL1
*     INTO TABLE @IT_MARA
*     FROM MARA
*     FOR ALL ENTRIES IN @IT_AUFM
*     WHERE MATNR = @IT_AUFM-MATNR
*      .
*    SORT IT_MARA  BY MATNR.
*    LOOP AT IT_AUFM INTO AUFM.
*
*      READ TABLE YCYL WITH KEY MATNR = AUFM-MATNR TYPE = '' .
*      IF SY-SUBRC = 0.
*        YCYL-SYL     = YCYL-SYL + AUFM-MENGE.
*        MODIFY YCYL INDEX SY-TABIX TRANSPORTING SYL.
*      ELSE.
*        CLEAR YCYL.
*        READ TABLE IT_MARA INTO MARA WITH KEY MATNR = AUFM-MATNR BINARY SEARCH.
*        YCYL-MATNR = MARA-MATNR.
*        YCYL-ZZL1  = MARA-ZZL1.
*        YCYL-SYL   = YCYL-SYL + AUFM-MENGE.
*        APPEND YCYL.
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.


  CALL FUNCTION 'ZFMS_05_GETPCTX'
    EXPORTING
      atnam  = 'Z21,Z112,Z76'
    TABLES
      intab  = intab
      outtab = outtab.
  DELETE outtab WHERE atwrt IS INITIAL.
  SORT outtab BY matnr charg atnam.

  CLEAR it_pcxx[].
  LOOP AT intab .
    CLEAR it_pcxx.
    READ TABLE outtab INTO DATA(outtab1) WITH KEY matnr = intab-matnr charg = intab-charg atnam = 'Z21' BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE outtab INTO DATA(outtab2) WITH KEY matnr = intab-matnr charg = intab-charg atnam = 'Z112' BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE outtab INTO DATA(outtab3) WITH KEY matnr = intab-matnr charg = intab-charg atnam = 'Z76' BINARY SEARCH.
        IF sy-subrc = 0 AND outtab3-atwrt = 'X'.
          it_pcxx-matnr   = intab-matnr.
          it_pcxx-charg   = intab-charg.
          it_pcxx-zpcdh   = outtab1-atwrt.
          it_pcxx-zpcdhh  = outtab2-atwrt.
          COLLECT it_pcxx.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE it_pcxx WHERE matnr IS INITIAL OR charg IS INITIAL OR zpcdh IS INITIAL OR zpcdhh IS INITIAL.

  IF it_pcxx[] IS NOT INITIAL.

    SORT it_pcxx BY zpcdh zpcdhh.

    SELECT
       ztpp_205a~zpcdh,
       ztpp_205a~zpcdhh,
       ztpp_205a~zmlj,
       ztpp_205a~zyf
      INTO TABLE @DATA(it_205)
      FROM  ztpp_205a
      INNER JOIN ztpp_205  ON ztpp_205~zpcdh  = ztpp_205a~zpcdh
      FOR ALL ENTRIES IN @it_pcxx
      WHERE  ztpp_205a~zpcdh  = @it_pcxx-zpcdh
         AND ztpp_205a~zpcdhh = @it_pcxx-zpcdhh
         AND ztpp_205~zpclx   NE 'WHT'
           .
    SORT it_205 BY zpcdh zpcdhh.
    SORT it_pcxx BY matnr charg.

*    SORT IT_LIPS BY MATNR CHARG.
*    LOOP AT IT_205 INTO DATA(ZTPP205).
*      READ TABLE IT_PCXX WITH KEY ZPCDH = ZTPP205-ZPCDH ZPCDHH = ZTPP205-ZPCDHH BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        CLEAR:LFIMG.
*        LOOP AT IT_PCXX FROM SY-TABIX.
*          IF IT_PCXX-ZPCDH NE ZTPP205-ZPCDH OR IT_PCXX-ZPCDHH NE ZTPP205-ZPCDHH.
*            EXIT.
*          ENDIF.
*          READ TABLE IT_LIPS INTO WA_LIPS WITH KEY MATNR = IT_PCXX-MATNR CHARG = IT_PCXX-CHARG BINARY SEARCH.
*          IF SY-SUBRC = 0.
*            LOOP AT IT_LIPS INTO WA_LIPS FROM SY-TABIX.
*              IF WA_LIPS-MATNR NE IT_PCXX-MATNR OR WA_LIPS-CHARG NE IT_PCXX-CHARG.
*                EXIT.
*              ENDIF.
*            ENDLOOP.
*            LFIMG = WA_LIPS-LFIMG.
*          ENDIF.
*        ENDLOOP.
*        KXQK-KHML = KXQK-KHML + ( ZTPP205-ZMLJ + ZTPP205-ZYUNF * '0.59' ) * LFIMG.
*      ENDIF.
*    ENDLOOP.

*KXQK-KHML
    LOOP AT it_lips INTO wa_lips.
      CASE wa_lips-vbtyp.
        WHEN 'J'.

          READ TABLE it_pcxx WITH KEY matnr = wa_lips-matnr charg = wa_lips-charg BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE it_205 INTO DATA(ztpp205) WITH KEY zpcdh = it_pcxx-zpcdh zpcdhh = it_pcxx-zpcdhh  BINARY SEARCH.
            IF sy-subrc = 0.
              kxqk-zckhml = kxqk-zckhml + ztpp205-zmlj * wa_lips-lfimg.
              kxqk-zcyfml = kxqk-zcyfml + ztpp205-zyf  * '0.59'  * wa_lips-lfimg.
            ENDIF.
          ELSE.
            READ TABLE it_ekbe INTO ekbe WITH KEY matnr = wa_lips-matnr charg = wa_lips-charg BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE it_pojg INTO DATA(pojg) WITH KEY ebeln = ekbe-ebeln ebelp = ekbe-ebelp BINARY SEARCH.
              IF sy-subrc = 0.
                IF pojg-kpein <> 0  .
                  kxqk-wgml = kxqk-wgml - ( pojg-kbetr / pojg-kpein ) *  '0.59' *  '1.03' * wa_lips-lfimg .
                ELSE.
                  kxqk-wgml = kxqk-wgml - pojg-kbetr  *  '0.59' *  '1.03' * wa_lips-lfimg .
                ENDIF.
              ENDIF.

              READ TABLE it_vbrp INTO wa_vbrp WITH KEY vgbel = wa_lips-vbeln vgpos = wa_lips-posnr BINARY SEARCH.
              IF sy-subrc EQ 0.
                LOOP AT it_vbrp INTO wa_vbrp FROM sy-tabix.
                  IF wa_vbrp-vgbel NE wa_lips-vbeln OR wa_vbrp-vgpos NE wa_lips-posnr.
                    EXIT.
                  ENDIF.
                  kxqk-wgml = kxqk-wgml + wa_vbrp-kzwi1 * '0.59' .
                ENDLOOP.
              ENDIF.

            ENDIF.

          ENDIF.

        WHEN 'T'.

          READ TABLE it_pcxx WITH KEY matnr = wa_lips-matnr charg = wa_lips-charg BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE it_205 INTO ztpp205 WITH KEY zpcdh = it_pcxx-zpcdh zpcdhh = it_pcxx-zpcdhh  BINARY SEARCH.
            IF sy-subrc = 0.
*              KXQK-THZCML = KXQK-THZCML + ( ZTPP205-ZMLJ + ZTPP205-ZYF * '0.59' ) * WA_LIPS-LFIMG.
              kxqk-thzckhml = kxqk-thzckhml + ztpp205-zmlj * wa_lips-lfimg.
              kxqk-thzcyfml = kxqk-thzcyfml + ztpp205-zyf  * '0.59'  * wa_lips-lfimg.
            ENDIF.
          ELSE.
            READ TABLE it_ekbe INTO ekbe WITH KEY matnr = wa_lips-matnr charg = wa_lips-charg BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE it_pojg INTO pojg WITH KEY ebeln = ekbe-ebeln ebelp = ekbe-ebelp BINARY SEARCH.
              IF sy-subrc = 0.
                IF pojg-kpein <> 0  .
                  kxqk-thwgml = kxqk-thwgml - ( pojg-kbetr / pojg-kpein ) *  '0.59' *  '1.03' * wa_lips-lfimg .
                ELSE.
                  kxqk-thwgml = kxqk-thwgml - pojg-kbetr  *  '0.59' *  '1.03' * wa_lips-lfimg .
                ENDIF.
              ENDIF.

              READ TABLE it_vbrp INTO wa_vbrp WITH KEY vgbel = wa_lips-vbeln vgpos = wa_lips-posnr BINARY SEARCH.
              IF sy-subrc EQ 0.
                LOOP AT it_vbrp INTO wa_vbrp FROM sy-tabix.
                  IF wa_vbrp-vgbel NE wa_lips-vbeln OR wa_vbrp-vgpos NE wa_lips-posnr.
                    EXIT.
                  ENDIF.
                  kxqk-thwgml = kxqk-thwgml + wa_vbrp-kzwi1 * '0.59' .
                ENDLOOP.
              ENDIF.

            ENDIF.

          ENDIF.

        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

  ENDIF.

  SORT it_543 BY matnr werks charg.
  SORT it_wcjg BY matnr543 matnr werks charg type.

  LOOP AT ycyl.

    tdname = ycyl-matnr.
    PERFORM getlongtext(zpubform) USING 'GRUN' tdname 'MATERIAL' CHANGING ycyl-guige     .

    LOOP AT it_wxpc INTO DATA(wxpc).
*      READ TABLE IT_543 INTO DATA(A543) WITH KEY MATNR = YCYL-MATNR BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        LOOP AT IT_543 INTO A543 FROM SY-TABIX.
*          IF A543-MATNR NE YCYL-MATNR.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
      READ TABLE it_543 WITH KEY matnr = wxpc-matnr werks = wxpc-werks charg = wxpc-charg BINARY SEARCH.
      IF sy-subrc = 0 AND it_543-matnr = ycyl-matnr.
      ELSE.
        READ TABLE it_wcjg INTO wcjg WITH KEY matnr543 = ycyl-matnr matnr = wxpc-matnr werks = wxpc-werks charg = wxpc-charg BINARY SEARCH.
        IF sy-subrc = 0.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR lv_sjshengyl.
      READ TABLE it_mchb INTO mchb WITH KEY matnr = wxpc-matnr charg = wxpc-charg BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT it_mchb INTO mchb FROM sy-tabix.
          IF mchb-matnr NE wxpc-matnr OR mchb-charg NE wxpc-charg.
            EXIT.
          ENDIF.
          ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mchb-clabs + mchb-cumlm.
          lv_sjshengyl =  lv_sjshengyl + mchb-clabs + mchb-cumlm.
          READ TABLE ot_kl WITH KEY matnr = wxpc-matnr charg = wxpc-charg BINARY SEARCH.
          IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
            datum = ot_kl-atwrt.
            kl = sy-datum - datum.
            IF kl > ycyl-kl .
              ycyl-kl = kl.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING wxpc-werks pspid ycyl-matnr wxpc-charg lv_sjshengyl.
        ENDIF.
      ENDIF.
      CLEAR lv_sjshengyl.
      READ TABLE it_mspr INTO mspr WITH KEY matnr = wxpc-matnr charg = wxpc-charg BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT it_mspr INTO mspr FROM sy-tabix.
          IF mspr-matnr NE wxpc-matnr OR mspr-charg NE wxpc-charg.
            EXIT.
          ENDIF.
          ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mspr-prlab + mspr-prspe.
          lv_sjshengyl =  lv_sjshengyl + mspr-prlab + mspr-prspe.
          READ TABLE ot_kl WITH KEY matnr = wxpc-matnr charg = wxpc-charg BINARY SEARCH.
          IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
            datum = ot_kl-atwrt.
            kl = sy-datum - datum.
            IF kl > ycyl-kl .
              ycyl-kl = kl.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING wxpc-werks pspid ycyl-matnr wxpc-charg lv_sjshengyl.
        ENDIF.
      ENDIF.


      CLEAR lv_sjshengyl.
      READ TABLE it_msrd INTO msrd WITH KEY matnr = wxpc-matnr charg = wxpc-charg BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT it_msrd INTO msrd FROM sy-tabix.
          IF msrd-matnr NE wxpc-matnr OR msrd-charg NE wxpc-charg.
            EXIT.
          ENDIF.
          ycyl-sjshengylkcl =  ycyl-sjshengylkcl + msrd-rdlab.
          lv_sjshengyl =  lv_sjshengyl + msrd-rdlab.
          READ TABLE ot_kl WITH KEY matnr = msrd-matnr charg = msrd-charg BINARY SEARCH.
          IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
            datum = ot_kl-atwrt.
            kl = sy-datum - datum.
            IF kl > ycyl-kl .
              ycyl-kl = kl.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING wxpc-werks pspid ycyl-matnr wxpc-charg lv_sjshengyl.
        ENDIF.
      ENDIF.

      CLEAR lv_sjshengyl.
      READ TABLE it_mslb INTO mslb WITH KEY matnr = wxpc-matnr charg = wxpc-charg BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT it_mslb INTO mslb FROM sy-tabix.
          IF mslb-matnr NE wxpc-matnr OR mslb-charg NE wxpc-charg.
            EXIT.
          ENDIF.
          ycyl-sjshengylkcl =  ycyl-sjshengylkcl + mslb-lblab.
          lv_sjshengyl =  lv_sjshengyl + mslb-lblab.
          READ TABLE ot_kl WITH KEY matnr = mslb-matnr charg = mslb-charg BINARY SEARCH.
          IF sy-subrc = 0 AND ot_kl-atwrt NS '0000'.
            datum = ot_kl-atwrt.
            kl = sy-datum - datum.
            IF kl > ycyl-kl .
              ycyl-kl = kl.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_sjshengyl > 0.
          PERFORM appendz215 USING wxpc-werks pspid ycyl-matnr wxpc-charg lv_sjshengyl.
        ENDIF.
      ENDIF.
    ENDLOOP.


    ycyl-sjshengyl = ycyl-sjshengylkcl + ycyl-sjshengyl411.
    ycyl-shengyl   = ycyl-syl - ycyl-dhl.
    ycyl-dhpc      = ycyl-dhl - ycyl-cgl.
    ycyl-sypc      = ycyl-syl - ycyl-cgl.
    ycyl-llshengyl = ycyl-dhl - ycyl-syl.
    MODIFY ycyl.

  ENDLOOP.

  " 保存ztfi215  2024-10-09 15:30:51 by lmw
  LOOP AT gt_z215.
    READ TABLE outtab001 WITH KEY matnr = gt_z215-matnr atnam = 'ZTCZL' BINARY SEARCH.
    IF sy-subrc = 0.
      gt_z215-ztczl = outtab001-atwrt.
      READ TABLE in_tab WITH KEY matnr = gt_z215-matnr.
      IF sy-subrc = 0.
        gt_z215-zdzbl = in_tab-zdzbl.
      ENDIF.
      MODIFY gt_z215.
    ENDIF.
    READ TABLE it_mara INTO mara WITH KEY matnr = gt_z215-matnr.
    IF sy-subrc = 0.
      gt_z215-meins = mara-meins.
      MODIFY gt_z215.
    ENDIF.
  ENDLOOP.
  DELETE gt_z215 WHERE zdzbl IS INITIAL.
  PERFORM savezfi215 USING pspid CHANGING z215[].

  kxqk-yfml = kxqk-yf * '0.59' .

  kxqk-khml1 = kxqk-zckhml + kxqk-zcyfml + kxqk-wgml - kxqk-thzckhml - kxqk-thzcyfml - kxqk-thwgml - kxqk-yfml + kxqk-bcml.

  SELECT
     *
    INTO TABLE @DATA(it_ztsd232)
    FROM ztsd232
    WHERE pspid = @pspid
    .
  LOOP AT it_ztsd232 INTO DATA(ztsd232).
    kxqk-khml = kxqk-khml + ztsd232-zygmlje / ztsd232-zhtbbxs.
  ENDLOOP.

*  "排产单是否录入考核毛利
*  SELECT SINGLE
*      ZTPP205~ZPCDH
*    FROM @IT_205 AS ZTPP205
*    WHERE ZTPP205~ZMLJ IS INITIAL
*    INTO @DATA(ZPCDH)
*    .
*  IF SY-SUBRC = 0 AND ZPCDH IS NOT INITIAL.
*    IF KXQK-BYSFSC = '1'.
*      KXQK-BYSFSC = '3'.
*    ELSE.
*      KXQK-BYSFSC = '2'.
*    ENDIF.
*  ENDIF.

  "modify hanwq 20240402
  SELECT
     ztfi_payeedoc~chgno_d,
     ztfi_payeedoc~transdate,
     ztfi_payeedoc~transtime,
     ztfi_splitcont~posnr,
     ztfi_splitcont~zdjbh,
     ztfi_splitcont~zsign,
     ztfi_splitcont~cfdat,
     ztfi_splitcont~dmbtr
    FROM ztfi_splitcont
    LEFT JOIN ztfi_payeedoc ON ztfi_payeedoc~chgno_d = ztfi_splitcont~chgno_d
    WHERE  ztfi_splitcont~pspnr IN @s_pspnr
    AND ztfi_splitcont~zsign NE '0'
    INTO TABLE @DATA(it_splitcont)
     .
  LOOP AT it_splitcont ASSIGNING FIELD-SYMBOL(<fs>)  .
    IF <fs>-zdjbh(4) = 'ZQZY' AND <fs>-zsign NE '1'.
      <fs>-chgno_d = 'DELETE'.
    ELSEIF <fs>-zdjbh(4) = 'ZQZY'.
      <fs>-transdate = <fs>-cfdat.
    ENDIF.
  ENDLOOP.

  DELETE it_splitcont WHERE chgno_d = 'DELETE'.

  SORT it_splitcont BY transdate transtime.

*根据表ZTFI_SPLITCONT~PSPNR（WBS）取ZTFI_SPLITCONT~ZDJBH（4）= ZQZY  AND  ZTFI_SPLITCONT~ZSIGN = 1的 ZTFI_SPLITCONT~DMBTR（货币金额-转出+转入）
  LOOP AT it_splitcont INTO DATA(splitcont).
    IF sy-tabix = 1.
      IF splitcont-transdate IS NOT INITIAL.
        jqqk-djdzrq   = splitcont-transdate.
      ELSE.
*        JQQK-DJDZRQ   = SPLITCONT-CFDAT.
        "期初收款未取到银行流水日期的统一成20230101 HANWQ 20230817
        jqqk-djdzrq   = '20230101'.
      ENDIF.
      kxqk-sjyfkje  = splitcont-dmbtr.
    ENDIF.
    kxqk-ssje  = kxqk-ssje + splitcont-dmbtr.
  ENDLOOP.
*退款取值逻辑：根据WBS号和客户编码 查询表ZTFI_PYREHD，WBS=ZTFI_PYREHD-PSPID;
*客户编码=ZTFI_PYREHD-LIFNR，取ZPYCAT等于KH和KD的，并且凭证号BELNR不等于空的，付款金额取DMBTR_ACT

  IF kxqk-htyfkje = 0.
    jqqk-djdzrq = htsxrq.
    CLEAR:kxqk-sjyfkje.
  ENDIF.

  SELECT
     SUM( ztfi_pyrehd~dmbtr_pln )
    FROM ztfi_pyrehd
    INNER JOIN @it_prps AS prps ON prps~wbswm = ztfi_pyrehd~pspid
    WHERE ztfi_pyrehd~zprstus IN ( '20' , '28' )
      AND ztfi_pyrehd~zpycat  IN ( 'KD' , 'KY' , 'KH','KC' )
    INTO @DATA(dmbtr_pln)
      .
  kxqk-ssje = kxqk-ssje - dmbtr_pln.

  IF kxqk-jsje NE 0.
    bl = kxqk-cwml / kxqk-jsje * 113 .
    kxqk-mll = bl && '%'.
    bl = kxqk-ssje / kxqk-jsje * 100.
    kxqk-htskbl = bl && '%'.
  ENDIF.

  kxqk-yszk = kxqk-jsje - kxqk-ssje.

  IF kxqk-qdje NE 0.
    bl = kxqk-sjyfkje / kxqk-qdje * 100.
    kxqk-yfkdzbl = bl && '%'.
    bl = kxqk-jsje / kxqk-qdje * 100.
    kxqk-htjewcd = bl && '%'.
  ENDIF.

  jqqk-jhzqc = ( jqqk-mpjhrq - jqqk-spjhrq ) - vbak-zjrthwb.

  IF jqqk-djdzrq IS NOT INITIAL .
    jqqk-htydspjhrq = jqqk-djdzrq + vbak-zjrth .
    jqqk-htydjhwbrq = jqqk-djdzrq + vbak-zjrth  + vbak-zjrthwb.
    IF jqqk-spjhrq IS NOT INITIAL.
      jqqk-ycjhts     = jqqk-spjhrq - jqqk-htydspjhrq.
    ENDIF.
  ENDIF.

*  "当预付金额是0时
*  "实际预付款金额、 定金到账日期、 预付款到账比例、合同约定首批交货日期、合同约定交货完毕日期、交货周期差、延迟交货天数 置 0
*  IF KXQK-HTYFKJE = 0.
*    CLEAR: KXQK-SJYFKJE ,JQQK-DJDZRQ ,KXQK-YFKDZBL,JQQK-HTYDSPJHRQ,JQQK-HTYDJHWBRQ,JQQK-JHZQC,JQQK-YCJHTS.
*  ENDIF.

  "详图 、 预算 人员
  SELECT SINGLE
     zxt
     zxtnr
     zys
     zysnr
    INTO ( htqk-zxtms , htqk-zxt , htqk-zysms , htqk-zys )
    FROM ztps010a
    WHERE pspid = pspid
    .
*技术员、预算员字段取值优化
*1. 预算员 技术员 都有 ，各展示各的；
*2. 预算员 技术员 都没有 ， 都展示 '客户' 两个字
*3. 预算员 有 ，技术员 没有  技术员 填“无”
*4. 预算员 没有 ，技术员 有  预算员 填“无”
  IF htqk-zxtms IS INITIAL AND htqk-zysms IS INITIAL.
    htqk-zysms = '客户'.
    htqk-zxtms = '客户'.
  ELSEIF htqk-zxtms IS INITIAL AND htqk-zysms IS NOT INITIAL .
    htqk-zxtms = '无'.
  ELSEIF htqk-zxtms IS NOT INITIAL AND htqk-zysms IS INITIAL .
    htqk-zysms = '无'.
  ENDIF.

  "ADD HANWQ 20230803 考核毛利率
  IF kxqk-jsje NE 0 .
    bl = kxqk-khml / kxqk-jsje * 100.
    kxqk-khmll = bl && '%'.
  ENDIF.

  "ADD HANWQ 20230922 原材余量表增加展示非本项目采购的钢卷数据
  IF it_aufm[] IS NOT INITIAL.

    SELECT DISTINCT
       mara~matnr,
       mara~zzl1
      FROM mara
      INNER JOIN @it_aufm AS aufm ON aufm~matnr = mara~matnr
      WHERE mara~matnr LIKE 'E02%'
      ORDER BY mara~matnr
      INTO TABLE @DATA(it_aufm_matnr)
       .

    SORT it_aufm BY matnr.

    LOOP AT it_aufm_matnr INTO DATA(aufm_matnr).
      CLEAR:ycyl.
      ycyl-matnr = aufm_matnr-matnr.
      ycyl-zzl1  = aufm_matnr-zzl1.

      tdname = ycyl-matnr.
      PERFORM getlongtext(zpubform) USING 'GRUN' tdname 'MATERIAL' CHANGING ycyl-guige     .

      READ TABLE it_aufm INTO aufm WITH KEY matnr = aufm_matnr-matnr BINARY SEARCH.
      IF sy-subrc = 0 .
        LOOP AT it_aufm INTO aufm FROM sy-tabix.
          IF aufm-matnr NE aufm_matnr-matnr.
            EXIT.
          ENDIF.
          IF aufm-bwart EQ '261'.
            ycyl-syl = ycyl-syl + aufm-menge.
          ELSEIF aufm-bwart EQ '262'.
            ycyl-syl = ycyl-syl - aufm-menge.
          ENDIF.
        ENDLOOP.
      ENDIF.
      APPEND ycyl.
    ENDLOOP.
  ENDIF.

*不是本项目采购的卷拉出去委外的原材消耗的显示
  SELECT
    b~ebeln,
    b~ebelp
    FROM ekkn AS b
    INNER JOIN @it_prps AS a ON a~wbsnm = b~ps_psp_pnr
    GROUP BY b~ebeln,b~ebelp
    INTO TABLE @DATA(lt_ekkn)
    .

  SELECT FROM @lt_ekkn AS b
    INNER JOIN ekpo AS c ON c~ebeln = b~ebeln AND b~ebelp = c~ebelp
    INNER JOIN ekbe AS d ON c~ebeln = d~ebeln AND c~ebelp = d~ebelp AND d~bwart IN ( '543' , '544' )
    INNER JOIN mseg AS e ON e~matnr = d~matnr AND e~charg = d~charg AND ( e~bwart = '101' OR e~bwart = '561') AND e~werks = d~werks
    INNER JOIN mara AS f ON d~matnr = f~matnr
    FIELDS
    d~matnr,
    f~zzl1,
    SUM(
    CASE WHEN d~bwart = '543' THEN d~bamng
         WHEN d~bwart = '544' THEN 0 - d~bamng
    END ) AS syl
    WHERE c~pstyp = '3' AND ( c~matnr NOT LIKE 'E01%' OR c~matnr NOT LIKE 'E02%' ) AND c~loekz <> 'L'
    AND e~ps_psp_pnr NOT IN (  SELECT prps~pspnr
                               FROM prps
                               INNER JOIN proj ON proj~pspnr = prps~psphi
                               WHERE proj~pspid = @pspid   )
    GROUP BY d~matnr, f~zzl1
    INTO TABLE @DATA(lt_ekbe).
  LOOP AT lt_ekbe ASSIGNING FIELD-SYMBOL(<eb>).
    CLEAR:ycyl.
    MOVE-CORRESPONDING <eb> TO ycyl.
    tdname = ycyl-matnr.
    PERFORM getlongtext(zpubform) USING 'GRUN' tdname 'MATERIAL' CHANGING ycyl-guige     .
    APPEND ycyl.
  ENDLOOP.
  "标识 ： 0 正常 ；1 本月发货 或 本月生产 ； 2 排产单未录入考核毛利 ； 3 本月发货 或 本月生产 AND 排产单未录入考核毛利
  "标识 ： MODIFY HANWQ 20230809
  " 0 正常 ；
  " 1 本月发货 或 本月生产 ；
  " 2 最后一次发货日期所在月份未录入考核毛利 ；
  " 3.最后一次发货日期所在月份未录入财务毛利
  " 4.收款比例不等于1
  " 5.本月补差
  kxqk-bysfsc = '0'.


  " 5.判断本月是否补差
  SELECT SINGLE
     erdat
    FROM @it_qcbc AS qcbc
    WHERE erdat BETWEEN @ksrq AND @jsrq
    INTO @DATA(erdat)
     .
  IF sy-subrc = 0 AND erdat IS NOT INITIAL.
    kxqk-bysfsc = '5'.
  ENDIF.

  SELECT SINGLE
     erdat
    FROM @it_bcxx AS bcxx
    WHERE erdat BETWEEN @ksrq AND @jsrq
    INTO @DATA(erdat1)
     .
  IF sy-subrc = 0 AND erdat1 IS NOT INITIAL.
    kxqk-bysfsc = '5'.
  ENDIF.
  " 6.判断上月是否有补差
  IF sy-datum <= dysh.
    SELECT SINGLE
      erdat
      FROM @it_bcxx2 AS bcxx
      WHERE left( erdat , 6 ) = left( @sgy , 6 )
      INTO @DATA(erdat2).
    IF sy-subrc = 0 AND erdat2 IS NOT INITIAL.
      kxqk-bysfsc = '6'.
    ENDIF.
  ENDIF.
  "2、最后一次发货日期所在月份未录入考核毛利
  SELECT SINGLE
     yf
    FROM @it_ztsd232 AS ztsd232
    WHERE yf = @jqqk-mpjhrq(6)
    INTO @DATA(yf)
     .
  IF sy-subrc NE 0 OR yf IS INITIAL.
    kxqk-bysfsc = '2'.
  ENDIF.

  "3、最后一次发货日期所在月份未录入财务毛利
  DATA:gjahr TYPE ztfi_213f-gjahr.
  DATA:poper TYPE ztfi_213f-poper.
  CLEAR:gjahr,poper.
  gjahr = jqqk-mpjhrq+0(4).
  poper = jqqk-mpjhrq+4(2).

  SELECT SINGLE
     poper
    FROM @it_ztfi213f AS ztfi213f
    WHERE gjahr = @gjahr
     AND  poper = @poper
     INTO @DATA(poper1)
      .
  IF sy-subrc NE 0 OR poper1 IS INITIAL.
    kxqk-bysfsc = '3'.
  ENDIF.

  " 4.收款比例不等于1
  IF kxqk-htskbl NE '100.00%'.
    kxqk-bysfsc = '4'.
  ENDIF.

  "1、判断本月是否生产投料
  SELECT SINGLE
      aufm~aufnr
    FROM @it_aufm_261 AS aufm
    WHERE aufm~budat BETWEEN @ksrq AND @jsrq
    INTO @DATA(aufnr)
     .
  IF sy-subrc = 0 AND aufnr IS NOT INITIAL.
    kxqk-bysfsc = '1'.
  ENDIF.

  "1、判断本月是否发货过账
  SELECT SINGLE
     lips~vbeln
    FROM @it_lips AS lips
    WHERE lips~wadat_ist BETWEEN @ksrq AND @jsrq
    INTO @DATA(vbeln)
     .
  IF sy-subrc = 0 AND vbeln IS NOT INITIAL.
    kxqk-bysfsc = '1'.
  ENDIF.
  flag = 'Y'.
  message = '查询成功！'.

  zfmdatasave2 'R'.


ENDFUNCTION.
FORM appendz215 USING    p_werks TYPE werks_d
                         p_pspid TYPE ps_pspid
                         p_matnr TYPE matnr
                         p_charg TYPE charg_d
                         p_prlab TYPE labst
                         .
  CLEAR:gt_z215.
  gt_z215-werks = p_werks.
  gt_z215-pspid = p_pspid.
  gt_z215-matnr = p_matnr.
  gt_z215-charg = p_charg.
  gt_z215-prlab = p_prlab.
  COLLECT gt_z215.
ENDFORM.
**********************************************************************
*保存项目完工报告原材余卷
**********************************************************************
FORM savezfi215 USING    p_pspid TYPE ps_pspid
                CHANGING t_215 LIKE gt_z215[].
  DATA: p_lfgja TYPE  mchbh-lfgja,
        lfmon   TYPE  mchbh-lfmon.
  DATA:lv_ps_psp_pnr TYPE ps_psp_pnr.
  DATA intab TYPE TABLE OF  zsfi219 WITH HEADER LINE.
  DATA outtab   TYPE TABLE OF   zsfi220 WITH HEADER LINE.

  DELETE FROM ztfi215 WHERE pspid = p_pspid.
  p