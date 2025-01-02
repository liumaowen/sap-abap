*&---------------------------------------------------------------------*
*& 包含               ZPPD204_QTMX
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form getqtmx
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getqtmx.
  DATA:lv_zgdzms  TYPE ztpp_206-psmng, "工单总米数
       lv_zms     TYPE ztpp_206-psmng,
       lv_zms_nw  TYPE ztpp_206-psmng,
       lv_zms1    TYPE ztpp_206-psmng,
       lv_zms1_nw TYPE ztpp_206-psmng,
       lv_zkjsh   TYPE ztpp_206q12-zkjsh, "开机损耗
       lv_zhjsh   TYPE ztpp_206q12-zhjsh. "换卷损耗
  DATA:lt_206Q12 TYPE TABLE OF ztpp_206q12 WITH HEADER LINE.
  DATA:lv_lines  TYPE sy-tabix,
       lv_lines1 TYPE sy-tabix,
       cha       TYPE ztpp_206-psmng,
       cha1      TYPE ztpp_206-psmng.

  CLEAR:pmtype.
  LOOP AT it_pctx.
    lv_zgdzms += it_pctx-zgdms.
  ENDLOOP.
  READ TABLE it_pctx INDEX 1.
  CASE it_pctx-zzl1.
    WHEN '丽彩鼎' OR '丽彩金' OR '丽彩钻' OR '丽彩幕'.
      pmtype = '1'.
    WHEN '压型板' .
      pmtype = '2'.
    WHEN 'Z型钢' OR 'C型钢'.
      pmtype = '3'.
  ENDCASE.
  SELECT * FROM ztpp_206q12 INTO TABLE lt_206q12 WHERE arbpl = it_pctx-arbpl AND werks = it_pctx-werks AND matkl = it_pctx-matnr+0(5) AND zbx = it_pctx-zbx.
  READ TABLE lt_206q12 INDEX 1.
  IF sy-subrc = 0.
    lv_zkjsh = lt_206q12-zkjsh.
    lv_zhjsh = lt_206q12-zhjsh.
  ENDIF.
  REFRESH:it_qtmx,it_qtmx1,it_qtmx2.
  PERFORM getqtdata.
  IF it_qtmx3[] IS INITIAL.
    MESSAGE i004 WITH '没有可齐套的钢卷'.
    EXIT.
  ENDIF.
  SORT it_qtmx3 BY sort zqtds_ky.
  LOOP AT it_qtmx3.
    IF pmtype = '1'.
      CASE it_qtmx3-znwb.
        WHEN '内板'.
          IF lv_zgdzms + lv_zkjsh > lv_zms1.
            lv_zms += it_qtmx3-zqtms_ky.
            lv_zms1 = lv_zms - lv_zhjsh.
            it_qtmx3-zsfxt = 'X'.
            APPEND it_qtmx3 TO it_qtmx.
            APPEND it_qtmx3 TO it_qtmx1.
          ELSE.
            EXIT.
          ENDIF.
        WHEN '外板'.
          IF lv_zgdzms + lv_zkjsh > lv_zms1_nw.
            lv_zms_nw += it_qtmx3-zqtms_ky.
            lv_zms1_nw = lv_zms_nw - lv_zhjsh.
            it_qtmx3-zsfxt = 'X'.
            APPEND it_qtmx3 TO it_qtmx.
            APPEND it_qtmx3 TO it_qtmx2.
          ELSE.
            EXIT.
          ENDIF.
      ENDCASE.
    ELSE.
      IF lv_zgdzms + lv_zkjsh > lv_zms1.
        lv_zms += it_qtmx3-zqtms_ky.
        lv_zms1 = lv_zms - lv_zhjsh.
        it_qtmx3-zsfxt = 'X'.
        APPEND it_qtmx3 TO it_qtmx.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF pmtype = '1'.
    IF lv_zgdzms + lv_zkjsh > lv_zms1 OR lv_zgdzms + lv_zkjsh > lv_zms1_nw.
      MESSAGE i004 WITH '钢卷齐套数量不满足工单生产'.
      EXIT.
    ENDIF.
  ELSE.
    IF lv_zgdzms + lv_zkjsh > lv_zms1.
      MESSAGE i004 WITH '钢卷齐套数量不满足工单生产'.
      EXIT.
    ENDIF.
  ENDIF.
  IF pmtype = '1'.
    lv_lines = lines( it_qtmx1[] ).
    lv_lines1 = lines( it_qtmx2[] ).
    READ TABLE it_qtmx1 INDEX lv_lines.
    cha = lv_zgdzms - it_qtmx1-zqtms_ky.
    it_qtmx1-zqtms = cha.
    it_qtmx1-zqtds = it_qtmx1-zqtms / it_qtmx1-ztm.
    LOOP AT it_qtmx WHERE index = it_qtmx1-index.
      MODIFY it_qtmx FROM it_qtmx1.
    ENDLOOP.
    READ TABLE it_qtmx2 INDEX lv_lines1.
    cha1 = lv_zgdzms - it_qtmx2-zqtms_ky.
    it_qtmx2-zqtms = cha.
    it_qtmx2-zqtds = it_qtmx2-zqtms / it_qtmx2-ztm.
    LOOP AT it_qtmx WHERE index = it_qtmx2-index.
      MODIFY it_qtmx FROM it_qtmx2.
    ENDLOOP.
  ELSE.
    lv_lines = lines( it_qtmx[] ).
    READ TABLE it_qtmx1 INDEX lv_lines.
    cha = lv_zgdzms - it_qtmx-zqtms_ky.
    it_qtmx-zqtms = cha.
    it_qtmx-zqtds = it_qtmx-zqtms / it_qtmx-ztm.
    MODIFY it_qtmx FROM it_qtmx INDEX lv_lines.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form getqtdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getqtdata.
  DATA:lt_206q2 TYPE TABLE OF ztpp_206q2 WITH HEADER LINE.
  DATA:lv_posid TYPE prps-posid,
       lv_zcpfl TYPE eban-zcpfl. "夹芯板 单板
  DATA:lv_zgdzms  TYPE ztpp_206-psmng, "工单总米数
       lv_zms     TYPE ztpp_206-psmng,
       lv_zms_nw  TYPE ztpp_206-psmng,
       lv_zms1    TYPE ztpp_206-psmng,
       lv_zms1_nw TYPE ztpp_206-psmng,
       lv_zkjsh   TYPE ztpp_206q12-zkjsh, "开机损耗
       lv_zhjsh   TYPE ztpp_206q12-zhjsh. "换卷损耗
  DATA:BEGIN OF lt_qgmat OCCURS 0,
         matnr TYPE matnr,
         charg TYPE charg_d,
       END OF lt_qgmat.
  DATA:lt_206Q12 TYPE TABLE OF ztpp_206q12 WITH HEADER LINE.
  DATA:lv_znwb TYPE ze_znwb.

  it_qtmx1[] = CORRESPONDING #( it_psrw[] ).
  CHECK it_qtmx1[] IS NOT INITIAL AND it_pctx[] IS NOT INITIAL.
  LOOP AT it_pctx.
    lv_zgdzms += it_pctx-zgdms.
  ENDLOOP.
  READ TABLE it_pctx INDEX 1.

  SELECT * FROM ztpp_206q12 INTO TABLE lt_206q12 WHERE arbpl = it_pctx-arbpl AND werks = it_pctx-werks AND matkl = it_pctx-matnr+0(5) AND zbx = it_pctx-zbx.
  READ TABLE lt_206q12 INDEX 1.
  IF sy-subrc = 0.
    lv_zkjsh = lt_206q12-zkjsh.
    lv_zhjsh = lt_206q12-zhjsh.
  ENDIF.
  lv_posid = it_pctx-posid.
  CASE it_pctx-matnr+0(1).
    WHEN 'A'.
      lv_zcpfl = '夹芯板'.
    WHEN 'B'.
      lv_zcpfl = '单板'.
    WHEN OTHERS.
      MESSAGE w004 WITH '这是除夹芯板和单板之外的类型，无法识别！'.
      EXIT.
  ENDCASE.
  IF lv_posid IS INITIAL.
    "成品工单无项目，则删除项目库存
    DELETE it_qtmx1 WHERE posid IS NOT INITIAL.
  ENDIF.
  CLEAR:gt_matcha[].
  LOOP AT it_qtmx1.
    gs_matcha-werks = it_qtmx1-werks.
    gs_matcha-matnr = it_qtmx1-matnr.
    gs_matcha-charg = it_qtmx1-charg.
    APPEND gs_matcha TO gt_matcha.
  ENDLOOP.
  "查询已齐套的明细
  SELECT
     q2~werks,
     q2~matnr,
     q2~charg,
     SUM( q2~zqtds ) AS zqtds,
     SUM( q2~zqtms ) AS zqtms
  FROM ztpp_206q2 AS q2
  INNER JOIN @gt_matcha AS ma ON  ma~werks = q2~werks
                              AND ma~matnr = q2~matnr
                              AND ma~charg = q2~charg
  WHERE q2~zdelbs = ''
  GROUP BY q2~werks,q2~matnr,q2~charg
  ORDER BY q2~werks,q2~matnr,q2~charg
  INTO CORRESPONDING FIELDS OF TABLE @lt_206q2.
  LOOP AT it_qtmx1.
    it_qtmx1-zpssl_in = it_qtmx1-zsl.
    it_qtmx1-zqtds_ky = it_qtmx1-zsl.
    it_qtmx1-zqtms_ky = it_qtmx1-zkcms.
    READ TABLE lt_206q2 WITH KEY werks = it_qtmx1-werks matnr = it_qtmx1-matnr charg = it_qtmx1-charg BINARY SEARCH.
    IF sy-subrc = 0.
      it_qtmx1-zpssl_in = it_qtmx1-zpssl_in - lt_206q2-zqtds.
      it_qtmx1-zqtds_ky = it_qtmx1-zsl - lt_206q2-zqtds.
      it_qtmx1-zqtms_ky = it_qtmx1-zkcms - lt_206q2-zqtms.
    ENDIF.
    MODIFY it_qtmx1.
  ENDLOOP.
  DELETE it_qtmx1 WHERE zqtds_ky IS INITIAL OR zqtms_ky IS INITIAL.
  it_qtmx2[] = it_qtmx1[].
  REFRESH:it_qtmx2.
  CLEAR: it_qtmx2.
  "匹配成品属性
  PERFORM matchattr USING    it_pctx
                             ''
                    CHANGING it_qtmx3[].
  it_qtmx1[] = it_qtmx3[].
  REFRESH:it_qtmx3.
  CLEAR: it_qtmx3.
  IF lv_posid IS NOT INITIAL. "有项目
    SELECT
      ekbe~matnr,
      ekbe~charg
    FROM ekbe
    INNER JOIN ekpo ON  ekbe~ebeln = ekpo~ebeln
                    AND ekbe~ebelp = ekpo~ebelp
    INNER JOIN eban ON  eban~banfn = ekpo~banfn
                    AND eban~bnfpo = ekpo~bnfpo
    INNER JOIN ebkn ON  eban~banfn = ebkn~banfn
                    AND eban~bnfpo = ebkn~bnfpo
    INNER JOIN prps ON ebkn~ps_psp_pnr = prps~pspnr
    WHERE prps~posid = @lv_posid
      AND eban~zcpfl = @lv_zcpfl
      AND NOT EXISTS ( SELECT * FROM  m_mbmps   "未被冲销
                       WHERE m_mbmps~sjahr = ekbe~gjahr AND
                             m_mbmps~smbln = ekbe~belnr AND
                             m_mbmps~smblp = ekbe~buzei )
    GROUP BY ekbe~matnr,ekbe~charg
    ORDER BY ekbe~matnr,ekbe~charg
    INTO TABLE @lt_qgmat.
    SORT it_qtmx1 BY matnr charg zqtds_ky.
    LOOP AT lt_qgmat.
      READ TABLE it_qtmx1 WITH KEY matnr = lt_qgmat-matnr charg = lt_qgmat-charg.
      IF sy-subrc = 0.
        LOOP AT it_qtmx1 FROM sy-tabix.
          IF it_qtmx1-matnr <> lt_qgmat-matnr OR it_qtmx1-charg <> lt_qgmat-charg.
            EXIT.
          ENDIF.
          APPEND it_qtmx1 TO it_qtmx3.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    CLEAR:lv_zms,lv_zms1,lv_zms_nw,lv_zms1_nw.
    SORT it_qtmx3 BY zqtds_ky.
    IF pmtype = '1'. "内外板
      LOOP AT it_qtmx3 WHERE znwb = '内板'.
        IF lv_zgdzms + lv_zkjsh > lv_zms1.
          lv_zms += it_qtmx1-zqtms_ky.
          lv_zms1 = lv_zms - lv_zhjsh.
          it_qtmx3-sort = 10.
          MODIFY it_qtmx3 TRANSPORTING sort.
        ELSE.
          APPEND it_qtmx3 TO it_qtmx1.
          DELETE it_qtmx3 WHERE index = it_qtmx3-index.
        ENDIF.
      ENDLOOP.
      LOOP AT it_qtmx3 WHERE znwb = '外板'.
        IF lv_zgdzms + lv_zkjsh > lv_zms1_nw.
          lv_zms_nw += it_qtmx1-zqtms_ky.
          lv_zms1_nw = lv_zms_nw - lv_zhjsh.
          it_qtmx3-sort = 10.
          MODIFY it_qtmx3 TRANSPORTING sort.
        ELSE.
          APPEND it_qtmx3 TO it_qtmx1.
          DELETE it_qtmx3 WHERE index = it_qtmx3-index.
        ENDIF.
      ENDLOOP.
      IF lv_zgdzms + lv_zkjsh <= lv_zms1 AND lv_zgdzms + lv_zkjsh <= lv_zms1_nw.
        EXIT.
      ENDIF.
    ELSE.
      LOOP AT it_qtmx3.
        IF lv_zgdzms + lv_zkjsh > lv_zms1.
          lv_zms += it_qtmx1-zqtms_ky.
          lv_zms1 = lv_zms - lv_zhjsh.
          it_qtmx3-sort = 10.
          MODIFY it_qtmx3 TRANSPORTING sort.
        ELSE.
          APPEND it_qtmx3 TO it_qtmx1.
          DELETE it_qtmx3 WHERE index = it_qtmx3-index.
        ENDIF.
      ENDLOOP.
      IF lv_zgdzms + lv_zkjsh <= lv_zms1.
        EXIT.
      ENDIF.
    ENDIF.
    "米数不够继续-同项目成品属性相同
    SORT it_qtmx1 BY zqtds_ky.
    LOOP AT it_qtmx1 WHERE posid = lv_posid.
      IF pmtype = '1'.
        CASE it_qtmx1-znwb.
          WHEN '内板'.
            IF lv_zgdzms + lv_zkjsh > lv_zms1.
              lv_zms += it_qtmx1-zqtms_ky.
              lv_zms1 = lv_zms - lv_zhjsh.
              it_qtmx1-sort = 20.
              APPEND it_qtmx1 TO it_qtmx3.
              DELETE it_qtmx1 WHERE index = it_qtmx1-index.
            ENDIF.
          WHEN '外板'.
            IF lv_zgdzms + lv_zkjsh > lv_zms1_nw.
              lv_zms_nw += it_qtmx1-zqtms_ky.
              lv_zms1_nw = lv_zms_nw - lv_zhjsh.
              it_qtmx1-sort = 20.
              APPEND it_qtmx1 TO it_qtmx3.
              DELETE it_qtmx1 WHERE index = it_qtmx1-index.
            ENDIF.
        ENDCASE.

      ELSE.
        IF lv_zgdzms + lv_zkjsh > lv_zms1.
          lv_zms += it_qtmx1-zqtms_ky.
          lv_zms1 = lv_zms - lv_zhjsh.
          it_qtmx1-sort = 20.
          APPEND it_qtmx1 TO it_qtmx3.
          DELETE it_qtmx1 WHERE index = it_qtmx1-index.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF pmtype = '1'.
      IF lv_zgdzms + lv_zkjsh <= lv_zms1 AND lv_zgdzms + lv_zkjsh <= lv_zms1_nw.
        EXIT.
      ENDIF.
    ELSE.
      IF lv_zgdzms + lv_zkjsh <= lv_zms1.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
  "根据成品属性查询非限制库存
  DELETE it_qtmx1 WHERE posid IS NOT INITIAL.
  SORT it_qtmx1 BY zqtds_ky.
  LOOP AT it_qtmx1.
    IF pmtype = '1'.
      CASE it_qtmx1-znwb.
        WHEN '内板'.
          IF lv_zgdzms + lv_zkjsh > lv_zms1.
            lv_zms += it_qtmx1-zqtms_ky.
            lv_zms1 = lv_zms - lv_zhjsh.
            it_qtmx1-sort = 30.
            APPEND it_qtmx1 TO it_qtmx3.
            DELETE it_qtmx1 WHERE index = it_qtmx1-index.
          ENDIF.
        WHEN '外板'.
          IF lv_zgdzms + lv_zkjsh > lv_zms1_nw.
            lv_zms_nw += it_qtmx1-zqtms_ky.
            lv_zms1_nw = lv_zms_nw - lv_zhjsh.
            it_qtmx1-sort = 30.
            APPEND it_qtmx1 TO it_qtmx3.
            DELETE it_qtmx1 WHERE index = it_qtmx1-index.
          ENDIF.
      ENDCASE.
    ELSE.
      IF lv_zgdzms + lv_zkjsh > lv_zms1.
        lv_zms += it_qtmx1-zqtms_ky.
        lv_zms1 = lv_zms - lv_zhjsh.
        it_qtmx1-sort = 30.
        APPEND it_qtmx1 TO it_qtmx3.
        DELETE it_qtmx1 WHERE index = it_qtmx1-index.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF pmtype = '1'.
    IF lv_zgdzms + lv_zkjsh <= lv_zms1 AND lv_zgdzms + lv_zkjsh <= lv_zms1_nw.
      EXIT.
    ENDIF.
  ELSE.
    IF lv_zgdzms + lv_zkjsh <= lv_zms1.
      EXIT.
    ENDIF.
  ENDIF.
  "根据成品属性查询产地/颜色相同，厚度+0.2范围内的非限制库存
  it_qtmx1[] = it_qtmx2[].
  LOOP AT it_qtmx3.
    READ TABLE it_qtmx1 WITH KEY index = it_qtmx3-index.
    IF sy-subrc = 0.
      DELETE it_qtmx1 WHERE index = it_qtmx1-index.
    ENDIF.
  ENDLOOP.
  DELETE it_qtmx1 WHERE posid IS NOT INITIAL.
  PERFORM matchattr USING    it_pctx

                             '1'
                    CHANGING it_qtmx3[].
ENDFORM.
**********************************************************************
*&匹配成品属性
**********************************************************************
FORM matchattr USING    wa_pctx LIKE LINE OF it_pctx
                        pp      TYPE char1
               CHANGING t_qtmx3 LIKE it_qtmx3[].
  DATA:BEGIN OF lt_field OCCURS 0,
         mname  TYPE char10,
         qname  TYPE char10,
         pmtype TYPE char1,
         whtype TYPE char1,
       END OF lt_field.
  DATA:wherestr  TYPE string,
       wherestr1 TYPE string.
  FIELD-SYMBOLS:<fs1> TYPE any,
                <fs2> TYPE any.
  DATA:lt_mx  LIKE it_qtmx1[],
       ls_mx  LIKE LINE OF lt_mx,
       lt_mx1 LIKE it_qtmx1[],
       ls_mx1 LIKE LINE OF lt_mx1,
       lt_mx2 LIKE it_qtmx1[],
       ls_mx2 LIKE LINE OF lt_mx2.
  DATA:p_sfqtbs TYPE char1.
  "板宽加量
  DATA:lt_260C TYPE TABLE OF ztpp_260c WITH HEADER LINE,
       lt_260T TYPE TABLE OF ztpp_260t WITH HEADER LINE.
  DATA:lv_nbjl  TYPE ztpp_260c-znbzkjl, "内板宽度
       lv_wbjl  TYPE ztpp_260c-znbzkjl, "外板宽度
       lv_zylzk TYPE ztpp_260t-zylzk.  "用料展宽
  RANGES: ra_bh FOR ztpp_260c-zhd.
  DATA:r_hd  TYPE RANGE OF atwrt WITH HEADER LINE,
       r_hd1 TYPE RANGE OF atwrt WITH HEADER LINE.

  "取配置表
  SELECT * FROM ztpp_260c WHERE werks = @wa_pctx-werks AND matkl = @wa_pctx-matnr+0(5) AND zbx = @wa_pctx-zbx INTO TABLE @lt_260C. "内外板展宽加量
  SELECT * FROM ztpp_260t WHERE werks = @wa_pctx-werks AND matkl = @wa_pctx-matnr+0(5) AND zbxgg = @wa_pctx-zbx INTO TABLE @lt_260t. "用料展宽

  lt_field[] = VALUE #( ( mname = 'ZCD001'   qname = 'ZWBCD' pmtype = '1' whtype = '0' )
                        ( mname = 'ZHD001'   qname = 'ZWBHD' pmtype = '1' whtype = '0' )
                        ( mname = 'ZYS001'   qname = 'ZWBYS' pmtype = '1' whtype = '0' )
                        ( mname = 'ZTCZL001' qname = 'ZWBTC' pmtype = '1' whtype = '0' )
                        ( mname = 'ZDCHL001' qname = 'ZWBDC' pmtype = '1' whtype = '0' )
                        ( mname = 'ZCZQD001' qname = 'ZWBQD' pmtype = '1' whtype = '0' )
                        ( mname = 'ZCD001'   qname = 'ZNBCD' pmtype = '1' whtype = '1' )
                        ( mname = 'ZHD001'   qname = 'ZNBHD' pmtype = '1' whtype = '1' )
                        ( mname = 'ZYS001'   qname = 'ZNBYS' pmtype = '1' whtype = '1' )
                        ( mname = 'ZTCZL001' qname = 'ZNBTC' pmtype = '1' whtype = '1' )
                        ( mname = 'ZDCHL001' qname = 'ZNBDC' pmtype = '1' whtype = '1' )
                        ( mname = 'ZCZQD001' qname = 'ZNBQD' pmtype = '1' whtype = '1' )
                        ( mname = 'ZCD001'   qname = 'ZCD'   pmtype = '2' whtype = '0' )
                        ( mname = 'ZHD001'   qname = 'ZHD'   pmtype = '2' whtype = '0' )
                        ( mname = 'ZYS001'   qname = 'ZYS'   pmtype = '2' whtype = '0' )
                        ( mname = 'ZTCZL001' qname = 'ZTCZL' pmtype = '2' whtype = '0' )
                        ( mname = 'ZDCHL001' qname = 'ZDCHL' pmtype = '2' whtype = '0' )
                        ( mname = 'ZCZQD001' qname = 'ZCZQD' pmtype = '2' whtype = '0' )
                        ( mname = 'ZCD001'   qname = 'ZCD'   pmtype = '3' whtype = '0' )
                        ( mname = 'ZHD001'   qname = 'ZHD'   pmtype = '3' whtype = '0' )
                        ( mname = 'ZDCHL001' qname = 'ZDCHL' pmtype = '3' whtype = '0' )
                        ( mname = 'ZCZQD001' qname = 'ZCZQD' pmtype = '3' whtype = '0' )
                        ( mname = 'ZCD001'   qname = 'ZWBCD' pmtype = '4' whtype = '0' )
                        ( mname = 'ZYS001'   qname = 'ZWBYS' pmtype = '4' whtype = '0' )
                        ( mname = 'ZCD001'   qname = 'ZNBCD' pmtype = '4' whtype = '1' )
                        ( mname = 'ZYS001'   qname = 'ZNBYS' pmtype = '4' whtype = '1' )
                        ( mname = 'ZHD001'   qname = 'ZHD'   pmtype = '5' whtype = '0' )
                        ( mname = 'ZYS001'   qname = 'ZYS'   pmtype = '5' whtype = '0' )
                        ( mname = 'ZHD001'   qname = 'ZHD'   pmtype = '6' whtype = '0' )
                      ).
  CASE it_pctx-matnr+0(1).
    WHEN 'A'.
      LOOP AT lt_260C WHERE zwbxz = wa_pctx-zbx.
*板厚没有数， 就不做区间对比
        IF wa_pctx-zbh IS NOT INITIAL.
          IF lt_260C-zoption = ''.

          ELSE.
            ra_bh-sign = 'I'.
            ra_bh-option = lt_260C-zoption.
            ra_bh-low = lt_260C-zhd.
            APPEND ra_bh.
          ENDIF.
          IF wa_pctx-zbh IN ra_bh.
            lv_nbjl = lt_260C-znbzkjl + wa_pctx-zbk.
            lv_wbjl = lt_260C-zwbzkjl + wa_pctx-zbk.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN 'B'.
      READ TABLE lt_260t INDEX 1.
      IF sy-subrc = 0.
        lv_zylzk = lt_260t-zylzk.
      ENDIF.
    WHEN OTHERS.
      MESSAGE w004 WITH '这是除夹芯板和单板之外的类型，无法识别！'.
      EXIT.
  ENDCASE.
  CASE pmtype.
    WHEN '1'. "'丽彩鼎' OR '丽彩金' OR '丽彩钻' OR '丽彩幕'
      IF pp = '1'.
        r_hd[] = VALUE #( ( sign = 'I' option = 'BT' low = wa_pctx-zwbhd high = wa_pctx-zwbhd + '0.2' ) ).
        r_hd1[] = VALUE #( ( sign = 'I' option = 'BT' low = wa_pctx-znbhd high = wa_pctx-znbhd + '0.2' ) ).
      ENDIF.
    WHEN '2' . "压型板
      IF pp = '1'.
        r_hd[] = VALUE #( ( sign = 'I' option = 'BT' low = wa_pctx-zhd high = wa_pctx-zhd + '0.2' ) ).
      ENDIF.
    WHEN '3'. "'Z型钢' OR 'C型钢'.
      IF pp = '1'.
        r_hd[] = VALUE #( ( sign = 'I' option = 'BT' low = wa_pctx-zhd high = wa_pctx-zhd + '0.2' ) ).
      ENDIF.
  ENDCASE.
  LOOP AT lt_field WHERE pmtype = pmtype.
    CASE lt_field-whtype.
      WHEN '0'.
        ASSIGN COMPONENT lt_field-qname OF STRUCTURE wa_pctx TO <fs1>.
        IF <fs1> <> '无' AND <fs1> IS NOT INITIAL.
          wherestr = |{ wherestr }AND { lt_field-mname } = '{ <fs1> }' |.
        ENDIF.
      WHEN '1'.
        ASSIGN COMPONENT lt_field-qname OF STRUCTURE wa_pctx TO <fs1>.
        IF <fs1> <> '无' AND <fs1> IS NOT INITIAL.
          wherestr1 = |{ wherestr1 }AND { lt_field-mname } = '{ <fs1> }' |.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  IF pp = '1'.
    wherestr = |{ wherestr } AND ZHD001 IN r_hd[]|.
  ENDIF.
  SHIFT wherestr LEFT  DELETING LEADING 'AND'.
  LOOP AT it_qtmx1 WHERE (wherestr).
    CASE it_pctx-matnr+0(1).
      WHEN 'A'.
        IF it_qtmx1-zkd001 < lv_wbjl.
          CONTINUE.
        ENDIF.
      WHEN 'B'.
        IF it_qtmx1-zkd001 < lv_zylzk.
          CONTINUE.
        ENDIF.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    ls_mx = it_qtmx1.
    IF pp = '1'.
      ls_mx-sort = 40.
    ENDIF.
    CASE pmtype.
      WHEN '1'.
        ls_mx-znwb = '外板'.
      WHEN OTHERS.
    ENDCASE.
    APPEND ls_mx TO lt_mx.
  ENDLOOP.
  CASE pmtype.
    WHEN '1'.
      IF lt_mx[] IS INITIAL.
        p_sfqtbs = ''.
      ELSE.
        IF pp = '1'.
          wherestr = |{ wherestr } AND ZHD001 IN r_hd1[]|.
        ENDIF.
        SHIFT wherestr1 LEFT  DELETING LEADING  'AND'.
        LOOP AT it_qtmx1 WHERE (wherestr1).
          IF it_qtmx1-zkd001 < lv_nbjl.
            CONTINUE.
          ENDIF.
          ls_mx1 = it_qtmx1.
          IF pp = '1'.
            ls_mx1-sort = 40.
          ENDIF.
          ls_mx-znwb = '内板'.
          APPEND ls_mx1 TO lt_mx1.
        ENDLOOP.
        IF lt_mx1[] IS INITIAL.
          p_sfqtbs = ''.
        ELSE.
          p_sfqtbs = 'X'.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      IF lt_mx[] IS INITIAL.
        p_sfqtbs = ''.
      ELSE.
        p_sfqtbs = 'X'.
      ENDIF.
  ENDCASE.
  IF p_sfqtbs = 'X'.
    APPEND LINES OF lt_mx TO t_qtmx3.
    APPEND LINES OF lt_mx1 TO t_qtmx3.
  ENDIF.
ENDFORM.
**********************************************************************
*&添加齐套钢卷
**********************************************************************
FORM addqt.
  LOOP AT it_psrw WHERE chbox = 'X'.
    IF pmtype = '1' AND it_psrw-znwb IS INITIAL.
      MESSAGE s004 WITH '请填写内外板！' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF it_psrw-zqtds IS INITIAL OR it_psrw-zqtms IS INITIAL.
      MESSAGE s004 WITH '请填写齐套吨数或者齐套米数！' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.
  LOOP AT it_psrw WHERE chbox = 'X'.
    IF it_psrw-zpssl_in LE 0.
      MESSAGE s004 WITH '配送数量应该大于0' DISPLAY LIKE 'E'.
 