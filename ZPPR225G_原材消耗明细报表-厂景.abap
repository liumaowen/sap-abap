*&---------------------------------------------------------------------*
*& Report ZRPP225G
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrpp225g MESSAGE-ID zmsg_gp.
TABLES:aufm,
       mkpf,
       t001l,
       t006a,
       ausp,
       prps,
       kna1,
       ztpp_205,
       t001w,
       rc68a,mara.
TYPE-POOLS:slis.
DATA:fieldcat TYPE slis_t_fieldcat_alv.
DATA:BEGIN OF itab OCCURS 0,
       mblnr      TYPE aufm-mblnr,
       mjahr      TYPE aufm-mjahr,
       aufnr      TYPE aufm-aufnr,
       ebeln      TYPE aufm-ebeln,
       erfmg      TYPE aufm-erfmg,
       erfme      TYPE aufm-erfme,
       charg      TYPE aufm-charg,
       bwart      TYPE aufm-bwart,
       werks      TYPE aufm-werks,
       lgort      TYPE aufm-lgort,
       matnr      TYPE aufm-matnr,
       usnam      TYPE mkpf-usnam,
       cpudt      TYPE mkpf-cpudt,
       ltrmi      TYPE afpo-ltrmi,
       zzl1       TYPE mara-zzl1,
       vbeln      TYPE ztpp_206-vbeln,
       posnr      TYPE ztpp_206-posnr,
       plnum      TYPE ztpp_206-plnum,
       plnumhh    TYPE ztpp_206-plnumhh,
       kunnr      TYPE vbak-kunnr,
       namek      TYPE kna1-name1,
       zxhgg      TYPE atwrt30,
       zwlcms     TYPE string,
       plnbez     TYPE afko-plnbez, "成品物料 MODIFY BY LZF 09.02.2023 08:28:18
       zcpwlcms   TYPE string, "成品长描述 MODIFY BY LZF 09.02.2023 08:28:41
       t006a      TYPE t006a-msehl,
       zysks      TYPE ze_zysks,
       zms        TYPE ztpp_205a-zms,
       zflmx      TYPE char10,
       namew      TYPE name1,
       lgobe      TYPE lgobe,
       arbpl      TYPE crhd-arbpl,
       ktext      TYPE crtx-ktext,
       zname      TYPE ztpp_206-zname,
       z01        TYPE atwrt30,
       z02        TYPE atwrt30,
       budat      TYPE budat,
       pspnr      TYPE prps-pspnr,
       posid      TYPE prps-posid,
       post1      TYPE prps-post1,
*& kkw
       zjjlx      TYPE ZTPP_205a-zjjlx,
       z03        TYPE atwrt,
       zqy        TYPE ZTPP_205a-zqy,
       zcj        TYPE ZTPP_205b-zcj,
       z16        TYPE atwrt,
       zname10    TYPE ztpp210-zname,
*& End  12.01.2023 11:16:35
*& kkw
       zpcdh      TYPE ztpp_206-zpcdh,
       zpcdhh     TYPE ztpp_206-zpcdhh,
       zxgyq      TYPE ztpp_205a-zxgyq,
       zcksl      TYPE ztpp_205d-zcksl,
       zcd        TYPE ztpp_206-zcd,
       zbckd      TYPE ztpp_206-zbckd,
       zbh        TYPE atwrt,
       zxgkdjjyq  TYPE ztpp_205b-zxgkdjjyq,
       zxczl      TYPE atwrt,
       zxccd      TYPE atwrt,
       zxchd      TYPE atwrt, "新材厚度 by lzf
       zxgsl      TYPE ztpp_205b-zxgsl,
       zkdsl      TYPE ztpp_205b-zkdsl,
       zjjsl      TYPE ztpp_205b-zjjsl,
       zcc_316    TYPE ztpp316-zcc,
       zzk_316    TYPE ztpp316-zzk,
       zcd_316    TYPE ztpp316-zcd,
       zkd_316    TYPE ztpp316-zkd,
       zxs_316    TYPE ztpp316-zxs,
       del        TYPE char1,
       zxishu     TYPE ztpp_205a-zxishu,
       ztm        TYPE ze_kbetr1,
*& End  16.01.2023 11:04:46
       toppage,
       ztlpfs     TYPE ztpp_206-psmng,
       meins      TYPE ztpp_206-meins,
       zscbz      TYPE ztpp316-zscbz,
       zsckd      TYPE p DECIMALS 3,
       zmcccsl    TYPE p DECIMALS 3,
       menge      TYPE mseg-menge,
       name_textc TYPE user_addr-name_textc,
       xhm        TYPE menge_d,
       xhto       TYPE menge_d,
       syzq       TYPE i,
       kukla      TYPE kukla,
       dmbtr      TYPE aufm-dmbtr,
       kzwi1      TYPE vbap-kzwi1,
       vprsv      TYPE mbew-vprsv,
       ps_psp_pnr TYPE aufm-ps_psp_pnr,
     END OF itab,
     BEGIN OF it_arbpl OCCURS 0,
       aufnr TYPE aufnr,
       aufpl TYPE afko-aufpl,
       arbid TYPE afvc-arbid,
       kostl TYPE kostl,
       arbpl TYPE crhd-arbpl,
       ktext TYPE crtx-ktext,
     END OF it_arbpl,
     it_zsmm206 TYPE TABLE OF zsmm206 WITH HEADER LINE,
     o_zsmm206  TYPE TABLE OF zsmm206,
     it_mchas   TYPE TABLE OF mcha WITH HEADER LINE,
     it_pctx    TYPE TABLE OF zsfms_getpctx WITH HEADER LINE,
     outtab001  TYPE TABLE OF zsfms_getpctx WITH HEADER LINE,
     it_matnr   TYPE TABLE OF ccvx_matnr WITH HEADER LINE,
     it_zpcdh   TYPE TABLE OF zspp205 WITH HEADER LINE,
     it_ysks    TYPE TABLE OF zspp205 WITH HEADER LINE,
     it_vbeln   TYPE TABLE OF lips_key WITH HEADER LINE,
     it_aufnr   TYPE TABLE OF aufnr_pre WITH HEADER LINE.
DATA:tabix TYPE sy-tabix.
RANGES:s_charg FOR aufm-charg.
FIELD-SYMBOLS:<fs> TYPE any.
DATA:itab316 LIKE TABLE OF itab WITH HEADER LINE.
DATA:zck  TYPE p DECIMALS 3, "计算门窗投料平方数用
     zpfs TYPE p DECIMALS 3.
DATA:zck_cc TYPE p DECIMALS 3, "计算门窗产出数量用
     zccs   TYPE p DECIMALS 3.
DATA:BEGIN OF it_cms OCCURS 0,
       matnr TYPE matnr,
       wlcms TYPE string,
     END OF it_cms.
DATA:BEGIN OF it_cmswerks OCCURS 0,
       matnr TYPE matnr,
       werks TYPE werks_d,
     END OF it_cmswerks.
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

DATA gt_amount LIKE TABLE OF gs_amount.
DATA: gt_ckmlhd TYPE TABLE OF ckmlhd,
      gs_ckmlhd TYPE ckmlhd.
DATA:BEGIN OF it_ydpjj OCCURS 0,
       matnr TYPE matnr,
       werks TYPE werks_d,
       pspnr TYPE prps-pspnr,
     END OF it_ydpjj.
DATA:curtp TYPE ckmlcr-curtp.
TYPES: gt_ckmlhd_temp TYPE STANDARD TABLE OF ckmlhd.
DATA:rbudat TYPE RANGE OF mseg-budat_mkpf WITH HEADER LINE.
INCLUDE zauth_check.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:s_werks FOR aufm-werks NO-DISPLAY DEFAULT '3008',
                 s_aufnr FOR aufm-aufnr.
  PARAMETERS: p_lfgja LIKE mchbh-lfgja OBLIGATORY DEFAULT sy-datum+0(4).
  PARAMETERS: s_lfmon LIKE mchbh-lfmon OBLIGATORY DEFAULT sy-datum+4(2).
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:p_10 RADIOBUTTON GROUP grd1 USER-COMMAND click DEFAULT 'X' MODIF ID mm,
             p_31 RADIOBUTTON GROUP grd1 MODIF ID mm,
             p_32 RADIOBUTTON GROUP grd1 MODIF ID mm.
SELECTION-SCREEN END OF BLOCK blk2.


AT SELECTION-SCREEN OUTPUT.
  %_s_aufnr_%_app_%-text = '生产订单'.
  %_s_werks_%_app_%-text = '工厂'.

START-OF-SELECTION.
  CLEAR auth_flag.
  PERFORM auth_check_swerks TABLES s_werks CHANGING auth_flag.
  CHECK auth_flag NE 'E'.
  AUTHORITY-CHECK OBJECT 'ZWSD06'
  ID 'ZWMKZ' FIELD '01'.
  IF sy-subrc <> 0.
    DATA(clear) = 'X'.
  ENDIF.
  REFRESH:rbudat.
  rbudat-low = p_lfgja && s_lfmon && '01'.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = rbudat-low
    IMPORTING
      e_date = rbudat-high.
  rbudat-sign = 'I'.
  rbudat-option = 'BT'.
  APPEND rbudat.
  CASE 'X'.
    WHEN p_10.
      curtp = '10'.
    WHEN p_31.
      curtp = '31'.
    WHEN p_32.
      curtp = '32'.
  ENDCASE.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  PERFORM alvshow.


FORM getdata.
  SELECT mseg~mblnr,
         mseg~mjahr,
         mseg~zeile,
         mseg~aufnr,
         mseg~ebeln,
         mseg~erfmg,
         mseg~erfme,
         mseg~charg,
         mseg~bwart,
         mseg~werks,
         mseg~lgort,
         mseg~matnr,
         mkpf~budat,
         mkpf~usnam,
         mkpf~cpudt,
         afpo~ltrmi,
         mara~zzl1,
         mseg~menge,
         mseg~mat_pspnr AS ps_psp_pnr,
         mseg~dmbtr
    INTO TABLE @DATA(it_tab)
    FROM mseg INNER JOIN mkpf ON mseg~mblnr = mkpf~mblnr
                             AND mseg~mjahr = mkpf~mjahr
              INNER JOIN afpo ON mseg~aufnr = afpo~aufnr
                             AND afpo~posnr = '0001'
              INNER JOIN mara ON mseg~matnr = mara~matnr
    WHERE mseg~werks IN @s_werks
     AND  mseg~aufnr IN @s_aufnr
     AND  mseg~charg IN @s_charg
     AND  mseg~bwart NE '101'
     AND mkpf~budat IN @rbudat
     AND  mseg~smbln = ''
     AND NOT EXISTS ( SELECT * FROM m_mbmps   "未被冲销
                                    WHERE m_mbmps~sjahr = mseg~mjahr AND
                                          m_mbmps~smbln = mseg~mblnr AND
                                          m_mbmps~smblp = mseg~zeile ).
  REFRESH:it_cms,it_cmswerks,it_ydpjj.
  LOOP AT it_tab INTO DATA(wa_tab).
    CLEAR:itab,it_matnr,it_aufnr,it_mchas.
    MOVE-CORRESPONDING wa_tab TO itab.
    CLEAR it_cms.
    it_cms-matnr = itab-matnr.
    COLLECT it_cms.
    CASE itab-bwart.
      WHEN '102'.
        itab-erfmg = itab-erfmg * -1.
        itab-menge = itab-menge * -1.
      WHEN '262'.
        itab-erfmg = itab-erfmg * -1.
        itab-menge = itab-menge * -1.
    ENDCASE.
    CLEAR:itab-ebeln.
    it_matnr-matnr = itab-matnr.
    it_aufnr-aufnr = itab-aufnr.
    it_mchas-matnr = itab-matnr.
    it_mchas-charg = itab-charg.
    it_cmswerks-matnr = wa_tab-matnr.
    it_cmswerks-werks = wa_tab-werks.
    it_ydpjj-matnr = itab-matnr.
    it_ydpjj-werks = itab-werks.
    it_ydpjj-pspnr = itab-ps_psp_pnr.
    COLLECT:it_aufnr, it_matnr,it_mchas,itab,it_cmswerks,it_ydpjj.
  ENDLOOP.
  IF itab[] IS INITIAL
    OR it_matnr[] IS INITIAL
    OR it_mchas[] IS INITIAL
    OR it_aufnr[] IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  SORT it_tab BY mblnr mjahr aufnr.
  SORT it_aufnr BY aufnr.
  SORT it_matnr BY matnr.
  SORT it_mchas BY matnr charg.
  IF it_cmswerks[] IS NOT INITIAL.
    "原料价格控制
    SELECT
      bwkey AS werks,
      matnr,
      vprsv,
      bwtar
       INTO TABLE @DATA(lt_mbew)
      FROM mbew
      FOR ALL ENTRIES IN @it_cmswerks
      WHERE mbew~bwkey = @it_cmswerks-werks
      AND mbew~matnr = @it_cmswerks-matnr.
    SORT lt_mbew BY werks matnr.
  ENDIF.
  DELETE it_ydpjj WHERE matnr IS INITIAL.
  IF it_ydpjj[] IS NOT INITIAL.
    SELECT *
  INTO TABLE gt_ckmlhd
  FROM ckmlhd
    FOR ALL ENTRIES IN it_ydpjj
    WHERE bwkey = it_ydpjj-werks
      AND matnr = it_ydpjj-matnr
      AND pspnr = it_ydpjj-pspnr
      AND vbeln = ''.
    SORT gt_ckmlhd BY kalnr.
    DELETE ADJACENT DUPLICATES FROM gt_ckmlhd COMPARING kalnr.
    REFRESH:gt_amount.
    SELECT * FROM t001w WHERE werks IN s_werks.
      PERFORM get_amount USING t001w-werks.
    ENDSELECT.
    SORT gt_amount BY matnr bwkey pspnr.
  ENDIF.
  "使用周期
  SELECT matnr,charg
    INTO TABLE @DATA(lt_syzq)
    FROM mchb
    FOR ALL ENTRIES IN @itab
    WHERE matnr = @itab-matnr
    AND charg = @itab-charg
    AND mchb~clabs > 0.
  SELECT matnr,charg
    APPENDING TABLE @lt_syzq
    FROM mspr
    FOR ALL ENTRIES IN @itab
    WHERE matnr = @itab-matnr
    AND charg = @itab-charg
    AND mspr~prlab > 0.
  SORT lt_syzq BY matnr charg.

*取描述
  SELECT *
    INTO TABLE @DATA(it_t001l)
    FROM t001l
    WHERE werks IN @s_werks.
  SORT it_t001l BY werks lgort.
  SELECT *
    INTO TABLE @DATA(it_t001w)
    FROM t001w
    WHERE werks IN @s_werks.
  SORT it_t001w BY werks.
  SELECT *
    INTO TABLE @DATA(it_t006a)
    FROM t006a
    WHERE spras = @sy-langu.
  SORT it_t006a BY msehi.
*取批次特征
  CALL FUNCTION 'ZFMS_05_GETPCTX'
    EXPORTING
      atnam  = 'Z01,Z02,Z03,Z16,ZKD,ZHD,ZTM'
    TABLES
      intab  = it_mchas
      outtab = it_pctx.
  SORT it_pctx BY matnr charg atnam.
*取001属性
*取物料属性
  PERFORM get001 TABLES it_matnr.
*取工作中心
  PERFORM getppkostl(zpubform) TABLES it_aufnr it_arbpl.
  SORT it_arbpl BY aufnr.
*取计划单
  SELECT *
    INTO TABLE @DATA(it_206)
    FROM ztpp_206
    FOR ALL ENTRIES IN @it_aufnr
    WHERE aufnr = @it_aufnr-aufnr.
  IF sy-subrc EQ 0.
    LOOP AT it_206 INTO DATA(wa_206).
      CLEAR:it_vbeln.
      it_vbeln-vbeln = wa_206-vbeln.
      COLLECT it_vbeln.
    ENDLOOP.
    SORT it_206 BY zpcdh zpcdhh.
*取排产单数据
    SELECT *
      INTO TABLE @DATA(it_205a)
      FROM ztpp_205a
      FOR ALL ENTRIES IN @it_206
      WHERE zpcdh = @it_206-zpcdh
      AND   zpcdhh = @it_206-zpcdhh.
    SORT it_205a BY zpcdh zpcdhh.
*205d
    SELECT *
      INTO TABLE @DATA(it_205d)
      FROM ztpp_205d
      FOR ALL ENTRIES IN @it_206
      WHERE zpcdh = @it_206-zpcdh.
    SORT it_205d BY zpcdh.
*取销售合同相关
    DELETE it_vbeln WHERE vbeln IS INITIAL.
    IF it_vbeln[] IS NOT INITIAL.
      SORT it_vbeln BY vbeln.
      SELECT vbak~vbeln,
             vbak~kunnr,
             vbak~ps_psp_pnr AS pspnr,
             prps~posid,
             prps~post1,
             kna1~name1 AS namek
        INTO TABLE @DATA(it_vbak)
        FROM vbak INNER JOIN prps ON vbak~ps_psp_pnr = prps~pspnr
                  INNER JOIN kna1 ON vbak~kunnr = kna1~kunnr
        FOR ALL ENTRIES IN @it_vbeln
        WHERE vbak~vbeln = @it_vbeln-vbeln.
      SORT it_vbak BY vbeln.
      SELECT DISTINCT
        kna1~kunnr,
        kna1~kukla,
        kna1~name1
        FROM @it_vbak AS it_vbak
        JOIN kna1 ON it_vbak~kunnr = kna1~kunnr
        ORDER BY kna1~kunnr
        INTO TABLE @DATA(lt_kna1)
        .
    ENDIF.
    SORT it_206 BY aufnr.
  ENDIF.
*取316
  SELECT *
    INTO TABLE @DATA(it_316)
    FROM ztpp316
    FOR ALL ENTRIES IN @it_tab
    WHERE aufnr = @it_tab-aufnr
    AND werks = @it_tab-werks.
  SORT it_316 BY aufnr matnr charg.
  SORT it_tab BY mblnr mjahr aufnr.
*取ztpp_213
  SELECT * INTO TABLE @DATA(lt_213)
    FROM ztpp_213
    FOR ALL ENTRIES IN @it_aufnr
    WHERE aufnr = @it_aufnr-aufnr.
  SORT lt_213 BY aufnr.
  REFRESH itab316.
  SELECT bname,name_textc INTO TABLE @DATA(lt_user)
    FROM user_addr.
  SORT lt_user BY bname.

  LOOP AT itab.
    READ TABLE it_pctx WITH KEY matnr = itab-matnr
                                charg = itab-charg
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      tabix = sy-tabix.
      LOOP AT it_pctx FROM tabix.
        IF it_pctx-matnr NE itab-matnr
          OR it_pctx-charg NE itab-charg.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT it_pctx-atnam OF STRUCTURE itab TO <fs>.
        IF sy-subrc EQ 0.
          <fs> = it_pctx-atwrt.
        ENDIF.
      ENDLOOP.
    ENDIF.
    READ TABLE it_tab INTO wa_tab WITH KEY mblnr = itab-mblnr
                                           mjahr = itab-mjahr
                                           aufnr = itab-aufnr
                                           BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-ebeln = wa_tab-ebeln.
    ENDIF.
    READ TABLE it_t001l INTO t001l WITH KEY werks = itab-werks
                                            lgort = itab-lgort
                                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-lgobe = t001l-lgobe.
    ENDIF.
    READ TABLE it_t001w INTO t001w WITH KEY werks = itab-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-namew = t001w-name1.
    ENDIF.
    READ TABLE it_t006a INTO t006a WITH KEY msehi = itab-erfme BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-t006a = t006a-msehl.
    ENDIF.

**    READ TABLE outtab001 WITH KEY matnr = itab-matnr
**                                  BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      tabix = sy-tabix.
**      LOOP AT outtab001 FROM tabix.
**        IF outtab001-matnr NE itab-matnr.
**          EXIT.
**        ENDIF.
**        ASSIGN COMPONENT outtab001-atnam OF STRUCTURE itab TO <fs>.
**        IF sy-subrc EQ 0.
**          <fs> = outtab001-atwrt.
**        ENDIF.
**      ENDLOOP.
**    ENDIF.
    READ TABLE outtab001 WITH KEY matnr = itab-matnr atnam = 'ZBH' BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zbh   = outtab001-atwrt.
      itab-zxchd = outtab001-atwrt.
    ENDIF.
    READ TABLE outtab001 WITH KEY matnr = itab-matnr atnam = 'ZXCZL' BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zxczl = outtab001-atwrt.
    ENDIF.
    READ TABLE outtab001 WITH KEY matnr = itab-matnr atnam = 'ZXCCD' BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zxccd = outtab001-atwrt.
    ENDIF.
    READ TABLE outtab001 WITH KEY matnr = itab-matnr atnam = 'ZSCKD' BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zsckd = outtab001-atwrt.
    ENDIF.


    READ TABLE it_arbpl WITH KEY aufnr = itab-aufnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-arbpl = it_arbpl-arbpl.
      itab-ktext = it_arbpl-ktext.
    ENDIF.
    READ TABLE it_206 INTO wa_206 WITH KEY aufnr = itab-aufnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      tabix = sy-tabix.
      itab-plnum = wa_206-plnum.
      itab-zname = wa_206-zname.
      READ TABLE lt_user INTO DATA(lw_user) WITH KEY bname = itab-zname BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-name_textc = lw_user-name_textc.
      ENDIF.
      itab-plnumhh = wa_206-plnumhh.
      itab-vbeln = wa_206-vbeln.
      itab-posnr = wa_206-posnr.
      itab-zpcdh = wa_206-zpcdh.
      itab-zcd = wa_206-zcd.
      itab-zbckd = wa_206-zbckd.
      itab-zpcdhh = wa_206-zpcdhh.
      itab-meins = 'M2'.
      CASE itab-matnr+0(3).
        WHEN 'D02'.
          CLEAR: zck,zpfs,zck_cc,zccs.
*          zck = wa_206-zms * itab-zsckd / 1000.
*          zpfs = zck.
*          itab-ztlpfs = itab-ztlpfs + zpfs.

          zck_cc = wa_206-zcd * itab-zsckd / 1000000.
          zccs = zck_cc * itab-menge.
          itab-zmcccsl = zccs.
        WHEN 'D01'.
          CLEAR: zck,zpfs,zck_cc,zccs.
*          zck = wa_206-zms * wa_205a-zbckd / 1000.
*          zpfs = zck.
*          itab-ztlpfs = itab-ztlpfs + zpfs.
          READ TABLE it_205a INTO DATA(wa_205a) WITH KEY zpcdh = wa_206-zpcdh
                                                                zpcdhh = wa_206-zpcdhh
                                                                BINARY SEARCH.
          IF sy-subrc EQ 0.
            zck_cc = wa_206-zcd * wa_205a-zbckd / 1000000.
            zccs = zck_cc * itab-menge.
            itab-zmcccsl = zccs.
          ENDIF.
      ENDCASE.

      SELECT SUM( zcksl ) INTO itab-zcksl FROM ztpp_205d WHERE zpcdh = wa_206-zpcdh.
      LOOP AT it_206 INTO wa_206 FROM tabix.
        IF wa_206-aufnr NE itab-aufnr.
          EXIT.
        ENDIF.
*        itab-zms = wa_206-zms + itab-zms.

*        itab-ztlpfs = itab-ztlpfs + wa_206-zms * wa_206-zxishu.
        CASE itab-matnr+0(3).
          WHEN 'D02'.
            CLEAR: zck,zpfs,zck_cc,zccs.
            zck = wa_206-zms * itab-zsckd / 1000.
            zpfs = zck.
            itab-ztlpfs = itab-ztlpfs + zpfs.

*            zck_cc = wa_206-zcd * itab-zsckd / 1000000.
*            zccs = zck_cc * itab-menge.
*            itab-zmcccsl = itab-zmcccsl + zccs.
          WHEN 'D01'.

          WHEN 'A13'.
            IF wa_206-psmng > 0.
              itab-ztlpfs = itab-ztlpfs + wa_206-psmng.
              itab-zmcccsl = itab-zmcccsl + wa_206-psmng.
            ELSE.
              itab-ztlpfs = itab-ztlpfs + wa_206-gsmng.
              itab-zmcccsl = itab-zmcccsl + wa_206-gsmng.
            ENDIF.

        ENDCASE.

        READ TABLE it_205a INTO wa_205a WITH KEY zpcdh = wa_206-zpcdh
                                                       zpcdhh = wa_206-zpcdhh
                                                       BINARY SEARCH.
        IF sy-subrc EQ 0.
          "验收块数：首先去ZTPP_205A中找到对应的QCYSKS，如果存在则取值，不存在则取ZTPP316中依据生产订单、计划单号、计划单行号找到对应的块数
          itab-zysks = itab-zysks + wa_205a-qcysks.
          itab-zms = itab-zms + wa_205a-qcysks * wa_205a-zcd.
*& kkw pp让取一个
          itab-zjjlx = wa_205a-zjjlx.
          itab-zqy = wa_205a-zqy.
          itab-zxgyq = wa_205a-zxgyq.
          itab-zxishu = wa_205a-zxishu.
* & End  12.01.2023 11:32:42
          CASE itab-matnr+0(3).
            WHEN 'D02'.
            WHEN 'D01'.
              CLEAR: zck,zpfs,zck_cc,zccs.
              zck = wa_206-zms * wa_205a-zbckd / 1000.
              zpfs = zck.
              itab-ztlpfs = itab-ztlpfs + zpfs.

*              zck_cc = wa_206-zcd * wa_205a-zbckd / 1000000.
*              zccs = zck_cc * itab-menge.
*              itab-zmcccsl = itab-zmcccsl + zccs.
          ENDCASE.


        ENDIF.
      ENDLOOP.

      READ TABLE it_vbak INTO DATA(wa_vbak) WITH KEY vbeln = itab-vbeln BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-kunnr = wa_vbak-kunnr.
        itab-pspnr = wa_vbak-pspnr.
        itab-posid = wa_vbak-posid.
        itab-post1 = wa_vbak-post1.
        itab-namek = wa_vbak-namek.
        IF clear = 'X'.
          READ TABLE lt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY kunnr = itab-kunnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            IF <fs_kna1>-kukla = '03'.
              CLEAR itab-namek.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*& kkw
      SELECT SINGLE zcj zxgkdjjyq zxgsl zkdsl zjjsl
        INTO ( itab-zcj,itab-zxgkdjjyq,itab-zxgsl,itab-zkdsl,itab-zjjsl )
        FROM ztpp_205b
        WHERE zpcdh = wa_206-zpcdh.
      SELECT SINGLE zname
        INTO itab-zname10
        FROM ztpp210
        WHERE aufnr = wa_206-aufnr.
      READ TABLE lt_213 INTO DATA(lw213) WITH KEY aufnr = itab-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-zflmx = 'X'.
      ENDIF.
*& End  12.01.2023 15:01:35
    ENDIF.
    PERFORM getlongtext(zpubform)
    USING 'GRUN' itab-matnr 'MATERIAL'
          CHANGING itab-zwlcms.

*成品物料
    SELECT SINGLE plnbez INTO itab-plnbez FROM afko WHERE aufnr = itab-aufnr.
*成品物料长描述
    IF itab-plnbez IS NOT INITIAL.
      PERFORM getlongtext(zpubform) USING 'GRUN' itab-plnbez 'MATERIAL' CHANGING itab-zcpwlcms.
    ENDIF.
    "消耗吨、消耗米、使用周期
    IF itab-bwart = '261' OR itab-bwart = '262'.
      IF itab-erfme = 'M'.
        itab-xhm = itab-erfmg.
        IF itab-ztm NE 0.
          itab-xhto = itab-erfmg / itab-ztm.
        ENDIF.
      ELSEIF itab-erfme = 'TO'.
        itab-xhm = itab-erfmg * itab-ztm..
        itab-xhto = itab-erfmg.
      ENDIF.
      " 使用周期  25.02.2023 12:15:02 by kkw
      READ TABLE lt_syzq INTO DATA(lw_syzq) WITH KEY matnr = itab-matnr charg = itab-charg BINARY SEARCH.
      IF sy-subrc EQ 0.

      ELSE.
        SELECT SINGLE MAX( budat_mkpf ),MIN( budat_mkpf ) INTO ( @DATA(max_date),@DATA(min_date) )
          FROM mseg
          WHERE matnr = @itab-matnr AND charg = @itab-charg.
        itab-syzq = max_date - min_date.
      ENDIF.
    ENDIF.

    IF itab-zysks IS INITIAL.
      READ TABLE it_316 INTO DATA(wa_316) WITH KEY aufnr = itab-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        tabix = sy-tabix.
        LOOP AT it_316 INTO wa_316 FROM tabix.
          IF  wa_316-aufnr NE itab-aufnr.
            EXIT.
          ENDIF.
          itab-zysks = wa_316-zzk + itab-zysks.

          IF itab-zysks LE 0.
            itab-zysks = itab-zysks + wa_316-zzk.
            itab-zms = itab-zms + wa_316-zzk * wa_316-zcd.
          ENDIF.
*& kkw 移动类型101展示追加316相关信息
          IF itab-bwart = '101' AND wa_316-matnr = itab-matnr AND wa_316-charg = itab-charg.
            itab-del = 'X'.
            CLEAR itab316.
            MOVE-CORRESPONDING itab TO itab316.
            itab316-zysks = wa_316-zzk.
            itab316-zcc_316 = wa_316-zcc.
            itab316-zzk_316 = wa_316-zzk.
            itab316-zcd_316 = wa_316-zcd.
            itab316-zkd_316 = wa_316-zkd.
            itab316-zxs_316 = wa_316-zxs.
            itab316-del = ''.
            itab316-zscbz = wa_316-zscbz.
            APPEND itab316.
          ENDIF.
*& End  16.01.2023 19:36:12
        ENDLOOP.
      ENDIF.
    ENDIF.

    READ TABLE lt_mbew INTO DATA(lw_mbew) WITH KEY werks = itab-werks matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-vprsv = lw_mbew-vprsv.

      IF lw_mbew-vprsv = 'V'.
        itab-kzwi1 = itab-dmbtr.
        IF itab-menge <> 0.
          itab-dmbtr = itab-dmbtr / itab-menge.
        ENDIF.
      ELSEIF lw_mbew-vprsv = 'S'.
        CLEAR itab-dmbtr.
        READ TABLE gt_amount INTO gs_amount WITH  KEY matnr = itab-matnr bwkey = itab-werks pspnr = itab-ps_psp_pnr BINARY SEARCH.
        IF sy-subrc EQ 0.
*            IF gs_amount-lbkum > 0.
*              itab-dmbtr = itab-menge * ( gs_amount-amount / gs_amount-lbkum ) .
*            ENDIF.
          itab-dmbtr = gs_amount-pvprs.
          itab-kzwi1 = itab-dmbtr * itab-menge.
        ENDIF.
      ENDIF.

    ENDIF.
    MODIFY itab.
  ENDLOOP.
  DELETE itab WHERE del = 'X'.
  APPEND LINES OF itab316 TO itab.

ENDFORM.
FORM alvshow.
  PERFORM init_fieldcat(zpubform) TABLES fieldcat USING:
'MBLNR' '物料凭证' '' '' 'X' '' ,
'MJAHR' '凭证年度' '' '' '' '' ,
'AUFNR' '生产订单' '' '' '' '' ,
'VBELN' '销售凭证' '' '' 'X' '' ,
'POSNR' '项目' '' '' '' '' ,
'PLNUM' '计划单号' '' '' '' '' ,
'PLNUMHH' '计划单行号' '' '' '' '' ,
'KUNNR' '客户编码' '' '' '' '' ,
'NAMEK' '客户名称' '' '' '' '' ,
'ZZL1' '品名' '' '' '' '' ,
'ZXHGG' '型号规格' '' '' '' '' ,
'MATNR' '物料号' '' '' '' '' ,
'ZWLCMS' '物料描述' '' '' '' '' ,
'PLNBEZ' '成品物料号' '' '' '' '' ,
'ZCPWLCMS' '成品物料长描述' '' '' '' '' ,
'EBELN' '采购订单' '' '' '' '' ,
'ERFMG' '数量' '' '' '' '' ,
'ERFME' '单位' '' '' '' '' ,
'T006A' '单位描述' '' '' '' '' ,
'CHARG' '批次' '' '' '' '' ,
'BWART' '消耗类型' '' '' '' '' ,
'ZYSKS' '验收块数' '' '' '' '' ,
'ZMS' '米数' '' '' '' '' ,
'ZFLMX' '废料明细' '' '' 'X' '' ,
'USNAM' '创建人' '' '' '' '' ,
'CPUDT' '创建日期' '' '' '' '' ,
'WERKS' '工厂' '' '' '' '' ,
'NAMEW' '工厂名称' '' '' '' '' ,
'LGORT' '库存地' '' '' '' '' ,
'LGOBE' '库存地描述' '' '' '' '' ,
'ARBPL' '工作中心' '' '' '' '' ,
'KTEXT' '工作中心描述' '' '' '' '' ,
'ZNAME' '业务人员' '' '' '' '' ,
'NAME_TEXTC' '业务人员描述' '' '' '' '' ,
'LTRMI' '交货日期' '' '' '' '' ,
'Z01' '自编号' '' '' '' '' ,
'Z02' '卷号' '' '' '' '' ,
'BUDAT' '减账日期' '' '' '' '' ,
'PSPNR' 'WBS要素' '' '' '' '' ,
'POSID' '项目编码' '' '' '' '' ,
'POST1' '项目名称' '' '' '' '' ,
'ZJJLX' '类型' '' '' '' '' ,
'Z03' '资源号' '' '' '' '' ,
'ZQY' '区域' '' '' '' '' ,
'ZCJ' '车间' '' '' '' '' ,
'Z16' '行备注' '' '' '' '' ,
'ZBCKD' '宽度' '' '' '' '' ,
'ZBH' '厚度' '' '' '' '' ,
'ZPCDH' '排产单号' '' '' '' '' ,
'ZPCDHH' '排产单行号' '' '' '' '' ,
'ZCD' '长度' '' '' '' '' ,
'ZXGKDJJYQ' '线管、开洞、加筋要求' '' '' '' '' ,
'ZXCZL' '芯材种类' '' '' '' '' ,
'ZXCCD' '芯材产地' '' '' '' '' ,
'ZXCHD' '芯材厚度' '' '' '' '' ,
'ZXGSL' '线管数量' '' '' '' '' ,
'ZKDSL' '开洞数量' '' '' '' '' ,
'ZJJSL' '加筋数量' '' '' '' '' ,
'ZCKSL' '冲孔数量' '' '' '' '' ,
'ZXGYQ' '线管要求' '' '' '' '' ,
'ZXISHU' '系数' '' '' '' '' ,
'ZTM' '出米率' '' '' '' '' ,
'XHM'     '消耗米  '  ''  '' '' '',
'XHTO'     '消耗吨  '  ''  '' '' '',
'SYZQ'     '使用周期  '  ''  '' '' '',

'ZTLPFS' '门窗投料平方数' '' '' '' '' ,
'MEINS' '投料单位' '' '' '' '' ,
'ZMCCCSL' '门窗产出数量' '' '' '' '' ,
'ZSCBZ' '生产班组' '' '' '' '' ,

'ZCC_316' '316尺寸' '' '' '' '' ,
'ZZK_316' '316块数' '' '' '' '' ,
'ZCD_316' '316长度' '' '' '' '' ,
'ZKD_316' '316宽度' '' '' '' '' ,
'ZXS_316' '316系数' '' '' '' '' ,
'ZNAME10' '配送保管' '' '' '' '' ,
'PS_PSP_PNR' '原料WBS' '' '' '' '' ,
'VPRSV' '原料价格控制' '' '' '' '' ,
'DMBTR' '单价' '' '' '' '' ,
'KZWI1' '金额' '' '' '' '' .
  LOOP AT fieldcat INTO DATA(wa_flcat).
    CASE wa_flcat-fieldname.
      WHEN 'ZFLMX'.
        wa_flcat-checkbox = 'X'.
    ENDCASE.
    MODIFY fieldcat FROM wa_flcat.
  ENDLOOP.
  PERFORM alvfm(zpubform) TABLES itab fieldcat USING '' ''.
ENDFORM.
*FORM SET_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  DATA LT_EXFCODE TYPE TABLE OF SY-UCOMM.
*  SET PF-STATUS 'STANDARD' EXCLUDING LT_EXFCODE.
*ENDFORM. "set_status
FORM top_of_page.
  DATA:it_list_commentary TYPE slis_t_listheader,
       wa_list_commentary TYPE slis_listheader,
       sjtms              TYPE numc10.
  CLEAR:wa_list_commentary,sjtms.
  REFRESH:it_list_commentary.

  sjtms = lines( itab ).
  PERFORM delzero(zpubform) CHANGING sjtms.

  wa_list_commentary-typ = 'S'.
  wa_list_commentary-key = '条目数:'.
  wa_list_commentary-info = sjtms.
  APPEND wa_list_commentary TO it_list_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_list_commentary[]
    EXCEPTIONS
      OTHERS             = 1.
ENDFORM.
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA wa LIKE LINE OF itab.
  CASE r_ucomm.
    WHEN '&IC1'.
      CHECK rs_selfield-tabindex <> 0 . "小计行总计行什么的忽略
      READ TABLE itab INTO wa INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'VBELN'.
          CALL FUNCTION 'ZFM_CALLSODJ'
            EXPORTING
              vbeln  = wa-vbeln
              intype = 'HT'
              mode   = 'S'.
        WHEN 'MBLNR'.
          PERFORM migo(zpubform) USING wa-mblnr wa-mjahr.
        WHEN 'ZFLMX'.
          CHECK wa-zflmx = 'X'.
          PERFORM getflmx USING wa-aufnr wa-werks.
      ENDCASE.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh = 'X'.
ENDFORM.

FORM get001 TABLES t_matnr STRUCTURE ccvx_matnr.
  DATA:BEGIN OF it_objek OCCURS 0,
         objek TYPE kssk-objek,
       END OF it_objek.
  CLEAR:it_objek[],outtab001[].
  DELETE t_matnr WHERE matnr IS INITIAL.
  CHECK t_matnr[] IS NOT INITIAL.
  SORT t_matnr BY matnr.
  LOOP AT t_matnr.
    CLEAR:it_objek.
    it_objek-objek = t_matnr-matnr.
    COLLECT it_objek.
  ENDLOOP.
*取001的特征
  SELECT ksml~clint,
         ksml~posnr,
         ksml~adzhl,
         ksml~klart,
         ksml~imerk AS atinn,
         cabn~atnam ,
         cabn~atfor ,
         cabn~anzst ,
         cabn~anzdz ,
         kssk~objek AS matnr
    INTO TABLE @DATA(it_ksml)
    FROM ksml INNER JOIN cabn ON ksml~imerk = cabn~atinn
              INNER JOIN kssk ON ksml~clint = kssk~clint
                             AND kssk~klart = '001'
    FOR ALL ENTRIES IN @it_objek
    WHERE kssk~objek = @it_objek-objek.
  IF sy-subrc EQ 0.
    SORT it_ksml BY klart matnr atinn.
    SELECT *
      INTO TABLE @DATA(it_ausp)
      FROM ausp
      FOR ALL ENTRIES IN @it_ksml
      WHERE klart = @it_ksml-klart
      AND   objek = @it_ksml-matnr
      AND   atinn = @it_ksml-atinn.
    SORT it_ksml BY matnr atinn.
    LOOP AT it_ausp INTO DATA(wa_ausp).
      CLEAR:outtab001.
      IF wa_ausp-atwrt IS INITIAL.
        CONTINUE.
      ENDIF.
      outtab001-matnr = wa_ausp-objek.
      READ TABLE it_ksml INTO DATA(wa_ksml) WITH KEY matnr = wa_ausp-objek
                                                     atinn = wa_ausp-atinn
                                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        outtab001-atnam = wa_ksml-atnam.
        outtab001-atwrt = wa_ausp-atwrt.
        APPEND outtab001.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT outtab001 BY matnr atnam.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GETFLMX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getflmx USING aufnr werks.
  DATA:rwerks TYPE RANGE OF t001l-werks,
       raufnr TYPE RANGE OF afko-aufnr.
  rwerks = VALUE #( sign = 'I' option = 'EQ' ( low = werks ) ).
  raufnr = VALUE #( sign = 'I' option = 'EQ' ( low = aufnr ) ).
  SUBMIT zppr215
  WITH s_werks IN rwerks
  WITH s_aufnr IN raufnr
  AND RETURN.
ENDFORM.

FORM get_amount USING p_werks.

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
      MOVE s_lfmon  TO  ls_periods-poper.
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
  APPEND LINES OF lt_amount TO gt_amount.
ENDFORM.

FORM build_kalnr_package
TABLES   it_ckmlhd_all    TYPE  gt_ckmlhd_temp
  et_ckmlhd_pack   TYPE  gt_ckmlhd_temp
USING    iv_package_size TYPE i
      iv_last_index   TYPE i
CHANGING cv_pack_index   TYPE i.

  DATA: lv_pck_size    TYPE  i,
        lv_pck_start   TYPE  i,
        ls_ckmlhd_pck2 TYPE  ckmlhd.

  CLEAR et_ckmlhd_pack.
  lv_pck_size   = 0.
  lv_pck_start  = cv_pack_index.

  CHECK lv_pck_start LE iv_last_index.

* build package