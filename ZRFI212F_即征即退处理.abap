*&---------------------------------------------------------------------*
*& Report ZRFI212F
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrfi212f MESSAGE-ID zmsg_gp.
TABLES:ztfi_jzjt_data,
       sscrfields,
       t001w,
       aufk,
       t161t,
       mseg,
       ekko,
       prps,
       cskt,
       bkpf,
       uf05a,
       t685t,
       t001k,
       cepc,
       cepct,
       ekbe,
       marc,
       zvmmpo,
       ckmlhd,
       fagl_segmt,
       ckmlcr,
       t001l,
       t001.
TYPE-POOLS:slis.
DATA:fieldcat TYPE slis_t_fieldcat_alv.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE STRUCTURE ztfi_jzjt_data.
DATA:toppage,
       sel,
       flag    TYPE char1, "X-PO，Y-CKMLCR,Z-凭证
       zwlcms  TYPE string,
       icon    TYPE icon-id,
       msg     TYPE bapi_msg,
       pvprs   TYPE ckmlcr-pvprs,
     END OF itab,
     BEGIN OF it_tab1 OCCURS 0.
       INCLUDE STRUCTURE mseg.
DATA: budat TYPE mkpf-budat,
       cpudt TYPE mkpf-cpudt,
     END OF it_tab1,
     BEGIN OF it_mblnr OCCURS 0,
       mblnr TYPE mblnr,
       mjahr TYPE mjahr,
       zeile TYPE mseg-zeile,
     END OF it_mblnr,
     BEGIN OF it_werks OCCURS 0,
       bukrs TYPE t001-bukrs,
       butxt TYPE t001-butxt,
       werks TYPE werks_d,
       name1 TYPE name1,
     END OF it_werks,
     BEGIN OF it_belnr OCCURS 0.
       INCLUDE STRUCTURE bkpf_key.
DATA: budat TYPE budat,
       msg   TYPE bapi_msg,
     END OF it_belnr,
     it_fipost TYPE TABLE OF zsfms_fipost WITH HEADER LINE,
     it_pspnr  TYPE TABLE OF ckf_wbs WITH HEADER LINE,
     it_kostl  TYPE TABLE OF hri6003 WITH HEADER LINE,
     it_marcs  TYPE TABLE OF atpmp WITH HEADER LINE,
     it_prctr  TYPE TABLE OF hri6002 WITH HEADER LINE,
     i_pctx    TYPE TABLE OF mcha WITH HEADER LINE,
     it_jzjt   TYPE TABLE OF ztfi_jzjt_data WITH HEADER LINE,
     it_jzjt1  TYPE TABLE OF ztfi_jzjt_data WITH HEADER LINE,
     it_jzjt2  TYPE TABLE OF ztfi_jzjt_data WITH HEADER LINE,
     it_jzjt3  TYPE TABLE OF ztfi_jzjt_data WITH HEADER LINE, "移位
     o_pctx    TYPE TABLE OF zsfms_getpctx WITH HEADER LINE,
     it_001    TYPE TABLE OF ztfi_jzjt001 WITH HEADER LINE,
     it_002    TYPE TABLE OF ztfi_jzjt002 WITH HEADER LINE,
     it_003    TYPE TABLE OF ztfi_jzjt003 WITH HEADER LINE,
     it_004    TYPE TABLE OF ztfi_jzjt004 WITH HEADER LINE,
     it_aufnr  TYPE TABLE OF aufnr_s WITH HEADER LINE,
     it_konv   TYPE TABLE OF v_konv_cds WITH HEADER LINE,
     it_ebeln  TYPE TABLE OF ekpo_key WITH HEADER LINE.
DATA:bdcdata   TYPE TABLE OF  bdcdata WITH HEADER LINE,
     messtab   TYPE TABLE OF  bdcmsgcoll WITH HEADER LINE,
     bdcreturn TYPE TABLE OF bapiret2 WITH HEADER LINE.
RANGES:s_kschl FOR konv-kschl,
       s_kschl1 FOR konv-kschl.
DATA:bktxt  TYPE bktxt,
     xblnr  TYPE xblnr,
     budat1 TYPE budat,
     budat  TYPE budat.
DATA:num    TYPE i,
     jxs    TYPE ekpo-menge,
     ebeln  TYPE ebeln,
     ebelp  TYPE ebelp,
     poper  TYPE poper,
     flag   TYPE char1,
     kbetr1 TYPE ze_jexsw,
     kbetr  TYPE konp-kbetr,
     kposn  TYPE konv-kposn,
     tabix  TYPE sy-tabix,
     datef  TYPE sy-datum,
     datet  TYPE sy-datum.
CONSTANTS:red   TYPE icon-id VALUE '@0A@',
          green TYPE icon-id VALUE '@08@'.
DATA:it_zvmmpo LIKE TABLE OF zvmmpo.
DATA:subrc TYPE sy-subrc.
DATA:itab2 LIKE TABLE OF itab WITH HEADER LINE.
DATA:itab3 LIKE TABLE OF itab WITH HEADER LINE.
DATA:itab4 LIKE TABLE OF itab WITH HEADER LINE.
DATA:jzjt_year  TYPE ztfi_jzjt_data-jzjt_year,
     jzjt_poper TYPE ztfi_jzjt_data-jzjt_poper.
DATA:p_save TYPE char1.
TYPES:BEGIN OF ty_ebeln,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
      END OF ty_ebeln.
DATA:gt_ebeln TYPE TABLE OF ty_ebeln WITH KEY ebeln ebelp,
     gs_ebeln LIKE LINE OF gt_ebeln.
TYPES:BEGIN OF ty_matcha,
        matnr TYPE matnr,
        charg TYPE charg_d,
      END OF ty_matcha.
DATA:gt_matcha TYPE TABLE OF ty_matcha WITH KEY matnr charg,
     gs_matcha LIKE LINE OF gt_matcha.
DATA:BEGIN OF gt_216 OCCURS 0,
       matnr TYPE matnr,
       charg TYPE charg_d,
       qmsl  TYPE menge_d,
       meins TYPE meins,
       dj    TYPE verpr,
     END OF gt_216.
DATA:BEGIN OF gt_aufm OCCURS 0,
       bwart      TYPE aufm-bwart,
       matnr      TYPE aufm-matnr,
       charg      TYPE aufm-charg,
       menge      TYPE aufm-menge,
       meins      TYPE aufm-meins,
       aufnr      TYPE aufm-aufnr,
       werks      TYPE aufm-werks,
       mblnr      TYPE aufm-mblnr,
       mjahr      TYPE aufm-mjahr,
       zeile      TYPE aufm-zeile,
       budat      TYPE aufm-budat,
       bldat      TYPE aufm-bldat,
       ps_psp_pnr TYPE aufm-ps_psp_pnr,
     END OF gt_aufm.
TYPES:BEGIN OF ty_aufnr,
        aufnr TYPE aufnr,
        werks TYPE werks_d,
      END OF ty_aufnr.
DATA:gt_aufnr TYPE TABLE OF ty_aufnr WITH KEY aufnr werks,
     gs_aufnr LIKE LINE OF gt_aufnr.
DATA: gt_zcpfl TYPE TABLE OF dd07v WITH HEADER LINE.
TYPES:BEGIN OF ty_ywdtail,
        matnr     TYPE matnr,
        charg     TYPE charg_d,
        value_new TYPE cdpos-value_new,
        value_old TYPE cdpos-value_old,
        udate     TYPE cdhdr-udate,
      END OF ty_ywdtail.
DATA:gt_ywdtail TYPE TABLE OF ty_ywdtail WITH EMPTY KEY,
     gs_ywdtail LIKE LINE OF gt_ywdtail.
DATA: BEGIN OF it_wlcms OCCURS 0,
        matnr TYPE matnr,
        wlcms TYPE string,
      END OF it_wlcms.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t01.
  PARAMETERS:p_bukrs LIKE t001-bukrs MODIF ID m MEMORY ID buk,
             p_month LIKE isellist-month DEFAULT sy-datum+0(6) MODIF ID m,
             p_stgrd LIKE uf05a-stgrd OBLIGATORY DEFAULT 'Z1' MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t02.
  PARAMETERS:p1 RADIOBUTTON GROUP grd1 USER-COMMAND click DEFAULT 'X',
             p2 RADIOBUTTON GROUP grd1,
             p3 RADIOBUTTON GROUP grd1.
*             P_SAVE AS CHECKBOX DEFAULT 'X' MODIF ID M1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE t03.
  SELECT-OPTIONS:s_mblnr FOR mseg-mblnr,
                 s_matnr FOR mseg-matnr.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN FUNCTION KEY :1,2,3,4,5.

LOAD-OF-PROGRAM.
  PERFORM getdomain(zpubform) TABLES gt_zcpfl USING 'ZDMM_ZCPFL'.

AT SELECTION-SCREEN OUTPUT.
  t01 = '筛选条件'.
  t03 = '测试用'.
  %_p_bukrs_%_app_%-text    = '公司代码'.
  %_p_month_%_app_%-text    = '期间'.
  t02 = '功能选择'.
  %_p1_%_app_%-text    = '过账'.
  %_p2_%_app_%-text     = '查询'.
  %_p3_%_app_%-text    = '冲销'.
*  %_P_SAVE_%_APP_%-TEXT    = '更新数据'.

  sscrfields-functxt_01 = '@0J@库存地配置'.
  sscrfields-functxt_02 = '@0J@成本中心配置'.
  sscrfields-functxt_03 = '@0J@采购订单类型配置'.
  sscrfields-functxt_04 = '@0J@税配置'.
  sscrfields-functxt_05 = '@0J@工作中心配置'.
  %_p_stgrd_%_app_%-text = '冲销原因'.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'M'.
        screen-required = 2.
      WHEN 'M1'.
        IF p1 = 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
      WHEN 'M2'.
        IF p3 = 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
    ENDCASE.
*    IF SCREEN-NAME = 'P_SAVE' .
*      SCREEN-INPUT = '0'.     "灰掉，只输出
*    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month.
  PERFORM selmonth(zpubform) CHANGING p_month.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM callprog(zpubform) USING 'ZTFI_JZJT001' 'V'.
    WHEN 'FC02'.
      PERFORM callprog(zpubform) USING 'ZTFI_JZJT002' 'V'.
    WHEN 'FC03'.
      PERFORM callprog(zpubform) USING 'ZTFI_JZJT003' 'V'.
    WHEN 'FC04'.
      PERFORM callprog(zpubform) USING 'ZTFI_JZJT004' 'V'.
    WHEN 'FC05'.
      PERFORM callprog(zpubform) USING 'ZTFI_JZJT005' 'V'.
  ENDCASE.

START-OF-SELECTION.
  IF p_bukrs IS INITIAL
    OR p_month IS INITIAL.
    MESSAGE s000(oo) WITH '公司代码及期间必填' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  IF p1 = 'X'
    AND p_save = 'X'
    AND it_jzjt[] IS NOT INITIAL.
    jzjt_year  = p_month+0(4).
    jzjt_poper = p_month+4(2).
    DELETE FROM ztfi_jzjt_data WHERE jzjt_year = jzjt_year AND jzjt_poper = jzjt_poper.
    COMMIT WORK.
    MODIFY ztfi_jzjt_data FROM TABLE it_jzjt.
    COMMIT WORK.
    MESSAGE s007 .
    IF sy-batch = 'X'.
      EXIT.
    ENDIF.
  ENDIF.
  PERFORM alvshow.


FORM getdata.
  CONCATENATE p_month '01' INTO datef.
  PERFORM getlast(zpubform) USING datef CHANGING datet.
  poper = p_month+4(2).
  SELECT SINGLE *
    FROM t001
  WHERE bukrs = p_bukrs.

  SELECT t001k~bukrs
         t001~butxt
         t001w~werks
         t001w~name1
  INTO TABLE it_werks
  FROM t001k INNER JOIN t001w ON t001k~bwkey = t001w~bwkey
             INNER JOIN t001 ON t001k~bukrs = t001~bukrs
  WHERE t001~bukrs = t001-bukrs.
  CHECK sy-subrc = 0.
  SORT it_werks BY werks.

  CASE 'X'.
    WHEN p1.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE itab
        FROM ztfi_jzjt_data
        WHERE bukrs = p_bukrs
        AND   jzjt_year = p_month+0(4)
        AND   jzjt_poper = poper
        AND   matnr IN s_matnr
      AND   mblnr IN s_mblnr.
      IF sy-subrc NE 0.
        PERFORM getgzdata.
      ENDIF.
    WHEN OTHERS.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE itab
        FROM ztfi_jzjt_data
        WHERE bukrs = p_bukrs
        AND   jzjt_year = p_month+0(4)
        AND   jzjt_poper = poper
        AND   matnr IN s_matnr
      AND   mblnr IN s_mblnr.
  ENDCASE.
  IF p3 = 'X'.
    DELETE itab WHERE belnr IS INITIAL
                   OR belnr = ''.
  ENDIF.
  IF itab[] IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH it_wlcms.
  LOOP AT itab.
    it_wlcms-matnr = itab-matnr.
    COLLECT it_wlcms.
  ENDLOOP.
  PERFORM getlongtextpl(zpubform) TABLES it_wlcms.
  SORT it_wlcms BY matnr.
  LOOP AT itab .
    READ TABLE it_wlcms WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zwlcms = it_wlcms-wlcms.
      MODIFY itab.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM alvshow.
  PERFORM getfieldcat(zpubform) USING 'ZTFI_JZJT_DATA' CHANGING fieldcat.
  PERFORM init_fieldcat(zpubform) TABLES fieldcat USING :
        'ICON' '状态' '' '' '' '',
        'MSG' '消息' '' '' '' '',
        'ZWLCMS' '物料描述' '' '' '' ''.
  LOOP AT fieldcat INTO DATA(wa_field).
    CASE wa_field-fieldname.
      WHEN 'BELNR'.
        wa_field-hotspot = 'X'.
    ENDCASE.
    MODIFY fieldcat FROM wa_field.
  ENDLOOP.
  CHECK fieldcat IS NOT INITIAL.
  PERFORM alvfm(zpubform) TABLES itab fieldcat USING 'X' ''.
ENDFORM.
FORM set_status USING rt_extab TYPE slis_t_extab.
  DATA lt_exfcode TYPE TABLE OF sy-ucomm.
  CASE 'X'.
    WHEN p1.
      APPEND 'BUT02' TO lt_exfcode.
    WHEN p2.
*      APPEND 'BUT01' TO LT_EXFCODE.
    WHEN p3.
      APPEND 'BUT01' TO lt_exfcode.
*      APPEND 'BUT02' TO LT_EXFCODE.
  ENDCASE.
  SET PF-STATUS 'STANDARD' EXCLUDING lt_exfcode.
ENDFORM. "set_status
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
        WHEN 'BELNR'.
          PERFORM fb03(zpubform) USING wa-belnr wa-gjahr wa-bukrs ''.
      ENDCASE.
    WHEN 'BUT01'."过账
      PERFORM but01.
    WHEN 'BUT02'."冲销
      PERFORM but02.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh = 'X'.
ENDFORM.
FORM but01.
  DATA:BEGIN OF it_gz OCCURS 0,
         jzjt_direc TYPE ztfi_jzjt_data-jzjt_direc,
         bukrs      TYPE ztfi_jzjt_data-bukrs,
         mwskz      TYPE ztfi_jzjt_data-mwskz,
         segment    TYPE ztfi_jzjt_data-segment,
         segment_s  TYPE ztfi_jzjt_data-segment,
         mwskz_tz   TYPE ztfi_jzjt_data-mwskz_tz,
         dmbtrtaxr  TYPE ztfi_jzjt_data-dmbtrtaxr,
         netpr      TYPE ztfi_jzjt_data-netpr,
         belnr      TYPE bkpf-belnr,
         gjahr      TYPE bkpf-gjahr,
         budat      TYPE bkpf-budat,
         msg        TYPE bapi_msg,
       END OF it_gz.
  CLEAR:it_gz[],bktxt,budat,xblnr.
  PERFORM getlast(zpubform) USING datef CHANGING budat.
  LOOP AT itab WHERE belnr IS INITIAL.
    CLEAR:it_gz.
    MOVE-CORRESPONDING itab TO it_gz.
    CLEAR:it_gz-belnr,it_gz-gjahr,it_gz-budat,
    it_gz-msg.
    COLLECT it_gz.
  ENDLOOP.
  SORT it_gz BY jzjt_direc mwskz segment segment_s mwskz_tz.
  LOOP AT it_gz.
    CLEAR:it_fipost[],bktxt.
    CLEAR:it_fipost.
    CASE it_gz-jzjt_direc.
      WHEN 'J'.
        bktxt = '非即征即退至即征即退进项税调整'.
        CLEAR:it_fipost.
        it_fipost-segment = it_gz-segment.
        it_fipost-bschl = '40'.
        it_fipost-hkont = '2221010110'.
        it_fipost-hkontsj = '2221010110'.
        it_fipost-xref2 = 'FTOJ'.
        it_fipost-zzfi03 = 'FJ/即征即退'.
        it_fipost-sgtxt = bktxt.
        it_fipost-dmbtr = it_gz-dmbtrtaxr.
        it_fipost-dmbtrsj = it_gz-netpr.
*        IT_FIPOST-AMT_BASE = IT_GZ-NETPR.
        it_fipost-mwskz = it_gz-mwskz_tz.
        APPEND it_fipost.
        CLEAR:it_fipost.
        it_fipost-segment = it_gz-segment_s.
        it_fipost-bschl = '50'.
        it_fipost-hkont = '2221010100'.
        it_fipost-hkontsj = '2221010100'.
        it_fipost-xref2 = 'FTOJ'.
        it_fipost-zzfi03 = 'FJ/即征即退'.
        it_fipost-sgtxt = bktxt.
        it_fipost-dmbtr = it_gz-dmbtrtaxr.
        it_fipost-dmbtrsj = it_gz-netpr.
*        IT_FIPOST-AMT_BASE = IT_GZ-NETPR.
        it_fipost-mwskz = it_gz-mwskz.
        APPEND it_fipost.
      WHEN 'F'.
        bktxt = '即征即退至非即征即退进项税调整'.
        CLEAR:it_fipost.
        it_fipost-segment = it_gz-segment.
        it_fipost-bschl = '50'.
        it_fipost-hkont = '2221010110'.
        it_fipost-hkontsj = '2221010110'.
        it_fipost-xref2 = 'JTOF'.
        it_fipost-zzfi03 = 'JF/即征即退'.
        it_fipost-sgtxt = bktxt.
        it_fipost-dmbtr = it_gz-dmbtrtaxr.
        it_fipost-dmbtrsj = it_gz-netpr.
*        IT_FIPOST-AMT_BASE = IT_GZ-NETPR.
        it_fipost-mwskz = it_gz-mwskz.
        APPEND it_fipost.
        CLEAR:it_fipost.
        it_fipost-segment = it_gz-segment_s.
        it_fipost-bschl = '40'.
        it_fipost-hkont = '2221010100'.
        it_fipost-hkontsj = '2221010100'.
        it_fipost-xref2 = 'JTOF'.
        it_fipost-zzfi03 = 'JF/即征即退'.
        it_fipost-sgtxt = bktxt.
        it_fipost-dmbtr = it_gz-dmbtrtaxr.
        it_fipost-dmbtrsj = it_gz-netpr.
*        IT_FIPOST-AMT_BASE = IT_GZ-NETPR.
        it_fipost-mwskz = it_gz-mwskz_tz.
        APPEND it_fipost.
    ENDCASE.

    PERFORM fb02 CHANGING it_gz-belnr it_gz-msg.

*    CALL FUNCTION 'ZFMS_13_FIPOST'
*      EXPORTING
*        BUKRS = P_BUKRS
*        BKTXT = BKTXT
*        BUDAT = BUDAT
*        BLART = 'ZT'
*        WAERS = 'CNY'
*        BLDAT = BUDAT
*        XBLNR = XBLNR
*      IMPORTING
*        BELNR = IT_GZ-BELNR
*        RTMSG = IT_GZ-MSG
*        GJAHR = IT_GZ-GJAHR
*      TABLES
*        INTAB = IT_FIPOST.
    LOOP AT itab WHERE jzjt_direc = it_gz-jzjt_direc
                    AND mwskz = it_gz-mwskz
                    AND segment = it_gz-segment
                    AND segment_s = it_gz-segment_s
                    AND mwskz_tz = it_gz-mwskz_tz.
      CLEAR:itab-msg.
      itab-msg = it_gz-msg.
      IF it_gz-belnr IS NOT INITIAL.
        itab-icon = green.
        itab-belnr = it_gz-belnr.
        itab-budat = budat.
        itab-jzjt_year = p_month+0(4).
        itab-jzjt_poper = poper.
        itab-gjahr = budat+0(4).
        UPDATE ztfi_jzjt_data
        SET    belnr = itab-belnr
               gjahr = itab-gjahr
               budat = itab-budat
               jzjt_year = itab-jzjt_year
               jzjt_poper = itab-jzjt_poper
        WHERE mblnr = itab-mblnr
        AND   mjahr = itab-mjahr
        AND   zeile = itab-zeile
        AND   kschl = itab-kschl.
      ELSE.
        itab-icon = red.
      ENDIF.
      MODIFY itab.
    ENDLOOP.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.
FORM but02.
  CLEAR:it_belnr[].
  LOOP AT itab WHERE belnr IS NOT INITIAL.
    CLEAR:it_belnr.
    it_belnr-bukrs = itab-bukrs.
    it_belnr-belnr = itab-belnr.
    it_belnr-gjahr = itab-gjahr.
    it_belnr-budat = itab-budat.
    COLLECT it_belnr.
  ENDLOOP.
  SORT it_belnr BY bukrs belnr gjahr.
  LOOP AT it_belnr.
    CLEAR:bkpf.
    SELECT SINGLE *
      FROM bkpf
      WHERE bukrs = it_belnr-bukrs
      AND   belnr = it_belnr-belnr
    AND   gjahr = it_belnr-gjahr.
    IF bkpf-stblg IS INITIAL.
      PERFORM fb08 CHANGING it_belnr-msg.
    ELSE.
      CLEAR:it_belnr-msg.
    ENDIF.
*    PERFORM FB08(ZPUBFORM) USING IT_BELNR-BELNR
*                                 IT_BELNR-GJAHR
*                                 IT_BELNR-BUKRS
*                                 P_STGRD
*                                 IT_BELNR-BUDAT
*                                 CHANGING IT_BELNR-MSG.
    LOOP AT itab WHERE bukrs = it_belnr-bukrs
                   AND belnr = it_belnr-belnr
                   AND gjahr = it_belnr-gjahr.
      CLEAR:itab-msg.
      itab-msg = it_belnr-msg.
      IF itab-msg IS INITIAL.
        itab-icon = green.
        CLEAR:itab-belnr.
      ELSE.
        itab-icon = red.
      ENDIF.
      MODIFY itab.
      UPDATE ztfi_jzjt_data
      SET    belnr = ''
      WHERE mblnr = itab-mblnr
      AND   mjahr = itab-mjahr
      AND   zeile = itab-zeile
      AND   kschl = itab-kschl.
    ENDLOOP.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GETGZDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getgzdata .
  CLEAR:it_jzjt1[],itab2[].
*取物料凭证
  SELECT mseg~*,
         mkpf~budat,
         mkpf~cpudt
    INTO TABLE @it_tab1
    FROM mseg INNER JOIN mkpf ON mseg~mblnr = mkpf~mblnr
                             AND mseg~mjahr = mkpf~mjahr
    WHERE mkpf~budat BETWEEN @datef AND @datet
    AND   mseg~bukrs = @p_bukrs
    AND   mseg~mblnr IN @s_mblnr
    AND   mseg~matnr IN @s_matnr
    AND   ( ( mseg~bwart IN ('101','Z62','Z02')
    AND     mseg~shkzg = 'S' )
    OR    (  mseg~bwart IN ('102','Z61','Z01')
    AND     mseg~shkzg = 'H' ) )
    AND   NOT EXISTS ( SELECT * FROM ztfi_jzjt_data   "已存在的不取
                                     WHERE ztfi_jzjt_data~mjahr = mseg~mjahr AND
                                           ztfi_jzjt_data~mblnr = mseg~mblnr AND
                                           ztfi_jzjt_data~zeile = mseg~zeile AND
  ztfi_jzjt_data~belnr <> '' ).
  DELETE it_tab1 WHERE bwart = '101' AND ( ebeln = '' OR ebeln IS INITIAL ).
  DELETE it_tab1 WHERE bwart = '102' AND ( ebeln = '' OR ebeln IS INITIAL ).

*取EKBE发票
  SELECT *
    INTO TABLE @DATA(it_ekbe)
    FROM ekbe
    FOR ALL ENTRIES IN @it_werks
    WHERE werks = @it_werks-werks
    AND   budat BETWEEN @datef AND @datet
    AND   lfbnr IN @s_mblnr
    AND   matnr IN @s_matnr
    AND   vgabe = '2'
  AND   lfbnr <> ''.
*取物料凭证
  LOOP AT it_ekbe INTO ekbe.
    CLEAR:it_mblnr,gs_ebeln.
    it_mblnr-mblnr = ekbe-lfbnr.
    it_mblnr-mjahr = ekbe-lfgja.
    it_mblnr-zeile = ekbe-lfpos.
    COLLECT: it_mblnr.
    gs_ebeln-ebeln = ekbe-ebeln.
    gs_ebeln-ebelp = ekbe-ebelp.
    COLLECT gs_ebeln INTO gt_ebeln.
  ENDLOOP.
  SELECT
    eban~ebeln,
    eban~ebelp,
    eban~zcpfl
  FROM eban
  INNER JOIN @gt_ebeln AS en ON  en~ebeln = eban~ebeln
                             AND en~ebelp = eban~ebelp
  WHERE eban~zcpfl = '2'
  ORDER BY eban~ebeln,eban~ebelp
  INTO TABLE @DATA(lt_eban).
  IF it_mblnr[] IS NOT INITIAL.
    SORT it_mblnr BY mblnr mjahr zeile.
    SELECT mseg~*,
           mkpf~budat,
           mkpf~cpudt
      APPENDING TABLE @it_tab1
      FROM mseg INNER JOIN mkpf ON mseg~mblnr = mkpf~mblnr
                               AND mseg~mjahr = mkpf~mjahr
      FOR ALL ENTRIES IN @it_mblnr
      WHERE mseg~mblnr = @it_mblnr-mblnr
      AND   mseg~mjahr = @it_mblnr-mjahr
      AND   mseg~zeile = @it_mblnr-zeile
      AND   NOT EXISTS ( SELECT * FROM ztfi_jzjt_data   "已存在的不取
                                     WHERE ztfi_jzjt_data~mjahr = mseg~mjahr AND
                                           ztfi_jzjt_data~mblnr = mseg~mblnr AND
                                           ztfi_jzjt_data~zeile = mseg~zeile AND
    ztfi_jzjt_data~belnr <> '' ).
  ENDIF.
  SORT it_tab1 BY mblnr mjahr zeile.
  DELETE ADJACENT DUPLICATES FROM it_tab1
  COMPARING mblnr mjahr zeile.
  IF it_tab1[] IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  SORT it_ekbe BY lfbnr lfgja lfpos.
  CLEAR:it_mblnr[].
*取基础配置
  SELECT *
    INTO TABLE @DATA(it_t685t)
    FROM t685t
  WHERE spras = @sy-langu.
  SORT it_t685t BY kschl.
  SELECT *
    INTO TABLE @DATA(it_t161t)
    FROM t161t
    WHERE spras = @sy-langu
  AND   bstyp = 'F'.
  SORT it_t161t BY bsart.
  SELECT *
    INTO TABLE @DATA(it_segmt)
    FROM fagl_segmt
  WHERE langu = @sy-langu.
  SORT it_segmt BY segment.
  SELECT *
    INTO TABLE @DATA(it_t001w)
  FROM t001w.
  SORT it_t001w BY werks.
  SELECT *
    INTO TABLE @DATA(it_t001l)
  FROM t001l.
  SORT it_t001l BY werks lgort.
  SELECT * INTO TABLE it_001 FROM ztfi_jzjt001.
  SORT it_001 BY werks lgort.
  SELECT *
    INTO TABLE it_002
    FROM ztfi_jzjt002
  WHERE kokrs = t001-ktopl.
  SORT it_002 BY kostl.
  SELECT * INTO TABLE it_003 FROM ztfi_jzjt003.
  SORT it_003 BY bsart kschl kschl_t.
  SELECT * INTO TABLE it_004 FROM ztfi_jzjt004.
  SORT it_004 BY mwskz.
  LOOP AT it_tab1.
    CLEAR:it_ebeln,it_aufnr,i_pctx,
    it_mblnr,it_prctr,it_marcs,
    it_kostl,it_pspnr.
    it_ebeln-ebeln = it_tab1-ebeln.
    it_ebeln-ebelp = it_tab1-ebelp.
    it_aufnr-aufnr = it_tab1-aufnr.
    i_pctx-matnr = it_tab1-matnr.
    i_pctx-charg = it_tab1-charg.
    it_mblnr-mblnr = it_tab1-mblnr.
    it_mblnr-mjahr = it_tab1-mjahr.
    it_mblnr-zeile = it_tab1-zeile.
    it_marcs-werks = it_tab1-werks.
    it_marcs-matnr = it_tab1-matnr.
    it_kostl-kostl = it_tab1-kostl.
    it_pspnr-pspnr = it_tab1-mat_pspnr.
    COLLECT :it_mblnr,i_pctx,it_aufnr,
    it_ebeln,it_marcs,it_kostl,it_pspnr.
    it_marcs-werks = it_tab1-umwrk.
    it_marcs-matnr = it_tab1-ummat.
    COLLECT it_marcs.
  ENDLOOP.
*取ZTFI_JZJT_DATA表
  IF it_mblnr[] IS NOT INITIAL.
    SORT it_mblnr BY mblnr mjahr zeile.
    SELECT *
      INTO TABLE it_jzjt
      FROM ztfi_jzjt_data
      FOR ALL ENTRIES IN it_mblnr
      WHERE mblnr = it_mblnr-mblnr
      AND   mjahr = it_mblnr-mjahr
    AND   zeile = it_mblnr-zeile.
    SORT it_jzjt BY mblnr mjahr zeile.
  ENDIF.
  DELETE it_pspnr WHERE pspnr IS INITIAL.
  IF it_pspnr[] IS NOT INITIAL.
    SORT it_pspnr BY pspnr.
    SELECT *
      INTO TABLE @DATA(it_prps)
      FROM prps
      FOR ALL ENTRIES IN @it_pspnr
    WHERE pspnr = @it_pspnr-pspnr.
    SORT it_prps BY pspnr.
  ENDIF.
  DELETE it_kostl WHERE kostl IS INITIAL.
  IF it_kostl[] IS NOT INITIAL.
    SORT it_kostl BY kostl.
    SELECT *
      INTO TABLE @DATA(it_cskt)
      FROM cskt
      FOR ALL ENTRIES IN @it_kostl
      WHERE kostl = @it_kostl-kostl
      AND   spras = @sy-langu
    AND   kokrs = @t001-ktopl.
    SORT it_cskt BY kostl.
  ENDIF.
  DELETE it_marcs WHERE werks IS INITIAL
                     OR matnr IS INITIAL.
  IF it_marcs[] IS NOT INITIAL.
    SORT it_marcs BY werks matnr.
    SELECT *
      INTO TABLE @DATA(it_marc)
      FROM marc
      FOR ALL ENTRIES IN @it_marcs
      WHERE werks = @it_marcs-werks
    AND   matnr = @it_marcs-matnr.
    SORT it_marc BY werks matnr.
  ENDIF.
  DELETE it_ebeln WHERE ebeln IS INITIAL
                     OR ebelp IS INITIAL.
  IF it_ebeln[] IS NOT INITIAL.
    SORT it_ebeln BY ebeln ebelp.
    SELECT *
      INTO TABLE it_zvmmpo
      FROM zvmmpo
      FOR ALL ENTRIES IN it_ebeln
      WHERE ebeln = it_ebeln-ebeln
      AND   ebelp = it_ebeln-ebelp
    AND   zvmmpo~mandt = sy-mandt.
    SORT it_zvmmpo BY ebeln ebelp.
*单独取351,352凭证
    SELECT *
      INTO TABLE @DATA(it_mseg)
      FROM mseg
      FOR ALL ENTRIES IN @it_ebeln
      WHERE ebeln = @it_ebeln-ebeln
      AND   ebelp = @it_ebeln-ebelp
      AND   bwart IN ( '351','352' )
      AND   smbln = ''
      AND   NOT EXISTS ( SELECT * FROM m_mbmps   "未被冲销
                                     WHERE m_mbmps~sjahr = mseg~mjahr AND
                                           m_mbmps~smbln = mseg~mblnr AND
    m_mbmps~smblp = mseg~zeile ).
    SORT it_mseg BY ebeln ebelp charg.
    "add hanwq 20230330
    SELECT
       ebeln,
       ebelp,
       ps_psp_pnr
      INTO TABLE @DATA(it_ekkn)
      FROM ekkn
      FOR ALL ENTRIES IN @it_ebeln
      WHERE ebeln = @it_ebeln-ebeln
      AND   ebelp = @it_ebeln-ebelp
    .
    SORT it_ekkn BY ebeln ebelp.

    SELECT
       pspnr,
       posid,
       post1
      INTO TABLE @DATA(it_prps1)
      FROM prps
     FOR ALL ENTRIES IN @it_ekkn
     WHERE pspnr = @it_ekkn-ps_psp_pnr
    .
    SORT it_prps1 BY pspnr.

    SELECT
         prcd_elements~*
      INTO TABLE @DATA(it_pojg)
      FROM prcd_elements
      INNER JOIN ztmmsdhh ON ztmmsdhh~posnr = prcd_elements~kposn
      FOR ALL ENTRIES IN @it_zvmmpo
      WHERE prcd_elements~knumv = @it_zvmmpo-knumv
        AND ztmmsdhh~ebelp      = @it_zvmmpo-ebelp
        AND prcd_elements~kschl IN ( 'ZWVZ' )
        AND prcd_elements~kinak = ''
    .
    SORT it_pojg BY knumv kposn kschl.
    "add hanwq 20230330

  ENDIF.

  DELETE it_aufnr WHERE aufnr IS INITIAL.
  IF it_aufnr[] IS NOT INITIAL.
    SORT it_aufnr BY aufnr.
    SELECT *
      INTO TABLE @DATA(it_aufk)
      FROM aufk
      FOR ALL ENTRIES IN @it_aufnr
    WHERE aufnr = @it_aufnr-aufnr.
    SORT it_aufk BY aufnr.
  ENDIF.

  "ADD HANWQ 20230330 BEGIN
  SELECT *
    INTO TABLE @DATA(it_cepc)
    FROM cepc
  .
  SORT it_cepc BY prctr.
  SELECT *
    INTO TABLE @DATA(it_cepct)
    FROM cepct
  WHERE  spras = @sy-langu.
  SORT it_cepct BY prctr.


  LOOP AT it_ekbe INTO ekbe WHERE bewtp = 'Q' AND mwskz <> 'J0' AND matnr+0(5) = 'E0201'.
    READ TABLE it_tab1 INTO DATA(wa_tab) WITH KEY mblnr = ekbe-lfbnr mjahr = ekbe-lfgja zeile = ekbe-lfpos BINARY SEARCH.
    IF sy-subrc = 0.
*      READ TABLE it_001 WITH KEY werks = wa_tab-werks lgort = wa_tab-lgort BINARY SEARCH.
*      subrc = sy-subrc.
      "READ TABLE IT_002 WITH KEY KOSTL = WA_TAB-KOSTL BINARY SEARCH.
      "IF SY-SUBRC = 0 OR SUBRC = 0 OR ( IT_002[] IS INITIAL AND WA_TAB-KOSTL IS INITIAL  ) .
      READ TABLE lt_eban INTO DATA(ls_eban) WITH KEY ebeln = ekbe-ebeln ebelp = ekbe-ebelp BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE gt_zcpfl WITH KEY domvalue_l = ls_eban-zcpfl BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_zvmmpo INTO DATA(wa_po) WITH KEY ebeln = ekbe-ebeln ebelp = ekbe-ebelp BINARY SEARCH.
          IF sy-subrc = 0.
            it_jzjt1-mjahr = ekbe-gjahr .
            it_jzjt1-mblnr = ekbe-belnr .
            it_jzjt1-zeile = ekbe-buzei .
            it_jzjt1-kschl = 'PBXX' .
            it_jzjt1-jzjt_year =  p_month+0(4).
            it_jzjt1-jzjt_poper =  poper .
            it_jzjt1-jzjt_type = 'REQ' .
            CASE ekbe-shkzg.
              WHEN 'H'.
                it_jzjt1-jzjt_direc = 'F' .
                it_jzjt1-lgort      = '' .
                it_jzjt1-lgober     = '' .
                it_jzjt1-werks      = '' .
                it_jzjt1-name1_r    = '' .
                it_jzjt1-jzjtsign_r = '' .
                it_jzjt1-umlgo = wa_tab-lgort .
                READ TABLE it_t001l INTO t001l WITH KEY werks = wa_tab-werks lgort = wa_tab-lgort BINARY SEARCH.
                IF sy-subrc EQ 0.
                  it_jzjt1-lgobes = t001l-lgobe.
                ENDIF.
                it_jzjt1-umwrk   = 'X' .
                it_jzjt1-name1_s = '' .
                it_jzjt1-jzjtsign_s = 'X' .
              WHEN 'S'.
                it_jzjt1-jzjt_direc = 'J' .
                it_jzjt1-lgort = ls_eban-zcpfl .
                it_jzjt1-lgober = gt_zcpfl-ddtext.
*                it_jzjt1-lgort = wa_tab-lgort .
*                READ TABLE it_t001l INTO t001l WITH KEY werks = itab-werks
*                                                            lgort = itab-lgort
*                                                            BINARY SEARCH.
*                IF sy-subrc EQ 0.
*                  it_jzjt1-lgober = t001l-lgobe.
*                ENDIF.
                it_jzjt1-werks      = ekbe-werks .
                READ TABLE it_t001w INTO t001w WITH KEY werks = ekbe-werks BINARY SEARCH.
                IF sy-subrc EQ 0.
                  it_jzjt1-name1_r = t001w-name1.
                ENDIF.
                it_jzjt1-jzjtsign_r = 'X' .
                it_jzjt1-umlgo   = '' .
                it_jzjt1-lgobes  = '' .
                it_jzjt1-umwrk   = '' .
                it_jzjt1-name1_s = '' .
                it_jzjt1-jzjtsign_s = '' .
            ENDCASE.
            it_jzjt1-jzjt_vtype = '发票' .
            it_jzjt1-bukrs = p_bukrs .
            READ TABLE it_werks WITH KEY bukrs = p_bukrs .
            IF sy-subrc = 0.
              it_jzjt1-butxt = it_werks-butxt .
            ENDIF.
            it_jzjt1-bwart = ekbe-bwart .
            it_jzjt1-ebeln = ekbe-ebeln .
            it_jzjt1-ebelp = ekbe-ebelp .
            it_jzjt1-bsart = wa_po-bsart .
            READ TABLE it_t161t INTO t161t WITH KEY bsart = wa_po-bsart BINARY SEARCH.
            IF sy-subrc EQ 0.
              it_jzjt1-bsartt = t161t-batxt.
            ENDIF.
            it_jzjt1-mwskz = ekbe-mwskz .
            it_jzjt1-budat_mkpf = ekbe-budat .
            it_jzjt1-cpudt_mkpf = ekbe-bldat .
            it_jzjt1-matnr = ekbe-matnr .
            it_jzjt1-charg = ekbe-charg .
            it_jzjt1-waers = ekbe-hswae .
            it_jzjt1-sjahr = ekbe-lfgja .
            it_jzjt1-smbln = ekbe-lfbnr .
            it_jzjt1-smblp = ekbe-lfpos .
            it_jzjt1-menge = ekbe-lsmng .
            it_jzjt1-meins = ekbe-lsmeh .
            it_jzjt1-netpr = ekbe-dmbtr .
            it_jzjt1-kostl = wa_tab-kostl ."成本中心  KOSTL   成本中心
            READ TABLE it_cskt INTO cskt WITH KEY kostl = wa_tab-kostl BINARY SEARCH.
            IF sy-subrc EQ 0.
              it_jzjt1-kostlt = cskt-ktext.
            ENDIF.
            it_jzjt1-prctr = wa_tab-prctr .
            READ TABLE it_cepct INTO cepct WITH KEY prctr = wa_tab-prctr BINARY SEARCH.
            IF sy-subrc EQ 0.
              it_jzjt1-prctrtr = cepct-ktext.
            ENDIF.
            it_jzjt1-prctr_s = mseg-prctr .
            READ TABLE it_cepct INTO cepct WITH KEY prctr = wa_tab-prctr BINARY SEARCH.
            IF sy-subrc EQ 0.
              it_jzjt1-prctrts = cepct-ktext.
            ENDIF.
*            IT_JZJT1-SEGMENT = WA_TAB-SEGMENT .
*            READ TABLE IT_SEGMT INTO FAGL_SEGMT WITH KEY SEGMENT = WA_TAB-SEGMENT BINARY SEARCH.
*            IF SY-SUBRC EQ 0.
*              IT_JZJT1-SEGMTR = FAGL_SEGMT-NAME.
*            ENDIF.
*            IT_JZJT1-SEGMENT_S = WA_TAB-SEGMENT_S .
*            READ TABLE IT_SEGMT INTO FAGL_SEGMT WITH KEY SEGMENT = WA_TAB-SEGMENT_S BINARY SEARCH.
*            IF SY-SUBRC EQ 0.
*              IT_JZJT1-SEGMTS = FAGL_SEGMT-NAME.
*            ENDIF.
            CLEAR jxs.
            READ TABLE it_pojg INTO DATA(wa_pojg) WITH KEY knumv = wa_po-knumv kposn = wa_po-ebelp kschl = 'ZWVZ' BINARY SEARCH.
            IF sy-subrc = 0.
              IF wa_pojg-kpein <> 0  .
                jxs = wa_pojg-kbetr / wa_pojg-kpein.
              ELSE.
                jxs = wa_pojg-kbetr.
              ENDIF.
            ENDIF.
            it_jzjt1-kbetr_tx   = jxs / 100 .
            it_jzjt1-dmbtrall = ekbe-dmbtr * ( 1 + jxs / 100 ) ."总额 DMBTRALL 净值*1+税率
            it_jzjt1-dmbtrtax   = it_jzjt1-dmbtrall - ekbe-dmbtr  ."税额  DMBTRTAX  净值*税率
            IF ekbe-menge NE 0 .
              it_jzjt1-dmbtrpritx = it_jzjt1-dmbtrall / ekbe-menge ."单价(含税)  DMBTRPRITX  总额/数量
              it_jzjt1-dmbtrpri   = ekbe-dmbtr / ekbe-menge       ."单价(不含税) DMBTRPRI  总额/数量
            ENDIF.
            it_jzjt1-charg     = wa_tab-charg.
            it_jzjt1-menge     = ekbe-menge.
            it_jzjt1-meins     = wa_tab-meins.

*调整税码
            PERFORM gettax(zpubform) USING '' it_jzjt1-mwskz
                  CHANGING it_jzjt1-kbetr_tx.
            READ TABLE it_004 WITH KEY mwskz = it_jzjt1-mwskz BINARY SEARCH.
            IF sy-subrc EQ 0.
              it_jzjt1-mwskz_tz = it_004-mwskz_txr.
            ENDIF.

            it_jzjt1-dmbtrtaxr = it_jzjt1-dmbtrtax.
            READ TABLE it_ekkn INTO DATA(wa_ekkn) WITH KEY ebeln = ekbe-ebeln ebelp = ekbe-ebelp BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE it_prps1 INTO DATA(wa_prps1) WITH KEY pspnr = wa_ekkn-ps_psp_pnr BINARY SEARCH.
              IF sy-subrc = 0.
                it_jzjt1-pspnr = wa_prps1-pspnr .
                it_jzjt1-posid = wa_prps1-posid .
                it_jzjt1-post1 = wa_prps1-post1 .
              ENDIF.
            ENDIF.
            it_jzjt1-zdate = sy-datum ."更新日期  ZDATE
            it_jzjt1-zname = sy-uname ."更新人  ZNAME
            it_jzjt1-ztime = sy-uzeit ."更新时间  ZTIME
            APPEND it_jzjt1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  "ADD HANWQ 20230330 END

  CALL FUNCTION 'ZFMS_05_GETPCTX'
    EXPORTING
      atnam  = 'ZCJ'
    TABLES
      intab  = i_pctx
      outtab = o_pctx.
  SORT o_pctx BY matnr charg.

*101,102只保留UB的
  SORT it_tab1 BY matnr charg ebeln ebelp.
  CLEAR:it_ebeln[].
  LOOP AT it_tab1.
    CLEAR:itab,ekbe,it_prctr.
    MOVE-CORRESPONDING it_tab1 TO itab.
    CLEAR:itab-mwskz.
    itab-pspnr = it_tab1-mat_pspnr.
    READ TABLE it_marc INTO marc WITH KEY werks = it_tab1-werks
                                          matnr = it_tab1-matnr
                                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-prctr = marc-prctr.
    ENDIF.
    READ TABLE it_marc INTO marc WITH KEY werks = it_tab1-umwrk
                                          matnr = it_tab1-ummat
                                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-prctr_s = marc-prctr.
    ENDIF.
    READ TABLE it_ekbe INTO ekbe WITH KEY lfbnr = it_tab1-mblnr
                                          lfgja = it_tab1-mjahr
                                          lfpos = it_tab1-zeile
                                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_tab1-dmbtr = ekbe-dmbtr.
    ENDIF.
    it_prctr-prctr = itab-prctr.
    COLLECT it_prctr.
    CLEAR it_prctr.
    it_prctr-prctr = itab-prctr.
    COLLECT it_prctr.
    CASE it_tab1-bwart.
      WHEN '101' OR '102'.
        READ TABLE it_zvmmpo INTO zvmmpo WITH KEY ebeln = it_tab1-ebeln BINARY SEARCH.
        IF sy-subrc EQ 0.
          CASE zvmmpo-bsart.
            WHEN 'UB'.
            WHEN OTHERS.
              DELETE it_tab1.
              CONTINUE.
          ENDCASE.
        ENDIF.
        READ TABLE o_pctx WITH KEY matnr = it_tab1-matnr charg = it_tab1-charg BINARY SEARCH.
        IF sy-subrc = 0 AND ( o_pctx-atwrt = 'Z' OR o_pctx-atwrt = 'L' ).
          itab-lgort = o_pctx-atwrt.
        ELSE.
          DELETE it_tab1.
          CONTINUE.
        ENDIF.
    ENDCASE.
    CASE it_tab1-shkzg.
      WHEN 'S'.
        READ TABLE it_001 WITH KEY werks = it_tab1-werks
                                   lgort = it_tab1-lgort
                                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-jzjtsign_r = 'X'.
        ENDIF.
        READ TABLE it_001 WITH KEY werks = it_tab1-umwrk
                                   lgort = it_tab1-umlgo
                                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-jzjtsign_s = 'X'.
        ENDIF.
        READ TABLE it_002 WITH KEY kostl = it_tab1-kostl BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-jzjtsign_s = 'X'.
        ENDIF.
*根据内部订单找到成本中心
        READ TABLE it_aufk INTO aufk WITH KEY aufnr = it_tab1-aufnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE it_002 WITH KEY kostl = aufk-kostv BINARY SEARCH.
          IF sy-subrc EQ 0.
            itab-jzjtsign_s = 'X'.
          ENDIF.
        ENDIF.
        CASE it_tab1-bwart.
          WHEN '101'.
            READ TABLE it_mseg INTO mseg WITH KEY ebeln = it_tab1-ebeln
                                                  ebelp = it_tab1-ebelp
                                                  charg = it_tab1-charg
                                                  BINARY SEARCH.
            IF sy-subrc EQ 0.
              tabix = sy-tabix.
              LOOP AT it_mseg INTO mseg FROM tabix.
                IF mseg-ebeln NE it_tab1-ebeln
                  OR mseg-ebelp NE it_tab1-ebelp
                  OR mseg-charg NE it_tab1-charg.
                  EXIT.
                ENDIF.
                READ TABLE it_001 WITH KEY werks = mseg-werks
                                           lgort = mseg-lgort
                                           BINARY SEARCH.
                IF sy-subrc EQ 0.
                  itab-jzjtsign_s = 'X'.
                ENDIF.
              ENDLOOP.
            ENDIF.
        ENDCASE.
      WHEN 'H'.
        READ TABLE it_001 WITH KEY werks = it_tab1-werks
                                   lgort = it_tab1-lgort
                                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-jzjtsign_s = 'X'.
        ENDIF.
        READ TABLE it_001 WITH KEY werks = it_tab1-umwrk
                                   lgort = it_tab1-umlgo
                                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-jzjtsign_r = 'X'.
        ENDIF.
        READ TABLE it_002 WITH KEY kostl = it_tab1-kostl BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-jzjtsign_r = 'X'.
        ENDIF.
*根据内部订单找到成本中心
        READ TABLE it_aufk INTO aufk WITH KEY aufnr = it_tab1-aufnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE it_002 WITH KEY kostl = aufk-kostv BINARY SEARCH.
          IF sy-subrc EQ 0.
            itab-jzjtsign_r = 'X'.
          ENDIF.
        ENDIF.
        CASE it_tab1-bwart.
          WHEN '102'.
            READ TABLE it_mseg INTO mseg WITH KEY ebeln = it_tab1-ebeln
                                                  ebelp = it_tab1-ebelp
                                                  charg = it_tab1-charg
                                                  BINARY SEARCH.
            IF sy-subrc EQ 0.
              tabix = sy-tabix.
              LOOP AT it_mseg INTO mseg FROM tabix.
                IF mseg-ebeln NE it_tab1-ebeln
                  OR mseg-ebelp NE it_tab1-ebelp
                  OR mseg-charg NE it_tab1-charg.
                  EXIT.
                ENDIF.
                READ TABLE it_001 WITH KEY werks = mseg-werks
                                           lgort = mseg-lgort
                                           BINARY SEARCH.
                IF sy-subrc EQ 0.
                  itab-jzjtsign_r = 'X'.
                ENDIF.
              ENDLOOP.
            ENDIF.
        ENDCASE.
    ENDCASE.
    IF itab-jzjtsign_r NE itab-jzjtsign_s.
      CLEAR:i_pctx[],i_pctx,
      o_pctx,ebeln,ebelp,ekko,s_kschl[],
      kposn,s_kschl1[],flag.
*获取批次特性
      i_pctx-matnr = itab-matnr.
      i_pctx-charg = itab-charg.
      COLLECT i_pctx.
      CALL FUNCTION 'ZFMS_05_GETPCTX'
        TABLES
          intab  = i_pctx
          outtab = o_pctx.
      SORT o_pctx BY matnr charg atnam.
      IF it_tab1-smbln IS INITIAL."参考凭证为空
        IF it_tab1-charg IS NOT INITIAL.
          READ TABLE o_pctx WITH KEY matnr = it_tab1-matnr
                                     charg = it_tab1-charg
                                     atnam = 'Z61'
                                     BINARY SEARCH.
          IF o_pctx-atwrt NE 'X'."参考凭证为空有批次且非自产
            IF it_tab1-ebeln IS NOT INITIAL.
              ebeln = it_tab1-ebeln.
              ebelp = it_tab1-ebelp.
            ELSE.
              READ TABLE o_pctx WITH KEY matnr = it_tab1-matnr
                                         charg = it_tab1-charg
                                         atnam = 'Z68'
                                         BINARY SEARCH.
              IF sy-subrc EQ 0.
                ebeln = o_pctx-atwrt.
              ENDIF.
              READ TABLE o_pctx WITH KEY matnr = it_tab1-matnr
                                         charg = it_tab1-charg
                                         atnam = 'Z69'
                                         BINARY SEARCH.
              IF sy-subrc EQ 0.
                ebelp = o_pctx-atwrt.
              ENDIF.
              itab-ebeln = ebeln.
              itab-ebelp = ebelp.
            ENDIF.
            kposn = ebelp.
            CLEAR it_ebeln.
            it_ebeln-ebeln = it_tab1-ebeln.
            it_ebeln-ebelp = it_tab1-ebelp.
            COLLECT it_ebeln.
*根据PO取采购订单类型
            SELECT SINGLE *
              FROM ekko
            WHERE ebeln = ebeln.
            LOOP AT it_003 WHERE bsart = ekko-bsart.
              CLEAR:s_kschl.
              s_kschl+0(3) = 'IEQ'.
              s_kschl1+0(3) = 'IEQ'.
              s_kschl-low = it_003-kschl.
              s_kschl1-low = it_003-kschl_t.
              COLLECT :s_kschl1,s_kschl.
            ENDLOOP.
            APPEND LINES OF s_kschl TO s_kschl1.
            IF s_kschl1[] IS NOT INITIAL.
              SELECT *
                INTO TABLE it_konv
                FROM v_konv_cds
                WHERE knumv = ekko-knumv
                AND   kposn = kposn
                AND   kschl IN s_kschl1
              AND   kinak = ''.
              SORT it_konv BY kschl.
              LOOP AT it_konv WHERE kschl IN s_kschl.
                CLEAR:kbetr1.
                kbetr1 = it_konv-kbetr.
                IF it_konv-kpein GT 0.
                  kbetr1 = it_konv-kbetr / it_konv-kpein.
                ENDIF.
                itab-dmbtrall = kbetr1.
                itab-kschl = it_konv-kschl.
                READ TABLE it_003 WITH KEY bsart = ekko-bsart
                                           kschl = itab-kschl
                                           BINARY SEARCH.
                IF sy-subrc EQ 0.
                  itab-kschl_t = it_003-kschl_t.
                  READ TABLE it_konv INTO DATA(konv) WITH KEY kschl = it_003-kschl_t BINARY SEARCH.
                  IF sy-subrc EQ 0.
*根据税率找到税码
                    CLEAR:kbetr,itab-mwskz.
                    kbetr = konv-kbetr * 10.
                    SELECT SINGLE a003~mwskz
                      INTO itab-mwskz
                      FROM a003 INNER JOIN konp ON a003~knumh = konp~knumh
                      WHERE a003~kschl = 'MWVS'
                      AND   a003~aland = 'CN'
                      AND   a003~kappl = 'TX'
                    AND   konp~kbetr = kbetr.
                    IF sy-subrc NE 0.
                      CLEAR:kbetr,itab-mwskz.
                      kbetr = konv-kbetr.
                      SELECT SINGLE a003~mwskz
                        INTO itab-mwskz
                        FROM a003 INNER JOIN konp ON a003~knumh = konp~knumh
                        WHERE a003~kschl = 'MWVS'
                        AND   a003~aland = 'CN'
                        AND   a003~kappl = 'TX'
                      AND   konp~kbetr = kbetr.
                    ENDIF.
                  ENDIF.
                ENDIF.
                IF ekbe IS NOT INITIAL.
                  itab-mwskz = ekbe-mwskz.
                ENDIF.
                itab-flag = 'X'.
                APPEND itab.
              ENDLOOP.
              IF sy-subrc NE 0.
                flag = 'X'.
              ENDIF.
*若采购订单为空，则取CKMLCR
            ELSE.
              flag = 'X'.
            ENDIF.
            CASE it_tab1-bwart.
              WHEN '311' OR '312'.
              WHEN OTHERS.
                IF flag = 'X'.
                  CLEAR:ckmlhd,ckmlcr.
                  SELECT SINGLE *
                    FROM ckmlhd
                    WHERE matnr = it_tab1-matnr
                    AND   bwkey = it_tab1-werks
                  AND   pspnr = it_tab1-mat_pspnr.
                  IF sy-subrc NE 0.
                    SELECT SINGLE *
                      FROM ckmlhd
                      WHERE matnr = it_tab1-matnr
                    AND   bwkey = it_tab1-werks.
                  ENDIF.
                  SELECT SINGLE *
                    FROM ckmlcr
                    WHERE kalnr = ckmlhd-kalnr
                    AND   curtp = '10'
                    AND   bdatj = p_month+0(4)
                  AND   poper = poper.
                  IF sy-subrc EQ 0.
                    itab-pvprs = ckmlcr-pvprs.
                    itab-mwskz = 'J2'.
                  ENDIF.
                  IF ekbe IS NOT INITIAL.
                    itab-mwskz = ekbe-mwskz.
                  ENDIF.
                  itab-flag = 'Y'.
                  APPEND itab.
                ENDIF.
            ENDCASE.
          ELSE."参考凭证为空且有批次自产
            IF it_tab1-umsok = 'Q'.
              CLEAR:ckmlhd,ckmlcr.
              SELECT SINGLE *
                FROM ckmlhd
                WHERE matnr = it_tab1-matnr
                AND   bwkey = it_tab1-werks
              AND   pspnr = it_tab1-mat_pspnr.
              SELECT SINGLE *
                FROM ckmlcr
                WHERE kalnr = ckmlhd-kalnr
                AND   curtp = '10'
                AND   bdatj = p_month+0(4)
              AND   poper = poper.
              IF sy-subrc EQ 0.
                itab-pvprs = ckmlcr-pvprs.
                itab-mwskz = 'J2'.
              ENDIF.
              IF ekbe IS NOT INITIAL.
                itab-mwskz = ekbe-mwskz.
              ENDIF.
              itab-flag = 'Y'.
              APPEND itab.
            ENDIF.
          ENDIF.
        ELSE."批次为空
          itab-mwskz = 'J2'.
          itab-dmbtrall = it_tab1-dmbtr.
          IF ekbe IS NOT INITIAL.
            itab-mwskz = ekbe-mwskz.
          ENDIF.
          itab-flag = 'Z'.
          APPEND itab.
        ENDIF.
*311的也直接取CKMLCR
        CASE it_tab1-bwart.
          WHEN '311' OR '312'.
            CLEAR:ckmlhd,ckmlcr.
            SELECT SINGLE *
              FROM ckmlhd
              WHERE matnr = it_tab1-matnr
              AND   bwkey = it_tab1-werks
            AND   pspnr = it_tab1-mat_pspnr.
            IF sy-subrc NE 0.
              SELECT SINGLE *
                FROM ckmlhd
                WHERE matnr = it_tab1-matnr
              AND   bwkey = it_tab1-werks.
            ENDIF.
            SELECT SINGLE *
              FROM ckmlcr
              WHERE kalnr = ckmlhd-kalnr
              AND   curtp = '10'
              AND   bdatj = p_month+0(4)
            AND   poper = poper.
            IF sy-subrc EQ 0.
              itab-pvprs = ckmlcr-pvprs.
              itab-mwskz = 'J2'.
            ENDIF.
            itab-flag = 'Y'.
            APPEND itab.
        ENDCASE.
      ELSE."若为冲销的,则找到表中原凭证,增加行
        READ TABLE it_jzjt WITH KEY mblnr = it_tab1-smbln
                                    mjahr = it_tab1-sjahr
                                    zeile = it_tab1-smblp
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          tabix = sy-tabix.
          LOOP AT it_jzjt FROM tabix.
            IF it_jzjt-mblnr NE it_tab1-smbln
              OR it_jzjt-mjahr NE it_tab1-sjahr
              OR it_jzjt-zeile NE it_tab1-smblp.
              EXIT.
            ENDIF.
            CLEAR:itab.
            MOVE-CORRESPONDING it_jzjt TO itab.
            itab-mblnr = it_tab1-mblnr.
            itab-mjahr = it_tab1-mjahr.
            itab-zeile = it_tab1-zeile.
            APPEND itab.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT itab BY mblnr mjahr zeile kschl.
  DELETE ADJACENT DUPLICATES FROM itab
  COMPARING mblnr mjahr zeile kschl.

*  DELETE IT_PRCTR WHERE PRCTR IS INITIAL.
*  IF IT_PRCTR[] IS NOT INITIAL.
*    SORT IT_PRCTR BY PRCTR.
*    SELECT *
*      INTO TABLE @DATA(IT_CEPC)
*      FROM CEPC
*      FOR ALL ENTRIES IN @IT_PRCTR
*      WHERE PRCTR = @IT_PRCTR-PRCTR
*      AND   KOKRS = @T001-KTOPL.
*    SORT IT_CEPC BY PRCTR.
*    SELECT *
*      INTO TABLE @DATA(IT_CEPCT)
*      FROM CEPCT
*      FOR ALL ENTRIES IN @IT_PRCTR
*      WHERE PRCTR = @IT_PRCTR-PRCTR
*      AND   KOKRS = @T001-KTOPL
*      AND   SPRAS = @SY-LANGU.
*    SORT IT_CEPCT BY PRCTR.
*  ENDIF.
  DELETE it_ebeln WHERE ebeln IS INITIAL
                     OR ebelp IS INITIAL.
  IF it_ebeln[] IS NOT INITIAL.
    SORT it_ebeln BY ebeln ebelp.
    SELECT *
      APPENDING TABLE it_zvmmpo
      FROM zvmmpo
      FOR ALL ENTRIES IN it_ebeln
      WHERE ebeln = it_ebeln-ebeln
      AND   ebelp = it_ebeln-ebelp
    AND   zvmmpo~mandt = sy-mandt.
    SORT it_zvmmpo BY ebeln.
  ENDIF.

  CLEAR:it_jzjt[].

  LOOP AT itab.
    CLEAR:it_jzjt.
    itab-butxt = t001-butxt.
    itab-jzjt_year = p_month+0(4).
    itab-jzjt_poper = poper.
    CASE 'X'.
      WHEN itab-jzjtsign_r.
        itab-jzjt_direc = 'J'.
      WHEN itab-jzjtsign_s.
        itab-jzjt_direc = 'F'.
    ENDCASE.
    READ TABLE it_prps INTO prps WITH KEY pspnr = itab-pspnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-posid = prps-posid.
      itab-post1 = prps-post1.
    ENDIF.
    READ TABLE it_cskt INTO cskt WITH KEY kostl = itab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-kostlt = cskt-ktext.
    ENDIF.
    READ TABLE it_t001l INTO t001l WITH KEY werks = itab-werks
                                            lgort = itab-lgort
                                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-lgober = t001l-lgobe.
    ENDIF.
    READ TABLE it_t001l INTO t001l WITH KEY werks = itab-umwrk
                                            lgort = itab-umlgo
                                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-lgobes = t001l-lgobe.
    ENDIF.
    READ TABLE it_t685t INTO t685t WITH KEY kschl = itab-kschl BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-kschlt = t685t-vtext.
    ENDIF.
    READ TABLE it_t685t INTO t685t WITH KEY kschl = itab-kschl_t BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-kschl_tt = t685t-vtext.
    ENDIF.
    READ TABLE it_zvmmpo INTO zvmmpo WITH KEY ebeln = itab-ebeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-bsart = zvmmpo-bsart.
    ENDIF.
    READ TABLE it_t161t INTO t161t WITH KEY bsart = itab-bsart BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-bsartt = t161t-batxt.
    ENDIF.
    READ TABLE it_cepc INTO cepc WITH KEY prctr = itab-prctr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-segment = cepc-segment.
    ENDIF.
    READ TABLE it_cepc INTO cepc WITH KEY prctr = itab-prctr_s BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-segment_s = cepc-segment.
    ENDIF.
    READ TABLE it_segmt INTO fagl_segmt WITH KEY segment = itab-segment BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-segmtr = fagl_segmt-name.
    ENDIF.
    READ TABLE it_segmt INTO fagl_segmt WITH KEY segment = itab-segment_s BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-segmts = fagl_segmt-name.
    ENDIF.
    READ TABLE it_t001w INTO t001w WITH KEY werks = itab-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-name1_r = t001w-name1.
    ENDIF.
    READ TABLE it_t001w INTO t001w WITH KEY werks = itab-umwrk BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-name1_s = t001w-name1.
    ENDIF.
    READ TABLE it_cepct INTO cepct WITH KEY prctr = itab-prctr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-prctrtr = cepct-ktext.
    ENDIF.
    READ TABLE it_cepct INTO cepct WITH KEY prctr = itab-prctr_s BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-prctrts = cepct-ktext.
    ENDIF.
    READ TABLE it_aufk INTO aufk WITH KEY aufnr = itab-aufnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-aufnrt = aufk-ktext.
    ENDIF.

*调整税码
    PERFORM gettax(zpubform) USING '' itab-mwskz
          CHANGING itab-kbetr_tx.
    READ TABLE it_004 WITH KEY mwskz = itab-mwskz BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-mwskz_tz = it_004-mwskz_txr.
    ENDIF.
    CASE itab-flag.
      WHEN 'Y'.
        itab-dmbtrpri = itab-pvprs.
        itab-dmbtrpritx = itab-pvprs * ( 1 + itab-kbetr_tx ).
        itab-dmbtrall = itab-dmbtrpritx * itab-menge.
        itab-netpr = itab-dmbtrpri * itab-menge.
        itab-dmbtrtax = itab-dmbtrall - itab-netpr.
      WHEN OTHERS.
*净值-税额-差额的计算
        itab-netpr = itab-dmbtrall / ( 1 + itab-kbetr_tx ).
        itab-dmbtrtax = itab-dmbtrall - itab-netpr.
        IF itab-menge GT 0.
          itab-dmbtrpritx = itab-dmbtrall / itab-menge.
          itab-dmbtrpri = itab-netpr / itab-menge.
        ENDIF.
    ENDCASE.
    itab-dmbtrtaxr = itab-dmbtrtax.
    IF itab-netpr IS INITIAL.
      DELETE itab.
      CONTINUE.
    ENDIF.
    MODIFY itab.
    MOVE-CORRESPONDING itab TO it_jzjt.
    it_jzjt-zdate = sy-datum.
    it_jzjt-ztime = sy-uzeit.
    it_jzjt-zname = sy-uname.
    APPEND it_jzjt.
  ENDLOOP.
  "add hanwq 20230430 begin
  TYPES:BEGIN OF zsfhjzjtbl,
          vbeln TYPE vbeln,
          jz    TYPE vbap-netwr,
          zje   TYPE vbap-netwr,
          bl    TYPE p DECIMALS 5,
        END OF zsfhjzjtbl.
  DATA:it_vbeln TYPE TABLE OF zsfhjzjtbl WITH HEADER LINE.
  CLEAR :it_vbeln[].

  SELECT
     ztmm226~zdh,
     ztmm226~bukrs,
     ztmm226~werks,
     ztmm226~lgort,
     ztmm226~zfylx,
     ztmm226~zywdh,
     ztmm226~zzl,
     ztmm226~zwldj,
     ztmm226~zysje,
     ztmm226~mwskz,
     ztmm226~zsl,
     ztmm226~zbhsdj,
     ztmm226~zse,
     ztmm226~zbhsje,
     ztmm226~zfpno,
     ztmm226~fisc_year,
     ztmm226~zzdrq,
     ztmm226~zxmh
    FROM ztmm226
    INNER JOIN rbkp ON rbkp~belnr = ztmm226~zfpno AND rbkp~gjahr = ztmm226~fisc_year
    WHERE rbkp~budat BETWEEN @datef AND @datet
    AND   rbkp~bukrs = @p_bukrs
    AND   rbkp~belnr IN @s_mblnr
    AND   rbkp~stblg = ''
    AND   ztmm226~zfylx = 'XSYF'
    ORDER BY ztmm226~zdh
    INTO TABLE @DATA(it_ztmm226)
  .

  LOOP AT it_ztmm226 INTO DATA(ztmm226).
    CLEAR:it_vbeln.
    it_vbeln-vbeln = ztmm226-zywdh.
    COLLECT it_vbeln.
  ENDLOOP.
  IF it_vbeln[] IS NOT INITIAL.
    SORT it_vbeln BY vbeln.

    SELECT
       vbap~vbeln,
       vbap~posnr,
       vbap~matnr,
       vbap~matkl,
       vbap~arktx,
       vbap~netwr,
       vbap~waerk,
       vbap~mvgr1,
       vbap~werks,
       vbap~lgort
      INTO TABLE @DATA(it_vbap)
      FROM vbap
*      INNER JOIN ZFICO_CONF_015 ON ZFICO_CONF_015~WERKS = VBAP~WERKS AND ZFICO_CONF_015~LGORT = VBAP~LGORT
      FOR ALL ENTRIES IN @it_vbeln
      WHERE vbap~vbeln = @it_vbeln-vbeln
    .
    SORT it_vbap BY vbeln posnr.

    SELECT
       werks,
       lgort
      FROM zfico_conf_015
      ORDER BY werks,lgort
      INTO TABLE @DATA(it_conf015)
    .

    LOOP AT it_vbap INTO DATA(vbap).
      CLEAR it_vbeln.
      READ TABLE it_conf015 INTO DATA(conf015) WITH KEY werks = vbap-werks lgort = vbap-lgort BINARY SEARCH.
      IF sy-subrc = 0.
        it_vbeln-vbeln = vbap-vbeln.
        it_vbeln-jz  = 0.
        it_vbeln-zje   = vbap-netwr.
        COLLECT it_vbeln.
      ELSE.
        READ TABLE it_vbeln WITH KEY vbeln = vbap-vbeln BINARY SEARCH.
        IF sy-subrc = 0.
          CASE vbap-mvgr1.
            WHEN 'FJZ'.
              it_vbeln-vbeln = vbap-vbeln.
              it_vbeln-jz  = 0.
              it_vbeln-zje = vbap-netwr.
              COLLECT it_vbeln.
            WHEN OTHERS.
              it_vbeln-vbeln = vbap-vbeln.
              it_vbeln-jz  = vbap-netwr.
              it_vbeln-zje = vbap-netwr.
              COLLECT it_vbeln.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDLOOP.
    LOOP AT it_vbeln.
      it_vbeln-bl = it_vbeln-jz / it_vbeln-zje.
      MODIFY it_vbeln.
    ENDLOOP.

    SELECT
      DISTINCT werks , prctr
      FROM ztmm_011
      ORDER BY werks
      INTO TABLE @DATA(it_ztmm011)
    .

    DELETE it_vbeln WHERE bl = 0.

    IF it_vbeln[] IS NOT INITIAL.

      SORT it_vbeln BY vbeln.

      LOOP AT it_ztmm226 INTO ztmm226.
        READ TABLE it_vbeln WITH KEY vbeln = ztmm226-zywdh BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR it_jzjt2.

          it_jzjt2-mjahr  = ztmm226-fisc_year.
          it_jzjt2-mblnr  = ztmm226-zdh.
          it_jzjt2-zeile  = '1'.
          it_jzjt2-kschl  = 'XSYF'.
          it_jzjt2-kschlt = '销售运费'.
          it_jzjt2-jzjt_year  = ztmm226-fisc_year.
          it_jzjt2-jzjt_poper = poper.
          it_jzjt2-jzjt_type  = 'RYF'.
          IF ztmm226-zse > 0.
            it_jzjt2-jzjt_direc = 'J'.
          ELSE.
            it_jzjt2-jzjt_direc = 'F'.
          ENDIF.
          it_jzjt2-jzjt_vtype = '销售运费税额按比例调整'.
          it_jzjt2-bukrs   = ztmm226-bukrs.
          READ TABLE it_werks WITH KEY bukrs = p_bukrs .
          IF sy-subrc = 0.
            it_jzjt2-butxt = it_werks-butxt .
          ENDIF.
          it_jzjt2-ebeln   = ztmm226-zywdh.
          it_jzjt2-ebelp   = '0'.
          it_jzjt2-mwskz   = ztmm226-mwskz.
          it_jzjt2-budat_mkpf = ztmm226-zzdrq.
          it_jzjt2-cpudt_mkpf = ztmm226-zzdrq.
          it_jzjt2-waers     = 'CNY'.
          it_jzjt2-sjahr     = ztmm226-fisc_year.
          it_jzjt2-smbln     = ztmm226-zdh.
          it_jzjt2-smblp     = '0'.
          it_jzjt2-dmbtrtax = abs( ztmm226-zse * it_vbeln-bl ).
          it_jzjt2-lgort     = ztmm226-lgort.
          it_jzjt2-umlgo     = ztmm226-lgort.
          READ TABLE it_t001l INTO t001l WITH KEY werks = itab-umwrk lgort = itab-umlgo BINARY SEARCH.
          IF sy-subrc EQ 0.
            it_jzjt2-lgober  = t001l-lgobe.
            it_jzjt2-lgobes  = t001l-lgobe.
          ENDIF.
          it_jzjt2-werks     = ztmm226-werks.
          it_jzjt2-umwrk     = ztmm226-werks.
          READ TABLE it_t001w INTO t001w WITH KEY werks = ztmm226-werks BINARY SEARCH.
          IF sy-subrc EQ 0.
            it_jzjt2-name1_r = t001w-name1.
            it_jzjt2-name1_s = t001w-name1.
          ENDIF.
          it_jzjt2-jzjtsign_r = 'X'.

          it_jzjt2-jzjtsign_s = '空'.
          READ TABLE it_ztmm011 INTO DATA(ztmm011) WITH KEY werks = ztmm226-werks BINARY SEARCH.
          IF sy-subrc = 0.
            it_jzjt2-prctr_s = ztmm011-prctr.
            READ TABLE it_cepct INTO cepct WITH KEY prctr = it_jzjt2-prctr_s BINARY SEARCH.
            IF sy-subrc EQ 0.
              it_jzjt2-prctrts = cepct-ktext.
            ENDIF.
          ENDIF.
          it_jzjt2-dmbtrtaxr = it_jzjt2-dmbtrtax.
          it_jzjt2-posid = ztmm226-zxmh.

*调整税码
          PERFORM gettax(zpubform) USING '' it_jzjt2-mwskz
                CHANGING it_jzjt2-kbetr_tx.
          READ TABLE it_004 WITH KEY mwskz = it_jzjt2-mwskz BINARY SEARCH.
          IF sy-subrc EQ 0.
            it_jzjt2-mwskz_tz = it_004-mwskz_txr.
          ENDIF.
          IF it_jzjt2-kbetr_tx NE 0.
            it_jzjt2-netpr      = it_jzjt2-dmbtrtax / it_jzjt2-kbetr_tx.
          ENDIF.
          it_jzjt2-dmbtrall   = it_jzjt2-netpr + it_jzjt2-dmbtrtax ."总额

          it_jzjt2-zdate = sy-datum .
          it_jzjt2-zname = sy-uname .
          it_jzjt2-ztime = sy-uzeit .
          APPEND it_jzjt2.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

  "add hanwq 20230430 end

  " 移位  2024-11-29 11:45:58 by lmw
  REFRESH: gt_ywdtail,it_jzjt3,itab4.
  CLEAR:gs_ywdtail.
  PERFORM getycj CHANGING gt_ywdtail.
  REFRESH gt_matcha.
  LOOP AT gt_ywdtail INTO gs_ywdtail.
    gs_matcha-matnr = gs_ywdtail-matnr.
    gs_matcha-charg = gs_ywdtail-charg.
    COLLECT gs_matcha INTO gt_matcha.
  ENDLOOP.
  PERFORM getzfi216 USING    gt_matcha
                    CHANGING gt_216[].

  LOOP AT gt_ywdtail INTO gs_ywdtail.
    CLEAR:it_jzjt3.
    it_jzjt3-matnr = gs_ywdtail-matnr.
    it_jzjt3-charg = gs_ywdtail-charg.
    it_jzjt3-mjahr = p_month+0(4).
    it_jzjt3-zeile = '1'.
    it_jzjt3-jzjt_year = p_month+0(4).
    it_jzjt3-jzjt_poper = poper.
    it_jzjt3-jzjt_vtype = '移位'.
    IF gs_ywdtail-value_old = 'J'.
      it_jzjt3-jzjt_direc = 'J'.
    ELSE.
      it_jzjt3-jzjtsign_r  = 'X'.
      it_jzjt3-jzjtsign_s   = 'X'.
      it_jzjt3-jzjt_direc = 'F'.
    ENDIF.
    it_jzjt3-lgort = gs_ywdtail-value_new.
    it_jzjt3-umlgo  = gs_ywdtail-value_old.
    it_jzjt3-budat = gs_ywdtail-udate.
    it_jzjt3-budat_mkpf = gs_ywdtail-udate.
    it_jzjt3-cpudt_mkpf = gs_ywdtail-udate.
    it_jzjt3-mwskz = 'J2'.
    it_jzjt3-kbetr_tx = '0.13'.
    READ TABLE gt_216 WITH KEY matnr = gs_ywdtail-matnr charg = gs_ywdtail-charg BINARY SEARCH.
    IF sy-subrc = 0.
      it_jzjt3-menge = gt_216-qmsl.
      it_jzjt3-meins = gt_216-meins.
      it_jzjt3-dmbtrpri = gt_216-dj.
      it_jzjt3-dmbtrpritx = it_jzjt3-dmbtrpri * ( 1 + it_jzjt3-kbetr_tx ).
      it_jzjt3-dmbtrall = it_jzjt3-dmbtrpritx * it_jzjt3-menge.
      it_jzjt3-netpr = it_jzjt3-dmbtrpri * it_jzjt3-menge.
      it_jzjt3-dmbtrtax = it_jzjt3-dmbtrall - it_jzjt3-netpr.
    ENDIF.
    APPEND it_jzjt3.
  ENDLOOP.

  " 工单投料  2024-11-30 09:09:52 by lmw
  SELECT
    fm~mblnr,
    fm~mjahr,
    fm~zeile,
    fm~budat,
    fm~bldat,
    fm~bwart,
    fm~matnr,
    fm~charg,
    CASE WHEN fm~shkzg = 'H' THEN 0 - fm~menge ELSE fm~menge END AS menge,
    fm~meins,
    fm~aufnr,
    fm~werks,
    fm~ps_psp_pnr
  FROM aufm AS fm
  INNER JOIN @gt_matcha AS m ON  fm~matnr = m~matnr
                             AND fm~charg = m~charg
  WHERE fm~bwart IN ( '261','262' )
    AND fm~werks = '3060'
  INTO CORRESPONDING FIELDS OF TABLE @gt_aufm.
  REFRESH gt_aufnr.
  LOOP AT gt_aufm.
    IF gt_aufm-aufnr IS NOT INITIAL AND gt_aufm-werks IS NOT INITIAL.
      gs_aufnr-aufnr = gt_aufm-aufnr.
      gs_aufnr-werks = gt_aufm-werks.
      COLLECT gs_aufnr INTO gt_aufnr.
    ENDIF.
  ENDLOOP.

  SELECT
    z206~aufnr,
    z206~werks,
    z206~arbpl,
    z5~ktext,
    z5~zsx
  FROM ztpp_206 AS z206
  INNER JOIN @gt_aufnr AS au ON  au~aufnr = z206~aufnr
                             AND au~werks = z206~werks
  INNER JOIN ztfi_jzjt005 AS z5 ON z5~arbpl = z206~arbpl
  GROUP BY z206~aufnr,z206~werks,z206~arbpl,z5~ktext,z5~zsx
  ORDER BY z206~aufnr,z206~werks
  INTO TABLE @DATA(lt_arbpl).
  LOOP AT gt_aufm.
    CLEAR it_jzjt3.
    it_jzjt3-mjahr = gt_aufm-mjahr.
    it_jzjt3-mblnr = gt_aufm-mblnr.
    it_jzjt3-zeile = gt_aufm-zeile.
    it_jzjt3-jzjt_year = p_month+0(4).
    it_jzjt3-jzjt_poper = poper.
    it_jzjt3-jzjt_vtype = '投料'.
    it_jzjt3-budat = gt_aufm-budat.
    it_jzjt3-budat_mkpf = gt_aufm-budat.
    it_jzjt3-cpudt_mkpf = gt_aufm-bldat.
    it_jzjt3-matnr = gt_aufm-matnr.
    it_jzjt3-charg = gt_aufm-charg.
    it_jzjt3-werks = gt_aufm-werks.
    it_jzjt3-bwart = gt_aufm-bwart.
    it_jzjt3-menge = gt_aufm-menge.
    it_jzjt3-meins = gt_aufm-meins.
    it_jzjt3-mwskz = 'J2'.
    it_jzjt3-kbetr_tx = '0.13'.
    CLEAR:ckmlhd,ckmlcr.
    SELECT SINGLE *
      FROM ckmlhd
      WHERE matnr = gt_aufm-matnr
        AND bwkey = gt_aufm-werks
        AND pspnr = gt_aufm-ps_psp_pnr.
    IF sy-subrc NE 0.
      SELECT SINGLE *
        FROM ckmlhd
        WHERE matnr = gt_aufm-matnr
          AND bwkey = gt_aufm-werks.
    ENDIF.
    SELECT SINGLE *
      FROM ckmlcr
      WHERE kalnr = ckmlhd-kalnr
      AND   curtp = '10'
      AND   bdatj = p_month+0(4)
      AND   poper = poper.
    IF sy-subrc EQ 0.
      it_jzjt3-dmbtrpri = ckmlcr-pvprs.
      it_jzjt3-dmbtrpritx = it_jzjt3-dmbtrpri * ( 1 + it_jzjt3-kbetr_tx ).
      it_jzjt3-dmbtrall = it_jzjt3-dmbtrpritx * it_jzjt3-menge.
      it_jzjt3-netpr = it_jzjt3-dmbtrpri * it_jzjt3-menge.
      it_jzjt3-dmbtrtax = it_jzjt3-dmbtrall - it_jzjt3-netpr.
    ENDIF.
    READ TABLE lt_arbpl INTO DATA(ls_arbpl) WITH KEY aufnr = gt_aufm-aufnr werks = gt_aufm-werks BINARY SEARCH.
    IF sy-subrc = 0.
      it_jzjt3-lgort = ls_arbpl-zsx.
    ENDIF.
    READ TABLE gt_ywdtail INTO gs_ywdtail WITH KEY matnr = gt_aufm-matnr charg = gt_aufm-charg.
    IF sy-subrc = 0.
      it_jzjt3-umlgo = gs_ywdtail-value_new.
      IF it_jzjt3-umlgo = 'J'.
        it_jzjt3-jzjt_direc = 'J'.
      ELSE.
        it_jzjt3-jzjt_direc = 'F'.
      ENDIF.
    ENDIF.
    IF ( it_jzjt3-umlgo = 'J' AND ( it_jzjt3-lgort = 'F' OR it_jzjt3-lgort = 'Z' ) )
       OR ( it_jzjt3-lgort = 'J' AND ( it_jzjt3-umlgo = 'F' OR it_jzjt3-umlgo = 'Z' ) ).
      APPEND it_jzjt3.
    ENDIF.
  ENDLOOP.


  APPEND LINES OF it_jzjt1[] TO it_jzjt[].
  APPEND LINES OF it_jzjt2[] TO it_jzjt[].
  APPEND LINES OF it_jzjt3[] TO it_jzjt[].
  itab2[]  = CORRESPONDING #( it_jzjt1[] ).
  itab3[]  = CORRESPONDING #( it_jzjt2[] ).
  itab4[]  = CORRESPONDING #( it_jzjt3[] ).
  APPEND LINES OF itab2[] TO itab[].
  APPEND LINES OF itab3[] TO itab[].
  APPEND LINES OF itab4[] TO itab[].


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FB02
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fb02 CHANGING p_belnr p_msg.
  DATA:datec   TYPE char10,
       dmbtrc1 TYPE char20,
       dmbtrc2 TYPE char20.
  CLEAR:bdcdata[],bdcreturn[],datec,it_fipost,dmbtrc1,
  dmbtrc2,p_belnr,p_msg.
  WRITE budat TO datec.
  READ TABLE it_fipost INTO DATA(wa_fipost1) INDEX 1.
  WRITE wa_fipost1-dmbtr TO dmbtrc1.
  WRITE wa_fipost1-dmbtrsj TO dmbtrc2.
  PERFORM delqfw(zpubform) CHANGING:dmbtrc1,dmbtrc2.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPMF05A' '0100'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                        'RF05A-NEWKO'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                         '/00'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BKPF-BLDAT'
                                                         datec.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BKPF-BLART'
                                                            'ZT'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BKPF-BUKRS'
                                                         p_bukrs.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BKPF-BUDAT'
                                                         datec.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BKPF-MONAT'
                                                         budat+5(1).
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BKPF-WAERS'
                                                         'CNY'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'RF05A-NEWBS'
                                                         wa_fipost1-bschl.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'RF05A-NEWKO'
                                                         wa_fipost1-hkont.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPMF05A' '0312'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                        'RF05A-NEWKO'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                         '/00'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-WRBTR'
                                                         dmbtrc1.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-FWBAS'
                                                         dmbtrc2.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-MWSKZ'
                                                          wa_fipost1-mwskz.
  CLEAR:it_fipost,dmbtrc1,dmbtrc2.
  READ TABLE it_fipost INTO DATA(wa_fipost2) INDEX 2.
  WRITE wa_fipost2-dmbtr TO dmbtrc1.
  WRITE wa_fipost2-dmbtrsj TO dmbtrc2.
  PERFORM delqfw(zpubform) CHANGING:dmbtrc1,dmbtrc2.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'RF05A-NEWBS'
                                                         wa_fipost2-bschl.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'RF05A-NEWKO'
                                                         wa_fipost2-hkont.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'DKACB-FMORE'
                                                          'X'.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPLKACB' '0002'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                         'COBL-SEGMENT'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                         '=ENTE'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'COBL-SEGMENT'
                                                         wa_fipost1-segment.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPMF05A' '0312'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                         'RF05A-NEWBS'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                          '/00'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-WRBTR'
                                                         dmbtrc1.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-FWBAS'
                                                         dmbtrc2.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-MWSKZ'
                                                         wa_fipost2-mwskz.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'DKACB-FMORE'
                                                          'X'.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPLKACB' '0002'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                         'COBL-SEGMENT'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                         '=ENTE'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'COBL-SEGMENT'
                                                         wa_fipost2-segment.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPMF05A' '0312'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                        'BSEG-WRBTR'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                         '=BU'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-WRBTR'
                                                         dmbtrc1.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-FWBAS'
                                                         dmbtrc2.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSEG-MWSKZ'
                                                         wa_fipost2-mwskz.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'DKACB-FMORE'
                                                          'X'.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPLKACB' '0002'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                         'COBL-SEGMENT'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                         '=ENTE'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'COBL-SEGMENT'
                                                         wa_fipost2-segment.

  PERFORM bdcfm(zpubform) TABLES bdcdata bdcreturn USING 'F-02' 'N'.
  READ TABLE bdcreturn WITH KEY type = 'S'
                                id = 'F5'
                                number = '312'.
  IF sy-subrc = 0.
    p_msg = 'SUCCESS'.
    p_belnr = bdcreturn-message_v1.
  ELSE.
    LOOP AT bdcreturn WHERE type CA 'AEX'.
      CONCATENATE bdcreturn-message p_msg INTO p_msg
      SEPARATED BY '/'.
    ENDLOOP.
    IF p_msg IS INITIAL.
      p_msg = '凭证生成失败'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FB08
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- IT_BELNR_MSG
*&---------------------------------------------------------------------*
FORM fb08  CHANGING p_it_belnr_msg.
  DATA:datec1 TYPE char10.
  CLEAR:bdcdata[],bdcreturn[],datec1,p_it_belnr_msg.
  WRITE it_belnr-budat TO datec1.
  PERFORM bdc_dynpro(zpubform) TABLES bdcdata      USING 'SAPMF05A' '0105'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_CURSOR'
                                                          'BSIS-BUDAT'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BDC_OKCODE'
                                                         '=BU'.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'RF05A-BELNS'
                                                          it_belnr-belnr.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BKPF-BUKRS'
                                                         it_belnr-bukrs.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'RF05A-GJAHS'
                                                         it_belnr-gjahr.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'UF05A-STGRD'
                                                          p_stgrd.
  PERFORM bdc_field(zpubform) TABLES bdcdata       USING 'BSIS-BUDAT'
                                                          datec1.
  PERFORM bdcfm(zpubform) TABLES bdcdata bdcreturn USING 'FB08' 'N'.
  LOOP AT bdcreturn WHERE type CA 'AEX'.
    CONCATENATE bdcreturn-message p_it_belnr_msg INTO p_it_belnr_msg
    SEPARATED BY '/'.
  ENDLOOP.
  IF sy-subrc NE 0.
  ELSE.
    IF p_it_belnr_msg IS INITIAL.
      p_it_belnr_msg = '凭证生成失败'.
    ENDIF.
  ENDIF.
ENDFORM.
**********************************************************************
*& 移车间
**********************************************************************
FORM getycj CHANGING  t_ywdtail LIKE gt_ywdtail.
  TYPES:BEGIN OF ty_tabkey,
          objectid TYPE cdpos-objectid,
          tabkey   TYPE cdpos-tabkey,
        END OF ty_tabkey.
  DATA:lt_tabkey TYPE TABLE OF ty_tabkey WITH KEY objectid tabkey,
       ls_tabkey LIKE LINE OF lt_tabkey.
  DATA:BEGIN OF lt_wlpc OCCURS 0,
         matnr     TYPE matnr,
         charg     TYPE charg_d,
         objectid  TYPE cdpos-objectid,
         changenr  TYPE cdpos-changenr,
         value_new TYPE cdpos-value_new,
         value_old TYPE cdpos-value_old,
         udate     TYPE cdhdr-udate,
       END OF lt_wlpc.
  DATA:lv_index     TYPE i,
       lv_value_old TYPE cdpos-value_old,
       lv_isj       TYPE char1. "是否含有J
  DATA:r_objectid TYPE RANGE OF cdpos-objectid WITH HEADER LINE.

  SELECT concat( ausp~objek,ausp~mafid ) AS objectid,
         concat( ausp~objek, cabn~atinn ) AS tabkey,
         mch1~matnr,
         mch1~charg,
         cabn~atinn,
         cabn~atnam,
         ausp~atwrt,
         ausp~objek,
         ausp~atzhl,
         ausp~mafid
    FROM mch1 INNER JOIN ausp ON mch1~cuobj_bm = ausp~objek
                              AND klart = '023'
              INNER JOIN cabn ON ausp~atinn = cabn~atinn
    WHERE cabn~atnam = 'ZCJ'
      AND left( mch1~matnr,5 ) = 'E0201'
    ORDER BY objectid
    INTO TABLE @DATA(lt_pctz).
  LOOP AT lt_pctz INTO DATA(ls_pctz).
    ls_tabkey-objectid = ls_pctz-objectid.
    ls_tabkey-tabkey = ls_pctz-tabkey.
    COLLECT ls_tabkey INTO lt_tabkey.
  ENDLOOP.
  SELECT
    dr~objectid,
    ps~tabkey,
    left( ps~tabkey,28 ) AS tabkey28,
    dr~udate,
    ps~changenr,
    ps~value_new,
    ps~value_old
  FROM cdpos AS ps
  INNER JOIN @lt_tabkey AS k ON  k~objectid = ps~objectid
                             AND left( ps~tabkey,28 ) = k~tabkey
  INNER JOIN cdhdr AS dr ON  dr~objectclas = ps~objectclas
                         AND dr~objectid   = ps~objectid
                         AND dr~changenr   = ps~changenr
  WHERE dr~udate BETWEEN @datef AND @datet
  ORDER BY dr~objectid,ps~changenr,value_old DESCENDING
  INTO TABLE @DATA(lt_cdpos).
  LOOP AT lt_cdpos INTO DATA(ls_cdpos) GROUP BY ( objectid = ls_cdpos-objectid changenr = ls_cdpos-changenr
                                                 size = GROUP SIZE
                                                 index = GROUP INDEX )
                                        ASCENDING ASSIGNING FIELD-SYMBOL(<group>).
    CLEAR:lt_wlpc,lv_index.
    LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<fs_meb>).
      lv_index += 1.
      CASE lv_index.
        WHEN 1.
          IF <fs_meb>-value_old IS INITIAL AND <fs_meb>-value_new IS NOT INITIAL.
            lt_wlpc-value_old = <fs_meb>-value_old.
            lt_wlpc-value_new = <fs_meb>-value_new.
          ELSE.
            lt_wlpc-value_old = <fs_meb>-value_old.
          ENDIF.
        WHEN 2.
          lt_wlpc-value_new = <fs_meb>-value_new.
      ENDCASE.
      lt_wlpc-udate = <fs_meb>-udate.
    ENDLOOP.
    IF lt_wlpc-value_new IS NOT INITIAL.
      lt_wlpc-objectid = <group>-objectid.
      lt_wlpc-changenr = <group>-changenr.
      APPEND lt_wlpc.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_wlpc INTO lt_wlpc GROUP BY ( objectid = lt_wlpc-objectid
                                                 size = GROUP SIZE
                                                 index = GROUP INDEX )
                               ASCENDING ASSIGNING FIELD-SYMBOL(<group_1>).
    CLEAR: r_objectid,lv_index,lv_value_old,lv_isj.
    LOOP AT GROUP <group_1> ASSIGNING FIELD-SYMBOL(<fs_meb_1>).
      lv_index += 1.
      lv_value_old = <fs_meb_1>-value_old.
      IF <fs_meb_1>-value_new = 'J' OR <fs_meb_1>-value_old = 'J'.
        lv_isj = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF ( lv_index = 1 AND lv_value_old IS INITIAL ) OR lv_isj = ''.
      r_objectid-sign = 'I'.
      r_objectid-option = 'EQ'.
      r_objectid-low = <group_1>-objectid.
      COLLECT r_objectid.
    ENDIF.
  ENDLOOP.
  DELETE lt_wlpc WHERE objectid IN r_objectid.
  LOOP AT lt_wlpc.
    READ TABLE lt_pctz INTO ls_pctz WITH KEY objectid = lt_wlpc-objectid BINARY SEARCH.
    IF sy-subrc = 0.
      lt_wlpc-matnr = ls_pctz-matnr.
      lt_wlpc-charg = ls_pctz-charg.
      MODIFY lt_wlpc.
    ENDIF.
  ENDLOOP.
  SORT lt_wlpc BY objectid changenr.
  t_ywdtail = CORRESPONDING #( lt_wlpc[] ).
ENDFORM.
**********************************************************************
*& 移位的取zfi216 的期末数量 单价 单位
**********************************************************************
FORM getzfi216 USING    t_matcha LIKE gt_matcha
               CHANGING t_gt_216 LIKE gt_216[].
  DATA: s_werks TYPE RANGE OF t001w-werks WITH HEADER LINE,
        s_matnr TYPE RANGE OF matnr WITH HEADER LINE,
        s_charg TYPE RANGE OF charg_d WITH HEADER LINE,
        p_bukrs TYPE t001-bukrs,
        p_lfmon TYPE mchbh-lfmon,
        p_lfgja TYPE mchbh-lfgja.
  DATA ls_data TYPE REF TO data.

  s_werks[] = VALUE #( sign = 'I' option = 'EQ' ( low = '3060' ) ).
  p_bukrs = '3060'.
  p_lfmon = p_month+4(2) - 1.
  p_lfgja = p_month+0(4).
  IF p_lfmon EQ 0.
    p_lfgja = p_lfgja - 1.
    p_lfmon = 12.
  ENDIF.
  LOOP AT t_matcha INTO gs_matcha.
    s_matnr-sign = 'I'.
    s_matnr-option = 'EQ'.
    s_matnr-low = gs_matcha-matnr.
    s_charg-sign = 'I'.
    s_charg-option = 'EQ'.
    s_charg-low = gs_matcha-charg.
    COLLECT: s_matnr,s_charg.
  ENDLOOP.
  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_false data = abap_true ).
  SUBMIT zrfi216 WITH s_werks IN s_werks
                 WITH s_matnr IN s_matnr
                 WITH s_charg IN s_charg
                 WITH s_lfmon EQ p_lfmon
                 WITH p_bukrs EQ p_bukrs
                 WITH p_lfgja EQ p_lfgja
                 WITH p_10    EQ 'X'
                 WITH p_31    EQ ''
                 WITH p_32    EQ ''
                 AND RETURN.
  CLEAR:ls_data.
  TRY .
      cl_salv_bs_runtime