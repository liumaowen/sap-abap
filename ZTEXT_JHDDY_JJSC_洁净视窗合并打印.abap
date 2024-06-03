*&---------------------------------------------------------------------*
*& 包含               ZTEXT_JHDDY_JJSC
*&---------------------------------------------------------------------*

FORM jhddy_jjsc USING t_plnum LIKE it_plnum.
  DATA:pcd_print_url   TYPE string,
       pcd_print_str   TYPE string,
       pcd_printre_str TYPE string,
       pcd_print_msg   TYPE string,
       pcd_print_sta   TYPE i.
  DATA:gs_order     TYPE t_order6,
       list_jjsc    TYPE TABLE OF t_jiejingshichuang,
       wa_list_jjsc LIKE LINE OF list_jjsc,
       intab        TYPE t_json1.
  DATA:it_dd07v1    TYPE TABLE OF dd07v WITH HEADER LINE.
  DATA:it_matnr  TYPE TABLE OF ccvx_matnr WITH HEADER LINE,
       outtab001 TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
  DATA:t_text TYPE TABLE OF zsmm202 WITH HEADER LINE.
  DATA:tmplid   TYPE string,
       zhangid  TYPE string,
       datajson TYPE string,
       osskey   TYPE string,
       url      TYPE string,
       rtype    TYPE bapi_mtype,
       rtmsg    TYPE bapi_msg,
       lv_guid  TYPE guid_16.
  DATA:BEGIN OF it_jibencanshu OCCURS 0, "基本参数
         jibencanshu TYPE string,
       END OF it_jibencanshu.

  CLEAR:gs_order.
  READ TABLE t_plnum INTO wa_plnum INDEX 1.
  SELECT SINGLE * INTO @DATA(wa_206) FROM ztpp_206 WHERE plnum = @wa_plnum-plnum AND del NE 'X'.
  SELECT SINGLE * INTO @DATA(wa_205)  FROM ztpp_205  WHERE zpcdh = @wa_206-zpcdh.
  SELECT SINGLE * INTO @DATA(wa_205b) FROM ztpp_205b WHERE zpcdh = @wa_206-zpcdh.
  gs_order-customername = wa_206-name1.
  gs_order-projectname = wa_206-post1.
  gs_order-zjhrq = wa_205b-zjhsj+0(4) && '-' && wa_205b-zjhsj+4(2) && '-' && wa_205b-zjhsj+6(2).
  SELECT
    SINGLE klabc
  INTO gs_order-customerlevel
  FROM vbak
  JOIN knvv ON vbak~kunnr = knvv~kunnr AND vbak~vtweg = knvv~vtweg AND vbak~vkorg = knvv~vkorg
  WHERE vbak~vbeln = wa_205-vbeln
  AND vbak~kunnr = wa_205-kunnr.
  SELECT
    SINGLE name_textc
  FROM user_addr
  WHERE bname = @wa_205-syusr
  INTO @gs_order-cusername.
  SELECT
    SINGLE name1
  FROM kna1
  WHERE kunnr = @wa_205b-zywy
  INTO @gs_order-yewuyuanname.
  SELECT
  SINGLE name1
  INTO gs_order-kefuname
  FROM kna1
  INNER JOIN vbak ON vbak~zhtly = kna1~kunnr
  WHERE vbak~vbeln = wa_205-vbeln.
  PERFORM getdomain(zpubform) TABLES it_dd07v1 USING 'ZD_ZBZFS'.

  LOOP AT t_plnum INTO wa_plnum.
    CLEAR wa_list_jjsc.
    SELECT SINGLE * INTO @wa_206 FROM ztpp_206 WHERE plnum = @wa_plnum-plnum AND del NE 'X'.
    wa_list_jjsc-zpcdh = wa_206-zpcdh.
    wa_list_jjsc-zpcdhh = |{ wa_206-zpcdhh ALPHA = OUT }|.
    wa_list_jjsc-aufnr = wa_206-aufnr.
    wa_list_jjsc-aufnr = wa_206-aufnr.
    wa_list_jjsc-matnr = wa_206-matnr.
    SELECT SINGLE zzl1 FROM mara WHERE matnr = @wa_206-matnr INTO @wa_list_jjsc-zzl1.
    SELECT SINGLE * INTO @DATA(wa_205a) FROM ztpp_205a WHERE zpcdh = @wa_206-zpcdh AND zpcdhh = @wa_206-zpcdhh.
    wa_list_jjsc-pingfangshu = wa_205a-zcd * wa_205a-zbckd / 1000000.
    wa_list_jjsc-zpingfangshu = wa_205a-zpingfangshu.
    SELECT
    SUM( psmng ) AS zsl
    FROM ztpp_206
    WHERE plnum = @wa_plnum-plnum
    AND del NE 'X'
    INTO @DATA(zsl).
    wa_list_jjsc-zsl = zsl.
    SELECT SINGLE * INTO @wa_205b FROM ztpp_205b WHERE zpcdh = @wa_206-zpcdh.
    wa_list_jjsc-zzyyq = wa_205b-zzyyq.
    READ TABLE it_dd07v1 WITH KEY domvalue_l = wa_205b-zbzfs BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_list_jjsc-zbzfs = it_dd07v1-ddtext.
    ENDIF.
    it_matnr-matnr = wa_206-matnr.
    COLLECT it_matnr.
    "排产备注
    CLEAR:t_text[].
    CALL FUNCTION 'ZFM_DEALLONGTEXT'
      EXPORTING
        intype = 'O'
        tdid   = 'PCBZ'
        sapno  = wa_list_jjsc-zpcdh
        sapmk  = 'PCD'
      TABLES
        t_text = t_text.
    LOOP AT t_text.
      wa_list_jjsc-zpcbz = wa_list_jjsc-zpcbz && t_text-text.
    ENDLOOP.
    APPEND wa_list_jjsc TO list_jjsc.
  ENDLOOP.
  "取001属性
  PERFORM get001 IN PROGRAM zpubform TABLES it_matnr outtab001 USING 'ZSCKD,ZDKKD,ZSCGD,ZDKGD,ZSHICHD,ZDKHD,ZCKHD,ZCNC,ZBLHD,ZBLCS,ZSCYB,ZLGLX,ZZKMF,ZNZGZJ'.
  SORT outtab001 BY matnr atnam.
  LOOP AT list_jjsc INTO wa_list_jjsc.
    CLEAR it_jibencanshu.
    "视窗厚度
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZSHICHD'.
    IF sy-subrc EQ 0.
      wa_list_jjsc-schoudu = outtab001-atwrt.
    ELSE.
      READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZDKHD'.
      IF sy-subrc EQ 0.
        wa_list_jjsc-schoudu = outtab001-atwrt.
      ELSE.
        READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZCKHD'.
        IF sy-subrc EQ 0.
          wa_list_jjsc-schoudu = outtab001-atwrt.
        ENDIF.
      ENDIF.
    ENDIF.
    "视窗宽度
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZSCKD'.
    IF sy-subrc EQ 0.
      wa_list_jjsc-sckuandu  = outtab001-atwrt.
    ELSE.
      READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZDKKD'.
      IF sy-subrc EQ 0.
        wa_list_jjsc-sckuandu  = outtab001-atwrt.
      ENDIF.
    ENDIF.
    "视窗高度
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZSCGD'.
    IF sy-subrc EQ 0.
      wa_list_jjsc-scgaodu  = outtab001-atwrt.
    ELSE.
      READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZDKGD'.
      IF sy-subrc EQ 0.
        wa_list_jjsc-scgaodu  = outtab001-atwrt.
      ENDIF.
    ENDIF.
    "视窗印边
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZSCYB'.
    IF sy-subrc EQ 0.
      wa_list_jjsc-scyinbian  = outtab001-atwrt.
    ENDIF.
    "窗内衬
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZCNC'.
    IF sy-subrc EQ 0.
      wa_list_jjsc-sccun  = outtab001-atwrt.
    ENDIF.
    "玻璃厚度
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZBLHD'.
    IF sy-subrc EQ 0.
      wa_list_jjsc-scbolihoudu  = outtab001-atwrt.
    ENDIF.
    "玻璃参数
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZBLCS' BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_list_jjsc-scbolicanshu = outtab001-atwrt.
    ENDIF.
    "窗龙骨
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZLGLX' BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_order-jibencanshu = outtab001-atwrt.
    ENDIF.
    "中空密封
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZZKMF' BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_order-jibencanshu = gs_order-jibencanshu && '、' && outtab001-atwrt.
    ENDIF.
    "内置干燥剂
    READ TABLE outtab001 WITH KEY matnr = wa_list_jjsc-matnr atnam = 'ZNZGZJ' BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_order-jibencanshu = gs_order-jibencanshu && '、' && outtab001-atwrt.
    ENDIF.
    it_jibencanshu-jibencanshu = gs_order-jibencanshu.
    COLLECT it_jibencanshu.
    MODIFY list_jjsc FROM wa_list_jjsc.
  ENDLOOP.
  IF lines( it_jibencanshu[] ) > 1.
    DATA(mmm) = '选中行基本参数不一致，不允许打印'.
    MESSAGE s000(oo) WITH mmm DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  intab-order = gs_order.
  intab-list_jjsc = list_jjsc.

  pcd_print_str = /ui2/cl_json=>serialize( data = intab  compress = abap_false pretty_name = 'L' ).

**********************************************************************

  tmplid   = '860' .
  zhangid  = ''.
  datajson = pcd_print_str.
*& 获取GUID，根据GUID生成文件
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_16 = lv_guid.

  osskey   = 'cgorder/' && lv_guid && '