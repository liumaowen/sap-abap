*&---------------------------------------------------------------------*
*& Include zppd201_v4_lchj
*& 丽彩鼎丽彩金丽彩钻核价
*&---------------------------------------------------------------------*


**********************************************************************
*& 打开丽彩核价屏幕
**********************************************************************
FORM openlchj USING p_zpcdh TYPE ztpp_205-zpcdh.
  CLEAR: gv_matkl,gv_reset.
  gv_matkl = wa_ggzd-matnr+0(5).
  CASE gv_matkl.
    WHEN 'A0100' OR 'A0200' OR 'A0400'.
      PERFORM gethead USING gv_reset.
      CALL SCREEN 9200.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM  gethead
*&---------------------------------------------------------------------*
*& 获取抬头数据和一些费用的默认值
*&---------------------------------------------------------------------*
FORM gethead USING p_reset TYPE char1.

  CLEAR: wa_lchj.
  wa_lchj-matnr = wa_ggzd-matnr.
  wa_lchj-zwlcms = lv_wlcms.
  wa_lchj-zzsl = wa_ggzd-zzsl.
  wa_lchj-meins = wa_ggzd-meins.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZBH'.
  IF sy-subrc EQ 0.
    wa_lchj-zbh = outtab001-atwrt.
    wa_lchj-zxchd = outtab001-atwrt.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZBK'.
  IF sy-subrc EQ 0.
    wa_lchj-zbk = outtab001-atwrt.
    "系数 = 板宽 / 1000
    wa_lchj-zxs = wa_lchj-zbk / 1000.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZBX'.
  IF sy-subrc EQ 0.
    wa_lchj-zbx = outtab001-atwrt.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZWBXZ'.
  IF sy-subrc EQ 0.
    wa_lchj-zwbxz = outtab001-atwrt.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZWBTC'.
  IF sy-subrc EQ 0.
    wa_lchj-zwbtc = outtab001-atwrt.
  ENDIF.
  IF wa_ggzd-zzks <> 0.
    wa_lchj-zbc = wa_ggzd-zzms / wa_ggzd-zzks.
  ENDIF.
  "运费
  READ TABLE it_pcd INDEX 1.
  IF sy-subrc = 0.
    wa_lchj-zyf = it_pcd-zyf.
    wa_lchj-zpr0 = it_pcd-zyhtdj.
    IF wa_lchj-zpr0 IS INITIAL. "首先取原合同单价，若为空，则取单价
      wa_lchj-zpr0 = it_pcd-zpr0.
    ENDIF.
  ENDIF.
  "优先取日志ztpp_205_hjlog 表中的数据
  SELECT * FROM ztpp_205_hjlog WHERE zpcdh = @wa_ggzd-zpcdh ORDER BY zdate DESCENDING,ztime DESCENDING INTO TABLE @DATA(lt_205_hjlog).
  IF sy-subrc = 0 AND p_reset = ''.
    MOVE-CORRESPONDING lt_205_hjlog[ 1 ] TO wa_lchj.
  ELSE.
    PERFORM getdefault.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM  getdefault
*&---------------------------------------------------------------------*
*& 一些费用的默认值
*&---------------------------------------------------------------------*
FORM getdefault.
  DATA:lv_zwbjsfm TYPE ztpp_205j4-zwbjsfm,
       lv_znbjsfm TYPE ztpp_205j4-znbjsfm.
  DATA:isvalue TYPE char1.

  CLEAR: lv_zwbjsfm,lv_znbjsfm.
  "瓦楞 挤出条+岩棉封边条+包装费 封边+海绵条+胶带+包装费
  SELECT * FROM ztpp_205j3 WHERE werks = @wa_ggzd-werks
                             AND matkl = @gv_matkl
                             AND zbx   = @wa_lchj-zbx
  INTO TABLE @DATA(lt_205j3).
  LOOP AT lt_205j3 INTO DATA(ls_205j3).
    IF wa_lchj-zbh > ls_205j3-zhdxx AND wa_lchj-zbh <= ls_205j3-zhdsx.
      wa_lchj-zwl = ls_205j3-zwl.
      CASE gv_matkl.
        WHEN 'A0200'."丽彩金
          wa_lchj-zjc_ym_bz  = ls_205j3-zfb.
        WHEN OTHERS.
          wa_lchj-zfb_hm_jd_bz = ls_205j3-zfb.
      ENDCASE.
      isvalue = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  "没有取到值，单价 = 板厚 / 10 + 1
  IF isvalue = ''.
    CASE gv_matkl.
      WHEN 'A0200'."丽彩金
        wa_lchj-zjc_ym_bz  = wa_lchj-zbh / 10 + 1.
      WHEN OTHERS.
        wa_lchj-zfb_hm_jd_bz = wa_lchj-zbh / 10 + 1.
    ENDCASE.
  ENDIF.
  "胶水-覆膜
  SELECT * FROM ztpp_205j4 WHERE werks = @wa_ggzd-werks AND matkl = @gv_matkl INTO TABLE @DATA(lt_205j4).
  SORT lt_205j4 BY zwbtc.
  READ TABLE lt_205j4 INTO DATA(ls_205j4) WITH KEY zwbtc = wa_lchj-zwbtc BINARY SEARCH.
  IF sy-subrc = 0.
    lv_zwbjsfm = ls_205j4-zwbjsfm.
    lv_znbjsfm = ls_205j4-znbjsfm.
  ELSE.
    READ TABLE lt_205j4 INTO ls_205j4 WITH KEY zwbtc = '其他' BINARY SEARCH.
    IF sy-subrc = 0.
      lv_zwbjsfm = ls_205j4-zwbjsfm.
      lv_znbjsfm = ls_205j4-znbjsfm.
    ENDIF.
  ENDIF.
  IF wa_ggzd-zwbfmyq = '06'. "不覆膜
    lv_zwbjsfm = lv_zwbjsfm - '1.2'.
  ENDIF.
  IF wa_ggzd-znbfmyq = '08'. "覆膜
    lv_zwbjsfm = lv_zwbjsfm + lv_znbjsfm.
  ENDIF.
  "胶水+覆膜: 单价 * 板宽 / 1000
  wa_lchj-zjs_fm = lv_zwbjsfm * wa_lchj-zbk / 1000.
  "制造费用 制造加价 / 母板制造费用 盒子板二次加工费
  PERFORM getzzfy.
  "转角板费用/短板切割
  PERFORM getzjb.
  "基准毛利 = 管控利润 * 毛利比例(0.59)
  SELECT
    SINGLE kwmeng
  FROM vbap
  WHERE vbeln = @wa_ggzd-vbeln
    AND posnr = @wa_ggzd-posnr
  INTO @DATA(lv_kwmeng). "合同数量

  IF lv_kwmeng > 0.
    SELECT * FROM ztpp_205j8
     WHERE werks = @wa_ggzd-werks
       AND matkl = @gv_matkl
       AND zbx   = @wa_lchj-zbx
    INTO TABLE @DATA(lt_205j8).
    LOOP AT lt_205j8 INTO DATA(ls_205j8).
      IF lv_kwmeng > ls_205j8-zddlxx AND lv_kwmeng <= ls_205j8-zddlsx.
        wa_lchj-zjcml = ls_205j8-zgklr * ls_205j8-zmlbl.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9200 OUTPUT.
  DATA lchj_extab TYPE slis_t_extab.
  CLEAR lchj_extab.
  REFRESH lchj_extab.

  LOOP AT SCREEN.
    CASE screen-group1.
*        "A1:WA_LCHJ-ZFB_HM_JD_BZ(封边+海绵条+胶带+包装费)
*      WHEN 'A1'.
*        IF lv_matkl = 'A0200'."丽彩金
*          screen-active = 0.
*        ENDIF.
*        "A2:WA_LCHJ-ZJC_YM_BZ(挤出条+岩棉封边条+包装费)
*      WHEN 'A2'.
*        IF lv_matkl <> 'A0200'."丽彩金
*          screen-active = 0.
*        ENDIF.
        "B1:WA_LCHJ-ZMBZZFY(母板制造费用) WA_LCHJ-ZHZBJGF(盒子板二次加工费)
        "B2:WA_LCHJ-ZMBYLCB(母板用料成本) WA_LCHJ-ZMBSH(母板损耗2%) WA_LCHJ-ZHZBSH(盒子板端头切割损耗0.05M)
        "   WA_LCHJ-ZHZBJGSH(盒子板加工损耗0.4%) WA_LCHJ-ZDTHB(端头处理-灰板等价格)
      WHEN 'B1' OR 'B2'.
        IF gv_matkl <> 'A0400'."丽彩钻
          screen-input = 0.
        ENDIF.
        "C1:WA_LCHJ-ZZZFY(制造费用)  WA_LCHJ-ZZZJJ(制造加价)  WA_LCHJ-ZZZSH(制造损耗)  WA_LCHJ-ZYLCB(用料成本)
      WHEN 'C1'.
        IF gv_matkl = 'A0400'."丽彩钻
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

  SET PF-STATUS 'STA9200' EXCLUDING  lchj_extab.
  SET TITLEBAR 'TIT9200' WITH '丽彩产品核价'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  DATA:nborwb TYPE i. "内板-1 外板-2
  DATA:wa_ztpp_205_hjlog TYPE ztpp_205_hjlog.

  save_ok = sy-ucomm..

  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'QX'.
      LEAVE TO SCREEN 0.
    WHEN 'NBYY'. " 内板引用
      nborwb = 1.
      PERFORM qgyy USING    nborwb
                            wa_lchj-zbk
                            wa_lchj-zbh
                            wa_lchj-zbx
                            ''
                   CHANGING wa_lchj-znbycms
                            wa_lchj-znbcm
                            wa_lchj-znbdj
                            wa_lchj-znbsh
                            wa_lchj-znbzj
                            wa_lchj-znbct.
    WHEN 'WBYY'. " 外板引用
      nborwb = 2.
      PERFORM qgyy USING    nborwb
                            wa_lchj-zbk
                            wa_lchj-zbh
                            wa_lchj-zbx
                            wa_lchj-zwbxz
                   CHANGING wa_lchj-zwbycms
                            wa_lchj-zwbcm
                            wa_lchj-zwbdj
                            wa_lchj-zwbsh
                            wa_lchj-zwbzj
                            wa_lchj-zwbct.
    WHEN 'HJJS'.
      PERFORM calc.
    WHEN 'RESET'.
      PERFORM reset.
    WHEN 'KCYY'.
      PERFORM kcyy.
    WHEN 'CON'.
      IF wa_lchj-zwbdj IS INITIAL.
        MESSAGE s004 WITH '外板吨价未填写' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      IF wa_lchj-znbdj IS INITIAL.
        MESSAGE s004 WITH '内板吨价未填写' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      LOOP AT it_pcd.
        it_pcd-zgkcb = wa_lchj-zgkcb.
        it_pcd-zjcml = wa_lchj-zjcml.
        it_pcd-zmlj = wa_lchj-zygml.
        MODIFY it_pcd TRANSPORTING zgkcb zjcml zmlj.
      ENDLOOP.
      CLEAR wa_ztpp_205_hjlog.
      MOVE-CORRESPONDING wa_lchj TO wa_ztpp_205_hjlog.
      PERFORM getnum(zpubform) USING 'PCDLCHJLOG' 'D' 4 wa_ztpp_205_hjlog-znum."按天生成流水号 4位
      wa_ztpp_205_hjlog-zpcdh = wa_ggzd-zpcdh.
      wa_ztpp_205_hjlog-zdate = sy-datlo.
      wa_ztpp_205_hjlog-ztime = sy-timlo.
      wa_ztpp_205_hjlog-zname = sy-uname.
      INSERT ztpp_205_hjlog FROM wa_ztpp_205_hjlog.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& FORM  kcyy
*&---------------------------------------------------------------------*
*& 库存引用
*&---------------------------------------------------------------------*
FORM kcyy.
  DATA:BEGIN OF wa_modal,
         nb_z01   TYPE char20,
         nb_charg TYPE char20,
         wb_z01   TYPE char20,
         wb_charg TYPE char20,
       END OF wa_modal.
  DATA:lt_z84    TYPE tt_z84,           "原采购卷自编号
       ls_z84    LIKE LINE OF lt_z84,
       lt_charg  TYPE tt_charg,         "批次号
       lt_charg1 TYPE tt_charg,         "批次号
       ls_charg  LIKE LINE OF lt_charg,
       lt_238    TYPE tt_238,           "库存使用记录
       lt_238_tp TYPE tt_238,           "库存使用记录_临时
       ls_238    LIKE LINE OF lt_238.
  TYPES:BEGIN OF ty_ebeln,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
        END OF ty_ebeln.
  DATA:lt_ebeln TYPE TABLE OF ty_ebeln WITH KEY ebeln ebelp,
       ls_ebeln LIKE LINE OF lt_ebeln.
  DATA:zkd      TYPE decfloat16,
       lv_zsfzj TYPE ztpp_205b-zsfnbzj. "内外板是否纵剪

  DEFINE addfields.
    CLEAR:fields.
    fields-tabname = &1.    " 字段结构
    fields-fieldname = &2.  " 字段名
    fields-fieldtext = &3.  " 显示文本
    fields-value = &4.      " 默认值
    fields-field_obl = &5.  " 是否必填，必填X
    fields-field_attr = &6. " 能不能修改，02-不可点 ''-可点
    APPEND fields.
  End-OF-DEFINITION.

  CLEAR:fields[],returncode.
  CLEAR:fields.

  addfields: 'ZSPP_234'   'WB_Z01'      '外板自编号'   ''   ''    ''  ,
             'ZSPP_234'   'WB_CHARG'    '外板批次号'   ''   ''    ''  ,
             'ZSPP_234'   'NB_Z01'      '内板自编号'   ''   ''    ''  ,
             'ZSPP_234'   'NB_CHARG'    '内板批次号'   ''   ''    ''  .
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = '库存引用'
    IMPORTING
      returncode      = returncode
    TABLES
      fields          = fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF returncode IS INITIAL.
    LOOP AT fields.
      CONDENSE fields-value NO-GAPS.
      ASSIGN COMPONENT fields-fieldname OF STRUCTURE wa_modal TO <fs>.
      IF sy-subrc EQ 0.
        <fs> = fields-value.
      ENDIF.
    ENDLOOP.
    IF wa_modal-nb_z01 IS NOT INITIAL.
      ls_z84-z84 = wa_modal-nb_z01.
      COLLECT ls_z84 INTO lt_z84.
    ENDIF.
    IF wa_modal-nb_charg IS NOT INITIAL.
      ls_charg-charg = wa_modal-nb_charg.
      COLLECT ls_charg INTO lt_charg.
    ENDIF.
    PERFORM kcyycal USING    lt_charg
                             lt_z84
                             wa_ggzd-zsfnbzj
                    CHANGING wa_lchj-znbycms
                             wa_lchj-znbcm
                             wa_lchj-znbdj
                             wa_lchj-znbsh
                             wa_lchj-znbzj
                             wa_lchj-znbct.
    REFRESH:lt_z84,lt_charg.
    CLEAR:ls_z84,ls_charg.
    IF wa_modal-wb_z01 IS NOT INITIAL.
      ls_z84-z84 = wa_modal-wb_z01.
      COLLECT ls_z84 INTO lt_z84.
    ENDIF.
    IF wa_modal-wb_charg IS NOT INITIAL.
      ls_charg-charg = wa_modal-wb_charg.
      COLLECT ls_charg INTO lt_charg.
    ENDIF.
    PERFORM kcyycal USING    lt_charg
                             lt_z84
                             wa_ggzd-zsfwbzj
                    CHANGING wa_lchj-zwbycms
                             wa_lchj-zwbcm
                             wa_lchj-zwbdj
                             wa_lchj-zwbsh
                             wa_lchj-zwbzj
                             wa_lchj-zwbct.

  ENDIF.
ENDFORM.
*库存引用的计算
FORM kcyycal USING    t_charg  TYPE tt_charg
                      t_z84    TYPE tt_z84
                      p_zsfzj TYPE ztpp_205b-zsfwbzj
             CHANGING p_zycms TYPE zspp225_nb-znbycms
                      p_zcm   TYPE zspp225_nb-znbcm
                      p_zdj   TYPE zspp225_nb-znbdj
                      p_zsh   TYPE zspp225_nb-znbsh
                      p_zzj   TYPE zspp225_nb-znbzj
                      p_zct   TYPE zspp225_nb-znbct.
  DATA:lt_z84    TYPE tt_z84,           "原采购卷自编号
       lt_charg  TYPE tt_charg,         "批次号
       lt_238    TYPE tt_238,           "库存使用记录
       lt_238_tp TYPE tt_238,           "库存使用记录_临时
       ls_238    LIKE LINE OF lt_238.
  TYPES:BEGIN OF ty_ebeln,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
        END OF ty_ebeln.
  DATA:lt_ebeln TYPE TABLE OF ty_ebeln WITH KEY ebeln ebelp,
       ls_ebeln LIKE LINE OF lt_ebeln.
  DATA:zkd      TYPE decfloat16.

  IF t_charg IS NOT INITIAL OR t_z84 IS NOT INITIAL.
    PERFORM getkcjl USING t_z84 t_charg lt_238_tp.
    " 针对原材价格为空的根据批次号查询  2024-07-31 14:33:01 by lmw
    LOOP AT lt_238_tp INTO ls_238 WHERE zycjg IS INITIAL.
      ls_ebeln-ebeln = ls_238-ebeln.
      ls_ebeln-ebelp = ls_238-ebelp.
      COLLECT ls_ebeln INTO lt_ebeln.
    ENDLOOP.
    IF lt_ebeln IS NOT INITIAL.
      SELECT
        mg~charg
      FROM mseg AS mg
      INNER JOIN @lt_ebeln AS en ON  mg~ebeln = en~ebeln
                                 AND mg~ebelp = en~ebelp
      INNER JOIN ekko      AS eo ON  eo~ebeln = en~ebeln
      WHERE eo~bsart = 'UB'
        AND mg~bwart = '351'
        AND mg~xauto = ''
        AND NOT EXISTS ( SELECT * FROM  m_mbmps   "未被冲销
                                  WHERE m_mbmps~sjahr = mg~mjahr AND
                                        m_mbmps~smbln = mg~mblnr AND
                                        m_mbmps~smblp = mg~zeile )
      GROUP BY mg~charg
      INTO TABLE @lt_charg.
    ENDIF.
    lt_238 = lt_238_tp.
    IF lt_charg IS NOT INITIAL.
      REFRESH: lt_238_tp,lt_z84.
      PERFORM getkcjl USING lt_z84 lt_charg lt_238_tp.
      APPEND LINES OF lt_238_tp TO lt_238.
    ENDIF.
    LOOP AT lt_238 INTO ls_238.
      ls_238-zycjg = ls_238-zycjg + ls_238-zysjg.
      SELECT SINGLE maktx FROM makt WHERE matnr = @ls_238-matnr AND spras = @sy-langu INTO @ls_238-zwlcms.
      MODIFY lt_238 FROM ls_238 TRANSPORTING zycjg zwlcms.
    ENDLOOP.
    DELETE lt_238 WHERE zycjg IS INITIAL.
    READ TABLE lt_238 INTO ls_238 INDEX 1.
    IF sy-subrc = 0.
      p_zdj = ls_238-zycjg.
      p_zcm = ls_238-ztm.
      p_zycms = ls_238-zwlcms.
      zkd = CONV decfloat16( ls_238-zkd ).
      "获取内外板出条
      PERFORM getct USING    wa_lchj-zwbxz wa_lchj-zbx wa_lchj-zbh wa_lchj-zbk zkd
                    CHANGING p_zct.
      "获取内外板损耗纵剪
      PERFORM getshzj USING    ls_238-zzl1 p_zsfzj
                      CHANGING p_zsh
                               p_zzj.
      PERFORM calc.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM  qgyy
*&---------------------------------------------------------------------*
*& 内外板引用请购单
*&---------------------------------------------------------------------*
*& -->  p_zycms    内板/外板原材描述
*& -->  p_zcm      内板/外板出米
*& -->  p_zdj      内板/外板吨价
*& -->  p_zsh      内板/外板损耗
*& -->  p_zzj      内板/外板纵剪
*& -->  p_zct      内板/外板出条
*& -->  p_zmj      内板/外板米价
*& <--  p_nw       内板/外板  内板-1 外板-2
*& <--  p_zbk      板宽
*& <--  p_zbh      板厚
*& <--  p_zbx      板型
*& <--  p_zwbxz    外板形状
*----------------------------------------------------------------------*
FORM qgyy USING    p_nw    TYPE i
                   p_zbk   TYPE zebckd
                   p_zbh   TYPE zebchd
                   p_zbx   TYPE zbx
                   p_zwbxz TYPE zwbxz
          CHANGING p_zycms TYPE zspp225_nb-znbycms
                   p_zcm   TYPE zspp225_nb-znbcm
                   p_zdj   TYPE zspp225_nb-znbdj
                   p_zsh   TYPE zspp225_nb-znbsh
                   p_zzj   TYPE zspp225_nb-znbzj
                   p_zct   TYPE zspp225_nb-znbct.

  DATA:lv_nwbkd TYPE decfloat16,         "原材宽度
       lv_zsfzj TYPE ztpp_205b-zsfnbzj   "内外板是否纵剪
       .
  DATA:it_matnr_nwb  TYPE TABLE OF ccvx_matnr WITH HEADER LINE, "引入钢卷-内外板
       outtab001_nwb TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
  DATA:lvv_zwbxz TYPE zwbxz.

  CLEAR:gs_lchj_operate,lv_qgd.

  CASE p_nw.
    WHEN 1.
      gs_lchj_operate-nbyy = 'X'.
      lv_zsfzj = wa_ggzd-zsfnbzj.
    WHEN 2.
      lv_zsfzj = wa_ggzd-zsfwbzj.
      gs_lchj_operate-wbyy = 'X'.
  ENDCASE.
  PERFORM pick. "打开请购单弹窗
  IF lv_qgd-matnr IS NOT INITIAL. "请购单选择明细后
    p_zycms = lv_qgd-zwlcms.
    p_zcm = floor( lv_qgd-zcm ).
    it_matnr_nwb-matnr = lv_qgd-matnr.
    APPEND it_matnr_nwb.
    "取001属性
    PERFORM get001 IN PROGRAM zpubform TABLES it_matnr_nwb outtab001_nwb USING 'ZKD'.
    SORT outtab001_nwb BY matnr atnam.
    READ TABLE outtab001_nwb WITH KEY matnr = lv_qgd-matnr atnam = 'ZKD' BINARY SEARCH.
    IF sy-subrc = 0.
      lv_nwbkd = outtab001_nwb-atwrt.
    ENDIF.
    p_zdj = lv_qgd-zdj.
    "获取内外板损耗/纵剪
    PERFORM getshzj USING    lv_qgd-zzl1 lv_zsfzj
                    CHANGING p_zsh
                             p_zzj.
    "获取内外板出条
    PERFORM getct USING    p_zwbxz p_zbx p_zbh p_zbk lv_nwbkd
                  CHANGING p_zct.
    PERFORM calc.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getct
*&---------------------------------------------------------------------*
*& 获取内外板损耗/纵剪
*&---------------------------------------------------------------------*
FORM getshzj USING    p_zzl1  TYPE mara-zzl1
                      p_zsfzj TYPE ztpp_205b-zsfnbzj
             CHANGING p_zsh   TYPE zspp225_nb-znbsh
                      p_zzj   TYPE zspp225_nb-znbzj.
  SELECT * FROM ztpp_205j1 INTO TABLE @DATA(lt_205j1).
  SORT lt_205j1 BY werks matkl zynam.

  READ TABLE lt_205j1 INTO DATA(ls_205j1) WITH KEY werks = wa_ggzd-werks matkl = wa_ggzd-matnr+0(5) zynam = p_zzl1 BINARY SEARCH.
  IF sy-subrc = 0.
    p_zsh = ls_205j1-zshjj.
    IF p_zsfzj = '1'.
      p_zzj = ls_205j1-zzjjj.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getct
*&---------------------------------------------------------------------*
*& 获取内板/外板出条
*&---------------------------------------------------------------------*
FORM getct USING    p_zwbxz  TYPE zwbxz
                    p_zbx    TYPE zbx
                    p_zbh    TYPE zebchd
                    p_zbk    TYPE zebckd
                    lv_nwbkd TYPE decfloat16
           CHANGING p_zct TYPE zspp225_nb-znbct.

  RANGES ra_bh FOR ztpp_260c-zhd.
  DATA:lvv_zwbxz TYPE zwbxz.
  DATA:lv_nwbjl TYPE ztpp_260c-znbzkjl.  "内板/外板加量

  IF p_zwbxz <> 'V8波纹'.
    lvv_zwbxz = ''.
  ENDIF.
  SELECT * FROM ztpp_260c WHERE werks = @wa_ggzd-werks AND matkl = @gv_matkl INTO TABLE @DATA(lt_260C).
  LOOP AT lt_260C INTO DATA(ls_260c) WHERE zbx   = p_zbx
                                       AND zwbxz = lvv_zwbxz
                                       .
*板厚没有数， 就不做区间对比
    IF p_zbh IS NOT INITIAL.
      IF ls_260c-zoption = ''.

      ELSE.
        ra_bh-sign = 'I'.
        ra_bh-option = ls_260c-zoption.
        ra_bh-low = ls_260c-zhd.
        APPEND ra_bh.
      ENDIF.
      IF p_zbh IN ra_bh.
        lv_nwbjl = ls_260c-znbzkjl.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  "出条 = 原材宽度 / （板宽+外板加量）
  IF ( p_zbk + lv_nwbjl ) <> 0.
    p_zct = floor( lv_nwbkd / ( p_zbk + lv_nwbjl ) ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  CALCULATE  INPUT
*&---------------------------------------------------------------------*
*       自动计算
*----------------------------------------------------------------------*
MODULE calculate INPUT.
  PERFORM calc.
ENDMODULE.
FORM calc.
  DATA:lv_zhjxs  TYPE ztpp_205j2-zhjxs, "芯材核价系数
       lv_zbkjjl TYPE ztpp_205j2-zjjl.  "板宽加减量

  "内板米价 = （吨价+损耗+纵剪）/ 出米 / 出条
  IF wa_lchj-znbct <> 0 AND wa_lchj-znbcm <> 0.
    wa_lchj-znbmj = ( wa_lchj-znbdj + wa_lchj-znbsh + wa_lchj-znbzj ) / wa_lchj-znbcm / wa_lchj-znbct.
  ENDIF.

  "外板米价 = （吨价+损耗+纵剪）/ 出米 / 出条
  IF wa_lchj-zwbct <> 0 AND wa_lchj-zwbcm <> 0.
    wa_lchj-zwbmj = ( wa_lchj-zwbdj + wa_lchj-zwbsh + wa_lchj-zwbzj ) / wa_lchj-zwbcm / wa_lchj-zwbct.
  ENDIF.

  SELECT * FROM ztpp_205j2
  WHERE werks = @wa_ggzd-werks
    AND zbx   = @wa_lchj-zbx
    AND matkl = @gv_matkl
  INTO TABLE @DATA(lt_205j2).
  SORT lt_205j2 BY werks zbx matkl zhdxx zhdsx.
  LOOP AT lt_205j2 INTO DATA(ls_205j2).
    IF wa_lchj-zxchd > ls_205j2-zhdxx AND wa_lchj-zxchd <= ls_205j2-zhdsx.
      lv_zhjxs = ls_205j2-zhjxs.
      lv_zbkjjl = ls_205j2-zjjl.
      EXIT.
    ENDIF.
  ENDLOOP.
  "芯材米价 = 芯材价格*1.065*板厚/1000*(板宽-90)/1000
  wa_lchj-zxcmj = wa_lchj-zxcjg * lv_zhjxs * wa_lchj-zxchd / 1000 * ( wa_lchj-zbk + lv_zbkjjl ) / 1000.
  "制造损耗
  PERFORM getzzzsh.
  "管控成本 = 用料成本+开机费+转角板+短板切割+运费
  wa_lchj-zgkcb = wa_lchj-zzylcb + wa_lchj-zkjf + wa_lchj-zzjbfy + wa_lchj-zdbqg + wa_lchj-zyf.
  "毛利价 = （合同价格-总成本）* 0.59
  wa_lchj-zygml = ( wa_lchj-zpr0 - wa_lchj-zgkcb ) * '0.59'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getzzfy
*&---------------------------------------------------------------------*
*& 获取制造费用
*&---------------------------------------------------------------------*
FORM getzzfy.
  DATA:r_zbx TYPE RANGE OF zbx WITH HEADER LINE,
       r_zbk TYPE RANGE OF zbk WITH HEADER LINE,
       r_zbh TYPE RANGE OF zbh WITH HEADER LINE.
  DATA:lv_zsftjbx TYPE ztpp_205j6-zsftjbx.

  SELECT
      *
  FROM ztpp_205j5
  WHERE werks = @wa_ggzd-werks
  INTO TABLE @DATA(lt_205j5).
  LOOP AT lt_205j5 INTO DATA(ls_205j5).
    CASE ls_205j5-ztjbx.
      WHEN '板型'.
        r_zbx-sign = 'I'.
        r_zbx-option = 'EQ'.
        r_zbx-low = ls_205j5-zbxkh.
        CONDENSE r_zbx-low NO-GAPS.
        COLLECT r_zbx.
      WHEN '板宽'.
        r_zbk-sign = 'I'.
        r_zbk-option = 'EQ'.
        r_zbk-low = ls_205j5-zbxkh.
        CONDENSE r_zbk-low NO-GAPS.
        COLLECT r_zbk.
      WHEN '板厚'.
        r_zbh-sign = 'I'.
        r_zbh-option = 'EQ'.
        r_zbh-low = ls_205j5-zbxkh.
        CONDENSE r_zbh-low NO-GAPS.
        COLLECT r_zbh.

    ENDCASE.
  ENDLOOP.
  "判断是否推荐板型
  IF wa_lchj-zbx IN r_zbx AND wa_lchj-zbk IN r_zbk AND wa_lchj-zbh IN r_zbh.
    lv_zsftjbx = '是'.
  ELSE.
    lv_zsftjbx = '否'.
  ENDIF.
  IF gv_matkl = 'A0400'. "丽彩钻
    SELECT * FROM ztpp_205j7
    WHERE werks   = @wa_ggzd-werks
      AND matkl   = @gv_matkl
      AND zbx     = @wa_lchj-zbx
      AND zsftjbx = @lv_zsftjbx
    INTO TABLE @DATA(lt_205j7).
    LOOP AT lt_205j7 INTO DATA(ls_205j7).
      IF wa_ggzd-zzms > ls_205j7-zpcmsxx AND wa_ggzd-zzms <= ls_205j7-zpcmssx.
        "母板制造费用
        wa_lchj-zmbzzfy = ls_205j7-zmbzzfy.
        "盒子板二次加工费
        wa_lchj-zhzbjgf = ls_205j7-zhzbjgf / wa_lchj-zbc.
        wa_lchj-zmbshl = ls_205j7-zmbshl / 100. "母板损耗率%
        wa_lchj-zhzbshl = ls_205j7-zhzbsh. "盒子板端头切割损耗(M/块)
        wa_lchj-zhzbjgshl = ls_205j7-zhzbjgsh / 100. "盒子板加工损耗(%)
        "端头处理-灰板等价格  = (配置表中)端头处理-灰板等价格*（板厚 / 50）/平均板长
        wa_lchj-zdthb = ls_205j7-zdthb * ( wa_lchj-zbh / 50 ) / wa_lchj-zbc.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    SELECT * FROM ztpp_205j6
    WHERE werks   = @wa_ggzd-werks
      AND matkl   = @gv_matkl
      AND zsftjbx = @lv_zsftjbx
    INTO TABLE @DATA(lt_205j6).
    LOOP AT lt_205j6 INTO DATA(ls_205j6).
      IF wa_ggzd-zzms > ls_205j6-zpcmsxx AND wa_ggzd-zzms <= ls_205j6-zpcmssx.
        "制造费用
        wa_lchj-zzzfy = ls_205j6-zzzfy.
        wa_lchj-zylcbsh = ls_205j6-zylcbsh / 100. "损耗率%
        "制造加价
        IF wa_lchj-zbh >= 150.
          wa_lchj-zzzjj  = ls_205j6-zzzfyjj .
        ENDIF.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF wa_lchj-zwbxz = '桔纹压花'.
      wa_lchj-zzzjj += 2 .
    ENDIF.
  ENDIF.
  "制造损耗
  PERFORM getzzzsh.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getzzzsh
*&---------------------------------------------------------------------*
*& 获取制造损耗模块费用
*&---------------------------------------------------------------------*
FORM getzzzsh.
  DATA: lv_zfy   TYPE p DECIMALS 2,
        lv_zfc   TYPE p DECIMALS 2,
        lv_zzzfy TYPE zspp225_zz-zzzfy.

  CASE gv_matkl.
    WHEN 'A0200'. "丽彩金
      lv_zfc = wa_lchj-zwl + wa_lchj-zjc_ym_bz + wa_lchj-zjs_fm.
    WHEN OTHERS.
      lv_zfc = wa_lchj-zwl + wa_lchj-zfb_hm_jd_bz + wa_lchj-zjs_fm.
  ENDCASE.

  CASE gv_matkl.
    WHEN 'A0400'. "丽彩钻
      lv_zzzfy = wa_lchj-zmbzzfy.
    WHEN OTHERS.
      lv_zzzfy = wa_lchj-zzzfy + wa_lchj-zzzjj.
  ENDCASE.

  lv_zfy = wa_lchj-zwbmj + wa_lchj-znbmj + wa_lchj-zxcmj
         + lv_zfc
         + lv_zzzfy.

  CASE gv_matkl.
    WHEN 'A0400'. "丽彩钻
      "母板用料成本
      wa_lchj-zmbylcb =  lv_zfy.
      "母板损耗2%
      wa_lchj-zmbsh =  wa_lchj-zmbylcb * wa_lchj-zmbshl.
      "盒子板端头切割损耗0.05M = 盒子板端头切割损耗率 / 平均板长 *母板用料成本
      wa_lchj-zhzbsh = wa_lchj-zhzbshl / wa_lchj-zbc * wa_lchj-zmbylcb.
      "盒子板加工损耗0.4% = 盒子板加工损耗 * 母板用料成本
      wa_lchj-zhzbjgsh = wa_lchj-zhzbjgshl * wa_lchj-zmbylcb.
      "总用料成本 = （盒子板二次加工费+母板用料成本+母板损耗+盒子板端头切割损耗+盒子板加工损耗+端头处理-灰板等价格+技术服务费）/系数
      wa_lchj-zzylcb = ( wa_lchj-zhzbjgf + wa_lchj-zmbylcb + wa_lchj-zmbsh + wa_lchj-zhzbsh + wa_lchj-zhzbjgsh + wa_lchj-zdthb + wa_lchj-zjsfwf ) / wa_lchj-zxs.
    WHEN OTHERS.
      "制造损耗
      wa_lchj-zzzsh = lv_zfy * wa_lchj-zylcbsh.
      "用料成本
      wa_lchj-zylcb = lv_zfy + wa_lchj-zzzsh.
      "总用料成本 = （用料成本 +技术服务费）/系数
      wa_lchj-zzylcb = ( wa_lchj-zylcb + wa_lchj-zjsfwf ) / wa_lchj-zxs.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getzjb
*&---------------------------------------------------------------------*
*& 获取转角板费用/短板切割
*&---------------------------------------------------------------------*
FORM getzjb.
  DATA:lv_je  TYPE p DECIMALS 3,
       lv_zks TYPE ztpp_205a-zks.

  DATA(lv_ZAZFS) = wa_ggzd-zazfs. "01-横装 02-竖装
  LOOP AT it_pcd.
    IF wa_ggzd-zjglx = 'B02'.
      CASE it_pcd-zzjblx.
        WHEN 'L型转角板'.
          CASE lv_ZAZFS.
            WHEN '01'.
              lv_je = lv_je + it_pcd-zyyks * 80.
      