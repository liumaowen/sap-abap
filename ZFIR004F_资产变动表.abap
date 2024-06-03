*&---------------------------------------------------------------------*
*& Report ZFIR004F
*&---------------------------------------------------------------------*
*& 资产变动表
*&---------------------------------------------------------------------*
REPORT zfir004f MESSAGE-ID zgp_msg.

TABLES: anla, anlz, anlb, anlbza,t001,ccss,csks,tfkbt,
        fagl_segmt,
        aufk,
        anlc,
        anlp,
        lfa1,
        bkpf.

DATA:
  g_firstdate LIKE sy-datum,      "当月第一天
  g_lastdate  LIKE sy-datum,      "当月最后一天
  g_anhwsl    LIKE t090na-anhwsl, "截止值码
  g_ahproz    LIKE t091p-ahproz,  "截止转换时的百分率
  g_str       LIKE anlc-kansw.

DATA      g_aibn1 TYPE string.
DATA: BEGIN OF it_list OCCURS 0,
        bukrs         LIKE anep-bukrs,   "公司代码
        anlue         LIKE anla-anlue,   "资产类别
*  add 资产类别描述
        txk20         LIKE ankt-txk20,   "资产类别描述
        anln1         LIKE anep-anln1,   "资产号码
        anln2         LIKE anep-anln2,   "资产子号码
        txt50         LIKE anla-txt50,   "资产名称
        txa50         LIKE anla-txa50,   "规格/载重吨
        sernr         LIKE anla-sernr,   "序列号
        invnr         LIKE anla-invnr,   "存货号/老系统资产编号
        menge         LIKE anla-menge,   "数量
*        zqingdun LIKE anla-menge,"船舶轻吨数
        anlkl         LIKE anla-anlkl,        "资产分类
        zzndjb        LIKE anlb-ndper,  "已使用期间
        meins         LIKE anla-meins,   "计量单位
        ivdat         LIKE anla-ivdat,   "原始过账日期
        invzu         LIKE anla-invzu,   "库存位置
        zugdt         LIKE anla-zugdt,   "资本化日期
        aktiv         LIKE anla-aktiv,   "资本化日期
        deakt         LIKE anla-deakt,   "不活动日期
        gsber         LIKE anlz-gsber,   "业务范围
        gtext         LIKE tgsbt-gtext,   "业务部门描述      ADD  TANZHIM
        kostl         LIKE anlz-kostl,   "成本中心
        ltext         LIKE cskt-ltext,   "成本中心说明
        func_area     LIKE csks-func_area, "功能范围
        fkbtx         LIKE tfkbt-fkbtx, "功能范围描述
        ktext         TYPE auftext,       " 项目描述
        kostlv        LIKE anlz-kostlv, "责任成本中心
        caufn         LIKE anlz-caufn,   "内部订单号
        raumn         LIKE anlz-raumn,   "房间号
        kfzkz         LIKE anlz-kfzkz,   "执照牌号
        ord41         LIKE t087t-ordtx,  "增加方式
        ord42         LIKE t087t-ordtx,  "资产用途
        ord43         LIKE t087t-ordtx,  "是否抵押
        ord44         LIKE t087t-ordtx,  "是否抵押
        gdlgrp        LIKE t087s-gdlgrp_txt, "资金来源
        lifnr         LIKE anla-lifnr,    "供应商编码
        herst         LIKE anla-herst,   "制造商
        vbund         LIKE anla-vbund,   "贸易伙伴
        typbz         LIKE anla-typbz,   "类型名
        zcyz          LIKE anlc-kansw,   "原值
        jzzb          LIKE anlc-kaufw,   "减值准备
        ljzj_zc       LIKE anlc-knafa,   "累计折旧（正常）
        ljzj_jhw      LIKE anlc-knafa,   "累计折旧（计划外）
        zcjz          LIKE anlc-knafa,   "净值
        "schrw LIKE anlb-schrw,   "残值
        schrw         LIKE anlc-kansw,   "残值
*        ZSCHRW        LIKE ANLB-SCHRW,  "残值率
        ahproz        LIKE t091p-ahproz,  "残值率
        zljzj_zc      LIKE  anlc-knafa, "尚需计提折旧
*        add  “上月折旧额”和“本月折旧额”
        month_1       LIKE anlp-nafaz,  "当月折旧
        month_2       LIKE anlp-nafaz,  "上月折旧
        zmonthje1(13) TYPE p DECIMALS 2, "月折旧额
        year_1        LIKE anlp-nafag,  "当前年度折旧    Added by Oliver Yuan
        zmonthje      LIKE anlb-schrw, "月折旧额
        afasl         LIKE anlb-afasl,   "折旧码
        zzndja        LIKE anlb-ndper,  "计划使用期间
        afabg         LIKE anlb-afabg,   "折旧开始日期
        afabe         LIKE anlc-afabe,   "折旧范围
        "ALV增加一个字段，剩余使用期间，显示在已使用期间之后，取数公式为（计划使用期间-已使用期间）进行修改 Start
        zsysyqj       LIKE anlb-ndper,  "剩余使用期间
        "ALV增加一个字段，剩余使用期间，显示在已使用期间之后，取数公式为（计划使用期间-已使用期间）进行修改 End

        "modify by zhangzq 2012-10-27 增加 供应商描述 原额字段
        name1	        LIKE lfa1-name1,    "供应商名称
        zcje          LIKE anlc-knafa,   "净额

        zcybh         TYPE string,        "资产原编号
        eaufn         LIKE anla-eaufn,    "投资订单
        ktext1        TYPE auftext,       " 投资订单描述
        anlhtxt       LIKE anlh-anlhtxt, "设备号
        segment       TYPE anlz-segment,
        aibn1         TYPE anla-aibn1,
        aibn2         TYPE anla-aibn2,
        urjhr         TYPE anla-urjhr,
        urwrt         TYPE anla-urwrt,
        antei         TYPE anla-antei,
        segmt         TYPE fagl_segmt-name,
        eknam         TYPE t024-eknam,
*        VBUND         TYPE ANLA-VBUND,
      END OF it_list.
TYPES: BEGIN OF ty_sk,
         anln1 TYPE anln1,
         eknam TYPE eknam,
       END OF ty_sk.
DATA: enam TYPE STANDARD TABLE OF ty_sk WITH EMPTY KEY.
DATA:BEGIN OF it_sel1 OCCURS 0,
       bukrs TYPE anla-bukrs,
       anln1 TYPE anla-anln1,
       anln2 TYPE anla-anln2,
     END OF it_sel1,
     BEGIN OF it_kostl OCCURS 0,
       kostl TYPE cskt-kostl,
     END OF it_kostl,
     BEGIN OF it_aufnr OCCURS 0,
       aufnr TYPE aufk-aufnr,
     END OF it_aufnr,
     BEGIN OF it_anln1 OCCURS 0,
       anln1 TYPE anla-anln1,
     END OF it_anln1,
     it_tfkbt  TYPE TABLE OF tfkbt WITH HEADER LINE,
     it_lifnr  TYPE TABLE OF lfa1_key WITH HEADER LINE,
     it_t091p  TYPE TABLE OF t091p WITH HEADER LINE,
     it_t090na TYPE TABLE OF t090na WITH HEADER LINE.
DATA:tabix TYPE sy-tabix..
FIELD-SYMBOLS: <it_list> LIKE it_list.
DATA: BEGIN OF it_afapl OCCURS 0,
        bukrs LIKE t093c-bukrs,  "公司代码
        afapl LIKE t093c-afapl,  "有关资产评估的折旧表
      END OF it_afapl.
DATA:r_month TYPE RANGE OF sy-datum WITH HEADER LINE.
DATA:jsrq  TYPE sy-datum,
     low   TYPE syst_datum,
     p_mon TYPE bkpf-monat, "本月期间 计算本月折旧额&上月折旧额
     zrq   TYPE char5.
DATA:msg TYPE bapi_msg.

DATA: BEGIN OF it_anla OCCURS 0,              "资产主记录
        bukrs   LIKE anla-bukrs,   "公司代码
        anlkl   LIKE anla-anlkl,   "资产分类
        anln1   LIKE anla-anln1,   "资产号码
        anln2   LIKE anla-anln2,   "资产子号码
        anlue   LIKE anla-anlue,   "资产类别
        txt50   LIKE anla-txt50,   "资产名称
        txa50   LIKE anla-txa50,   "规格/载重吨
*        txk20 like ankt-txk20,   "资产类别描述
        sernr   LIKE anla-sernr,   "序列号
        invnr   LIKE anla-invnr,   "存货号/老系统资产编号
        menge   LIKE anla-menge,   "数量
        meins   LIKE anla-meins,   "计量单位
        ivdat   LIKE anla-ivdat,   "最后库存日     Added by Oliver 20120413
        invzu   LIKE anla-invzu,   "库存位置
        zugdt   LIKE anla-zugdt,   "资本化日期
        aktiv   LIKE anla-aktiv,   "资本化日期
        deakt   LIKE anla-deakt,   "不活动日期
        ord41   LIKE anla-ord41,   "增加方式
        ord42   LIKE anla-ord42,   "资产用途
        ord43   LIKE anla-ord43,   "是否抵押
        ord44   LIKE anla-ord43,   "是否抵押
        gdlgrp  LIKE anla-gdlgrp, "资金来源
        lifnr   LIKE anla-lifnr,   "供应商编码
        herst   LIKE anla-herst,   "制造商
        vbund   LIKE anla-vbund,   "贸易伙伴
        typbz   LIKE anla-typbz,   "类型名

        name1	  LIKE lfa1-name1,   "供应商名称
        gsber   LIKE anlz-gsber,   "业务范围
        kostl   LIKE anlz-kostl,   "成本中心
        kostlv  LIKE anlz-kostlv, "责任成本中心
        caufn   LIKE anlz-caufn,   "内部订单
        raumn   LIKE anlz-raumn,   "房间号
        kfzkz   LIKE anlz-kfzkz,   "执照牌号（车辆牌照号）

        eaufn   LIKE anla-eaufn,    "投资订单
        anlhtxt LIKE anlh-anlhtxt,
        segment TYPE anlz-segment,
        aibn1   TYPE anla-aibn1,
        aibn2   TYPE anla-aibn2,
        urjhr   TYPE anla-urjhr,
        urwrt   TYPE anla-urwrt,
        antei   TYPE anla-antei,
      END OF it_anla.

*DATA: BEGIN OF it_anlz OCCURS 0,              "时间相关资产分配
*        bukrs LIKE anlz-bukrs,   "公司代码
*        anln1 LIKE anlz-anln1,   "资产号码
*        anln2 LIKE anlz-anln2,   "资产子号码
*        gsber LIKE anlz-gsber,   "业务范围
*        kostl LIKE anlz-kostl,   "成本中心
*        kostlv LIKE anlz-kostlv, "责任成本中心
*        caufn LIKE anlz-caufn,   "内部订单
*        raumn LIKE anlz-raumn,   "房间号
*        kfzkz LIKE anlz-kfzkz,   "执照牌号（车辆牌照号）
*      END OF it_anlz.

DATA: BEGIN OF it_anek OCCURS 0,            "资产过帐凭证抬头
        bukrs LIKE anek-bukrs,   "公司代码
        anln1 LIKE anek-anln1,   "资产号码
        anln2 LIKE anek-anln2,   "资产子号码
        gjahr LIKE anek-gjahr,   "会计年度
        lnran LIKE anek-lnran,   "会计年资产行项目的序号
        budat LIKE anek-budat,   "过帐日期
        belnr LIKE anek-belnr,   "参考凭证编号
        buzei LIKE anek-buzei,   "会计凭证中的行项目数
      END OF it_anek.

DATA: BEGIN OF it_anep OCCURS 0,          "资产行项目
        bukrs  LIKE anep-bukrs,   "公司代码
        anln1  LIKE anep-anln1,   "资产号码
        anln2  LIKE anep-anln2,   "资产子号码
        gjahr  LIKE anep-gjahr,   "会计年度
        lnran  LIKE anep-lnran,   "会计年资产行项目的序号
        budat  LIKE anek-budat,   "过帐日期
        belnr  LIKE anek-belnr,   "参考凭证编号
        buzei  LIKE anek-buzei,   "会计凭证中的行项目数
        afabe  LIKE anep-afabe,   "折旧范围
        bzdat  LIKE anep-bzdat,   "资产价值日
        bwasl  LIKE anep-bwasl,   "资产业务类型
        anbtr  LIKE anep-anbtr,   "记帐金额
        lnsan  LIKE anep-lnsan,   "冲销的资产行项的序列号
        xawbt  LIKE anep-xawbt,   "标记：不同记帐金额输入
        anbtra LIKE anep-anbtr,  "资产原值
        anbtrb LIKE anep-anbtr,  "减值准备
        anbtrc LIKE anep-anbtr,  "累计折旧（正常）
        anbtrd LIKE anep-anbtr,  "累计折旧（未计划）
      END OF it_anep.

DATA: BEGIN OF it_anea OCCURS 0,        "比例值的资产行项目
        bukrs LIKE anea-bukrs,   "公司代码
        anln1 LIKE anea-anln1,   "资产号码
        anln2 LIKE anea-anln2,   "资产子号码
        gjahr LIKE anea-gjahr,   "会计年度
        lnran LIKE anea-lnran,   "会计年资产行项目的序号
        afabe LIKE anea-afabe,   "折旧范围
        invzv LIKE anea-invzv,   "比例累积投资授权
        aufwv LIKE anea-aufwv,   "有关替换值的比例累积重估
        aufwl LIKE anea-aufwl,   "当年有关替换值的比例重估
        nafav LIKE anea-nafav,   "比例累积正常折旧
        safav LIKE anea-safav,   "比例累计特别折旧
        aafav LIKE anea-aafav,   "比例的累积计划外折旧
        invzl LIKE anea-invzl,   "有关此年的比例投资授权
        nafal LIKE anea-nafal,   "此年的比例正常折旧
        safal LIKE anea-safal,   "此年的比例特别折旧
        aafal LIKE anea-aafal,   "此年的比例计划外折旧
      END OF it_anea.

DATA: BEGIN OF it_anlp OCCURS 0,          "资产期间价值
        bukrs   LIKE anlp-bukrs,   "公司代码
        gjahr   LIKE anlp-gjahr,   "会计年度
        peraf   LIKE anlp-peraf,   "折旧计算期
        anln1   LIKE anlp-anln1,   "资产号码
        anln2   LIKE anlp-anln2,   "资产子号码
        afaber  LIKE anlp-afaber, "折旧范围
        bwasl   LIKE anep-bwasl,   "资产业务类型
        aufwz   LIKE anlp-aufwz,   "待过帐的重置值重估
        nafaz   LIKE anlp-nafaz,   "记帐的正常折旧
        safaz   LIKE anlp-safaz,   "待过帐的特殊折旧
        aafaz   LIKE anlp-aafaz,   "待过帐的计划外折旧
        belnr   LIKE anlp-belnr,   "凭证编号
*        anbtrc like anlp-nafaz,
        zanbtrb LIKE anlp-nafaz,
        zanbtrc LIKE anlp-nafaz,
        zanbtrd LIKE anlp-nafaz,
        budat   LIKE bkpf-budat,
        nafag   LIKE anlp-nafag,   "记帐的正常折旧
        safag   LIKE anlp-safag,   "待过帐的特殊折旧
        aafag   LIKE anlp-aafag,   "待过帐的计划外折旧
      END OF it_anlp.

DATA: BEGIN OF it_t087t OCCURS 0,         "评估组描述
        ordnr LIKE t087t-ordnr,   "评审小组号
        ord4x LIKE t087t-ord4x,                             "评审小组1-4
        ordtx LIKE t087t-ordtx,   "描述
      END OF it_t087t.

DATA: BEGIN OF it_t087s OCCURS 0,                           "评估组8描述
        gdlgrp     LIKE t087s-gdlgrp,         "资金来源
        gdlgrp_txt LIKE t087s-gdlgrp_txt,                   "评估组8位文本
      END OF it_t087s.

DATA: BEGIN OF it_ankt OCCURS 0,        "资产类别描述
        anlkl LIKE ankt-anlkl,
        txk20 LIKE ankt-txk20,
      END OF it_ankt.

DATA: BEGIN OF it_tgsbt OCCURS 0,     "业务部门名称
        gsber LIKE tgsbt-gsber,
        gtext LIKE tgsbt-gtext,
      END OF it_tgsbt.

DATA: BEGIN OF it_anlc OCCURS 0,     "资产值字段
        bukrs LIKE anlc-bukrs,   "公司代码
        anln1 LIKE anlc-anln1,   "资产号码
        anln2 LIKE anlc-anln2,   "资产子号码
        gjahr LIKE anlc-gjahr,   "会计年度
        afabe LIKE anlc-afabe,   "折旧范围
        kansw LIKE anlc-kansw,   "累积购置和生产费用
        kaufw LIKE anlc-kaufw,   "重置价值的累计重估
        knafa LIKE anlc-knafa,   "累计正常折旧
        ksafa LIKE anlc-ksafa,   "累计特殊折旧
        kaafa LIKE anlc-kaafa,   "累积计划外折旧
        answl LIKE anlc-answl,   "该年度影响资产值的业务
        aufwb LIKE anlc-aufwb,   "重置价值的重估记帐
        nafag LIKE anlc-nafag,   "记帐在当前年的正常折旧
        safag LIKE anlc-safag,   "在当前财会年度中的记帐的特别折旧
        aafag LIKE anlc-aafag,   "有关年的计划外折旧记帐
        aufwl LIKE anlc-aufwl,   "当年有关替换值的比例重估
        nafal LIKE anlc-nafal,   "此年的比例正常折旧
        safal LIKE anlc-safal,   "此年的比例特别折旧
        aafal LIKE anlc-aafal,   "此年的比例计划外折旧
        aufwv LIKE anlc-aufwv,   "有关替换值的比例累积重估
        nafav LIKE anlc-nafav,   "比例累积正常折旧
        safav LIKE anlc-safav,   "比例累计特别折旧
        aafav LIKE anlc-aafav,   "比例的累积计划外折旧
        aufwp LIKE anlc-aufwp,   "重置值的计划重估
        nafap LIKE anlc-nafap,   "年内已计划正常折旧
        safap LIKE anlc-safap,   "年内已计划特别折旧
        aafap LIKE anlc-aafap,   "年内已预定的未计划折旧
        aufng LIKE anlc-aufng,   "有关累积正常折旧的记帐评估
        zusna LIKE anlc-zusna,   "在正常折旧上的价值增加
        zussa LIKE anlc-zussa,   "在特别折旧上的价值增加
        zusaa LIKE anlc-zusaa,   "在无计划折旧上的价值增加
      END OF it_anlc.

DATA: BEGIN OF it_anlb OCCURS 0,          "折旧期限
        bukrs LIKE anlb-bukrs,   "公司代码
        anln1 LIKE anlb-anln1,   "资产号码
        anln2 LIKE anlb-anln2,   "资产子号码
        afabe LIKE anlb-afabe,   "折旧范围
        schrw LIKE anlb-schrw,   "资产残值
        afasl LIKE anlb-afasl,   "折旧码
        ndjar LIKE anlb-ndjar,   "计划年使用期
        ndper LIKE anlb-ndper,   "计划使用期间
        afabg LIKE anlb-afabg,   "折旧计算开始日期
      END OF it_anlb.

DATA: BEGIN OF it_anlbza OCCURS 0,   "时间相关折旧条款
        bukrs LIKE anlbza-bukrs,   "公司代码
        anln1 LIKE anlbza-anln1,   "资产号码
        anln2 LIKE anlbza-anln2,   "资产子号码
        afabe LIKE anlbza-afabe,   "折旧范围
        schrw LIKE anlbza-schrw,   "资产残值
        afasl LIKE anlbza-afasl,   "折旧码
        ndjar LIKE anlbza-ndjar,   "计划年使用期
        ndper LIKE anlbza-ndper,   "计划使用期间
        afabg LIKE anlb-afabg,     "折旧计算开始日期
      END OF it_anlbza.

DATA: it_peraf1 TYPE peraf,
      it_peraf2 TYPE peraf.
*RANGES s_bukrs FOR t001-bukrs.

DATA: l_monat    LIKE bkpf-monat,
      l_budat(8) TYPE c,
      g_count    TYPE i.                                          "记录数量
DATA tp_msg(50).
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS: s_bukrs FOR t001-bukrs MEMORY ID obligatory. "公司代码
  PARAMETERS: p_gjahr LIKE anek-gjahr DEFAULT sy-datum+0(4) OBLIGATORY.    "会计年度
  SELECT-OPTIONS: s_mon   FOR bkpf-monat DEFAULT sy-datum+4(2) NO-EXTENSION OBLIGATORY.    "期间
SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.


*& PBO
AT SELECTION-SCREEN OUTPUT.
  t1 = '筛选条件'.
  %_s_bukrs_%_app_%-text = '公司代码'.
  %_p_gjahr_%_app_%-text = '会计年度'.
  %_s_mon_%_app_%-text = '期间'.

AT SELECTION-SCREEN.

  PERFORM auth_check .
  PERFORM check_data.

START-OF-SELECTION.
  REFRESH: it_list, it_anla,  it_anlc, it_anek, it_anep, "it_anlz,
           it_anea, it_anlp, it_anlb, it_anlbza, it_t087t.
  PERFORM getmonth CHANGING msg.
  IF msg IS NOT INITIAL.
    MESSAGE s004 WITH msg DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM updatelog(zreplog) IF FOUND.

  PERFORM frm_alv.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       提取报表所需的资产相关数据
*----------------------------------------------------------------------*
FORM get_data.

* 取评估组描述
  SELECT ordnr ord4x ordtx INTO TABLE it_t087t
    FROM t087t
  WHERE spras = sy-langu.
  SORT it_t087t BY ordnr ord4x.

  SELECT gdlgrp gdlgrp_txt INTO TABLE it_t087s
    FROM t087s
  WHERE spras = sy-langu.
  SORT it_t087s BY gdlgrp.

  SELECT anlkl txk20 INTO TABLE it_ankt
  FROM ankt
  WHERE spras = sy-langu.
  SORT it_ankt BY anlkl.

  SELECT gsber gtext INTO TABLE it_tgsbt
  FROM tgsbt
  WHERE spras = sy-langu.
  SORT it_tgsbt BY gsber.
  SELECT * INTO TABLE it_t090na FROM t090na.
  SORT it_t090na BY afapl afasl.
  SELECT * INTO TABLE it_t091p FROM t091p.
  SORT it_t091p BY anhwsl.
  SELECT * INTO TABLE it_tfkbt FROM tfkbt WHERE spras = sy-langu.
  SORT it_tfkbt BY  fkber.
* 取资产主记录
  SELECT anla~bukrs
         anla~anln1
         anla~anln2
         anlkl
         anlue
         txt50
         txa50
         sernr
         invnr
         menge
         meins
         ivdat
         invzu
         aktiv
         deakt
         ord41
         ord42
         ord43
         ord44
         gdlgrp
         lifnr
         herst
         vbund
         typbz
         gsber
         kostl
         kostlv
         caufn
         raumn
         kfzkz
         eaufn
         zugdt
         aktiv
         anlh~anlhtxt
         anlz~segment
         anla~aibn1
         anla~aibn2
         anla~urjhr
         anla~urwrt
         anla~antei
    INTO CORRESPONDING FIELDS OF TABLE it_anla
    FROM anla INNER JOIN anlz
      ON anlz~bukrs = anla~bukrs
      AND anlz~anln1 = anla~anln1
      AND anlz~anln2 = anla~anln2
      INNER  JOIN anlh ON  anlh~bukrs = anla~bukrs
      AND anlh~anln1 = anla~anln1
    WHERE anla~bukrs IN s_bukrs
    AND   aktiv NE '00000000'   "资产资本化日期
    AND   ( aktiv IN r_month OR deakt IN r_month )
*    AND   bdatu GE g_lastdate  "有效日期结束
*    AND   adatu LE g_lastdate "有效期起始日期
  .
  SORT it_anla BY bukrs anln1 anln2.
  DATA(tnam) = it_anla[].
  SORT tnam BY anln1.
  DELETE ADJACENT DUPLICATES FROM tnam COMPARING anln1.
  IF tnam[] IS NOT INITIAL.
    SELECT
      a~anln1,
      c~eknam
      FROM ekkn AS a
      INNER JOIN ekko AS b ON a~ebeln = b~ebeln
      INNER JOIN t024 AS c ON b~ekgrp = c~ekgrp
      FOR ALL ENTRIES IN @tnam
      WHERE a~anln1 = @tnam-anln1
      INTO CORRESPONDING FIELDS OF TABLE @enam.
    SORT enam BY anln1 eknam.
    DELETE ADJACENT DUPLICATES FROM enam COMPARING ALL FIELDS.
  ENDIF.

** 取时间相关资产分配
*  SELECT bukrs anln1 anln2 gsber kostl kostlv caufn raumn kfzkz
*    INTO TABLE it_anlz
*    FROM anlz
*    WHERE bukrs IN s_bukrs
*    AND   anln1 IN s_anln1
*    AND   anln2 IN s_anln2
**    AND   gsber IN s_gsber
*    AND   kostl IN s_kostl
*    AND   kostlv IN s_kostlv   "成本中心对资产负责
*    AND   bdatu GE g_lastdate  "有效日期结束
*    AND   adatu LE g_lastdate "有效期起始日期
**    AND   caufn IN s_caufn.   "Added by Oliver 20120413
*    .
*  SORT it_anlz BY bukrs anln1 anln2.
  IF it_anla[] IS NOT INITIAL.
* 取折旧期限
    SELECT bukrs anln1 anln2 afabe schrw afasl ndjar ndper afabg
      INTO TABLE it_anlb
      FROM anlb
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
    AND   afabe  = '01'."in s_afabe.  实际折旧范围
    SORT it_anlb BY bukrs anln1 anln2.

* 取折旧期限
    SELECT bukrs anln1 anln2 afabe schrw afasl ndjar ndper
      INTO TABLE it_anlbza
      FROM anlb
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   afabe  = '01'   "in s_afabe
      AND   adatu <= g_lastdate
    AND   bdatu >= g_lastdate.
    SORT it_anlbza BY bukrs anln1 anln2.
    LOOP AT it_anlbza.
      READ TABLE it_anlb WITH KEY bukrs = it_anlbza-bukrs anln1 = it_anlbza-anln1 anln2 = it_anlbza-anln2 afabe = it_anlbza-afabe BINARY SEARCH.
      IF sy-subrc = 0.
        it_anlbza-afabg = it_anlb-afabg.
        MODIFY it_anlbza TRANSPORTING afabg.
        CLEAR it_anlb.
      ENDIF.
    ENDLOOP.

* 取资产值字段
    SELECT bukrs anln1 anln2 gjahr afabe kansw answl kaufw aufwb
           aufwl aufwv knafa ksafa kaafa nafag safag aufng zusna
           zussa zusaa nafav safav aafav nafal safal aafal aafag
           aufwp nafap safap aafap
      INTO CORRESPONDING FIELDS OF TABLE it_anlc
      FROM anlc
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   gjahr = p_gjahr
    AND   afabe  = '01'."in s_afabe.
    SORT it_anlc BY bukrs anln1 anln2.

* 取凭证抬头资产过帐
    SELECT bukrs anln1 anln2 gjahr lnran budat belnr buzei
      INTO CORRESPONDING FIELDS OF TABLE it_anek
      FROM anek
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   gjahr = p_gjahr
    AND   monat IN s_mon.
*    AND   budat GT g_lastdate.
    SORT it_anek BY bukrs anln1 anln2 gjahr lnran.
  ENDIF.

  IF NOT it_anek[] IS INITIAL.
*   取资产行项目
    SELECT bukrs anln1 anln2 gjahr lnran belnr buzei afabe bzdat bwasl anbtr xawbt lnsan
      INTO CORRESPONDING FIELDS OF TABLE it_anep
      FROM anep
      FOR ALL ENTRIES IN it_anek
      WHERE bukrs = it_anek-bukrs
      AND   anln1 = it_anek-anln1
      AND   anln2 = it_anek-anln2
      AND   gjahr = it_anek-gjahr
      AND   lnran = it_anek-lnran
      AND   afabe  = '01' "in s_afabe
      AND   bwasl NE 'Z31'
    AND   bwasl NOT BETWEEN 600 AND 699.
*      and   bzdat gt g_lastdate.
    SORT it_anep BY bukrs anln1 anln2 gjahr lnran afabe.

*   取比例值的资产行项目
    IF NOT it_anep[] IS INITIAL.
      SELECT bukrs anln1 anln2 gjahr lnran afabe invzv aufwv
             aufwl nafav safav aafav invzl nafal safal aafal
        INTO TABLE it_anea
        FROM anea
        FOR ALL ENTRIES IN it_anep
        WHERE bukrs = it_anep-bukrs
        AND   anln1 = it_anep-anln1
        AND   anln2 = it_anep-anln2
        AND   gjahr = it_anep-gjahr
        AND   lnran = it_anep-lnran   "会计年资产行项目的序号
      AND   afabe = it_anep-afabe.
      SORT it_anea BY bukrs anln1 anln2 gjahr lnran afabe.
    ENDIF.
  ENDIF.

* 取资产期间价值
  SELECT bukrs gjahr peraf anln1 anln2 afaber
         aufwz nafaz safaz aafaz belnr
         nafag safag aafag
    INTO CORRESPONDING FIELDS OF TABLE it_anlp
    FROM anlp
    WHERE bukrs IN s_bukrs
    AND   gjahr = p_gjahr
    AND   afaber  = '01'  "in s_afabe  实际的或派生的折旧范围
  AND   peraf IN s_mon.
  SORT it_anlp BY bukrs anln1 anln2 peraf afaber.
ENDFORM.                                    "get_data

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       处理数据生成用于输出alv的内部表
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_nafag LIKE anlp-nafag,
        l_safag LIKE anlp-safag,
        l_aafag LIKE anlp-aafag.
  DATA: l_flag(1),
        l_x         TYPE p15_months,
        l_datetime  LIKE sy-datum,
        l_gjahrm(6) TYPE c.
  SELECT *
    INTO TABLE @DATA(it_segm)
    FROM fagl_segmt
    WHERE langu = @sy-langu.
  SORT it_segm BY segment.
  LOOP AT it_anlc.
    CLEAR it_list.
    it_list-bukrs = it_anlc-bukrs.
    it_list-anln1 = it_anlc-anln1.
    it_list-anln2 = it_anlc-anln2.
    it_list-afabe = it_anlc-afabe.
    it_list-zcyz = it_anlc-kansw + it_anlc-answl.
    it_list-jzzb = ( it_anlc-kaufw + it_anlc-aufwb + it_anlc-aufwl + it_anlc-aufwv ) * -1.

    it_list-ljzj_zc = ( it_anlc-knafa + it_anlc-nafav ) * -1.
    it_list-ljzj_jhw = ( it_anlc-aafag + it_anlc-aafal ) * -1.

    COLLECT it_list.
    PERFORM fillsel.
  ENDLOOP.

  LOOP AT it_anep.
    READ TABLE it_anek WITH KEY bukrs = it_anep-bukrs
                                anln1 = it_anep-anln1
                                anln2 = it_anep-anln2
                                gjahr = it_anep-gjahr
                                lnran = it_anep-lnran
                                BINARY SEARCH.

    it_anep-anbtra = it_anep-anbtr.

*    if it_anep-xawbt = space or it_anep-xawbt is initial.
    READ TABLE it_anea WITH KEY bukrs = it_anep-bukrs
                                anln1 = it_anep-anln1
                                anln2 = it_anep-anln2
                                gjahr = it_anep-gjahr
                                lnran = it_anep-lnran
                                afabe = it_anep-afabe
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_anep-anbtrb = - it_anea-aufwv - it_anea-aufwl.
      IF g_lastdate+4(4) EQ '1231'.
        it_anep-anbtrc = - it_anea-nafav - it_anea-nafal - it_anea-safav - it_anea-safal - it_anea-aafav - it_anea-aafal.
        it_anep-anbtrd = 0.
      ELSE.
        it_anep-anbtrc = - it_anea-nafav - it_anea-nafal - it_anea-safav - it_anea-aafav.
        it_anep-anbtrd = - it_anea-safal - it_anea-aafal.
      ENDIF.
    ENDIF.

    CLEAR it_list.
    it_list-bukrs = it_anep-bukrs.
    it_list-anln1 = it_anep-anln1.
    it_list-anln2 = it_anep-anln2.
    it_list-afabe = it_anep-afabe.


    it_list-zcyz = - it_anep-anbtra.
    it_list-jzzb = - it_anep-anbtrb.
    it_list-ljzj_zc = - it_anep-anbtrc.
    it_list-ljzj_jhw = - it_anep-anbtrd.
    COLLECT it_list.
    PERFORM fillsel.
  ENDLOOP.

  LOOP AT it_anlp.
    IF it_anlp-aufwz <> 0.
      it_anlp-bwasl = 'Z31'.
      it_anlp-zanbtrb = it_anlp-aufwz.
    ENDIF.

    IF it_anlp-nafaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrc = it_anlp-nafaz.
    ENDIF.

    IF it_anlp-safaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrd = it_anlp-safaz.
    ENDIF.

    IF it_anlp-aafaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrd = it_anlp-zanbtrd + it_anlp-aafaz.
    ENDIF.

    IF it_anlp-peraf > 12.
      l_monat = 12.
    ELSE.
      l_monat = it_anlp-peraf.
    ENDIF.
    CLEAR: l_datetime,l_gjahrm.
    CONCATENATE it_anlp-gjahr l_monat '01' INTO l_datetime.
    CONCATENATE it_anlp-gjahr it_anlp-peraf+1(2) INTO l_gjahrm.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = l_datetime
      IMPORTING
        e_date = it_anlp-budat.

    IF l_gjahrm > g_lastdate+0(6).
      CLEAR it_list.
      it_list-bukrs = it_anlp-bukrs.
      it_list-anln1 = it_anlp-anln1.
      it_list-anln2 = it_anlp-anln2.
      it_list-afabe = it_anlp-afaber.
      it_list-jzzb = it_anlp-zanbtrb.
      it_list-ljzj_zc = it_anlp-zanbtrc.
      it_list-ljzj_jhw = it_anlp-zanbtrd.
      COLLECT it_list.
      PERFORM fillsel.
    ENDIF.
  ENDLOOP.
********ADD BY DONGPZ BEGIN AT 01.03.2023 09:32:08
  LOOP AT it_anla.
    CLEAR:it_lifnr.
    it_lifnr-lifnr = it_anla-lifnr.
    COLLECT it_lifnr.
  ENDLOOP.
  DELETE it_lifnr WHERE lifnr IS INITIAL.
  IF it_lifnr[] IS NOT INITIAL.
    SORT it_lifnr BY lifnr.
    SELECT *
      INTO TABLE @DATA(it_lfa1)
      FROM lfa1
      FOR ALL ENTRIES IN @it_lifnr
      WHERE lifnr = @it_lifnr-lifnr.
    SORT it_lfa1 BY lifnr.
  ENDIF.
  IF it_sel1[] IS NOT INITIAL.
    SORT it_sel1 BY bukrs anln1 anln2.
    SELECT *
      INTO TABLE @DATA(gt_anla)
      FROM anla
      FOR ALL ENTRIES IN @it_sel1
      WHERE bukrs = @it_sel1-bukrs
      AND   anln1 = @it_sel1-anln1
      AND   anln2 = @it_sel1-anln2.
    SORT gt_anla BY bukrs anln1 anln2.
  ENDIF.

********ADD BY DONGPZ END AT 01.03.2023 09:32:08
  CLEAR:it_sel1[].
  LOOP AT it_list ASSIGNING <it_list>.
    READ TABLE it_anla WITH KEY bukrs = <it_list>-bukrs
                                anln1 = <it_list>-anln1
                                anln2 = <it_list>-anln2
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_list.
    ELSEIF sy-subrc EQ 0.
      <it_list>-anlue = it_anla-anlue.
      <it_list>-txt50 = it_anla-txt50.
      <it_list>-txa50 = it_anla-txa50.
      <it_list>-invnr = it_anla-invnr.
      <it_list>-sernr = it_anla-sernr.
      <it_list>-menge = it_anla-menge.
      <it_list>-meins = it_anla-meins.
      <it_list>-ivdat = it_anla-ivdat.  "最后库存日
      <it_list>-invzu = it_anla-invzu.
      <it_list>-zugdt = it_anla-zugdt.  "资本化日期
      <it_list>-deakt = it_anla-deakt.
      <it_list>-herst = it_anla-herst.
      <it_list>-vbund = it_anla-vbund.
      <it_list>-typbz = it_anla-typbz.
      <it_list>-anlkl = it_anla-anlkl.
      <it_list>-lifnr = it_anla-lifnr.
      <it_list>-segment = it_anla-segment.
      <it_list>-aibn1   = it_anla-aibn1  .
      <it_list>-aibn2   = it_anla-aibn2  .
      <it_list>-urjhr   = it_anla-urjhr  .
      <it_list>-urwrt   = it_anla-urwrt  .
      <it_list>-antei   = it_anla-antei  .
      READ TABLE it_segm INTO fagl_segmt WITH KEY segment = <it_list>-segment BINARY SEARCH.
      IF sy-subrc EQ 0.
        <it_list>-segmt = fagl_segmt-name.
      ENDIF.

      <it_list>-eaufn = it_anla-eaufn.    "投资订单
      <it_list>-anlhtxt = it_anla-anlhtxt.
      <it_list>-raumn = it_anla-raumn.
      READ TABLE it_lfa1 INTO lfa1 WITH KEY lifnr = it_anla-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-name1 = lfa1-name1.
      ELSE."当资产编号没用供应商编码时，以固定资产主数据中的供应商名称替换。LIUZEHNG
        READ TABLE gt_anla INTO anla WITH KEY bukrs = it_anla-bukrs
                                              anln1 = it_anla-anln1
                                              anln2 = it_anla-anln2
                                              BINARY SEARCH.
        IF sy-subrc EQ 0.
          <it_list>-name1 = anla-liefe.
        ENDIF.
      ENDIF.

      READ TABLE it_t087s WITH KEY gdlgrp = it_anla-gdlgrp BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-gdlgrp = it_t087s-gdlgrp_txt.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '1'
                                   ord4x = it_anla-ord41
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord41 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '2'
                                   ord4x = it_anla-ord42
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord42 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '3'
                                   ord4x = it_anla-ord43
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord43 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '4'
                                   ord4x = it_anla-ord44
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord44 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_ankt WITH  KEY anlkl = it_anla-anlkl BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-txk20 = it_ankt-txk20.
      ENDIF.

*      READ TABLE it_anlz WITH KEY bukrs = <it_list>-bukrs
*                             anln1 = <it_list>-anln1
*                             anln2 = <it_list>-anln2
*                             BINARY SEARCH.
*      IF sy-subrc EQ 0.
      <it_list>-gsber = it_anla-gsber.
      <it_list>-kostl = it_anla-kostl.
      <it_list>-kostlv = it_anla-kostlv.
      <it_list>-raumn = it_anla-raumn.
      <it_list>-kfzkz = it_anla-kfzkz.
      <it_list>-caufn = it_anla-caufn.
*      ENDIF.

      "12、  根据用户输入条件：BUKRS、ANLN1、 ANLN2、AFABE、GJAHR、ZMONTHH查选表ANLBZA取BUKRS、
      "ANLN1、 ANLN2、AFASL、NDJAR、NDPER、SCHRW、AFABG关联ZANEPTEMP2，取ZANEPTEMP2所有字段+
      "AFASL、NDJAR、NDPER、SCHRW、AFABG，生成表ZANEPTEMP3：如果未找到ANLBZA记录，则根据用户输
      "入条件：BUKRS、ANLN1、 ANLN2、AFABE关联ANLB，生成表ZANEPTEMP3
      READ TABLE it_anlbza WITH KEY bukrs = <it_list>-bukrs
                            anln1 = <it_list>-anln1
                            anln2 = <it_list>-anln2
                            afabe = <it_list>-afabe
                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF it_anlbza-schrw <> 0.
          <it_list>-schrw = it_anlbza-schrw.
        ELSE.
          READ TABLE it_afapl WITH KEY bukrs = <it_list>-bukrs BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR:g_anhwsl,g_ahproz.
            READ TABLE it_t090na WITH KEY afapl = it_afapl-afapl
                                          afasl = it_anlbza-afasl
                                          BINARY SEARCH.
            IF sy-subrc EQ 0.
              g_anhwsl = it_t090na-anhwsl.
              READ TABLE it_t091p WITH KEY anhwsl = g_anhwsl BINARY SEARCH.
              IF sy-subrc EQ 0.
                g_ahproz = it_t091p-ahproz.
*                <IT_LIST>-SCHRW = ( <IT_LIST>-ZCYZ - <IT_LIST>-JZZB - <IT_LIST>-SCHRW ) * G_AHPROZ / 100.

                <it_list>-schrw =  <it_list>-zcyz * g_ahproz / 100.
*                <IT_LIST>-SCHRW =  <IT_LIST>-SCHRW / 100.
                <it_list>-ahproz = g_ahproz.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        <it_list>-afasl = it_anlbza-afasl.
        <it_list>-afabg = it_anlbza-afabg.
        <it_list>-zzndja = it_anlbza-ndjar * 12 + it_anlbza-ndper.

        CLEAR l_x.
        IF <it_list>-afabg >= g_lastdate.
          CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
            EXPORTING
              begda  = g_lastdate
              endda  = <it_list>-afabg
            IMPORTING
              months = l_x.
        ELSE.
          CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
            EXPORTING
              begda  = <it_list>-afabg
              endda  = g_lastdate
            IMPORTING
              months = l_x.
        ENDIF.
        <it_list>-zzndjb = l_x.

        IF <it_list>-zzndjb > <it_list>-zzndja.
          <it_list>-zzndjb = <it_list>-zzndja.
        ENDIF.
      ELSE.
        READ TABLE it_anlb WITH KEY bukrs = <it_list>-bukrs
                              anln1 = <it_list>-anln1
                              anln2 = <it_list>-anln2
                              afabe = <it_list>-afabe
                              BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF it_anlb-schrw <> 0.
            <it_list>-schrw = it_anlb-schrw.
          ELSE.
            READ TABLE it_afapl WITH KEY bukrs = <it_list>-bukrs BINARY SEARCH.
            IF sy-subrc = 0.
              CLEAR:g_anhwsl,g_ahproz.
              READ TABLE it_t090na WITH KEY afapl = it_afapl-afapl
                                            afasl = it_anlb-afasl
                                            BINARY SEARCH.
              IF sy-subrc EQ 0.
                g_anhwsl = it_t090na-anhwsl.
                READ TABLE it_t091p WITH KEY anhwsl = g_anhwsl BINARY SEARCH.
                IF sy-subrc EQ 0.
                  g_ahproz = it_t091p-ahproz.
                  " <it_list>-schrw = ( <it_list>-zcyz - <it_list>-jzzb ) * g_ahproz / 100.
                  <it_list>-schrw = <it_list>-zcyz * g_ahproz / 100.
                  <it_list>-ahproz = g_ahproz.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          <it_list>-afasl = it_anlb-afasl.
          <it_list>-afabg = it_anlb-afabg.
          <it_list>-zzndja = it_anlb-ndjar * 12 + it_anlb-ndper.

          CLEAR l_x.
          IF <it_list>-afabg >= g_lastdate.
            CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
              EXPORTING
                begda  = g_lastdate
                endda  = <it_list>-afabg
              IMPORTING
                months = l_x.
          ELSE.
            CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
              EXPORTING
                begda  = <it_list>-afabg
                endda  = g_lastdate
              IMPORTING
                months = l_x.
          ENDIF.
          <it_list>-zzndjb = l_x.

          IF <it_list>-zzndjb > <it_list>-zzndja.
            <it_list>-zzndjb = <it_list>-zzndja.
          ENDIF.
        ENDIF.
      ENDIF.

      IF g_lastdate+4(4) EQ '1231'.
        <it_list>-ljzj_zc = <it_list>-ljzj_zc + <it_list>-ljzj_jhw.
        <it_list>-ljzj_jhw = 0.
      ENDIF.

      IF <it_list>-zzndjb = <it_list>-zzndja.  "已使用期间 = 计划使用期间
        <it_list>-zmonthje1 = 0.
      ELSE.
        <it_list>-zmonthje1 = ( <it_list>-zcyz - <it_list>-jzzb - <it_list>-ljzj_zc - <it_list>-schrw ) / ( <it_list>-zzndja - <it_list>-zzndjb ).
        "  下月折旧 = ( zcyz "原值 - jzzb "减值准备 - ljzj_zc "累计折旧（正常）- schrw 残值 )  / ( 计划使用期间 - 实际使用期间 )
      ENDIF.

      <it_list>-zmonthje = <it_list>-zmonthje1. "下月折旧
      "modify by zhangzq 2012-10-27
      "<it_list>-zcjz = <it_list>-zcyz - <it_list>-jzzb - <it_list>-ljzj_zc - <it_list>-ljzj_jhw."净值 = 原值 - 减值准备 - 累计折旧（计划内） - 累计折旧（计划外）
      <it_list>-zcjz = <it_list>-zcyz - <it_list>-ljzj_zc."净值 = 原值 - 累计折旧（计划内）
      <it_list>-zcje = <it_list>-zcyz - <it_list>-ljzj_zc."净额 = 原值 - 累计折旧（计划内）
*        <IT_LIST>-ZSCHRW = ( <IT_LIST>-SCHRW / <IT_LIST>-ZCYZ ) * 100 . "残值率
      <it_list>-zljzj_zc = <it_list>-zcyz - <it_list>-ljzj_zc - <it_list>-schrw."尚需计提折旧

      "ENDIF. "LUZHENG 20230505
      CLEAR:it_kostl,it_aufnr,it_sel1.
      it_sel1-bukrs = <it_list>-bukrs.
      it_sel1-anln1 = <it_list>-anln1.
      it_sel1-anln2 = <it_list>-anln2.
      it_kostl-kostl = <it_list>-kostl.
      it_aufnr-aufnr = <it_list>-caufn.
      it_anln1-anln1 = <it_list>-anln1.
      COLLECT:it_aufnr,it_sel1,it_anln1,it_kostl.
      it_aufnr-aufnr = <it_list>-eaufn.
      COLLECT:it_aufnr.
    ENDIF.
  ENDLOOP.
  DELETE it_kostl WHERE kostl IS INITIAL.
  IF it_kostl[] IS NOT INITIAL.
    SORT it_kostl BY kostl.
    SELECT csks~kostl,
           csks~func_area,
           cskt~ktext,
           cskt~ltext
      INTO TABLE @DATA(it_cskt)
      FROM csks INNER JOIN cskt ON csks~kostl = cskt~kostl
                               AND csks~kokrs = cskt~kokrs
                               AND csks~datbi = cskt~datbi
      FOR ALL ENTRIES IN @it_kostl
      WHERE csks~kostl = @it_kostl-kostl
      AND   cskt~spras = @sy-langu
      AND   csks~kokrs = 'WISD'.
    SORT it_cskt BY kostl.
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
  IF it_sel1[] IS NOT INITIAL.
    SORT it_sel1 BY bukrs anln1 anln2.
    SELECT *
      INTO TABLE @DATA(it_anlp)
      FROM anlp
      FOR ALL ENTRIES IN @it_sel1
      WHERE bukrs = @it_sel1-bukrs
      AND   anln1 = @it_sel1-anln1
      AND   anln2 = @it_sel1-anln2
      AND   gjahr = @p_gjahr.
*      AND   PERAF = @IT_PERAF1.  "折旧计算期
    SORT it_anlp BY bukrs anln1 anln2 peraf afaber.
    SELECT *
      INTO TABLE @DATA(gt_anlc)
      FROM anlc
      FOR ALL ENTRIES IN @it_sel1
      WHERE bukrs = @it_sel1-bukrs
      AND   anln1 = @it_sel1-anln1
      AND   anln2 = @it_sel1-anln2
      AND   gjahr = @p_gjahr
*      AND   PERAF = @IT_PERAF1.  "折旧计算期
      AND   afabe = '01'.
    SORT gt_anlc BY bukrs anln1 anln2.
  ENDIF.
*找原始资产编号
  DELETE it_anln1 WHERE anln1 IS INITIAL.
*  IF IT_ANLN1[] IS NOT INITIAL.
*    SORT IT_ANLN1 BY ANLN1.
*    SELECT *
*      INTO TABLE GT_ANLA
*      FROM ANLA
*      FOR ALL ENTRIES IN IT_ANLN1
*      WHERE ANLN1 = IT_ANLN1-ANLN1
*      AND   BUKRS IN S_BUKRS.
*    SORT GT_ANLA BY ANLN1.
*  ENDIF.
  "ALV增加一个字段，剩余使用期间，显示在已使用期间之后，取数公式为（计划使用期间-已使用期间）

  LOOP AT it_list ASSIGNING <it_list>.
*    IF <it_list>-zcyz = 0 AND
*       <it_list>-jzzb = 0 AND
*       <it_list>-ljzj_zc = 0 AND
*       <it_list>-ljzj_jhw = 0 AND
*       <it_list>-zcjz = 0.
*      " DELETE it_list.
*    ELSE.
    READ TABLE enam INTO DATA(wa_nam) WITH KEY anln1 = <it_list>-anln1 BINARY SEARCH.
    IF sy-subrc = 0.
      <it_list>-eknam = wa_nam-eknam.
    ENDIF.
    CLEAR wa_nam.
*  取项目描述 aufk
    READ TABLE it_cskt INTO DATA(wa_cskt) WITH KEY kostl = <it_list>-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-ltext = wa_cskt-ltext.
      <it_list>-func_area = wa_cskt-func_area.
    ENDIF.
    READ TABLE it_tfkbt WITH KEY fkber = <it_list>-func_area BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-fkbtx = it_tfkbt-fkbtx.
    ENDIF.
    READ TABLE it_aufk INTO aufk WITH KEY aufnr = <it_list>-caufn BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-ktext = aufk-ktext.
    ENDIF.
    READ TABLE it_aufk INTO aufk WITH KEY aufnr = <it_list>-eaufn BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-ktext1 = aufk-ktext.
    ENDIF.

    READ TABLE it_tgsbt WITH  KEY gsber = <it_list>-gsber BINARY SEARCH.
    IF sy-subrc = 0.
      <it_list>-gtext = it_tgsbt-gtext.
    ENDIF.
    <it_list>-zsysyqj = <it_list>-zzndja - <it_list>-zzndjb.
*    ENDIF.

*变更折旧日期

    IF <it_list>-zugdt IS INITIAL.
      <it_list>-zugdt = <it_list>-aktiv.
    ENDIF.
*&
*    CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE '
*      EXPORTING
*        MONTHS  = 1
*        OLDDATE = <IT_LIST>-ZUGDT
*      IMPORTING
*        NEWDATE = <IT_LIST>-AFABG.
    "本期折旧&上期折旧
    IF <it_list>-bukrs IN s_bukrs.
      it_peraf1 = p_mon .
      it_peraf2 = p_mon - 1.

*    SELECT SINGLE nafaz INTO <IT_LIST>-month_1 FROM anlp
*      WHERE  bukrs = <IT_LIST>-bukrs "= <<IT_LIST>>-bukrs
*         AND anln1 = <IT_LIST>-anln1
*         AND anln2 = <IT_LIST>-anln2
*         AND gjahr = p_gjahr
*         AND peraf = it_peraf1.  "折旧计算期
      READ TABLE it_anlp INTO anlp WITH KEY bukrs = <it_list>-bukrs
                                            anln1 = <it_list>-anln1
                                            anln2 = <it_list>-anln2
                                            peraf = it_peraf1
                                            afaber = '01'
                                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        tabix = sy-tabix.
        LOOP AT it_anlp INTO anlp FROM tabix.
          IF anlp-bukrs NE <it_list>-bukrs
            OR anlp-anln1 NE <it_list>-anln1
            OR anlp-anln2 NE <it_list>-anln2
            OR anlp-peraf NE it_peraf1
            OR anlp-afaber NE '01'.
            EXIT.
          ENDIF.
          <it_list>-month_1 = <it_list>-month_1 + anlp-nafaz.
        ENDLOOP.
      ENDIF.
      READ TABLE it_anlp INTO anlp WITH KEY bukrs = <it_list>-bukrs
                                            anln1 = <it_list>-anln1
                                            anln2 = <it_list>-anln2
                                            peraf = it_peraf2
                                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        <it_list>-month_2 = anlp-nafaz.
      ENDIF.

      <it_list>-month_1 = 0 - <it_list>-month_1.
      <it_list>-month_2 = 0 - <it_list>-month_2.
*      READ TABLE gt_anlc INTO anlc WITH KEY bukrs = <it_list>-bukrs
*                                            anln1 = <it_list>-anln1
*                                            anln2 = <it_list>-anln2
*                                            BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        tabix = sy-tabix.
*        LOOP AT gt_anlc INTO anlc FROM tabix.
*          IF anlc-bukrs NE <it_list>-bukrs
*            OR anlc-anln1 NE <it_list>-anln1
*            OR anlc-anln2 NE <it_list>-anln2.
*            EXIT.
*          ENDIF.
*          <it_list>-year_1 = anlc-nafag + anlc-safag + anlc-aafag.
*          <it_list>-year_1 = 0 - <it_list>-year_1.
*        ENDLOOP.
*      ENDIF.
      READ TABLE it_anlp INTO anlp WITH KEY bukrs = <it_list>-bukrs
                                      anln1 = <it_list>-anln1
                                      anln2 = <it_list>-anln2
                                      afaber = '01'
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        tabix = sy-tabix.
        LOOP AT it_anlp INTO anlp FROM tabix.
          IF anlp-bukrs NE <it_list>-bukrs
            OR anlp-anln1 NE <it_list>-anln1
            OR anlp-anln2 NE <it_list>-anln2
            OR anlp-peraf NE it_peraf1
            OR anlp-afaber NE '01'.
            EXIT.
          ENDIF.
          <it_list>-year_1 = <it_list>-year_1 + anlp-nafap.
        ENDLOOP.
        <it_list>-year_1 = 0 - <it_list>-year_1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "本期折旧&上期折旧
*  LOOP AT IT_LIST WHERE BUKRS IN S_BUKRS.
*    IT_PERAF1 = P_MONAT .
*    IT_PERAF2 = P_MONAT - 1.
*
**    SELECT SINGLE nafaz INTO it_list-month_1 FROM anlp
**      WHERE  bukrs = it_list-bukrs "= <it_list>-bukrs
**         AND anln1 = it_list-anln1
**         AND anln2 = it_list-anln2
**         AND gjahr = p_gjahr
**         AND peraf = it_peraf1.  "折旧计算期
*    READ TABLE IT_ANLP INTO ANLP WITH KEY BUKRS = IT_LIST-BUKRS
*                                          ANLN1 = IT_LIST-ANLN1
*                                          ANLN2 = IT_LIST-ANLN2
*                                          AFABER = '01'
*                                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      TABIX = SY-TABIX.
*      LOOP AT IT_ANLP INTO ANLP FROM TABIX.
*        IF ANLP-BUKRS NE IT_LIST-BUKRS
*          OR ANLP-ANLN1 NE IT_LIST-ANLN1
*          OR ANLP-ANLN2 NE IT_LIST-ANLN2
*          OR ANLP-AFABER NE '01'.
*          EXIT.
*        ENDIF.
*        IT_LIST-MONTH_1 = IT_LIST-MONTH_1 + ANLP-NAFAZ.
*      ENDLOOP.
*    ENDIF.
*    READ TABLE IT_ANLP INTO ANLP WITH KEY BUKRS = IT_LIST-BUKRS
*                                          ANLN1 = IT_LIST-ANLN1
*                                          ANLN2 = IT_LIST-ANLN2
*                                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      IT_LIST-MONTH_2 = ANLP-NAFAZ.
*    ENDIF.
*
*    IT_LIST-MONTH_1 = 0 - IT_LIST-MONTH_1.
*    IT_LIST-MONTH_2 = 0 - IT_LIST-MONTH_2.
*    READ TABLE GT_ANLC INTO ANLC WITH KEY BUKRS = IT_LIST-BUKRS
*                                          ANLN1 = IT_LIST-ANLN1
*                                          ANLN2 = IT_LIST-ANLN2
*                                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      TABIX = SY-TABIX.
*      LOOP AT GT_ANLC INTO ANLC FROM TABIX.
*        IF ANLC-BUKRS NE IT_LIST-BUKRS
*          OR ANLC-ANLN1 NE IT_LIST-ANLN1
*          OR ANLC-ANLN2 NE IT_LIST-ANLN2.
*          EXIT.
*        ENDIF.
*        IT_LIST-YEAR_1 = ANLC-NAFAG + ANLC-SAFAG + ANLC-AAFAG.
*        IT_LIST-YEAR_1 = 0 - IT_LIST-YEAR_1.
*      ENDLOOP.
*    ENDIF.
*    MODIFY IT_LIST TRANSPORTING MONTH_1 MONTH_2 YEAR_1.
*  ENDLOOP.
*&------计算资产原编号----MODIFY BY JW_ZHANGZQ---BEGIN-------------------*
  LOOP AT it_list.
    CLEAR g_aibn1.
    PERFORM frm_get_aibn1 USING it_list-anln1 it_list-bukrs.
    it_list-zcybh = g_aibn1.

    IF NOT g_aibn1 IS INITIAL.
      MODIFY it_list.
    ENDIF.
  ENDLOOP.
*&------计算资产原编号----MODIFY BY JW_ZHANGZQ---END---------------------*

  "ALV增加一个字段，剩余使用期间，显示在已使用期间之后，取数公式为（计划使用期间-已使用期间）进行修改 End
ENDFORM.                                    "process_data

*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       检查用户输入的查询条件
*----------------------------------------------------------------------*
FORM check_data.
  DATA: l_bukrs     LIKE t001-bukrs,
        l_anlkl     LIKE ankb-anlkl,
        l_rcomp     LIKE t880-rcomp,
        l_datec(10) TYPE c,
        l_afaber    LIKE t093-afaber.

* 检查公司代码
  SELECT SINGLE bukrs INTO l_bukrs
    FROM t001
  WHERE bukrs IN s_bukrs.

  IF sy-subrc NE 0.
*   MESSAGE：公司代码不存在
    MESSAGE e009(zxmd_msg_fico).
  ENDIF.


* 检查折旧范围
  "  if not s_afabe is initial.
  SELECT bukrs afapl INTO TABLE it_afapl
    FROM t093c
  WHERE bukrs IN s_bukrs.
  SORT it_afapl BY bukrs.

  IF NOT it_afapl[] IS INITIAL.
    SELECT afaber INTO l_afaber
      FROM t093
      FOR ALL ENTRIES IN it_afapl
      WHERE afapl = it_afapl-afapl
      AND   xstore = 'X'
      AND   afaber = '01'."in s_afabe.
    ENDSELECT.
  ENDIF.

  IF sy-subrc NE 0.
*     MESSAGE：折旧范围不存在
    MESSAGE e011(zxmd_msg_fico).
  ENDIF.
  " endif.

* 检查期间
*  IF p_mon GT 16 OR p_mon LT 1.
**   MESSAGE：期间应在1－16范围内
*    MESSAGE e012(zxmd_msg_fico).
*  ENDIF.

* 检查旧资产数据转帐的起息日
  CLEAR: l_datec, g_firstdate, g_lastdate.
  "ZZTR028资产清单报表（修改推导主程序，与ZZTR117一致）Start
*  concatenate p_gjahr p_monat '01' into l_datec.
  IF p_mon > 12.
    CONCATENATE p_gjahr  '1201' INTO l_datec.
  ELSE.
    CONCATENATE p_gjahr p_mon '01' INTO l_datec.
  ENDIF.
  " ZZTR028资产清单报表（修改推导主程序，与ZZTR117一致）End
  g_firstdate = l_datec.

  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = g_firstdate
    IMPORTING
      e_date = g_lastdate.

  DATA: BEGIN OF ta_bukrs OCCURS 0.
          INCLUDE STRUCTURE t093c.
  DATA: END OF ta_bukrs.

  REFRESH ta_bukrs.
  CLEAR ta_bukrs.

  SELECT bukrs datum
    INTO CORRESPONDING FIELDS OF TABLE ta_bukrs
    FROM t093c
    WHERE bukrs IN s_bukrs
  AND datum < g_firstdate.
  IF sy-subrc <> 0.
*    MESSAGE e016.
*    EXIT.
  ELSE.
    IF ta_bukrs[] IS INITIAL.
*      MESSAGE e016.
*      EXIT.
    ELSE.
      REFRESH s_bukrs.
      CLEAR s_bukrs.
      LOOP AT ta_bukrs.
        s_bukrs-sign = 'I'.
        s_bukrs-option = 'EQ'.
        s_bukrs-low = ta_bukrs-bukrs.
        APPEND s_bukrs.
      ENDLOOP.
      REFRESH ta_bukrs.
      CLEAR ta_bukrs.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_data

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AIBN1
*&---------------------------------------------------------------------*
*       text  查找原始资产 ADD by jw_zhangzq 20130227
*----------------------------------------------------------------------*
FORM frm_get_aibn1 USING aibn1 bukrs.
  DATA: i_aibn1 TYPE aibn1, i_bukrs TYPE bukrs.
  CLEAR: i_aibn1 ,i_bukrs.

  SELECT SINGLE aibn1
    INTO i_aibn1
    FROM anla
    WHERE anln1 = aibn1
     AND bukrs = bukrs.
  IF i_aibn1 IS INITIAL.
    EXIT.
  ENDIF.
  IF g_aibn1 IS NOT INITIAL.
    CONCATENATE i_aibn1 '/' g_aibn1 INTO g_aibn1.
  ELSE.
    g_aibn1 = i_aibn1.
  ENDIF.
  IF i_aibn1 = aibn1.
    EXIT.
  ENDIF.
  PERFORM frm_get_aibn1 USING i_aibn1 bukrs.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM auth_check .
  DATA: BEGIN OF tab_bukrs OCCURS 0,
          bukrs LIKE t001-bukrs,
        END OF tab_bukrs.
  DATA: l_message TYPE string.
*检查公司代码
  IF NOT s_bukrs IS INITIAL .
    SELECT bukrs
      INTO TABLE tab_bukrs
        FROM t001            "  TVKOT
          WHERE
    bukrs IN s_bukrs.
    IF tab_bukrs[] IS NOT INITIAL.
      LOOP AT tab_bukrs.
        AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                 ID 'BUKRS' FIELD tab_bukrs-bukrs
                 ID 'ACTVT' FIELD '03'.
        IF sy-subrc <> 0.
          "  MESSAGE e019(zfi_message) WITH tab_bukrs-bukrs.
          MESSAGE '无权对此公司操作' TYPE 'E'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " AUTH_CHECK


*&---------------------------------------------------------------------*
*&      Form  FRM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_alv." 输出ALV格式

  DATA:
    ls_layout   TYPE  slis_layout_alv,
    it_fieldcat TYPE  slis_t_fieldcat_alv,
    wa_fieldcat TYPE  slis_fieldcat_alv.

  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra             = 'X'.
  ls_layout-detail_popup = 'X'.

  DEFINE  make_output_title.
    CLEAR wa_fieldcat.
    wa_fieldcat-fieldname   = &1.
    wa_fieldcat-seltext_s   = &2.
    wa_fieldcat-seltext_m   = &2.
    wa_fieldcat-seltext_l   = &2.
    wa_fieldcat-reptext_ddic = &2.
    "WA_FIELDCAT-TECH = &3.
    wa_fieldcat-emphasize  = &3.  "列颜色
    IF &1 = 'PERNR' OR &1 = 'ENAME'.
      wa_fieldcat-fix_column = 'X'.
    ENDIF.
    wa_fieldcat-fix_column = &4.

    APPEND wa_fieldcat TO it_fieldcat.
  END-OF-DEFINITION.

*  CASE SY-TCODE .
*    WHEN 'ZFI002'.
  make_output_title 'BUKRS' '公司代码' 'C410' 'X'.
  make_output_title 'ANLKL' '资产分类' 'C410' 'X'.
  make_output_title 'TXK20' '资产分类描述' 'C410' 'X'.
  make_output_title 'ANLN1' '资产编号' 'C410' 'X'.
  make_output_title 'TXT50' '资产名称' 'C410' 'X'.
  make_output_title 'SERNR' '序列号' 'C410' 'X'.
  make_output_title 'ANLN2' '资产子编号' ' ' ' '.
  make_output_title 'TXA50' '实物编号' ' ' ''.
  make_output_title 'MENGE' '数量' ' ' ''.
  make_output_title 'MEINS' '计量单位' ' ' ''.
  make_output_title 'KOSTL' '成本中心' ' ' ''.
  make_output_title 'LTEXT' '成本中心描述' ' ' ''.
  make_output_title 'FKBTX' '费用属性' ' ' ''.
  "  make_output_title 'KOSTLV' '责任成本中心' ' ' ''.
  make_output_title 'CAUFN' '内部订单' ' ' ''.
*  MAKE_OUTPUT_TITLE 'KTEXT' '内部订单描述' ' ' ''.
  make_output_title 'EAUFN' '投资订单' ' ' ''.
  make_output_title 'KTEXT1' '投资订单描述' ' ' ''.
  "      make_output_title 'RAUMN' '房间' ' ' ''.

  make_output_title 'ORD41' '资产小类' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD41T' '增加方式' ' '.
  make_output_title 'ORD42' '使用状态' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD42T' '资金来源' ' '.
*  make_output_title 'ORD43' '使用状态' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD43T' '使用状态' ' '.
*      make_output_title 'ORD44' '资产细分' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD44T' '资产细分' ' '.
*      make_output_title 'KFZKZ' '车辆牌照号' ' ' ''.

  make_output_title 'LIFNR' '供应商' ' ' ''.
  make_output_title 'NAME1' '供应商描述' ' ' ''.
  "      make_output_title 'HERST' '制造商' ' ' ''.

  "MAKE_OUTPUT_TITLE 'AIBN1' '原始资产编码' ' '.
  make_output_title 'ZUGDT' '资本化日期' ' ' ''.
  make_output_title 'DEAKT' '不活动日期' ' ' ''.
  make_output_title 'AFASL' '折旧码' ' ' ''.
  make_output_title 'AFABG' '开始计提折旧日期' ' ' ''.
  "MAKE_OUTPUT_TITLE 'NDJAR' '计划使用年限' ' '.
  "MAKE_OUTPUT_TITLE 'NDPER' '计划使用期间' ' '.
  make_output_title 'ZZNDJA' '计划使用期间' ' ' ''.
  make_output_title 'ZCYZ' '原值' ' ' ''.

  make_output_title 'JZZB' '减值准备' ' ' ''.
  make_output_title 'LJZJ_ZC' '累计折旧' ' ' ''.
  "MAKE_OUTPUT_TITLE 'LJZJ_JHW' '累计折旧（未计划）' ' ' ''.
  make_output_title 'ZCJZ' '净值' ' ' ''.
  make_output_title 'ZCJE' '净额' ' ' ''.
  make_output_title 'AHPROZ' '残值率%' ' ' ''.
  make_output_title 'SCHRW' '预计残值' ' ' ''.
  make_output_title 'ZLJZJ_ZC' '尚需计提折旧' ' ' ''.
  make_output_title 'ZZNDJB' '已使用期间' ' ' ''.
  make_output_title 'ZSYSYQJ' '剩余使用期间' ' ' ''.
  make_output_title 'YEAR_1' '当前年度已折旧额' ' ' ''.
  make_output_title 'MONTH_2' '上月折旧额' ' ' ''.
  make_output_title 'MONTH_1' '本月折旧额' ' ' ''.
  make_output_title 'ZMONTHJE' '下月折旧额' ' ' ''.
  make_output_title 'ZCYBH' '资产原编号' ' ' ''.
  make_output_title 'SEGMENT' '段' ' ' ''.
  make_output_title 'SEGMT' '段描述' ' ' ''.
  make_output_title 'AIBN1' '原始资产' ' ' ''.
  make_output_title 'AIBN2' '原始资产2' ' ' ''.
  make_output_title 'URJHR' '原始购置年度' ' ' ''.
  make_output_title 'URWRT' '原始值' ' ' ''.
  make_output_title 'EKNAM' '采购员' ' ' ''.
  make_output_title 'ANTEI' '厂内产品百分比' ' ' ''.


*    WHEN 'ZFI'.
*      MAKE_OUTPUT_TITLE 'BUKRS' '公司代码' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'ANLKL' '资产分类' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'TXK20' '资产分类描述' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'ANLN1' '资产编号' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'TXT50' '资产名称' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'SERNR' '序列号' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'ANLN2' '资产子编号' ' ' ' '.
*      MAKE_OUTPUT_TITLE 'TXA50' '型号规格' ' ' ''.
*      MAKE_OUTPUT_TITLE 'MENGE' '数量' ' ' ''.
*      MAKE_OUTPUT_TITLE 'MEINS' '计量单位' ' ' ''.
*      MAKE_OUTPUT_TITLE 'KOSTL' '成本中心' ' ' ''.
*      MAKE_OUTPUT_TITLE 'LTEXT' '成本中心描述' ' ' ''.
*      "  make_output_title 'KOSTLV' '责任成本中心' ' ' ''.
*      "     make_output_title 'CAUFN' '内部订单' ' ' ''.
*      "      make_output_title 'KTEXT' '内部订单描述' ' ' ''.
*      MAKE_OUTPUT_TITLE 'EAUFN' '投资订单' ' ' ''.
*      MAKE_OUTPUT_TITLE 'KTEXT1' '投资订单描述' ' ' ''.
*      "   make_output_title 'RAUMN' '房间' ' ' ''.
*
*      MAKE_OUTPUT_TITLE 'ORD41' '资产小类' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD41T' '增加方式' ' '.
*      MAKE_OUTPUT_TITLE 'ORD42' '投资原因' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD42T' '资金来源' ' '.
*      MAKE_OUTPUT_TITLE 'ORD43' '使用状态' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD43T' '使用状态' ' '.
*      "   make_output_title 'ORD44' '资产细分' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD44T' '资产细分' ' '.
**      make_output_title 'KFZKZ' '车辆牌照号' ' ' ''.
*
**      make_output_title 'LIFNR' '供应商' ' ' ''.
**      make_output_title 'NAME1' '供应商描述' ' ' ''.
**      make_output_title 'HERST' '制造商' ' ' ''.
*
*      "MAKE_OUTPUT_TITLE 'AIBN1' '原始资产编码' ' '.
*      "  make_output_title 'ZUGDT' '资本化日期' ' ' ''.
*      "      make_output_title 'DEAKT' '不活动日期' ' ' ''.
*      "      make_output_title 'AFASL' '折旧码' ' ' ''.
*      MAKE_OUTPUT_TITLE 'AFABG' '开始计提折旧日期' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'NDJAR' '计划使用年限' ' '.
*      "MAKE_OUTPUT_TITLE 'NDPER' '计划使用期间' ' '.
**      make_output_title 'ZZNDJA' '计划使用期间' ' ' ''.
*
**
**      make_output_title 'JZZB' '减值准备' ' ' ''.
**      make_output_title 'LJZJ_ZC' '累计折旧' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'LJZJ_JHW' '累计折旧（未计划）' ' ' ''.
*
*      "      make_output_title 'ZCJE' '净额' ' ' ''.
*
*
*      MAKE_OUTPUT_TITLE 'ZZNDJB' '已使用期间' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZSYSYQJ' '剩余使用期间' ' ' ''.
**      make_output_title 'YEAR_1' '当前年度已折旧额' ' ' ''.
**      make_output_title 'MONTH_2' '上月折旧额' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZCYZ' '原值' ' ' ''.
*      MAKE_OUTPUT_TITLE 'MONTH_1' '本月折旧额' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZCJZ' '净值' ' ' ''.
*      MAKE_OUTPUT_TITLE 'SCHRW' '预计残值' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZLJZJ_ZC' '尚需计提折旧' ' ' ''.
*      MAKE_OUTPUT_TITLE 'LJZJ_ZC' '累计折旧' ' ' ''.
**      make_output_title 'ZMONTHJE' '下月折旧额' ' ' ''.
**      make_output_title 'ZCYBH' '资产原编号' ' ' ''.
*  ENDCASE.

*    make_output_title 'BUKRS' '公司代码' 'C410' 'X'.
*    make_output_title 'ANLKL' '资产分类' 'C410' 'X'.
*    make_output_title 'TXK20' '资产分类描述' 'C410' 'X'.
*    make_output_title 'ANLN1' '资产编号' 'C410' 'X'.
*    make_output_title 'TXT50' '资产名称' 'C410' 'X'.
*    make_output_title 'SERNR' '序列号(原资产编号)' 'C410' 'X'.
*    make_output_title 'ANLN2' '资产子编号' ' ' ' '.
*    make_output_title 'TXA50' '型号规格' ' ' ''.
*    make_output_title 'MENGE' '数量' ' ' ''.
*    make_output_title 'MEINS' '计量单位' ' ' ''.
*    make_output_title 'KOSTL' '成本中心' ' ' ''.
*    make_output_title 'LTEXT' '成本中心描述' ' ' ''.
*    make_output_title 'KOSTLV' '责任成本中心' ' ' ''.
*    make_output_title 'CAUFN' '内部订单' ' ' ''.
*    make_output_title 'KTEXT' '内部订单描述' ' ' ''.
  make_output_title 'RAUMN' '房间' ' ' ''.
*
*    make_output_title 'ORD41' '增加方式' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD41T' '增加方式' ' '.
*    make_output_title 'ORD42' '资金来源' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD42T' '资金来源' ' '.
*    make_output_title 'ORD43' '使用状态' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD43T' '使用状态' ' '.
*    make_output_title 'ORD44' '资产细分' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD44T' '资产细分' ' '.
*    make_output_title 'KFZKZ' '车辆牌照号' ' ' ''.
*
*    make_output_title 'LIFNR' '供应商' ' ' ''.
*    make_output_title 'NAME1' '供应商描述' ' ' ''.
  make_output_title 'HERST' '制造商' ' ' ''.
*
*    "MAKE_OUTPUT_TITLE 'AIBN1' '原始资产编码' ' '.
*    make_output_title 'AKTIV' '资本化日期' ' ' ''.
*    make_output_title 'DEAKT' '不活动日期' ' ' ''.
*    make_output_title 'AFASL' '折旧码' ' ' ''.
*    make_output_title 'AFABG' '开始计提折旧日期' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'NDJAR' '计划使用年限' ' '.
*    "MAKE_OUTPUT_TITLE 'NDPER' '计划使用期间' ' '.
*    make_output_title 'ZZNDJA' '计划使用期间' ' ' ''.
*    make_output_title 'ZCYZ' '原值' ' ' ''.
*
*    make_output_title 'JZZB' '减值准备' ' ' ''.
*    make_output_title 'LJZJ_ZC' '累计折旧' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'LJZJ_JHW' '累计折旧（未计划）' ' ' ''.
*    make_output_title 'ZCJZ' '净值' ' ' ''.
*    make_output_title 'ZCJE' '净额' ' ' ''.
*    make_output_title 'SCHRW' '预计残值' ' ' ''.
*
*    make_output_title 'ZZNDJB' '已使用期间' ' ' ''.
*    make_output_title 'ZSYSYQJ' '剩余使用期间' ' ' ''.
*    make_output_title 'YEAR_1' '当前年度已折旧额' ' ' ''.
*    make_output_title 'MONTH_2' '上月折旧额' ' ' ''.
*    make_output_title 'MONTH_1' '本月折旧额' ' ' ''.
*    make_output_title 'ZMONTHJE' '下月折旧额' ' ' ''.
*    make_output_title 'ZCYBH' '资产原编号' ' ' ''.
  make_output_title 'INVNR' '旧资产号' ' ' ''.
  make_output_title 'ANLHTXT' '设备号' ' ' ''.

  make_output_title ' INVZU' '库存注记' ' ' ''.

  LOOP AT it_fieldcat INTO wa_fieldcat.
    CASE wa_fieldcat-fieldname.
      WHEN 'ANLN1' OR 'ANLN2' OR 'KOSTL'
        OR 'CAUFN' OR 'EAUFN' OR 'LIFNR'
        OR 'SEGMENT' OR 'AIBN1' OR 'AIBN2'
        OR 'INVNR'.
        wa_fieldcat-no_zero = 'X'.
    ENDCASE.
    MODIFY it_fieldcat FROM wa_fieldcat.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK           = ' '
*     I_BYPASSING_BUFFER          = ' '
*     I_BUFFER_ACTIVE             = ' '
      i_callback_program          = sy-repid
*     I_CALLBACK_PF_STATUS_SET    = 'PF_STATUS_SET'
      i_callback_user_command     = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE      = ' '
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
*     I_CALLBACK_HTML_END_OF_LIST = ' '
*     I_STRUCTURE_NAME            = 'ZSJTHRR0037'
*     I_BACKGROUND_ID             = ' '
*     I_GRID_TITLE                =
*     I_GRID_SETTINGS             =
      is_layout                   = ls_layout
      it_fieldcat                 = it_fieldcat
      i_save                      = 'X'
      i_html_height_top           = '0'
    TABLES
      t_outtab                    = it_list
* EXCEPTIONS
*     PROGRAM_ERROR               = 1
*     OTHERS                      = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    "FRM_ALV

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
            rs_selfield TYPE slis_selfield.
  CLEAR it_list.
  CASE r_ucomm.
    WHEN '&IC1'.
      CASE rs_selfield-fieldname.
        WHEN 'ANLN1' OR 'ANLN2'.
          READ TABLE it_list INTO it_list INDEX rs_selfield-tabindex.
          IF sy-subrc = 0.
*            IF rs_selfield-fieldname = 'BELNR'.
*              SET PARAMETER ID 'BLN' FIELD WA_DATA1-BELNR.
*            ELSEIF rs_selfield-fieldname = 'AUGBL'.
*              SET PARAMETER ID 'BLN' FIELD WA_DATA1-AUGBL.
*            ENDIF.

            SET PARAMETER ID 'AN1' FIELD it_list-anln1.
            SET PARAMETER ID 'AN2' FIELD it_list-anln2.
            SET PARAMETER ID 'BUK' FIELD it_list-bukrs.
            CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
          ENDIF.
          "WHEN 'KOSTL'.
          "          READ TABLE IT_LIST INTO IT_LIST INDEX RS_SELFIELD-TABINDEX.
          "          IF SY-SUBRC = 0.
          "            SET PARAMETER ID 'KOS' FIELD IT_LIST-KOSTL.
          "            CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
          "          ENDIF.
          "        WHEN 'KOSTLV'.
          "          READ TABLE IT_LIST INTO IT_LIST INDEX RS_SELFIELD-TABINDEX.
          "          IF SY-SUBRC = 0.
          "            SET PARAMETER ID 'KOS' FIELD IT_LIST-KOSTLV.
          "            CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
          "          ENDIF.
        WHEN 'CAUFN'."内部订单
          READ TABLE it_list INTO it_list INDEX rs_selfield-tabindex.

          IF sy-subrc = 0.
            SET PARAMETER ID 'ANR' FIELD it_list-caufn.
            CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'EAUFN'."投资订单
          READ TABLE it_list INTO it_list INDEX rs_selfield-tabindex.

          IF sy-subrc = 0.
            SET PARAMETER ID 'ANR' FIELD it_list-eaufn.
            CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.                    "USER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DOCUMENT   text
*----------------------------------------------------------------------*
FORM html_top_of_page USING document TYPE REF TO cl_dd_document.

  DATA: text TYPE sdydo_text_element.
  DATA: m_p      TYPE i,
        m_buffer TYPE string.
*  CASE SY-TCODE .
*    WHEN 'ZFIR017'.
  text =  '固定资产明细表'.
*    WHEN 'ZFIR018'.
*      TEXT =  '固定资产折旧表'.
*  ENDCASE.
  CALL METHOD document->add_text
    EXPORTING
      text          = text
      sap_style     = 'HEADING'               " 显示文字的STYLE设置
      sap_color     = cl_dd_document=>list_total_int
      sap_fontsize  = cl_dd_document=>large
      sap_fontstyle = cl_dd_document=>serif
      sap_emphasis  = cl_dd_document=>emphasis.

  CALL METHOD document->new_line. "换行
  CALL METHOD document->new_line.

  CALL METHOD document->add_icon     " 插入图片
    EXPORTING
      sap_icon = 'ICON_DATE'.

  text = '报表日期 : '.
  CALL METHOD document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Strong'.

  CALL METHOD document->add_gap
    EXPORTING
      width = 2.

  CONCATENATE p_gjahr '年' zrq '月' INTO text.
  CALL METHOD document->add_text
    EXPORTING
      text      = text
      sap_style = 'Key'.

  CALL METHOD document->add_gap
    EXPORTING
      width = 10.

  text = '出表人 : '.
  CALL METHOD document->add_text      "插入文本
    EXPORTING
      text         = text
      sap_emphasis = 'Strong'.

  CALL METHOD document->add_gap      "插入位置
    EXPORTING
      width = 2.

  text = sy-uname.
  CALL METHOD document->add_text
    EXPORTING
      text      = te