*&---------------------------------------------------------------------*
*& Report ZPPR255
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr255 MESSAGE-ID zgp_msg.

TABLES:likp.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:hour TYPE p DECIMALS 1.
TYPES:BEGIN OF ty_itab,
        vbeln         TYPE vbak-vbeln,             "发货单号
        zysfzr        TYPE vbak-zysfzr,            "负责人
        zlyrqsj       TYPE char19,                 "履约提交
        zbjrq         TYPE vbak-zbjrq,             "报价日期
        zbjsj         TYPE vbak-zbjsj,             "报价时间
        zbjrqsj       TYPE char19,                 "报价时间
        zbj_ly        TYPE hour,                   "报价时效 = 报价时间-履约提交时间
        zzbrqsj       TYPE char19,                 "中标时间
        zzb_bj        TYPE hour,                   "订车时效 = 中标时间-报价时间
        zzhrq         TYPE vbak-zzhrq,             "预计装车日期
        zzhsj         TYPE vbak-zzhsj,             "预计装车时间
        zzhrqsj       TYPE char19,                 "预计装车时间
        zrcrq         TYPE vbak-zrcrq,             "进厂日期
        zrcsj         TYPE vbak-zrcsj,             "进厂时间
        zrcrqsj       TYPE char19,                 "车辆进厂
        zzhrq_start   TYPE vbak-zzhrq,             "装车开始日期
        zzhsj_start   TYPE vbak-zzhsj,             "装车开始时间
        zzhrqsj_start TYPE char19,                 "装车开始
        zzhrq_end     TYPE vbak-zzhrq,             "装车结束日期
        zzhsj_end     TYPE vbak-zzhsj,             "装车结束时间
        zzhrqsj_end   TYPE char19,                 "装车结束
        zwait         TYPE hour,                   "车辆等待时间 = 装车开始-车辆进厂
        zzhsx         TYPE hour,                   "装车时效 = 装车结束-装车开始
        zxhrq         TYPE vbak-zxhrq,             "预计卸货日期
        zxhsj         TYPE vbak-zxhsj,             "预计卸货时间
        zxhrqsj       TYPE char19,                 "预计卸货
        zccrq         TYPE vbak-zccrq,             "出厂日期
        zccsj         TYPE vbak-zccsj,             "出厂时间
        zccrqsj       TYPE char19,                 "出厂时间
        zbgrq         TYPE datum,                  "保管出库日期
        zbgsj         TYPE tims,                   "保管出库时间
        zbgrqsj       TYPE char19,                 "保管出库
        zcc_bg        TYPE hour,                   "厂区停留时间 = 车辆出厂时间-保管出库时间
        zhdrq         TYPE datum,                  "回单上传日期
        zhdsj         TYPE tims,                   "回单上传时间
        zhdrqsj       TYPE char19,                 "回单上传
        zhd_cc        TYPE hour,                   "实际运输时效 = 回单上传-车辆出厂时间
        zyjxh_zh      TYPE hour,                   "预计运输时效 = 预计卸货时间-预计装车时间
        sel,
      END OF ty_itab.
DATA: itab    TYPE TABLE OF ty_itab WITH EMPTY KEY,
      wa_itab LIKE LINE OF itab.
* 动态内表
DATA: lo_struct   TYPE REF TO cl_abap_structdescr,
      lo_element  TYPE REF TO cl_abap_elemdescr,
      lo_new_type TYPE REF TO cl_abap_structdescr,
      lo_new_tab  TYPE REF TO cl_abap_tabledescr,
      lo_data     TYPE REF TO data,
      dyn_wa      TYPE REF TO data,
      lt_comp     TYPE cl_abap_structdescr=>component_table,
      lt_tot_comp TYPE cl_abap_structdescr=>component_table,
      la_comp     LIKE LINE OF lt_comp.
* 访问动态表的字段符号
FIELD-SYMBOLS: <f_tab>  TYPE table,
               <f_line> TYPE any.
TYPES:BEGIN OF ty_vbeln,
        vbeln TYPE vbak-vbeln,
      END OF ty_vbeln.
DATA:gt_vbeln TYPE TABLE OF ty_vbeln WITH NON-UNIQUE KEY vbeln,
     gv_vbeln LIKE LINE OF gt_vbeln.
DATA: BEGIN OF gt_tp OCCURS 0,
        type  TYPE string,
        text  TYPE string,
        matkl TYPE string,
      END OF gt_tp.
DATA: BEGIN OF gt_fname OCCURS 0,
        fname TYPE string,
        ftext TYPE string,
      END OF gt_fname.
DATA:r_matkl   TYPE RANGE OF matkl WITH HEADER LINE.
DATA:BEGIN OF gt_malips OCCURS 0,
       vbeln  TYPE lips-vbeln,
       zerdat TYPE lips-zerdat,
       zertim TYPE lips-zertim,
     END OF gt_malips.
DATA:fieldname TYPE string,
     fieldtext TYPE string.
DATA:zerdat_s   TYPE ze_erdat, "开始日期
     zertim_s   TYPE ze_ertim, "开始时间
     zerdat_e   TYPE ze_erdat, "日期+时间
     zertim_e   TYPE ze_ertim, "结束日期
     zdat_tim_s TYPE char19,   "结束日期
     zdat_tim_e TYPE char19,   "日期+时间
     cha        TYPE hour.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:
*                 s_werks FOR t001l-werks OBLIGATORY,
*                 s_posid FOR prps-posid,
*                 s_post1 FOR prps-post1,"项目名称
                 s_erdat FOR likp-erdat,     "发货单制单日期
                 s_ckdat FOR likp-wadat_ist. "出库单出库日期
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '功能选择'.
  %_s_erdat_%_app_%-text = '制单日期'.
  %_s_ckdat_%_app_%-text = '出库日期'.

  gt_fname[] = VALUE #( ( fname = 'ZHRQSJ_START' ftext = '装车开始'  )
                        ( fname = 'ZHRQSJ_END'   ftext = '装车结束'   )
                        ( fname = 'ZHSX'         ftext = '装车时效' )
                      ).
  gt_tp[] = VALUE #( ( type = 'LC'  text = '丽彩'   matkl = 'A0100,A0200,A0300,A0400'  )
                     ( type = 'ZWJ' text = '折弯件' matkl = 'B0201,B0202,B0203,B0204,B0205,B0206,B0207,B0208' )
                     ( type = 'MQ'  text = '幕墙'  matkl = 'A0500'  )
                     ( type = 'LT'  text = '檩条'  matkl = 'B0106'  )
                     ( type = 'YW'  text = '压瓦'  matkl = 'B0101'  )
                     ( type = 'DM'  text = '得默'  matkl = 'A1601,A1602,B0206'  )
                   ).


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
  PERFORM get_dynamic_table. "获取动态内表
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
  SELECT
      a~vbeln,
      a~zysfzr,
      a~zzhrq,
      a~zzhsj,
      a~zrcrq,
      a~zrcsj,
      a~zxhrq,
      a~zxhsj,
      a~zbjrq,
      a~zbjsj,
      a~zccrq,
      a~zccsj
  FROM vbak AS a
  INNER JOIN lips AS b ON b~vgbel = a~vbeln
  INNER JOIN likp AS c ON c~vbeln = b~vbeln
  WHERE left( a~vbeln,2 ) EQ 'FH'
   AND  a~zjhfs = 'B'
   AND  a~erdat     IN @s_erdat
   AND  c~wadat_ist IN @s_ckdat
  ORDER BY a~vbeln,c~wadat_ist,c~spe_wauhr_ist
  INTO CORRESPONDING FIELDS OF TABLE @itab.
  DELETE ADJACENT DUPLICATES FROM itab COMPARING vbeln.
  IF itab IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH: gt_vbeln.
  LOOP AT itab INTO wa_itab.
    gv_vbeln-vbeln = wa_itab-vbeln.
    COLLECT gv_vbeln INTO gt_vbeln.
  ENDLOOP.

  "履约提交时间（最后一次）
  SELECT
    b~vbeln,
    a~zdate,
    a~ztime
  FROM ztsdzdlog AS a
  INNER JOIN @gt_vbeln AS b ON a~sapno = b~vbeln
  WHERE a~fieldname = 'ZTTZT'
  AND  a~value_o = 'A'
  AND  a~value_n = 'B'
  ORDER BY vbeln,zdate DESCENDING,ztime DESCENDING
  INTO TABLE @DATA(it_kfdate).
  DELETE ADJACENT DUPLICATES FROM it_kfdate COMPARING vbeln.
  "中标时间（最后一次）
  SELECT
    b~vbeln,
    a~zdate,
    a~ztime
  FROM ztsdzdlog AS a
  INNER JOIN @gt_vbeln AS b ON a~sapno = b~vbeln
  WHERE a~fieldname = 'ZTTZT'
  AND  a~value_o = 'B'
  AND  a~value_n = 'C'
  ORDER BY vbeln,zdate DESCENDING,ztime DESCENDING
  INTO TABLE @DATA(it_zbdate).
  DELETE ADJACENT DUPLICATES FROM it_zbdate COMPARING vbeln.
*  第一包扫码日期
  SELECT
    a~vgbel AS vbeln,
    a~zerdat,
    a~zertim
  FROM lips AS a
  INNER JOIN @gt_vbeln AS b ON a~vgbel = b~vbeln
  WHERE a~zckfs IN ('1','2')
  ORDER BY vbeln, zerdat, zertim
  INTO TABLE @DATA(sm_first).
  DELETE ADJACENT DUPLICATES FROM sm_first COMPARING vbeln.
*  最后一包扫码日期
  SELECT
    a~vgbel AS vbeln,
    a~zerdat,
    a~zertim
  FROM lips AS a
  INNER JOIN @gt_vbeln AS b ON a~vgbel = b~vbeln
  WHERE a~zckfs IN ('1','2')
  ORDER BY vbeln, zerdat DESCENDING, zertim DESCENDING
  INTO TABLE @DATA(sm_last).
  DELETE ADJACENT DUPLICATES FROM sm_last COMPARING vbeln.
*回单上传时间
  SELECT
    b~vgbel AS vbeln,
    CASE WHEN a~zhcjrq IS INITIAL THEN a~zyjrq ELSE a~zhcjrq END AS zhdrq
  FROM ztsdurl AS a
  INNER JOIN lips AS b ON a~vbeln = b~vbeln
  INNER JOIN @gt_vbeln AS c ON b~vgbel = c~vbeln
  WHERE a~zmk = 'POD'
  ORDER BY vbeln,zhdrq
  INTO TABLE @DATA(gt_sdurl).
  DELETE ADJACENT DUPLICATES FROM gt_sdurl COMPARING vbeln.
*保管出库
  SELECT
    c~vgbel AS vbeln,
    a~wadat_ist AS dat,
    a~spe_wauhr_ist AS tim
  FROM likp AS a
  INNER JOIN lips AS c ON a~vbeln = c~vbeln
  INNER JOIN @gt_vbeln AS b ON c~vgbel = b~vbeln
  ORDER BY vbeln, dat, tim
  INTO TABLE @DATA(gt_bgck).
  DELETE ADJACENT DUPLICATES FROM gt_bgck COMPARING vbeln.
*各物料组装车时间
  SELECT
    a~vgbel AS vbeln,
    a~zerdat,
    a~zertim,
    a~matkl
  FROM lips AS a
  INNER JOIN @gt_vbeln AS b ON a~vgbel = b~vbeln
  WHERE a~zckfs IN ('1','2')
  ORDER BY vbeln
  INTO TABLE @DATA(lt_matdat).


*赋值alv内表
  LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
    "履约提交
    READ TABLE it_kfdate INTO DATA(wa_kfdate) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      PERFORM getdatzet USING    wa_kfdate-zdate
                                 wa_kfdate-ztime
                        CHANGING <fs_itab>-zlyrqsj.
    ENDIF.
    "报价时间
    PERFORM getdatzet USING    <fs_itab>-zbjrq
                               <fs_itab>-zbjsj
                      CHANGING <fs_itab>-zbjrqsj.
    "报价时效
    PERFORM getcha    USING    wa_kfdate-zdate
                               <fs_itab>-zbjrq
                               wa_kfdate-ztime
                               <fs_itab>-zbjsj
                      CHANGING <fs_itab>-zbj_ly.
    "中标时间
    READ TABLE it_zbdate INTO DATA(wa_zbdate) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      PERFORM getdatzet  USING     wa_zbdate-zdate
                                   wa_zbdate-ztime
                         CHANGING  <fs_itab>-zzbrqsj.
      "订车时效
      PERFORM getcha     USING     <fs_itab>-zbjrq
                                   wa_zbdate-zdate
                                   <fs_itab>-zbjsj
                                   wa_zbdate-ztime
                         CHANGING  <fs_itab>-zzb_bj.
    ENDIF.
    "预计装车时间
    PERFORM getdatzet   USING   <fs_itab>-zzhrq
                                <fs_itab>-zzhsj
                       CHANGING <fs_itab>-zzhrqsj.
    "车辆入厂
    PERFORM getdatzet   USING   <fs_itab>-zrcrq
                                <fs_itab>-zrcsj
                       CHANGING <fs_itab>-zrcrqsj.
    "装车开始
    READ TABLE sm_first INTO DATA(wa_first) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      <fs_itab>-zzhrq_start = wa_first-zerdat.
      <fs_itab>-zzhsj_start = wa_first-zertim.
      PERFORM getdatzet USING    <fs_itab>-zzhrq_start
                                 <fs_itab>-zzhsj_start
                        CHANGING <fs_itab>-zzhrqsj_start.
    ENDIF.
    "装车结束
    READ TABLE sm_last INTO DATA(wa_last) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      <fs_itab>-zzhrq_end = wa_last-zerdat.
      <fs_itab>-zzhsj_end = wa_last-zertim.
      PERFORM getdatzet USING    <fs_itab>-zzhrq_end
                                 <fs_itab>-zzhsj_end
                        CHANGING <fs_itab>-zzhrqsj_end.
    ENDIF.
    "车辆等待时间
    PERFORM getcha USING    <fs_itab>-zrcrq
                            <fs_itab>-zzhrq_start
                            <fs_itab>-zrcsj
                            <fs_itab>-zzhsj_start
                   CHANGING <fs_itab>-zwait.
    "装车时效
    PERFORM getcha USING    <fs_itab>-zzhrq_start
                            <fs_itab>-zzhrq_end
                            <fs_itab>-zzhsj_start
                            <fs_itab>-zzhsj_end
                   CHANGING <fs_itab>-zzhsx.
    "预计卸货
    PERFORM getdatzet USING    <fs_itab>-zxhrq
                               <fs_itab>-zxhsj
                      CHANGING <fs_itab>-zxhrqsj.
    "预计运输时效
    PERFORM getcha    USING    <fs_itab>-zzhrq
                               <fs_itab>-zxhrq
                               <fs_itab>-zzhsj
                               <fs_itab>-zxhsj
                      CHANGING <fs_itab>-zyjxh_zh.
    "回单上传日期
    READ TABLE gt_sdurl INTO DATA(gv_sdurl) WITH KEY vbeln =  <fs_itab>-vbeln.
    IF sy-subrc = 0.
      <fs_itab>-zhdrq = gv_sdurl-zhdrq.
      PERFORM getdatzet USING    <fs_itab>-zhdrq
                                 <fs_itab>-zhdsj
                        CHANGING <fs_itab>-zhdrqsj.
    ENDIF.
    "出厂日期
    PERFORM getdatzet USING    <fs_itab>-zccrq
                               <fs_itab>-zccsj
                      CHANGING <fs_itab>-zccrqsj.
    "实际运输时效
    PERFORM getcha    USING    <fs_itab>-zccrq
                               <fs_itab>-zhdrq
                               <fs_itab>-zccsj
                               <fs_itab>-zhdsj
                      CHANGING <fs_itab>-zhd_cc.
    "保管出库
    READ TABLE gt_bgck INTO DATA(gv_bgck) WITH KEY vbeln =  <fs_itab>-vbeln.
    IF sy-subrc = 0.
      PERFORM getdatzet USING    gv_bgck-dat
                                 gv_bgck-tim
                        CHANGING <fs_itab>-zbgrqsj.
      PERFORM getcha    USING    gv_bgck-dat
                                 <fs_itab>-zccrq
                                 gv_bgck-tim
                                 <fs_itab>-zccsj
                        CHANGING <fs_itab>-zcc_bg.
    ENDIF.
  ENDLOOP.
  MOVE-CORRESPONDING itab TO <f_tab>.
  "各物料组时间赋值
  LOOP AT <f_tab> ASSIGNING <f_line>.
    ASSIGN COMPONENT 'VBELN' OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_vbeln_value>).
    LOOP AT gt_tp.
      REFRESH: r_matkl,gt_malips.
      CLEAR:zerdat_s,zertim_s,zerdat_e,zertim_e,zdat_tim_s,zdat_tim_e,cha.
      PERFORM splitstr(zpubform) TABLES r_matkl USING gt_tp-matkl ','.
      LOOP AT lt_matdat INTO DATA(wa_matdat) WHERE vbeln = <f_vbeln_value> AND matkl IN r_matkl.
        CLEAR gt_malips.
        MOVE-CORRESPONDING wa_matdat TO gt_malips.
        APPEND gt_malips.
      ENDLOOP.
      IF gt_malips[] IS NOT INITIAL.
        SORT gt_malips BY zerdat zertim.
        zerdat_s = gt_malips[ 1 ]-zerdat.
        zertim_s = gt_malips[ 1 ]-zertim.
        SORT gt_malips BY zerdat DESCENDING zertim DESCENDING.
        zerdat_e = gt_malips[ 1 ]-zerdat.
        zertim_e = gt_malips[ 1 ]-zertim.
        LOOP AT gt_fname.
          DATA(compname) = |{ gt_tp-type }{ gt_fname-fname }|.
          ASSIGN COMPONENT compname OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_dat_value>).
          CLEAR <f_dat_value>.
          CASE sy-tabix.
            WHEN 1.
              PERFORM getdatzet USING    zerdat_s
                                         zertim_s
                                CHANGING <f_dat_value>.
            WHEN 2.
              PERFORM getdatzet USING    zerdat_e
                                         zertim_e
                                CHANGING <f_dat_value>.

            WHEN 3.
              PERFORM getcha    USING    zerdat_s
                                         zerdat_e
                                         zertim_s
                                         zertim_e
                                CHANGING <f_dat_value>.
            WHEN OTHERS.
              MESSAGE s004 WITH 'gt_fname出现第4行了，请修改逻辑' DISPLAY LIKE 'E'.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  REFRESH itab.
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
'VBELN         '  '发货通知单号                '  'VBAK'  'VBELN',
'ZYSFZR        '  '负责人                    '  'VBAK'  'ZYSFZR',
'ZLYRQSJ       '  '履约提交                   '  ''  '',
'ZBJRQSJ       '  '报价时间 '                   ''  '',
'ZBJ_LY        '  '报价时效              '  ''  '',
'ZZBRQSJ       '  '中标时间                      '  ''  '',
'ZZB_BJ        '  '订车时效                      '  ''  '',
'ZZHRQSJ       '  '预计装车时间    '                    ''  '',
'ZRCRQSJ       '  '车辆进厂时间       '                   '' '',
'ZZHRQSJ_START '  '装车开始 '                      ''  '',
'ZZHRQSJ_END   '  '装车结束                         '  ''  '',
'ZWAIT         '  '车辆等待时间               '       ''  '',
'ZZHSX         '  '装车时效'                  ''  '',
'ZBGRQSJ       '  '保管出库                       '  ''  '',
'ZCCRQSJ       '  '车辆出厂                        '  ''  '',
'ZCC_BG       '  '厂区停留时间                    '  ''  '',
'ZHDRQSJ       '  '回单上传                       '  ''  '',
'ZXHRQSJ       '  '预计卸货                    '  ''  '',
'ZHD_CC       '  '实际运输时效                  '  ''  '',
'ZYJXH_ZH     '  '预计运输时效                  '  ''  ''.
  LOOP AT gt_tp.
    LOOP AT gt_fname.
      fieldname = gt_tp-type && gt_fname-fname.
      fieldtext = gt_tp-text && gt_fname-ftext.
      PERFORM catlg_set TABLES fldct USING
             fieldname  fieldtext  ''  ''.
    ENDLOOP.
  ENDLOOP.

  i_title               = lines( <f_tab> ) .
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
      t_outtab                 = <f_tab>
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

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CHECK rs_selfield-tabindex <> 0 . "小计行总计行什么的忽略
      READ TABLE itab INTO wa INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN ''.

        WHEN OTHERS.
      ENDCASE.
    WHEN 'VIEW'. "查看明细
      DATA(num) = 0.
      CLEAR num.
      MOVE-CORRESPONDING <f_tab> TO itab.
      LOOP AT itab INTO wa_itab WHERE sel = 'X'.
        num += 1.
      ENDLOOP.
      IF num NE 1.
        MESSAGE s022 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM view USING wa_itab-vbeln.
  ENDCASE.
  "不需要刷新
*  rs_selfield-row_stable = 'X'.
*  rs_selfield-col_stable = 'X'.
*  rs_selfield-refresh    = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form getdatzet
*&---------------------------------------------------------------------*
*& 日期时间拼接
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdatzet USING s_dat TYPE sy-datum
                     s_zet TYPE sy-timlo
               CHANGING s_datzet.
  IF s_dat IS NOT INITIAL.
    s_datzet = |{ s_dat+0(4) }-{ s_dat+4(2) }-{ s_dat+6(2) } { s_zet+0(2) }:{ s_zet+2(2) }:{ s_zet+4(2) }|.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getcha
*&---------------------------------------------------------------------*
*& 获取日期时间差，返回小时单位
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getcha USING    p_d1 TYPE d
                     p_d2 TYPE d
                     p_t1 TYPE t
                     p_t2 TYPE t
            CHANGING p_m TYPE hour .
  DATA:p_hour TYPE i.
  CALL FUNCTION 'DELTA_TIME_DAY_HOUR'
    EXPORTING
      t1      = p_t1
      t2      = p_t2
      d1      = p_d1
      d2      = p_d2
    IMPORTING
      minutes = p_hour.
  p_m = p_hour / 60.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_dynamic_table
*&---------------------------------------------------------------------*
*& 获取动态内表
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_dynamic_table.
* 1. 从现有类型获取组件
  lo_struct ?= cl_abap_typedescr=>describe_by_name( 'TY_ITAB' ).
  lt_comp  = lo_struct->get_components( ).
  APPEND LINES OF lt_comp TO lt_tot_comp.
  LOOP AT gt_tp.
*
*   元素描述
    LOOP AT gt_fname.
      IF sy-tabix = 3.
        lo_element ?= cl_abap_elemdescr=>describe_by_name( 'HOUR' ).
        CONCATENATE gt_tp-type gt_fname-fname INTO la_comp-name.
        la_comp-type = cl_abap_elemdescr=>get_p(
          p_length   = lo_element->length
          p_decimals = lo_element->decimals ).
      ELSE.
        lo_element ?= cl_abap_elemdescr=>describe_by_name( 'CHAR19' ).
*   字段名称
        CONCATENATE gt_tp-type gt_fname-fname INTO la_comp-name.
*
*   字段类型
        la_comp-type = cl_abap_elemdescr=>get_c(
          p_length = lo_element->length ).
      ENDIF.
*
*   填写组件表
      APPEND la_comp TO lt_tot_comp.
      CLEAR: la_comp.
    ENDLOOP.

  ENDLOOP.
* 3. 创建新类型
  lo_new_type = cl_abap_structdescr=>create( lt_tot_comp ).
*
* 4. 新建表格类型
  lo_new_tab = cl_abap_tabledescr=>create(
    p_line_type  = lo_new_type
    p_table_kind = cl_abap_tabledescr=>tablekind_std
    p_unique     = abap_false ).
*
* 5. 用于处理新表类型的数据
  CREATE DATA lo_data TYPE HANDLE lo_new_tab.
*
* 6. 字段中的新内部表符号
  ASSIGN lo_data->* TO <f_tab>.
* 7. 创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <f_tab>.
* 8. 创建动态工作区
  ASSIGN dyn_wa->* TO <f_line>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form view
*&---------------------------------------------------------------------*
*& 查看明细
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM view USING p_vbeln TYPE vbak-vbeln.
  DATA:fieldcat TYPE slis_t_fieldcat_alv.
  DATA:gt_dtab LIKE TABLE OF zspp222 WITH HEADER LINE.
  CALL FUNCTION 'ZFM_GP_OCP_PP_GETPHXX'
    EXPORTING
      vbeln   = p_vbeln
    TABLES
      out_tab = gt_dtab[].

  PERFORM catlg_set TABLES fieldcat
                    USING:
    'ZPHZ ' '配货站      '  ''  '',
    'ZLXR ' '联系人     '  ''  '',
    'ZLXDH ' '联系电话  '  ''  '',
    'ZPRICE ' '报价     '