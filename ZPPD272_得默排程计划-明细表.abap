*&---------------------------------------------------------------------*
*& Report ZPPD272
*&---------------------------------------------------------------------*
*& 得默排程计划-明细表
*&---------------------------------------------------------------------*
REPORT zppd272 MESSAGE-ID zmsg_gp.

CONSTANTS:tcode1 TYPE sy-tcode VALUE 'ZPP271',
          tcode2 TYPE sy-tcode VALUE 'ZPP272',
          tcode3 TYPE sy-tcode VALUE 'ZPP273',
          tcode4 TYPE sy-tcode VALUE 'ZPP274',
          tcode5 TYPE sy-tcode VALUE 'ZPP275',
          tcode6 TYPE sy-tcode VALUE 'ZPP276',
          tcode7 TYPE sy-tcode VALUE 'ZPP277'.
TABLES:sscrfields,t001l,ztpp_205,prps.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE TYPE ztpp_270a.
DATA:  id      LIKE icon-id, " 状态 ICON_LED_RED  ICON_LED_GREEN
       werks   TYPE ztpp_270-werks,
       posid   TYPE ztpp_270-posid,
       zpcdh   TYPE ztpp_270-zpcdh,
       aufnr   TYPE ztpp_270-aufnr,
       zxdrq   TYPE ztpp_270-zxdrq,
       matnr   TYPE ztpp_270-matnr,
       zwbcms  TYPE ztpp_270-zwbcms,
       znbcms  TYPE ztpp_270-znbcms,
       zddl_m  TYPE ztpp_270-zddl_m,
       zddl_m2 TYPE ztpp_270-zddl_m2,
       zpchs   TYPE ztpp_270-zpchs,
       zsfyp   TYPE ztpp_270-zsfyp,
       zjhrq   TYPE ztpp_270-zjhrq,
       zqtrq   TYPE ztpp_270-zqtrq,
       zdbh    TYPE ztpp_270-zdbh,
       zjfsx   TYPE ztpp_270-zjfsx,
       zplh    TYPE ztpp_270-zplh,
       zplkd   TYPE ztpp_270-zplkd,
       zpcbzjs TYPE ztpp_270-zpcbzjs,
       zsclj   TYPE ztpp_270-zsclj,
       sel,
     END OF itab.

DATA:it_matnr  TYPE TABLE OF ccvx_matnr WITH HEADER LINE,
     outtab001 TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
TYPES:BEGIN OF ty_pch, "排程号
        zpch TYPE ztpp_270-zpch,
      END OF ty_pch.
DATA:gt_pch TYPE TABLE OF ty_pch WITH KEY zpch,
     gv_pch LIKE LINE OF gt_pch.
TYPES:BEGIN OF ty_lx, "类型
        code  TYPE sy-tcode,
        text  TYPE ztpp_270a-zgxmc,
        tabix TYPE sy-tabix,
        zsdpl TYPE ztpp_270a-zsdpl,
        zxdpl TYPE ztpp_270a-zxdpl,
      END OF ty_lx.
DATA:gt_lx TYPE TABLE OF ty_lx WITH KEY code,
     gv_lx LIKE LINE OF gt_lx,
     lx    TYPE ty_lx. "当前工序
DATA:wherestr TYPE string.
DATA:BEGIN OF gt_edit_field OCCURS 0,
       type  TYPE string,
       field TYPE string,
       text  TYPE string,
       edit  TYPE char1,
     END OF gt_edit_field.
DATA:answer TYPE char1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR t001l-werks DEFAULT '3009',
                 s_zpcdh FOR ztpp_205-zpcdh,
                 s_posid FOR prps-posid,
                 s_matnr FOR ztpp_205-matnr,
                 s_sydat FOR ztpp_205-sydat.
  PARAMETERS p_all TYPE char1 AS CHECKBOX USER-COMMAND singleclick.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '功能选择'.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_zpcdh_%_app_%-text = '排产单号'.
  %_s_posid_%_app_%-text = '项目编码'.
  %_s_matnr_%_app_%-text = '物料编码'.
  %_s_sydat_%_app_%-text = '创建日期'.
  %_p_all_%_app_%-text = '全部'.

  gt_lx   = VALUE #( ( text = '开平'     code = tcode2    tabix = 1   zsdpl = '配料'        zxdpl = '落料'       )
                     ( text = '塔冲'     code = tcode3    tabix = 2   zsdpl = '开平'        zxdpl = '折弯'       )
                     ( text = '折弯'     code = tcode4    tabix = 3   zsdpl = '落料'        zxdpl = '复合'       )
                     ( text = '复合'     code = tcode5    tabix = 4   zsdpl = '折弯'        zxdpl = '开槽/组装'  )
                     ( text = '开槽'     code = tcode6    tabix = 5   zsdpl = '复合'        zxdpl = '组装'       )
                     ( text = '组装'     code = tcode7    tabix = 6   zsdpl = '复合/开槽'   zxdpl = '打包'       )
                    ).
  gt_edit_field[] = VALUE #( type = '开平' (   text = '排产日期'          field = 'ZGXPCRQ'    edit = 'X'   )
                                           (   text = '计划数量(m)'       field = 'ZJH_M'      edit = 'X'   )
                                           (   text = '实际完成情况'      field = 'ZSJ_M'      edit = 'X'   )
                                           (   text = '达标率(%)'         field = 'ZDBL_M'     edit = ''    )
                                           (   text = '人员数量'          field = 'ZRY'        edit = 'X'   )
                                           (   text = '更换料卷数'        field = 'ZGHLJS'     edit = 'X'   )
                                           (   text = '更换料件定额(H)'   field = 'ZGHLDE'     edit = ''    )
                                           (   text = '生产料定额(H)'     field = 'ZSCLDE'     edit = ''    )
                                           (   text = '总定额(H)'         field = 'ZZDE'       edit = 'X'    )
                             type = '塔冲' (   text = '排产日期'          field = 'ZGXPCRQ'    edit = 'X'   )
                                           (   text = '计划数量(片)'      field = 'ZJH_PC'     edit = 'X'    )
                                           (   text = '实际完成情况'      field = 'ZSJ_PC'      edit = 'X'   )
                                           (   text = '达标率(%)'         field = 'ZDBL_PC'     edit = ''    )
                                           (   text = '人员数量'          field = 'ZRY'         edit = 'X'   )
                                           (   text = '设备台数'          field = 'ZSBTS'       edit = 'X'   )
                                           (   text = '总定额(H)'         field = 'ZZDE'        edit = 'X'    )
                             type = '折弯' (   text = '排产日期'           field = 'ZGXPCRQ'     edit = 'X'   )
                                           (   text = '计划数量-爱克(片)'     field = 'ZJH_PC'    edit = 'X'    )
                                           (   text = '实际完成情况-爱克'     field = 'ZSJ_PC'      edit = 'X'   )
                                           (   text = '达标率-爱克(%)'       field = 'ZDBL_PC'      edit = ''    )
                                           (   text = '设备台数-爱克'        field = 'ZSBTS'        edit = 'X'  )
                                           (   text = '班次-爱克'            field = 'ZBC'         edit = 'X'  )
                                           (   text = '总定额-爱克(H)'       field = 'ZZDE'        edit = 'X'    )
                                           (   text = '计划数量-RAS(片)'     field = 'ZJH2_PC'     edit = 'X'    )
                                           (   text = '实际完成情况-RAS'     field = 'ZSJ2_PC'      edit = 'X'   )
                                           (   text = '达标率-RAS(%)'       field = 'ZDBL2_PC'      edit = ''    )
                                           (   text = '设备台数-RAS'        field = 'ZSBTS2'        edit = 'X'  )
                                           (   text = '班次-RAS'            field = 'ZBC2'         edit = 'X'  )
                                           (   text = '总定额-RAS(H)'       field = 'ZZDE2'        edit = 'X'    )
                             type = '复合' (   text = '排产日期'            field = 'ZGXPCRQ'      edit = 'X'   )
                                           (   text = '计划数量(m2)'        field = 'ZJH_M2'       edit = 'X'  )
                                           (   text = '计划数量(块)'        field = 'ZJH_K'        edit = 'X'  )
                                           (   text = '实际完成数量(m2)'    field = 'ZSJ_M2'       edit = 'X'   )
                                           (   text = '实际完成数量(块)'    field = 'ZSJ_K'        edit = 'X'   )
                                           (   text = '达标率(m2)'         field = 'ZDBL_M2'      edit = ''   )
                                           (   text = '达标率(块)'         field = 'ZDBL_K'       edit = ''   )
                                           (   text = '人员数量'           field = 'ZRY'          edit = 'X'   )
                                           (   text = '压机炉数'           field = 'ZYJLS'        edit = ''   )
                                           (   text = '压机总定额(H)'       field = 'ZZDE'        edit = 'X'    )
                             type = '开槽' (   text = '排产日期'            field = 'ZGXPCRQ'     edit = 'X'   )
                                           (   text = '计划数量(m2)'        field = 'ZJH_M2'      edit = 'X'  )
                                           (   text = '计划数量(块)'        field = 'ZJH_K'      edit = 'X'  )
                                           (   text = '实际完成数量(m2)'    field = 'ZSJ_M2'      edit = 'X'   )
                                           (   text = '实际完成数量(块)'    field = 'ZSJ_K'      edit = 'X'   )
                                           (   text = '达标率(m2)'         field = 'ZDBL_M2'     edit = ''   )
                                           (   text = '达标率(块)'         field = 'ZDBL_K'     edit = ''   )
                                           (   text = '人员数量'           field = 'ZRY'        edit = 'X'   )
                                           (   text = '总定额(H)'          field = 'ZZDE'       edit = 'X'    )
                            type = '组装' (   text = '排产日期'            field = 'ZGXPCRQ'    edit = 'X'   )
                                          (   text = '计划数量(m)'         field = 'ZJH_M'      edit = 'X'  )
                                          (   text = '计划数量(m2)'        field = 'ZJH_M2'      edit = 'X'  )
                                          (   text = '计划数量(块)'        field = 'ZJH_K'      edit = 'X'  )
                                          (   text = '实际完成数量(m)'     field = 'ZSJ_M'      edit = 'X'   )
                                          (   text = '实际完成数量(m2)'    field = 'ZSJ_M2'      edit = 'X'   )
                                          (   text = '实际完成数量(块)'    field = 'ZSJ_K'       edit = 'X'   )
                                          (   text = '达标率(m)'          field = 'ZDBL_M'      edit = ''   )
                                          (   text = '达标率(m2)'         field = 'ZDBL_M2'     edit = ''   )
                                          (   text = '达标率(块)'         field = 'ZDBL_K'      edit = ''   )
                                          (   text = '人员数量'           field = 'ZRY'         edit = 'X'   )
                                          (   text = '订单定额'           field = 'ZDDDE'       edit = ''    )
                                          (   text = '组数'               field = 'ZZS'        edit = 'X'    )
                                          (   text = '单/双班'            field = 'ZDSB'       edit = 'X'    )
                                          (   text = '总定额(H)'          field = 'ZZDE'       edit = 'X'    )
                            ).
  PERFORM settitle.

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
  CLEAR: wherestr,itab[],itab.
  IF p_all = 'X'.
    wherestr = `ztpp_270a~zdelbs = '' `.
  ELSE.
    wherestr = `ztpp_270a~zdelbs = '' ` && ` AND ztpp_270a~zycbs = ''`.
  ENDIF.
  READ TABLE gt_lx INTO gv_lx WITH KEY code = sy-tcode.
  IF sy-subrc NE 0. "默认值
    lx-code = 'ZPP271'.
    lx-text = '开平'.
    lx-tabix = 1.
  ELSE.
    lx = gv_lx.
  ENDIF.

  SELECT
    *
   FROM ztpp_270a
   INNER JOIN ztpp_270 ON ztpp_270~zpch = ztpp_270a~zpch
   WHERE ztpp_270~werks IN @s_werks
     AND ztpp_270~zpcdh IN @s_zpcdh
     AND ztpp_270~posid IN @s_posid
     AND ztpp_270~matnr IN @s_matnr
     AND ztpp_270a~zgxxfrq IN @s_sydat
     AND ztpp_270a~zgxmc = @lx-text
     AND (wherestr)
   ORDER BY ztpp_270a~zpch,ztpp_270a~zpchh
   INTO CORRESPONDING FIELDS OF TABLE @itab.
  IF itab[] IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  REFRESH:gt_pch.
  LOOP AT itab.
    gv_pch-zpch = itab-zpch.
    COLLECT gv_pch INTO gt_pch.
  ENDLOOP.

  SELECT
      z270a~zpch,
      z270a~zgxmc
  FROM ztpp_270a AS z270a
  INNER JOIN @gt_pch AS zpc ON zpc~zpch = z270a~zpch
  WHERE z270a~zdelbs = ''
  GROUP BY z270a~zpch,z270a~zgxmc
  ORDER BY z270a~zpch,z270a~zgxmc
  INTO TABLE @DATA(lt_zpch).

  LOOP AT itab.
    IF lx-tabix < lines( gt_lx ).
      itab-id = icon_led_red.
      LOOP AT gt_lx INTO gv_lx FROM lx-tabix + 1.
        READ TABLE lt_zpch INTO DATA(lv_zpch) WITH KEY zpch = itab-zpch zgxmc = gv_lx-text BINARY SEARCH.
        IF sy-subrc = 0.
          itab-id = icon_led_green.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF lx-text = '复合'.
      itab-zyjls = itab-zplh.
    ENDIF.
    MODIFY itab.
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
  DATA: gt_events TYPE  slis_t_event,
        gs_events TYPE  slis_alv_event.

  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局

  IF lx-tabix < lines( gt_lx ).
    PERFORM catlg_set TABLES fldct
                      USING:
  'ID       '  '是否下发     '      '        '     '        '    ''      .
  ENDIF.
  PERFORM catlg_set TABLES fldct
                    USING:
'ZPCH     '  '排程号       '      'ZTPP_270'     'ZPCH   '     ''      ,
'ZPCHH    '  '排程行号     '      'ZTPP_270A'     'ZPCHH   '   ''      ,
'WERKS    '  '工厂         '      'ZTPP_270'     'WERKS   '    ''      ,
'POSID    '  '项目编号     '      'ZTPP_270'     'POSID   '    ''      ,
'ZPCDH    '  '排产单号     '      'ZTPP_270'     'ZPCDH   '    ''      ,
'AUFNR    '  '工单号       '      'ZTPP_270'     'AUFNR   '    ''      ,
'MATNR    '  '物料编码     '      'ZTPP_270'     'MATNR   '    ''      ,
'ZBX      '  '板型         '      'ZTPP_270'     'ZBX     '    ''      ,
'ZWBCMS   '  '外板长描述   '      'ZTPP_270'     'ZWBCMS  '    ''      ,
'ZNBCMS   '  '内板长描述   '      'ZTPP_270'     'ZNBCMS  '    ''      ,
'ZDDL_M   '  '订单量(M)    '      'ZTPP_270'     'ZDDL_M  '    ''      ,
'ZDDL_M2  '  '订单量(M2)    '     'ZTPP_270'     'ZDDL_M2 '    ''      ,
'ZBXKS    '  '订单板型块数 '      'ZTPP_270A'     'ZBXKS  '    ''      ,
'ZPCHS    '  '排产单行数   '      'ZTPP_270'     'ZPCHS   '    ''      ,
'ZSFYP    '  '是否样品     '      'ZTPP_270'     'ZSFYP   '    ''      ,
'ZJHRQ    '  '交付日期     '      'ZTPP_270'     'ZJHRQ   '    ''      ,
'ZQTRQ    '  '齐套日期     '      'ZTPP_270'     'ZQTRQ   '    ''      ,
'ZDBH     '  '打包号       '      'ZTPP_270'     'ZDBH    '    ''      ,
'ZJFSX    '  '交付顺序     '      'ZTPP_270'     'ZJFSX   '    ''      ,
'ZPLH     '  '配炉号       '      'ZTPP_270'     'ZPLH    '    ''      .

  LOOP AT gt_edit_field WHERE type = lx-text.
    PERFORM catlg_set TABLES fldct USING:
      gt_edit_field-field  gt_edit_field-text      'ZTPP_270A  '     gt_edit_field-field    gt_edit_field-edit      .
  ENDLOOP.
  PERFORM catlg_set TABLES fldct
                  USING:
'ZPLKD    '  '配料宽度     '      'ZTPP_270'      'ZPLKD   '   ''      ,
'ZSDPL    '  '配料流程-上道   '    'ZTPP_270A'     'ZSDPL   '  ''       ,
'ZXDPL    '  '配料流程-下道   '    'ZTPP_270A'     'ZXDPL   '  ''       ,
'ZPCBZJS  '  '排产备注解释    '    'ZTPP_270'     'ZPCBZJS '   ''       .

  i_title               = lines( itab ) .
  CONDENSE i_title.
  CONCATENATE '条目数:' i_title INTO i_title.
  gs_events-name = 'DATA_CHANGED'.
  gs_events-form = 'DATA_CHANGED_FORM'.
  APPEND gs_events TO gt_events.

  DATA: ls_grid_settings TYPE lvc_s_glay.
  ls_grid_settings-edt_cll_cb = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      it_fieldcat              = fldct[]
      i_save                   = 'A'
      is_variant               = varnt
      is_layout                = slayt
      i_grid_settings          = ls_grid_settings
      i_grid_title             = i_title
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
      it_events                = gt_events
    TABLES
      t_outtab                 = itab[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
**********************************************************************
* 单元格编辑事件
**********************************************************************
FORM data_changed_form  USING
         er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: ls_good TYPE lvc_s_modi.
  DATA: ls_stbl TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.


*  ASSIGN er_data_changed->mp_mod_rows->* TO <zfield>.
  DATA lv_value TYPE lvc_value.
  LOOP AT er_data_changed->mt_good_cells INTO ls_good.
    IF ls_good-value IS INITIAL.
      lv_value = 0.
    ELSE.
      lv_value = ls_good-value.
    ENDIF.
    READ TABLE itab INDEX ls_good-row_id.
    CASE ls_good-fieldname.
      WHEN 'ZJH_M'.
        IF lv_value NE 0.
          itab-zdbl_m = itab-zsj_m / lv_value * 100.
        ENDIF.
        IF lx-text = '开平'.
          itab-zsclde = lv_value * 8 / 60.
        ENDIF.
      WHEN 'ZSJ_M'.
        IF itab-zjh_m NE 0.
          itab-zdbl_m = lv_value / itab-zjh_m * 100.
        ENDIF.
      WHEN 'ZJH_M2'.
        IF lv_value NE 0.
          itab-zdbl_m2 = itab-zsj_m2 / lv_value * 100.
        ENDIF.
      WHEN 'ZSJ_M2'.
        IF itab-zjh_m2 NE 0.
          itab-zdbl_m2 = lv_value / itab-zjh_m2 * 100.
        ENDIF.
      WHEN 'ZJH_K'.
        IF lv_value NE 0.
          itab-zdbl_k = itab-zsj_k / lv_value * 100.
        ENDIF.
      WHEN 'ZSJ_K'.
        IF itab-zjh_k NE 0.
          itab-zdbl_k = lv_value / itab-zjh_k * 100.
        ENDIF.
      WHEN 'ZJH_PC'.
        IF lv_value NE 0.
          itab-zdbl_pc = itab-zsj_pc / lv_value * 100.
        ENDIF.
      WHEN 'ZSJ_PC'.
        IF itab-zjh_pc NE 0.
          itab-zdbl_pc = lv_value / itab-zjh_pc * 100.
        ENDIF.
      WHEN 'ZJH2_PC'.
        IF lv_value NE 0.
          itab-zdbl2_pc = itab-zsj2_pc / lv_value * 100.
        ENDIF.
      WHEN 'ZSJ2_PC'.
        IF itab-zjh2_pc NE 0.
          itab-zdbl2_pc = lv_value / itab-zjh2_pc * 100.
        ENDIF.
      WHEN 'ZGHLJS'.
        itab-zghlde = lv_value * 21 / 60.
    ENDCASE.
    MODIFY itab INDEX ls_good-row_id.
  ENDLOOP.

  ls_stbl-row = 'X'. "基于行的稳定刷新
  ls_stbl-col = 'X'. "基于列稳定刷新
  CALL METHOD lr_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stbl.
ENDFORM.                    "DATA_CHANGED_FORM
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
                USING p_field p_text p_reftab p_reffld p_edit.
  DATA: ls_fldct TYPE slis_fieldcat_alv.

  ls_fldct-fieldname     =  p_field.
  ls_fldct-seltext_l     =  p_text.
  ls_fldct-ddictxt       =  'L'.
  ls_fldct-ref_fieldname =  p_reffld.
  ls_fldct-ref_tabname   =  p_reftab.
  ls_fldct-edit   =  p_edit.
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
    WHEN 'ID' OR 'ZPCH' OR 'ZPCHH'.
      ls_fldct-fix_column = 'X'.
    WHEN 'ZPCDH'.
      ls_fldct-hotspot = 'X'.
    WHEN OTHERS.
  ENDCASE.

  APPEND ls_fldct TO fldcattab .
  CLEAR ls_fldct .
ENDFORM.

FORM set_status USING rt_extab TYPE slis_t_extab.

  CLEAR: rt_extab.
  REFRESH rt_extab.
  IF lx-tabix = lines( gt_lx ).
    APPEND 'XFGX' TO rt_extab.
  ENDIF.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab .
  PERFORM settitle.
ENDFORM.

FORM settitle.
  DATA:title TYPE title-text_line.
  CLEAR title.
  READ TABLE gt_lx INTO gv_lx WITH KEY code = sy-tcode.
  IF sy-subrc = 0.
    title = '得默排程计划-' && gv_lx-text.
  ELSE.
    title = '得默排程计划-开平'.
  ENDIF.
  SET TITLEBAR 'TIT1000' WITH title.
ENDFORM.

FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lv_rtmsg TYPE bapi_msg.
  DATA wa LIKE LINE OF itab.
  DATA num TYPE i.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CHECK rs_selfield-tabindex <> 0 . "小计行总计行什么的忽略
      READ TABLE itab INTO wa INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'ZPCDH'.
          CHECK wa-zpcdh IS NOT INITIAL.
          SUBMIT zppd201_v4 WITH p_submit = 'X'
                            WITH p_werks = wa-werks
                            WITH p_zpcdh  = wa-zpcdh
                            AND RETURN.

        WHEN OTHERS.
      ENDCASE.
    WHEN 'SAVE1'.
      IF NOT line_exists( itab[ sel = 'X' ] ).
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM save1.
    WHEN 'XFGX'. "下发工序
      CLEAR num.
      LOOP AT itab WHERE sel = 'X'.
        num += 1.
      ENDLOOP.
      IF num = 0.
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF num > 1.
        MESSAGE s004 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM xfgx.
    WHEN 'HIDE'. "隐藏工序
      CLEAR num.
      LOOP AT itab WHERE sel = 'X'.
        num += 1.
      ENDLOOP.
      IF num = 0.
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM hide.
    WHEN 'DEL'. "删除工序
      CLEAR num.
      LOOP AT itab WHERE sel = 'X'.
        num += 1.
      ENDLOOP.
      IF num = 0.
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM del.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.
**********************************************************************
* 下发工序
**********************************************************************
FORM xfgx.
  DATA: t_spopli TYPE TABLE OF spopli  WITH HEADER LINE,
        answer   TYPE char1.
  DATA:zgxmc TYPE ztpp_270a-zgxmc.
  DATA:wa_ztpp_270  TYPE ztpp_270,
       lt_ztpp_270a TYPE TABLE OF ztpp_270a WITH HEADER LINE.
  DATA:max_pchh TYPE ztpp_270a-zpchh.

  READ TABLE itab WITH KEY sel = 'X'.
  LOOP AT gt_lx INTO gv_lx FROM lx-tabix + 1.
    CLEAR t_spopli.
    IF itab-id = icon_led_red AND itab-zsclj CS gv_lx-text.
      t_spopli-varoption = gv_lx-text.
      APPEND t_spopli.
    ENDIF.
  ENDLOOP.
  IF t_spopli[] IS INITIAL.
    MESSAGE s000(oo) WITH '没有可用的工序' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
*     CURSORLINE         = 1
*     MARK_FLAG          = ' '
*     MARK_MAX           = 1
*     START_COL          = 0
*     START_ROW          = 0
      textline1          = '请选择工序：不允许重复下发'
*     TEXTLINE2          = ' '
*     TEXTLINE3          = ' '
      titel              = '下发工序'
*     DISPLAY_ONLY       = ' '
    IMPORTING
      answer             = answer
    TABLES
      t_spopli           = t_spopli
    EXCEPTIONS
      not_enough_answers = 1
      too_much_answers   = 2
      too_much_marks     = 3
      OTHERS             = 4.

  IF answer = 'A' .
    MESSAGE s000(oo) WITH '取消' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  READ TABLE t_spopli WITH KEY selflag  = 'X'.
  IF sy-subrc = 0.
    zgxmc = t_spopli-varoption.
  ENDIF.


  SELECT
   MAX( zpchh ) AS zpchh
  FROM ztpp_270a
  WHERE zpch = @itab-zpch
  INTO @max_pchh.
  SELECT
   *
  FROM ztpp_270a
  WHERE zpch = @itab-zpch
    AND zdelbs = ''
    AND zgxmc = @lx-text
  INTO TABLE @DATA(lt_270a).

  SELECT
    SINGLE *
  FROM ztpp_270
  WHERE zpch = @itab-zpch
  INTO @wa_ztpp_270.

  LOOP AT lt_270a INTO DATA(lv_270a).
    CLEAR lt_ztpp_270a.
    lt_ztpp_270a-zpch = lv_270a-zpch.
    lt_ztpp_270a-zgxmc = zgxmc.
    max_pchh += 10.
    lt_ztpp_270a-zpchh = max_pchh.
    lt_ztpp_270a-zbx = lv_270a-zbx.
    lt_ztpp_270a-zbxks = lv_270a-zbxks.
    READ TABLE gt_lx INTO gv_lx WITH KEY text = zgxmc. "配料流程上道  配料流程下道 赋默认值
    IF sy-subrc = 0.
      lt_ztpp_270a-zsdpl = gv_lx-zsdpl.
      lt_ztpp_270a-zxdpl = gv_lx-zxdpl.
    ENDIF.
    lt_ztpp_270a-zgxxfrq = sy-datlo.
    lt_ztpp_270a-zgxxfusr = sy-uname.
    lt_ztpp_270a-zjh_m = wa_ztpp_270-zddl_m.
    lt_ztpp_270a-zjh_m2 = wa_ztpp_270-zddl_m2.
    lt_ztpp_270a-zjh_k = wa_ztpp_270-zzks.
    APPEND lt_ztpp_270a.
  ENDLOOP.

  MODIFY ztpp_270a FROM TABLE lt_ztpp_270a.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s059 WITH '下发工序成功'.
    PERFORM getdata.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s059 WITH '保存失败' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
**********************************************************************
*保存数据
**********************************************************************
FORM save1.
  DATA:lt_ztpp_270a TYPE TABLE OF ztpp_270a WITH HEADER LINE.
  CLEAR:gt_pch,gv_pch.
  LOOP AT itab WHERE sel = 'X'.
    gv_pch-zpch = itab-zpch.
    COLLECT gv_pch INTO gt_pch.
  ENDLOOP.
  SELECT
    ztpp_270a~*
  FROM ztpp_270a
  INNER JOIN @gt_pch AS zpc ON  zpc~zpch  = ztpp_270a~zpch
  WHERE ztpp_270a~zdelbs = ''
    AND ztpp_270a~zgxmc = @lx-text
  ORDER BY ztpp_270a~zpch,ztpp_270a~zpchh
  INTO CORRESPONDING FIELDS OF TABLE @lt_ztpp_270a.
  LOOP AT lt_ztpp_270a ASSIGNING FIELD-SYMBOL(<f_270a>).
    READ TABLE itab WITH KEY zpch = <f_270a>-zpch zpchh = <f_270a>-zpchh.
    IF sy-subrc = 0.
      LOOP AT gt_edit_field WHERE type = lx-text.
        ASSIGN COMPONENT gt_edit_field-field OF STRUCTURE itab TO FIELD-SYMBOL(<f_itab_value>).
        ASSIGN COMPONENT gt_edit_field-field OF STRUCTURE <f_270a> TO FIELD-SYMBOL(<f_270a_value>).
        <f_270a_value> = <f_itab_value>.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  MODIFY ztpp_270a FROM TABLE lt_ztpp_270a.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s059 WITH '保存成功'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s059 WITH '保存失败' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
**********************************************************************
* 隐藏工序
**********************************************************************
FORM hide.
  DATA:lt_270a  TYPE TABLE OF ztpp_270a WITH HEADER LINE.

  CLEAR:gt_pch,gv_pch,answer.
  PERFORM confirmact(zpubform) USING '确定要隐藏工序吗' CHANGING answer.
  CHECK answer = '1'.
  LOOP AT itab WHERE sel = 'X'.
    gv_pch-zpch = itab-zpch.
    COLLECT gv_pch INTO gt_pch.
  ENDLOOP.
  SELECT
    z270a~*
  FROM ztpp_270a AS z270a
  INNER JOIN @gt_pch AS pc ON pc~zpch = z270a~zpch
  WHERE z270a~zdelbs = ''
    AND z270a~zycbs  = ''
    AND z270a~zgxmc = @lx-text
  ORDER BY z270a~zpch,z270a~zgxmc
  INTO CORRESPONDING FIELDS OF TABLE @lt_270a.

  LOOP AT lt_270a INTO DATA(lv_270a).
    lv_270a-zycbs = 'X'.
    lv_270a-zycdate = sy-datlo.
    lv_270a-zycusr = sy-uname.
    MODIFY lt_270a FROM lv_270a TRANSPORTING zycbs zycdate zycusr.
  ENDLOOP.
  IF lt_270a[] IS INITIAL.
    MESSAGE s059 WITH '已经隐藏了，不要重复隐藏了' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  MODIFY ztpp_270a FROM TABLE lt_270a[].
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s059 WITH '隐藏工序成功'.
    PERFORM getdata.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s059 WITH '隐藏工序失败' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
**********************************************************************
* 删除工序
**********************************************************************
FORM del.
  DATA:return TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA:lt_270a     TYPE TABLE OF ztpp_270a WITH HEADER LINE.
  DATA:BEGIN OF lt_gxmc OCCURS 0,
         zpch  TYPE ztpp_270a-zpch,
         zgxmc TYPE ztpp_270a-zgxmc,
       END OF lt_gxmc.

  CLEAR:gt_pch,gv_pch,answer.
  PERFORM confirmact(zpubform) USING '确定要删除工序吗' CHANGING answer.
  CHECK answer = '1'.
  LOOP AT itab WHERE sel = 'X'.
    gv_pch-zpch = itab-zpch.
    COLLECT gv_pch INTO gt_pch.
  ENDLOOP.
  SELECT
    z270a~*
  FROM ztpp_270a AS z270a
  INNER JOIN @gt_pch AS pc ON pc~zpch = z270a~zpch
  WHERE z270a~zdelbs = ''
  ORDER BY z270a~zpch,z270a~zgxmc
  INTO CORRESPONDING FIELDS OF TABLE @lt_270a.
  LOOP AT lt_270a.
    lt_gxmc-zpch = lt_270a-zpch.
    lt_gxmc-zgxmc = lt_270a-zgxmc.
    COLLECT lt_gxmc.
  ENDLOOP.
  SORT lt_gxmc BY zpch zgxmc.
  IF lx-tabix < lines( gt_lx ).
    LOOP AT gt_pch INTO gv_pch.
      LOOP AT gt_lx INTO gv_lx FROM lx-tabix + 1.
        READ TABLE lt_gxmc WITH KEY zpch = gv_pch-zpch zgxmc = gv_lx-text TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc = 0.
          "存在***排程计划，不允许删除开平排程计划
          DATA(msg) = |{ gv_pch-zpch }存在下道排程计划，不允许删除{ lx-text }排程计划|.
          PERFORM inmsg(zpubform) TABLES return USING 'ZMSG_GP' 'E' '059' msg '' '' ''.
          DELETE lt_270a WHERE zpch = gv_pch-zpch.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
  IF lt_270a[] 