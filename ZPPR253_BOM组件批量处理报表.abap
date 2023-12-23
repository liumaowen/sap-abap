*&---------------------------------------------------------------------*
*& Report ZPPR_TEST_02
*&---------------------------------------------------------------------*
*& Title : ZPPR101 *
*& Application : PP 　 *
*& Subject : 批量更改BOM组件 *
*& Req Date : 20231015 *
*&---------------------------------------------------------------------*
REPORT zppr253 MESSAGE-ID zgp_msg.

TABLES:sscrfields,t001l,afko,mast,t023.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid,
      tabix LIKE sy-tabix.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0,
       werks     TYPE mast-werks,
       matnr     TYPE mast-matnr,
       stlan     TYPE mast-stlan,
       cpwlcms   TYPE string,
       stlal     TYPE mast-stlal,
       posnr     TYPE stpo-posnr,
       idnrk     TYPE stpo-idnrk,
       idnrk_n   TYPE stpo-idnrk, "更改后组件编码
       menge     TYPE stpo-menge,
       menge_n   TYPE stpo-menge, "更改后组件数据
       meins     TYPE stpo-meins,
       meins_n   TYPE stpo-meins,
       zjcpwlcms TYPE string,
       zgylx     TYPE char1,
       zscbb     TYPE char1,
       andat     TYPE mast-andat,
       ligth     TYPE iconname, "状态  icon_led_red 红灯   icon_led_green 绿灯
       msg       TYPE string, "消息文本
       sel,
     END OF itab.
DATA itab1 LIKE TABLE OF itab.
DATA:BEGIN OF lt_mapl OCCURS 0,
       werks TYPE mapl-werks,
       matnr TYPE mapl-matnr,
     END OF lt_mapl.
DATA:BEGIN OF lt_mkal OCCURS 0,
       werks TYPE mkal-werks,
       matnr TYPE mkal-matnr,
     END OF lt_mkal.
DATA:num TYPE i.

DATA:lt_stpo TYPE STANDARD TABLE OF stpo_api02 WITH HEADER LINE.
DATA:
  it_stpo    TYPE TABLE OF stpo_api03,
  is_stpo    TYPE stpo_api03,
  is_stko    TYPE stko_api01,
  lt_stko    TYPE TABLE OF stko_api02,
  ls_stpo    TYPE stpo_api02,
  ls_stko    TYPE stko_api02,
  l_prio     TYPE alprf_bi,
  l_datuv    TYPE csap_mbom-datuv,

  lv_warning TYPE capiflag-flwarning,
  ls_stko_o  TYPE stko_api02.
DATA:is_stko_add TYPE stko_api01,
     is_stpo_add TYPE stpo_api02,
     it_stpo_del TYPE stpo_api03,
     it_stpo_add TYPE TABLE OF stpo_api03,
     lt_stpo_del TYPE TABLE OF stpo_api03.

DATA: go_grid2   TYPE REF TO cl_gui_alv_grid, "##NEEDED,
      go_contp   TYPE REF TO cl_gui_custom_container, "##NEEDED,
      go_cont2   TYPE REF TO cl_gui_container, "##NEEDED,
      go_spli1   TYPE REF TO cl_gui_splitter_container, "##NEEDED
      gt_fldc3   TYPE lvc_t_fcat , "##NEEDED.
      gw_layout3 TYPE lvc_s_layo . "##NEEDED.
DATA:it_filter TYPE lvc_t_fidx.
DATA:BEGIN OF gt_bom_add OCCURS 0,
       chbox,
       matnr TYPE mast-matnr,
       menge TYPE stpo-menge,
       meins TYPE stpo-meins,
     END OF gt_bom_add.
DATA:ok_code LIKE sy-ucomm,
     save_ok LIKE sy-ucomm.
DATA:BEGIN OF it_tabix OCCURS 0,
       tabix TYPE sy-tabix,
     END OF it_tabix.
DATA: BEGIN OF it_wlcms OCCURS 0,
        matnr TYPE matnr,
        wlcms TYPE string,
      END OF it_wlcms.
DATA: BEGIN OF it_maxmast OCCURS 0,
        werks TYPE werks_d,
        matnr TYPE matnr,
        stlan TYPE stlan,
        stlal TYPE stlal,
        posnr TYPE stpo-posnr,
      END OF it_maxmast.

FIELD-SYMBOLS: <wa>      TYPE any.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR mast-werks DEFAULT '3000' OBLIGATORY NO INTERVALS NO-EXTENSION,
                 s_andat FOR mast-andat,
                 s_matnr FOR mast-matnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t2.
  PARAMETERS: p_ck RADIOBUTTON GROUP rad2 DEFAULT 'X',
              p_xz RADIOBUTTON GROUP rad2,
              p_xg RADIOBUTTON GROUP rad2,
              p_sc RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  t1 = '选择条件'.
  t2 = '功能选择'.
  %_s_werks_%_app_%-text = '工厂'.
  %_s_andat_%_app_%-text = '创建日期'.
  %_s_matnr_%_app_%-text = '物料'.
  %_p_ck_%_app_%-text      = '查看'.
  %_p_xz_%_app_%-text      = '新增'.
  %_p_xg_%_app_%-text      = '修改'.
  %_p_sc_%_app_%-text      = '删除'.

  INCLUDE zppr_bom_class.

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
  SELECT
    mast~werks,
    mast~matnr,
    mast~stlan,
    mast~stlal,
    mast~andat,
    stpo~posnr,
    stpo~idnrk,
    stpo~menge,
    stpo~meins
    INTO CORRESPONDING FIELDS OF TABLE @itab
    FROM mast
    JOIN stas ON stas~stlnr = mast~stlnr AND stas~stlal = mast~stlal AND stas~stlty = 'M'
    JOIN stpo ON stpo~stlkn = stas~stlkn AND stpo~stlnr = stas~stlnr AND stpo~stlty = 'M'
    WHERE mast~werks IN @s_werks
    AND mast~matnr IN @s_matnr
    AND mast~andat IN @s_andat.
  IF itab[] IS INITIAL.
    MESSAGE s004 WITH '无数据' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  REFRESH:it_wlcms.
  LOOP AT itab.
    CLEAR: it_wlcms.
    IF itab-matnr IS NOT INITIAL.
      it_wlcms-matnr = itab-matnr.
      COLLECT it_wlcms.
    ENDIF.
    IF itab-idnrk IS NOT INITIAL.
      it_wlcms-matnr = itab-idnrk.
      COLLECT it_wlcms.
    ENDIF.
  ENDLOOP.
  PERFORM getlongtextpl(zpubform) TABLES it_wlcms.
  SORT it_wlcms BY matnr.

  SELECT werks,matnr INTO TABLE @lt_mapl FROM mapl WHERE werks IN @s_werks AND matnr IN @s_matnr.
  SORT lt_mapl BY matnr.
  SELECT werks,matnr INTO TABLE @lt_mkal FROM mkal WHERE werks IN @s_werks AND matnr IN @s_matnr.
  SORT lt_mkal BY matnr.
  LOOP AT itab.
    READ TABLE it_wlcms WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-cpwlcms = it_wlcms-wlcms.
    ENDIF.
    READ TABLE it_wlcms WITH KEY matnr = itab-idnrk BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zjcpwlcms = it_wlcms-wlcms.
    ENDIF.
    READ TABLE lt_mapl WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zgylx = 'X'.
    ENDIF.
    READ TABLE lt_mkal WITH KEY matnr = itab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-zscbb = 'X'.
    ENDIF.
    MODIFY itab.
  ENDLOOP.
  SORT itab BY matnr stlan stlal posnr.
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
'LIGTH '  '标识'  ''  '' '',
'MSG '  '消息'  ''  '' '',
'WERKS '  '工厂'  'MAST'  'WERKS' '',
'MATNR '  '成品物料号'  'MAST'  'MATNR' '',
'CPWLCMS '  '成品物料描述'  ''  '' '',
'STLAN '  '用途'  'MAST'  'STLAN' '',
'STLAL '  '备选'  'MAST'  'STLAL' '',
'POSNR '  '组件号'  'STPO'  'POSNR' '',
'IDNRK '  '组件物料号'  'MAST'  'MATNR' ''.
  IF p_xg = 'X'.
    PERFORM catlg_set TABLES fldct USING:
'IDNRK_N '  '变更后料号'  'MAST'  'MATNR' 'X'.
  ENDIF.
  PERFORM catlg_set TABLES fldct USING:
'ZJCPWLCMS '  '组件物料描述'  ''  '' '',
'MENGE '  '数量'  'STPO'  'MENGE' '',
'MEINS '  '单位'  'STPO'  'MENGE' ''.
  IF p_xg = 'X'.
    PERFORM catlg_set TABLES fldct USING:
'MENGE_N '  '变更后数量'  'STPO'  'MENGE' 'X',
'MEINS_N '  '变更后单位'  'STPO'  'MEINS' 'X'.
  ENDIF.
  PERFORM catlg_set TABLES fldct USING:
'ZGYLX '  '工艺路线'  ''  '' '',
'ZSCBB '  '生产版本'  ''  '' '',
'ANDAT '  'BOM创建时间'  'MAST'  'ANDAT' ''.

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
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
      i_grid_title             = i_title
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
                USING p_field p_text p_reftab p_reffld p_reedit.
  DATA: ls_fldct TYPE slis_fieldcat_alv.

  ls_fldct-fieldname     =  p_field.
  ls_fldct-seltext_l     =  p_text.
  ls_fldct-ddictxt       =  'L'.
  ls_fldct-ref_fieldname =  p_reffld.
  ls_fldct-ref_tabname   =  p_reftab.
  ls_fldct-edit   =  p_reedit.
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

  IF p_ck = 'X'.
    APPEND 'ZADD' TO rt_extab.
    APPEND 'ZEDIT' TO rt_extab.
    APPEND 'ZDEL' TO rt_extab.
  ELSEIF p_xz = 'X'.
    APPEND 'ZEDIT' TO rt_extab.
    APPEND 'ZDEL' TO rt_extab.
  ELSEIF p_xg = 'X'.
    APPEND 'ZADD' TO rt_extab.
    APPEND 'ZDEL' TO rt_extab.
  ELSE.
    APPEND 'ZADD' TO rt_extab.
    APPEND 'ZEDIT' TO rt_extab.
  ENDIF.

  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab .
ENDFORM.

FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lv_rtmsg TYPE bapi_msg.
  DATA: ls_fldct TYPE slis_fieldcat_alv.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CHECK rs_selfield-tabindex <> 0 . "小计行总计行什么的忽略
      READ TABLE itab INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'IDNRK'.
          PERFORM mm03(zpubform) USING itab-idnrk.
        WHEN 'MATNR'.
          PERFORM mm03(zpubform) USING itab-matnr.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZREFRESH'. "刷新
      PERFORM getdata.
    WHEN 'ZADD'.
      PERFORM zadd.
    WHEN 'ZEDIT'.
      PERFORM zedit_bom_compt.
    WHEN 'ZDEL'.
      PERFORM zdel_bom_compt.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form zadd_bom_compt
*&---------------------------------------------------------------------*
*& 添加BOM组件信息
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zadd_bom_compt .
  DATA:return        TYPE string,
       lv_fl_warning TYPE char1.
  DATA:ls_posnr TYPE stpo-posnr.
  CLEAR num.
  LOOP AT gt_bom_add WHERE chbox = 'X'.
    IF gt_bom_add-matnr IS INITIAL OR gt_bom_add-meins IS INITIAL OR gt_bom_add-menge IS INITIAL.
      MESSAGE s045 WITH '第' sy-tabix '行数据填写完整' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    SELECT * FROM marm WHERE matnr = @gt_bom_add-matnr AND meinh = @gt_bom_add-meins INTO TABLE @DATA(it_marm).
    IF sy-subrc NE 0.
      MESSAGE s045 WITH '第' sy-tabix '行物料单位不匹配' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    num += 1.
  ENDLOOP.
  IF num IS INITIAL.
    MESSAGE s024 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT it_maxmast.
*&L3.read BOM
    CLEAR:lt_stpo,lt_stpo[],
    lt_stko,
    it_stpo,it_stpo[],
    is_stko,
    it_stpo_add,it_stpo_add[],
    is_stko_add.
    CLEAR:l_datuv.

*&读取bom原始数据
    CALL FUNCTION 'CSAP_MAT_BOM_READ'
      EXPORTING
        material    = it_maxmast-matnr
        plant       = it_maxmast-werks
        bom_usage   = it_maxmast-stlan
        alternative = it_maxmast-stlal
      TABLES
        t_stpo      = lt_stpo
        t_stko      = lt_stko
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      CLEAR:it_stpo,ls_posnr.
      READ TABLE lt_stko INTO ls_stko INDEX 1.
      MOVE-CORRESPONDING ls_stko TO is_stko_add.
      l_datuv = sy-datum.
      ls_posnr = it_maxmast-posnr."BOM行号根据最大号+10
**&1.行明细赋值
      LOOP AT gt_bom_add WHERE chbox = 'X'.
        ls_posnr = ls_posnr + 10.
        is_stpo_add-item_no = ls_posnr.
        is_stpo_add-item_categ = 'L'.
        is_stpo_add-component = gt_bom_add-matnr.
        is_stpo_add-comp_qty = gt_bom_add-menge.
        is_stpo_add-comp_unit = gt_bom_add-meins.
        is_stpo_add-rel_prod   = 'X'.
        APPEND is_stpo_add TO it_stpo_add.
      ENDLOOP.

**&2.新增
      CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
        EXPORTING
          material           = it_maxmast-matnr
          plant              = it_maxmast-werks
          bom_usage          = it_maxmast-stlan
          alternative        = it_maxmast-stlal
          valid_from         = l_datuv
          i_stko             = is_stko_add
          fl_commit_and_wait = 'X'
          fl_complete        = 'X'
*         fl_new_item        = 'X'
        IMPORTING
          fl_warning         = lv_fl_warning
        TABLES
          t_stpo             = it_stpo_add
        EXCEPTIONS
          error              = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = return.
        LOOP AT itab WHERE sel = 'X' AND matnr = it_maxmast-matnr AND werks = it_maxmast-werks AND stlan = it_maxmast-stlan AND stlal = it_maxmast-stlal.
          itab-ligth = icon_led_red.
          itab-msg = itab-msg = '添加BOM组件失败:' && return.
          MODIFY itab TRANSPORTING ligth msg.
        ENDLOOP.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        LOOP AT itab WHERE sel = 'X' AND matnr = it_maxmast-matnr AND werks = it_maxmast-werks AND stlan = it_maxmast-stlan AND stlal = it_maxmast-stlal.
          itab-ligth = icon_led_green.
          itab-msg = '添加BOM组件成功！'.
          MODIFY itab TRANSPORTING ligth msg.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.
*      PERFORM frm_refresh_alv_pcd.
  LEAVE TO SCREEN 0.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form zedit_bom_compt
*&---------------------------------------------------------------------*
*& 修改BOM组件信息
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zedit_bom_compt .
  DATA:return        TYPE string.
  CLEAR num.
  LOOP AT itab WHERE sel = 'X'.
    IF itab-idnrk_n IS INITIAL AND itab-meins_n IS INITIAL AND itab-menge_n IS INITIAL.
      MESSAGE s045 WITH '第' sy-tabix '行数据都为空，请至少填写一项' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF itab-idnrk_n IS NOT INITIAL AND itab-meins_n IS NOT INITIAL.
      SELECT * FROM marm WHERE matnr = @itab-idnrk_n AND meinh = @itab-meins_n INTO TABLE @DATA(it_marm).
      IF sy-subrc NE 0.
        MESSAGE s045 WITH '第' sy-tabix '行物料单位不匹配' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ELSEIF itab-idnrk_n IS NOT INITIAL AND itab-meins_n IS INITIAL.
      SELECT * FROM marm WHERE matnr = @itab-idnrk_n AND meinh = @itab-meins INTO TABLE @it_marm.
      IF sy-subrc NE 0.
        MESSAGE s045 WITH '第' sy-tabix '行物料单位不匹配' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ELSEIF itab-idnrk_n IS INITIAL AND itab-meins_n IS NOT INITIAL.
      SELECT * FROM marm WHERE matnr = @itab-idnrk AND meinh = @itab-meins_n INTO TABLE @it_marm.
      IF sy-subrc NE 0.
        MESSAGE s045 WITH '第' sy-tabix '行物料单位不匹配' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.
    num += 1.
  ENDLOOP.
  IF num IS INITIAL.
    MESSAGE s024 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  LOOP AT itab WHERE sel = 'X'.
*&L3.read BOM
    CLEAR:lt_stpo,lt_stpo[],lt_stko,it_stpo,it_stpo[],is_stko,it_stpo_add,it_stpo_add[],is_stko_add,lt_stpo_del.
    CLEAR:l_datuv.

*&读取bom原始数据
    CALL FUNCTION 'CSAP_MAT_BOM_READ'
      EXPORTING
        material    = itab-matnr
        plant       = itab-werks
        bom_usage   = itab-stlan
        alternative = itab-stlal
      TABLES
        t_stpo      = lt_stpo
        t_stko      = lt_stko
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    SORT lt_stpo BY item_no component.
    READ TABLE lt_stpo WITH KEY item_no = itab-posnr component = itab-idnrk BINARY SEARCH.
*根据BOM项目号删除需要删除的项目
    IF sy-subrc = 0.
      lt_stpo-fldelete = 'X'.
      APPEND lt_stpo TO lt_stpo_del.
    ENDIF.
    READ TABLE lt_stko INTO ls_stko INDEX 1.
    MOVE-CORRESPONDING ls_stko TO is_stko_add.

*&2.删除
*& 先将组件项目 打上删除标记
    CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
      EXPORTING
        material           = itab-matnr
        plant              = itab-werks
        bom_usage          = itab-stlan
        alternative        = itab-stlal
        valid_from         = l_datuv
        i_stko             = is_stko
        fl_commit_and_wait = 'X'
        fl_bom_create      = 'X'
*       fl_new_item        = 'X'
      TABLES
        t_stpo             = lt_stpo_del
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.
    IF sy-subrc = 0.

*&2.数据新增
*    is_stpo-id_item_no = itab-posnr.
*    is_stpo-fldelete = 'X'.
*    APPEND is_stpo TO it_stpo.
      is_stpo_add-item_no = itab-posnr.
      is_stpo_add-item_categ = 'L'.
      IF itab-idnrk_n IS INITIAL.  is_stpo_add-component = itab-idnrk.  ELSE.  is_stpo_add-component = itab-idnrk_n.  ENDIF.
      IF itab-menge_n IS INITIAL.  is_stpo_add-comp_qty = itab-menge.   ELSE.  is_stpo_add-comp_qty = itab-menge_n.   ENDIF.
      IF itab-meins_n IS INITIAL.  is_stpo_add-comp_unit = itab-meins.  ELSE.  is_stpo_add-comp_unit = itab-meins_n.  ENDIF.
      is_stpo_add-rel_prod   = 'X'.
      APPEND is_stpo_add TO it_stpo_add.

*&3.新增
      CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
        EXPORTING
          material           = itab-matnr
          plant              = itab-werks
          bom_usage          = itab-stlan
          alternative        = itab-stlal
          valid_from         = l_datuv
          i_stko             = is_stko_add
          fl_commit_and_wait = 'X'
          fl_bom_create      = 'X'
          fl_new_item        = 'X'
          fl_complete        = 'X'
        TABLES
          t_stpo             = it_stpo_add
        EXCEPTIONS
          error              = 1
          OTHERS             = 2.


      IF sy-subrc <> 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = return.
        itab-ligth = icon_led_red.
        itab-msg = itab-msg = '更新BOM组件失败:' && return.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        itab-ligth = icon_led_green.
        itab-msg = '更新BOM组件成功！'.

      ENDIF.
    ELSE.

    ENDIF.
    MODIFY itab TRANSPORTING ligth msg.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form zdel_bom_compt
*&---------------------------------------------------------------------*
*& 删除BOM组件信息
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zdel_bom_compt .
  CLEAR num.
  LOOP AT itab WHERE sel = 'X'.
    num += 1.
  ENDLOOP.
  IF num IS INITIAL.
    MESSAGE s024 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT itab WHERE sel = 'X'.

    CLEAR:lt_stpo,lt_stpo[],lt_stko,it_stpo,it_stpo[],is_stko,lt_stpo_del.

*&读取bom原始数据
    CALL FUNCTION 'CSAP_MAT_BOM_READ'
      EXPORTING
        material    = itab-matnr
        plant       = itab-werks
        bom_usage   = itab-stlan
        alternative = itab-stlal
      TABLES
        t_stpo      = lt_stpo
        t_stko      = lt_stko
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    SORT lt_stpo BY item_no component.
    READ TABLE lt_stpo WITH KEY item_no = itab-posnr component = itab-idnrk BINARY SEARCH.
*根据BOM项目号删除需要删除的项目
    IF sy-subrc = 0.
      lt_stpo-fldelete = 'X'.
      APPEND lt_stpo TO lt_stpo_del.
    ENDIF.

    IF lt_stpo_del IS NOT INITIAL.
      CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
        EXPORTING
          material           = itab-matnr
          plant              = itab-werks
          bom_usage          = itab-stlan
          alternative        = itab-stlal
          valid_from         = l_datuv
          i_stko             = is_stko
          fl_commit_and_wait = 'X'
          fl_bom_create      = 'X'
*         fl_new_item        = 'X'
        IMPORTING
          fl_warning         = lv_warning
          o_stko             = ls_stko_o
        TABLES
          t_stpo             = lt_stpo_del
        EXCEPTIONS
          error              = 1
          OTHERS             = 2.

      IF sy-subrc <> 0.
        itab-ligth = icon_led_red.
        itab-msg = '删除BOM组件失败！'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSEIF lv_warning = 'X'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = itab-msg.

        itab-ligth = icon_led_green.
      ENDIF.

    ELSE.
      itab-ligth = icon_led_red.
      itab-msg = '删除BOM组件失败！'.
    ENDIF.

    MODIFY itab TRANSPORTING ligth msg.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'STA9001'.
  SET TITLEBAR 'TIT9001' WITH '添加BOM行明细信息'.

  IF go_grid2 IS INITIAL.
    CREATE OBJECT go_contp
      EXPORTING
        container_name              = 'CON2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CREATE OBJECT go_grid2
      EXPORTING
        i_parent = go_contp.
    PERFORM catset3 TABLES gt_fldc3
                    USING: 'CHBOX' '' '' '全选',
                           'MATNR' 'MAST' 'MATNR' '物料编码',
                           'MENGE' 'STPO' 'MENGE' '数量',
                           'MEINS' 'STPO' 'MEINS' '单位'.

*注册事件
    CALL METHOD cl_gui_cfw=>dispatch.

    CREATE OBJECT lcl_event_receiver_bom.

    SET HANDLER lcl_event_receiver_bom->handle_double_click2 FOR go_grid2.
    SET HANDLER lcl_event_receiver_bom->handle_toolbar2 FOR go_grid2."增加按钮
    SET HANDLER lcl_event_receiver_bom->handle_command2 FOR go_grid2."按钮响应
    SET HANDLER lcl_event_receiver_bom->handle_hotspot_click2 FOR go_grid2."热点点击
    SET HANDLER lcl_event_receiver_bom->handle_on_f42 FOR go_grid2.
    SET HANDLER lcl_event_receiver_bom->handle_data_changed2 FOR go_grid2.
    go_grid2->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified )."mc_evt_enter
    go_grid2->register_delayed_event( i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select )."MC_EVT_DELAYED_CHANGE_SELECT
    PERFORM callalvn.
  ELSE.
    PERFORM callalvn.
  ENDIF.

*刷新ALV
  PERFORM refreshalv.

ENDMODULE.

FORM refreshalv .
  DATA: is_stable  TYPE lvc_s_stbl.

  is_stable-row = 'X'.
  is_stable-col = 'X'.
  IF go_grid2 IS NOT INITIAL.
    CALL METHOD go_grid2->refresh_table_display
      EXPORTING
        is_stable = is_stable
*       I_SOFT_REFRESH = 'X'
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL METHOD cl_gui_cfw=>flush.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALLALVN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM callalvn .
  PERFORM callalv(zpubform)
  TABLES gt_bom_add[] USING go_grid2 gt_fldc3 'ZJ01'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'QX'.
      LEAVE TO SCREEN 0.
    WHEN 'CONF'.
      PERFORM zadd_bom_compt.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ALV_DISPLAY_ADDBOMZJ OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE alv_display_addbomzj OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  catset1
*&---------------------------------------------------------------------*
FORM catset3 TABLES t_fldcat USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname =  pv_field.    "字段名
  ls_fldcat-scrtext_l =  pv_text.     "长描述
  ls_fldcat-coltext   =  pv_text.     "列描述
  ls_fldcat-ref_table =  pv_reftab.   "参考表名
  ls_fldcat-ref_field =  pv_reffld.   "参考字段名
  ls_fldcat-col_opt   = 'A'.          "自动优化列宽

  CASE ls_fldcat-fieldname.
    WHEN 'MATNR'.
      ls_fldcat-edit_mask = '==MATN1'.
      ls_fldcat-edit   = 'X'.
    WHEN 'MENGE'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero = 'X'.
      ls_fldcat-edit   = 'X'.
    WHEN 'MEINS'.
      ls_fldcat-edit_mask = '==CUNIT'.
      ls_fldcat-edit   = 'X'.
    WHEN 'CHBOX'.
      ls_fldcat-hotspot   = 'X'.
      ls_fldcat-edit   = 'X'.
      ls_fldcat-checkbox   = 'X'.
      ls_fldcat-fix_column   = 'X'.
    WHEN OTHERS.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZADD
*&      text  添加组件
*&---------------------------------------------------------------------*
FORM zadd.
  TYPES:BEGIN OF ty_sel,
          werks TYPE werks_d,
          matnr TYPE matnr,
          stlan TYPE stlan,
          stlal TYPE stlal,
        END OF ty_sel.
  DATA:it_sel TYPE TABLE OF ty_sel WITH KEY werks matnr stlan stlal,
       wa_sel LIKE LINE OF it_sel.
  CLEAR num.
  REFRESH: gt_bom_add,it_maxmast.
  LOOP AT itab WHERE sel = 'X'.
    wa_sel-werks = itab-werks.
    wa_sel-matnr = itab-matnr.
    wa_sel-stlan = itab-stlan.
    wa_sel-stlal = itab-stlal.
    COLLECT wa_sel INTO it_sel.
    num += 1.
  ENDLOOP.
  IF num IS INITIAL.
    MESSAGE s024 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  SELECT
    mast~werks,
    mast~matnr,
    mast~stlan,
    mast~stlal,
    MAX( stpo~posnr ) AS posnr
    FROM mast
    INNER JOIN @it_sel AS a ON mast~werks = a~werks AND mast~matnr = a~matnr AND mast~stlan = a~stlan AND mast~stlal  = a~stlal
    JOIN stas ON stas~stlnr = mast~stlnr AND stas~stlal = mast~st