*&---------------------------------------------------------------------*
*& Report ZPPR248A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr248a MESSAGE-ID zmsg_gp.

TABLES:sscrfields,t001l,mara,ztpp_206,rc68a.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_itab,
        zname TYPE ztpp_248a-zname, "品名/工作中心
        zlb   TYPE char2,           "类别
        sel,
      END OF ty_itab.
DATA:itab    TYPE TABLE OF ty_itab WITH EMPTY KEY,
     wa_itab LIKE LINE OF itab.
TYPES:BEGIN OF ty_temp,             "临时表结构
        zname TYPE ztpp_248a-zname, "品名/工作中心
        sel,
      END OF ty_temp.
TYPES:BEGIN OF ty_zlb,
        text  TYPE char2,
        field TYPE char6,
      END OF ty_zlb.
DATA:gt_zlb TYPE TABLE OF ty_zlb WITH HEADER LINE.
DATA:gt_248a TYPE TABLE OF ztpp_248a WITH EMPTY KEY,
     gv_248a LIKE LINE OF gt_248a.
DATA:r_zname TYPE RANGE OF ztpp_248a-zname WITH HEADER LINE.
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
FIELD-SYMBOLS: <f_temp>  TYPE table,
               <f_tline> TYPE any.
DATA:lastdate TYPE sy-datum, "某月最后一天日期
     lastday  TYPE i.        "某月天数
TYPES:BEGIN OF ty_ck,
        zname TYPE ztpp_248a-zname,   "品名/工作中心
        date  TYPE likp-wadat_ist,
        ckl   TYPE lips-lfimg,
      END OF ty_ck.
DATA:gt_ck TYPE TABLE OF ty_ck WITH HEADER LINE. "出库信息按工作中心和日期求和
DATA:BEGIN OF gt_rk OCCURS 0,        "入库信息按工作中心和日期求和
       zname TYPE ztpp_248a-zname,   "品名/工作中心
       date  TYPE likp-wadat_ist,
       rkl   TYPE lips-lfimg,
     END OF gt_rk.
DATA:BEGIN OF gt_zckl OCCURS 0.       "出库单+调拨单+领料单
       INCLUDE TYPE ty_ck.
DATA:  type TYPE string,            "类型：出库单，调拨单，领料单
     END OF gt_zckl.
DATA:gt_zckl_hz TYPE TABLE OF ty_ck WITH HEADER LINE. "出库单+调拨单+领料单 的汇总

DATA:index TYPE i,
     tabix TYPE i,
     dat   TYPE char2.
DATA:BEGIN OF lv_data,
       kucun TYPE menge_d,
       ruku  TYPE menge_d,
       chuku TYPE menge_d,
     END OF lv_data.
DATA:BEGIN OF gt_cpkc OCCURS 0,    "成品库存
       arbpl TYPE ztpp_206-arbpl,
       zzl1  TYPE mara-zzl1,
       post1 TYPE ztpp_206-post1,
       zklts TYPE i,               "库龄
       clabs TYPE mchb-clabs,
     END OF gt_cpkc.
DATA:BEGIN OF gt_clkc OCCURS 0,    "超龄库存
       zname TYPE ztpp_248a-zname, "品名/工作中心
       clabs TYPE mchb-clabs,
     END OF gt_clkc.
TYPES:BEGIN OF ty_pmdl,    "品名大类
        type TYPE mara-zzl1,
        zzl1 TYPE mara-zzl1,
      END OF ty_pmdl.
DATA:gt_pmdl TYPE TABLE OF ty_pmdl WITH EMPTY KEY,
     gv_pmdl LIKE LINE OF gt_pmdl.
DATA:r_zzl1 TYPE RANGE OF mara-zzl1 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS:p_werks TYPE t001l-werks OBLIGATORY DEFAULT '3000',
             p_month LIKE isellist-month DEFAULT sy-datum+0(6) OBLIGATORY.
  SELECT-OPTIONS:s_zzl1 FOR mara-zzl1 MODIF ID s1,
                 s_arbpl FOR rc68a-arbpl MODIF ID s2.
  PARAMETERS:p_zdep  TYPE ztpp_248a-zdep NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t2.
  PARAMETERS:p1 RADIOBUTTON GROUP grd1 USER-COMMAND singleclick DEFAULT 'X' MODIF ID m1,
             p2 RADIOBUTTON GROUP grd1 MODIF ID m1.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN FUNCTION KEY :1.

INITIALIZATION.
  t1 = '选择条件'.
  t2 = '功能选择'.
  %_p_werks_%_app_%-text = '工厂'.
  %_p_month_%_app_%-text = '月份'.
  %_s_zzl1_%_app_%-text = '品名'.
  %_s_arbpl_%_app_%-text = '工作中心'.
  %_p1_%_app_%-text = '钢品一部'.
  %_p2_%_app_%-text = '钢品二部'.
  sscrfields-functxt_01 = '@0J@月度库存配置'.

  gt_zlb[] = VALUE #( ( field = 'KUCUN'  text = '库存'  )
                      ( field = 'RUKU'   text = '入库'  )
                      ( field = 'CHUKU'  text = '出库'  )
                    ).
  gt_pmdl   = VALUE #( ( type = '檩条'    zzl1 = 'C型钢'               )
                       ( type = '檩条'    zzl1 = 'Z型钢'               )
                       ( type = '檩条'    zzl1 = '焊接C型钢'           )
                       ( type = '幕墙'    zzl1 = '凯撒'                )
                       ( type = '幕墙'    zzl1 = '丽彩幕'              )
                       ( type = '幕墙'    zzl1 = '丽彩幕（研发专用）'  )
                       ( type = '折弯件'  zzl1 = '折弯件'              )
                       ( type = '折弯件'  zzl1 = '折弯件-丽彩类'       )
                       ( type = '折弯件'  zzl1 = '折弯件（研发专用）'  )
                       ( type = '压型板'  zzl1 = '压型板'             )
                       ( type = '压型板'  zzl1 = '承重板'             )
                       ( type = '压型板'  zzl1 = '落水管直管'         )
                       ( type = '压型板'  zzl1 = '落水管弯头'         )
                       ( type = '压型板'  zzl1 = '压型板（研发专用）' )
                     ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month.
  PERFORM selmonth(zpubform) CHANGING p_month.


AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM callprog(zpubform) USING 'ZTPP_248A' 'V'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE 'X'.
      WHEN p1.
        CASE screen-group1.
          WHEN 'S1'.
            screen-active = 0.
        ENDCASE.
      WHEN p2.
        CASE screen-group1.
          WHEN 'S2'.
            screen-active = 0.
        ENDCASE.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

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
  "获取某月天数
  DATA selectdate TYPE sy-datum.
  selectdate = p_month && '01'.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = selectdate
    IMPORTING
      last_day_of_month = lastdate
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF lastdate IS NOT INITIAL.
    lastday = lastdate+6(2).
  ENDIF.
  CASE 'X'.
    WHEN p1.
      p_zdep = '钢品一部'.
      MOVE-CORRESPONDING s_arbpl[] TO r_zname[].
    WHEN OTHERS.
      p_zdep = '钢品二部'.
      MOVE-CORRESPONDING s_zzl1[] TO r_zname[].
  ENDCASE.

  REFRESH r_zzl1.
  LOOP AT gt_pmdl INTO gv_pmdl.
    r_zzl1-sign = 'I'.
    r_zzl1-option = 'EQ'.
    r_zzl1-low = gv_pmdl-zzl1.
    COLLECT r_zzl1.
  ENDLOOP.

  "获取动态alv内表
  PERFORM get_dynamic_table.
  "获取动态temp内表
  PERFORM get_dynamic_table_temp.
  "查询月度库存表
  SELECT
    *
  FROM ztpp_248a
  WHERE zdep   = @p_zdep
    AND zmonth = @p_month
    AND zname  IN @r_zname
    INTO TABLE @gt_248a.
  IF gt_248a IS INITIAL.
    MESSAGE s000(oo) WITH '没有查询到当月库存，请点击月度库存配置' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT
      'I' AS sign,
      'EQ' AS option,
      zname AS low
  FROM @gt_248a AS z248a
  INTO CORRESPONDING FIELDS OF TABLE @r_zname.

  "获取出库单信息
  REFRESH: gt_ck,gt_zckl,gt_zckl_hz.
  PERFORM getckxx USING selectdate lastdate.
  SORT gt_ck BY zname date.
  "获取入库信息
  REFRESH gt_rk.
  PERFORM getrkxx USING selectdate lastdate.
  "获取领料单信息
  PERFORM getllxx USING selectdate lastdate.
  "获取调拨单信息
  PERFORM getdbxx USING selectdate lastdate.
  LOOP AT gt_zckl.
    MOVE-CORRESPONDING gt_zckl TO gt_zckl_hz.
    COLLECT gt_zckl_hz.
  ENDLOOP.
  SORT gt_zckl_hz BY zname date.
  "获取超龄库存
  REFRESH:gt_clkc,gt_cpkc.
  PERFORM getklxx.
  SORT gt_clkc BY zname.

  DATA:fieldname  TYPE char7,
       fieldname1 TYPE char7.
  MOVE-CORRESPONDING gt_248a TO <f_temp>.
  LOOP AT <f_temp> ASSIGNING <f_tline>.
    CLEAR: index,tabix,dat.
    ASSIGN COMPONENT 'ZNAME' OF STRUCTURE <f_tline> TO FIELD-SYMBOL(<f_zname>).
    DO lastday TIMES.
      index = sy-index.
      dat = index.
      dat = |{ dat ALPHA = IN }|.
      LOOP AT gt_zlb.
        tabix = sy-tabix.
        fieldname = gt_zlb-field && index.
        ASSIGN COMPONENT fieldname OF STRUCTURE <f_tline> TO FIELD-SYMBOL(<f_value>).
        CLEAR <f_value>.
        CASE tabix.
          WHEN 1.
            "1号库存量赋值
            IF index = 1.
              READ TABLE gt_248a INTO gv_248a WITH KEY zname = <f_zname>.
              IF sy-subrc = 0.
                <f_value> = gv_248a-clabs.
              ENDIF.
            ELSE.
              <f_value> = lv_data-kucun + lv_data-ruku - lv_data-chuku.
            ENDIF.
            lv_data-kucun = <f_value>.
          WHEN 2.
            READ TABLE gt_rk WITH KEY zname = <f_zname> date+6(2) = dat BINARY SEARCH.
            IF sy-subrc = 0.
              <f_value> = gt_rk-rkl.
            ENDIF.
            lv_data-ruku = <f_value>.
          WHEN 3.
            READ TABLE gt_zckl_hz WITH KEY zname = <f_zname> date+6(2) = dat BINARY SEARCH.
            IF sy-subrc = 0.
              <f_value> = gt_zckl_hz-ckl.
            ENDIF.
            lv_data-chuku = <f_value>.
        ENDCASE.
      ENDLOOP.
    ENDDO.
  ENDLOOP.

  "赋值alv 动态内表
  LOOP AT <f_temp> ASSIGNING <f_tline>.
    CLEAR: index,tabix.
    ASSIGN COMPONENT 'ZNAME' OF STRUCTURE <f_tline> TO <f_zname>.
    LOOP AT gt_zlb. "类别
      tabix = sy-tabix.
      ASSIGN COMPONENT 'ZNAME' OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_zname1>).
      <f_zname1> = <f_zname>.
      ASSIGN COMPONENT 'ZLB' OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_zlb>).
      <f_zlb> = gt_zlb-text.
      DO lastday TIMES.
        index = sy-index.
        fieldname = gt_zlb-field && index.
        fieldname1 = 'CLABS' && index.
        ASSIGN COMPONENT fieldname OF STRUCTURE <f_tline> TO <f_value>.
        ASSIGN COMPONENT fieldname1 OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_value1>).
        <f_value1> = <f_value>.
      ENDDO.
      APPEND <f_line> TO <f_tab>.
    ENDLOOP.
    CLEAR <f_line>.
    <f_zname1> = <f_zname>.
    <f_zlb> = '超龄'.
    ASSIGN COMPONENT 'CLABS1' OF STRUCTURE <f_line> TO <f_value1>.
    READ TABLE gt_clkc WITH KEY zname = <f_zname> BINARY SEARCH.
    IF sy-subrc = 0.
      <f_value1> = gt_clkc-clabs.
    ENDIF.
    APPEND <f_line> TO <f_tab>.
  ENDLOOP.
  FREE <f_temp>.

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
'ZNAME '  '品名/工作中心'  'ZTPP_248A'  'ZNAME',
'ZLB '    '类别'          '         '  '     '.
  DO lastday TIMES.
    DATA(fld) = 'CLABS' && sy-index.
    DATA(name) = sy-index && '日'.
    PERFORM catlg_set TABLES fldct
                  USING:
      fld  name  'ZTPP_248A'  'CLABS'.
  ENDDO.

  i_title               = lines( <f_tab> ) .
  CONDENSE i_title.
  CONCATENATE '月份:' p_month '    条目数:' i_title INTO i_title.

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
  DATA:fieldname TYPE slis_fieldname,
       date      TYPE sy-datum.
  DATA:fieldcat TYPE slis_t_fieldcat_alv.
  DATA:gt_dtab LIKE gt_zckl[],
       gv_dtab LIKE LINE OF gt_dtab.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CHECK rs_selfield-tabindex <> 0 . "小计行总计行什么的忽略
      READ TABLE <f_tab> INTO <f_line> INDEX rs_selfield-tabindex.
      ASSIGN COMPONENT 'ZLB' OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_ZLB>).
      ASSIGN COMPONENT 'ZNAME' OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_ZNAME>).
      CHECK <f_ZLB> IS NOT INITIAL AND <f_ZLB> = '出库'.
      CHECK rs_selfield-fieldname CS 'CLABS'.
      CLEAR: dat,fieldname.
      fieldname = rs_selfield-fieldname.
      SHIFT fieldname LEFT DELETING LEADING 'CLABS'.
      dat = fieldname.
      dat = |{ dat ALPHA = IN }|.
      date = p_month && dat.
      LOOP AT gt_zckl WHERE zname = <f_ZNAME> AND date = date.
        MOVE-CORRESPONDING gt_zckl TO gv_dtab.
        APPEND gv_dtab TO gt_dtab.
      ENDLOOP.
      IF gt_dtab[] IS INITIAL.
        MESSAGE s059 WITH '没有数据' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      PERFORM catlg_set TABLES fieldcat
                  USING:
      'TYPE ' '单据类型      '  ''  '',
      'CKL  ' '数量     '  ''  ''.

      slayt-colwidth_optimize = 'X'. "  colwidth_optimize
      slayt-zebra             = 'X'.
      slayt-box_fieldname     = ''.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program    = sy-repid
          it_fieldcat           = fieldcat[]
          i_save                = 'A' "控制缺省/特定用户
          is_layout             = slayt
*         IS_VARIANT            = IS_VARIANT
          i_screen_start_column = 30
          i_screen_end_column   = 170
          i_screen_start_line   = 10
          i_screen_end_line     = 20
        TABLES
          t_outtab              = gt_dtab[]
        EXCEPTIONS
          program_error         = 1
          OTHERS                = 2.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_dynamic_table
*&---------------------------------------------------------------------*
*& 获取动态内表<f_tab>
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_dynamic_table.
  CLEAR:lo_struct,lt_comp,lt_tot_comp,lo_new_type,lo_new_tab.
* 1. 从现有类型获取组件
  lo_struct ?= cl_abap_typedescr=>describe_by_name( 'TY_ITAB' ).
  lt_comp  = lo_struct->get_components( ).
  APPEND LINES OF lt_comp TO lt_tot_comp.
  DO lastday TIMES.
    la_comp-name = 'CLABS' && sy-index.
    lo_element ?= cl_abap_elemdescr=>describe_by_name( 'LABST' ).
    la_comp-type = cl_abap_elemdescr=>get_p(
      p_length   = lo_element->length
      p_decimals = lo_element->decimals ).
*   填写组件表
    APPEND la_comp TO lt_tot_comp.
    CLEAR: la_comp.
  ENDDO.
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
*& Form get_dynamic_table
*&---------------------------------------------------------------------*
*& 获取动态内表<f_temp>
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_dynamic_table_temp.
  CLEAR:lo_struct,lt_comp,lt_tot_comp,lo_new_type,lo_new_tab.
* 1. 从现有类型获取组件
  lo_struct ?= cl_abap_typedescr=>describe_by_name( 'TY_TEMP' ).
  lt_comp  = lo_struct->get_components( ).
  APPEND LINES OF lt_comp TO lt_tot_comp.
  CLEAR index.
  DO lastday TIMES.
    index = sy-index.
    LOOP AT gt_zlb.
      la_comp-name = gt_zlb-field && index.
      lo_element ?= cl_abap_elemdescr=>describe_by_name( 'LABST' ).
      la_comp-type = cl_abap_elemdescr=>get_p(
        p_length   = lo_element->length
        p_decimals = lo_element->decimals ).
*   填写组件表
      APPEND la_comp TO lt_tot_comp.
      CLEAR: la_comp.
    ENDLOOP.
  ENDDO.

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
  ASSIGN lo_data->* TO <f_temp>.
* 7. 创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <f_temp>.
* 8. 创建动态工作区
  ASSIGN dyn_wa->* TO <f_tline>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form getckxx
*&---------------------------------------------------------------------*
*& 获取出库量
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getckxx USING s_date TYPE sy-datum
                   e_date TYPE sy-datum.
  DATA:BEGIN OF gt_210 OCCURS 0, "出库信息
         plnum     TYPE ztpp_206-plnum,
         zzl1      TYPE mara-zzl1,
         lfimg     TYPE lips-lfimg,
         wadat_ist TYPE likp-wadat_ist,
       END OF gt_210.
  DATA: s_vkorg     TYPE RANGE OF vbak-vkorg,
        s_vtweg     TYPE RANGE OF  vbak-vtweg,
        s_wadat_ist TYPE RANGE OF likp-wadat_ist.
  DATA ls_data TYPE REF TO data.
  TYPES:BEGIN OF ty_plnum,
          plnum TYPE ztpp_206-plnum,
        END OF ty_plnum.
  DATA:lt_plnum TYPE TABLE OF ty_plnum WITH KEY plnum,
       lv_plnum LIKE LINE OF lt_plnum.

  s_vkorg = VALUE #( sign = 'I' option = 'EQ' ( low = p_werks ) ).
  s_vtweg = VALUE #( sign = 'I' option = 'EQ' ( low = '10' ) ).
  s_wadat_ist = VALUE #( sign = 'I' option = 'BT' ( low = s_date high = e_date ) ).
  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_false data = abap_true ).
  SUBMIT zrsd210
        WITH s_vkorg IN s_vkorg
        WITH s_vtweg IN s_vtweg
        WITH s_wadat IN s_wadat_ist
        WITH p1 EQ ''
    AND RETURN.
  CLEAR:ls_data.
  TRY .
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data = ls_data ).
    CATCH  cx_salv_bs_sc_runtime_info.
      MESSAGE s000(oo) WITH '无法获取报表的数据' DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.
  ASSIGN ls_data->* TO FIELD-SYMBOL(<fs_tab>).
  cl_salv_bs_runtime_info=>clear_all( ).

  IF <fs_tab> IS ASSIGNED.
    MOVE-CORRESPONDING <fs_tab> TO gt_210[].
    UNASSIGN <fs_tab>.
  ENDIF.
  CASE 'X'.
    WHEN p1. "钢品一部
      LOOP AT gt_210.
        IF gt_210-plnum IS NOT INITIAL.
          lv_plnum-plnum = gt_210-plnum.
          COLLECT lv_plnum INTO lt_plnum.
        ENDIF.
      ENDLOOP.
      SELECT
        z206~plnum,
        z206~arbpl
      FROM ztpp_206 AS z206
      INNER JOIN @lt_plnum AS pl ON z206~plnum = pl~plnum
      WHERE z206~arbpl IN @r_zname
      GROUP BY z206~plnum,z206~arbpl
      ORDER BY z206~plnum
      INTO TABLE @DATA(lt_206).
      DELETE gt_210 WHERE zzl1 = '丽彩钻母板'.
      LOOP AT gt_210.
        READ TABLE lt_206 INTO DATA(wa_206) WITH KEY plnum = gt_210-plnum BINARY SEARCH.
        IF sy-subrc = 0.
          gt_ck-zname = wa_206-arbpl.
          gt_ck-date = gt_210-wadat_ist.
          gt_ck-ckl = gt_210-lfimg.
          COLLECT gt_ck.
        ENDIF.
      ENDLOOP.
    WHEN p2. "钢品二部
      DELETE gt_210 WHERE zzl1 NOT IN r_zzl1.
      LOOP AT gt_210.
        READ TABLE gt_pmdl INTO gv_pmdl WITH KEY zzl1 = gt_210-zzl1.
        IF sy-subrc = 0.
          gt_ck-zname = gv_pmdl-type.
          gt_ck-date = gt_210-wadat_ist.
          gt_ck-ckl = gt_210-lfimg.
          COLLECT gt_ck.
        ENDIF.
      ENDLOOP.
  ENDCASE.
  LOOP AT gt_ck.
    MOVE-CORRESPONDING gt_ck TO gt_zckl.
    gt_zckl-type = '出库单'.
    APPEND gt_zckl.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getrkxx
*&---------------------------------------------------------------------*
*& 获取入库量
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getrkxx USING s_date TYPE sy-datum
                   e_date TYPE sy-datum.
  DATA: s_wadat_ist TYPE RANGE OF likp-wadat_ist.

  s_wadat_ist = VALUE #( sign = 'I' option = 'BT' ( low = s_date high = e_date ) ).
  CASE 'X'.
    WHEN p1.
      SELECT
        arbpl,
        aufnr
      FROM ztpp_206
      WHERE arbpl IN @r_zname
        AND aufnr IS NOT INITIAL
        AND werks = @p_werks
      GROUP BY arbpl,aufnr
      INTO TABLE @DATA(lt_206_aufnr).

      SELECT
         z206~arbpl AS zname,
         budat AS date,
         SUM( menge ) AS rkl
      FROM aufm
      INNER JOIN @lt_206_aufnr AS z206 ON z206~aufnr = aufm~aufnr
      INNER JOIN mara AS ma ON ma~matnr = aufm~matnr
      WHERE bwart = '101'
        AND werks = @p_werks
        AND ma~zzl1 NE '丽彩钻母板'
        AND budat IN @s_wadat_ist
        GROUP BY z206~arbpl,budat
        ORDER BY z206~arbpl,budat
        INTO TABLE @gt_rk.
    WHEN p2.
      SELECT
         pm~type AS zname,
         aufm~budat AS date,
         SUM( aufm~menge ) AS rkl
      FROM aufm
      INNER JOIN mara AS ra ON aufm~matnr = ra~matnr
      INNER JOIN @gt_pmdl AS pm ON pm~zzl1 = ra~zzl1
      WHERE aufm~bwart = '101'
        AND werks = @p_werks
        AND aufm~budat IN @s_wadat_ist
        AND pm~type IN @r_zname
        GROUP BY pm~type,budat
        ORDER BY pm~type,budat
        INTO TABLE @gt_rk.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form getllxx
*&---------------------------------------------------------------------*
*& 获取领料量
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getllxx USING s_date TYPE sy-datum
                   e_date TYPE sy-datum.
  DATA: s_wadat_ist TYPE RANGE OF likp-wadat_ist.

  s_wadat_ist = VALUE #( sign = 'I' option = 'BT' ( low = s_date high = e_date ) ).

  CASE 'X'.
    WHEN p1.
*      SELECT
*        z206~arbpl AS zname,
*        z230~budat AS date,
*        SUM( CASE WHEN z230~bwart = 'Z61' THEN z230~zlyl ELSE z230~zlyl * -1 END ) AS ckl
*      FROM ztmm230 AS z230
*      INNER JOIN mara AS ma ON z230~matnr = ma~matnr
*      INNER JOIN ztpp_206 AS z206 ON z230~aufnr = z206~aufnr
*      WHERE z206~arbpl IN @r_zname
*        AND z206~aufnr IS NOT INITIAL
*        AND z230~budat IN @s_wadat_ist
*        AND ma~zzl1 NE '丽彩钻母板'
*        AND z230~werks = @p_werks
*        AND z230~bwart IN ( 'Z61','Z62' )
*      GROUP BY z206~arbpl,z230~budat
*      ORDER BY z206~arbpl,z230~budat
*      INTO TABLE @gt_ck.

    WHEN p2.
      SELECT
        pm~type AS zname,
        z230~budat AS date,
        SUM( CASE WHEN z230~bwart = 'Z61' THEN z230~zlyl ELSE z230~zlyl * -1 END ) AS ckl
      FROM ztmm230 AS z230
      INNER JOIN mara AS ma ON z230~matnr = ma~matnr
      INNER JOIN @gt_pmdl AS pm ON pm~zzl1 = ma~zzl1
      WHERE z230~budat IN @s_wadat_ist
        AND pm~type IN @r_zname
        AND z230~werks = @p_werks
        AND z230~bwart IN ( 'Z61','Z62' )
      GROUP BY pm~type,z230~budat
      ORDER BY pm~type,z230~budat
      INTO TABLE @gt_ck.
  ENDCASE.
  LOOP AT gt_ck.
    MOVE-CORRESPONDING gt_ck TO gt_zckl.
    gt_zckl-type = '领料单'.
    APPEND gt_zckl.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getdbxx
*&---------------------------------------------------------------------*
*& 获取调拨量
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdbxx USING s_date TYPE sy-datum
                   e_date TYPE sy-datum.
  DATA: s_wadat_ist TYPE RANGE OF likp-wadat_ist.

  s_wadat_ist = VALUE #( sign = 'I' option = 'BT' ( low = s_date high = e_date ) ).

  CASE 'X'.
    WHEN p1.
*      SELECT
*        z206~arbpl AS zname,
*        mg~budat_mkpf AS date,
*        SUM( z221~zdbsl ) AS ckl
*      FROM ztmm220 AS z220
*      INNER JOIN ztmm221  AS z221 ON  z220~zdbdh = z220~zdbdh
*      INNER JOIN mseg     AS mg   ON  mg~mblnr = z220~mblnr
*                                   AND mg~mjahr = z220~mjahr
*      INNER JOIN aufm     AS au   ON  au~matnr = z221~matnr
*                                  AND au~charg = z221~charg
*                                  AND au~bwart = '101'
*      INNER JOIN ztpp_206 AS z206 ON z206~aufnr = au~aufnr
*      WHERE z220~del = ''
*        AND z206~arbpl IN @r_zname
*        AND z220~werks = @p_werks
*        AND z220~zdckcd IN ( 'C001','C002','C004' )
*        AND mg~budat_mkpf IN @s_wadat_ist
*        AND z221~matnr IS NOT INITIAL
*        AND z221~charg IS NOT INITIAL
*      GROUP BY z206~arbpl,mg~budat_mkpf
*      ORDER BY z206~arbpl,mg~budat_mkpf
*      INTO TABLE @gt_ck.
    WHEN p2.
      SELECT
        pm~type AS zname,
        z220~zdbrq  AS date,
        SUM( z221~zdbsl ) AS ckl
      FROM ztmm220 AS z220
      INNER JOIN ztmm221  AS z221 ON  z221~zdbdh = z220~zdbdh
      INNER JOIN mara     AS ma   ON  ma~matnr   = z221~matnr
      INNER JOIN @gt_pmdl AS pm   ON  pm~zzl1     = ma~zzl1
      WHERE z220~del = ''
        AND z220~werks = @p_werks
        AND z220~zdckcd IN ( 'C001','C002','C004' )
        AND z220~zdbrq IN @s_wadat_ist
        AND z221~matnr IS NOT INITIAL
        AND pm~type IN @r_zname
      GROUP BY pm~type,z220~zdbrq
      ORDER BY pm~type,z220~zdbrq
      INTO TABLE @gt_ck.
  ENDCASE.
  LOOP AT gt_ck.
    MOVE-CORRESPONDING gt_ck TO gt_zckl.
    gt_zckl-type = '调拨单'.
    APPEND gt_zckl.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getclxx
*&---------------------------------------------------------------------*
*& 获取库龄
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getklxx.
  DATA ls_data TYPE REF TO data.

  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_false data = abap_true ).
  SUBMIT zppr248
        WITH p_werks EQ p_werks
    AND RETURN.
  CLEAR:ls_data.
  TRY .
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data = ls_data ).
    CATCH  cx_salv_bs_sc_runtime_info.
      MESSAGE s000(oo) WITH '无法获取报表的数据' DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.
  ASSIGN ls_data->* TO FIELD-SYMBOL(<fs_tab>).
  cl_salv_bs_runtime_info=>clear_all( ).

  IF <fs_tab> IS ASSIGNED.
    MOVE-CORRESPONDING <fs_tab> TO gt_cpkc[].
    UNASSIGN <fs_tab>.
  ENDIF.
  REFRESH gt_clkc.
  CASE 'X'.
    WHEN p1.
      LOOP AT gt_cpkc WHERE arbpl IS INITIAL.
        IF gt_cpkc-zzl1 = '丽彩鼎'.
          IF gt_cpkc-post1 = '包电梯项目' OR gt_cpkc-post1 = '威海优康利莱产业园项目'.
            gt_cpkc-arbpl = '3000ALC1'.
          ENDIF.
          IF gt_cpkc-post1 = '包苏宁重庆智慧物流项目'.
            gt_cpkc-arbpl = '3000ALC2'.
          ENDIF.
          MODIFY gt_cpkc TRANSPORTING arbpl.
        ENDIF.
      ENDLOOP.
      DELETE gt_cpkc WHERE arbpl IS INITIAL OR zklts < 90 OR arbpl NOT IN r_zname.
      