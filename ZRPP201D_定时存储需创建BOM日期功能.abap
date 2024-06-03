*&---------------------------------------------------------------------*
*& Report ZRPP201D
*&---------------------------------------------------------------------*
*& 定时存储需创建BOM日期功能
*&---------------------------------------------------------------------*
REPORT zrpp201d MESSAGE-ID zgp_msg.

TABLES:sscrfields,t001l,mara,ztpp_216a.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_out,
        werks TYPE ztpp_216a-werks,
        matnr TYPE ztpp_216a-matnr,
        zxfrq TYPE ztpp_216a-zxfrq,
        zxfsj TYPE ztpp_216a-zxfsj,
        zxfr  TYPE ztpp_216a-zxfr,
      END OF ty_out.
DATA:it_upload TYPE TABLE OF ty_out WITH HEADER LINE.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE TYPE ty_out.
DATA:  vbeln TYPE vbeln,
       posnr TYPE posnr,
       matkl TYPE mara-matkl,
       maktx TYPE makt-maktx,
       zzl1  TYPE mara-zzl1,
       zisxs TYPE ztpp_216a-zisxs,
       mes   TYPE char20,
       sel,
     END OF itab.
DATA:BEGIN OF gt_wermat OCCURS 0,
       werks TYPE ztpp_216a-werks,
       matnr TYPE ztpp_216a-matnr,
     END OF gt_wermat.
DATA: return TYPE TABLE OF bapiret2 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS:p_werks TYPE t001l-werks OBLIGATORY DEFAULT '3000'.
  SELECT-OPTIONS:s_matkl FOR mara-matkl      MODIF ID m1,
                 s_zzl1 FOR mara-zzl1        MODIF ID m1,
                 s_matnr FOR mara-matnr      MODIF ID m1,
                 s_zxfrq FOR ztpp_216a-zxfrq MODIF ID m1.
  PARAMETERS:p_sel AS CHECKBOX USER-COMMAND singleclick.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN FUNCTION KEY:1.

INITIALIZATION.
  t1 = '功能选择'.
  %_p_werks_%_app_%-text = '工厂'.
  %_s_zxfrq_%_app_%-text = '下发日期'.
  %_s_matkl_%_app_%-text = '成品物料组'.
  %_s_zzl1_%_app_%-text = '品名'.
  %_s_matnr_%_app_%-text = '成品编码'.
  %_p_sel_%_app_%-text = '剪切板导入（勿复制标题）'.
  sscrfields-functxt_01 = '@14@无合同导出模板'.

  PERFORM setfldct.



AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      REFRESH fldct.
      PERFORM setfldct.
      PERFORM itabstructoclip(zpubform) USING fldct '' ''.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  CASE 'X'.
    WHEN p_sel.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'M1'.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.

START-OF-SELECTION.

  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  IF sy-batch = ''.
    PERFORM getdata.
  ELSE.
    PERFORM frm_get_data.
    PERFORM autosave.
  ENDIF.
  PERFORM updatelog(zreplog) IF FOUND.
  IF sy-batch = ''.
    PERFORM alvshow.
  ELSE.

  ENDIF.
*&---------------------------------------------------------------------*
*& Form getdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdata .
  IF p_sel = 'X'.
    PERFORM cliptoitab(zpubform) TABLES it_upload.
    IF it_upload[] IS INITIAL.
      MESSAGE s028 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    DATA tabix TYPE i.
    REFRESH: return,gt_wermat.
    LOOP AT it_upload.
      tabix = sy-tabix.
      PERFORM addzero_matnr(zpubform) CHANGING it_upload-matnr.
      DO 5 TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE it_upload TO FIELD-SYMBOL(<fs>).
        IF sy-subrc = 0 AND ( <fs> IS INITIAL OR <fs> = '' ).
          READ TABLE fldct INTO DATA(wa_fldct) INDEX sy-index.
          PERFORM inmsg(zpubform) TABLES return USING 'ZGP_MSG' 'E' '043'
                       tabix wa_fldct-seltext_l '' ''.
          EXIT.
        ENDIF.
      ENDDO.
      "判断工厂和物料号是否有重复行
      READ TABLE gt_wermat WITH KEY werks = it_upload-werks matnr = it_upload-matnr.
      IF sy-subrc = 0.
        DATA(msg) = |工厂：{ it_upload-werks }，物料号：{ it_upload-matnr }，有重复行|.
        PERFORM inmsg(zpubform) TABLES return USING 'ZGP_MSG' 'E' '004'
             msg '' '' ''.
      ELSE.
        gt_wermat-werks = it_upload-werks.
        gt_wermat-matnr = it_upload-matnr.
        COLLECT gt_wermat.
      ENDIF.
      MODIFY it_upload.
    ENDLOOP.

    IF return[] IS NOT INITIAL.
      PERFORM showmsg(zpubform) TABLES return.
      STOP.
    ENDIF.
    DATA(it_upload1) = it_upload[].
    SELECT
        up~matnr,
        mara~matkl,
        makt~maktx,
        mara~zzl1
    FROM @it_upload1 AS up
    INNER JOIN mara ON mara~matnr = up~matnr
    INNER JOIN makt ON makt~matnr = up~matnr
    INTO TABLE @DATA(lt_mara).
    REFRESH itab.
    LOOP AT it_upload.
      MOVE-CORRESPONDING it_upload TO itab.
      READ TABLE lt_mara INTO DATA(lv_mara) WITH KEY matnr = itab-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        itab-maktx = lv_mara-maktx.
        itab-matkl = lv_mara-matkl.
        itab-zzl1 = lv_mara-zzl1.
      ENDIF.
      APPEND itab.
    ENDLOOP.
  ELSE.
    SELECT
        ztpp_216a~werks,
        ztpp_216a~vbeln,
        ztpp_216a~posnr,
        mara~matkl,
        ztpp_216a~matnr,
        makt~maktx,
        mara~zzl1,
        ztpp_216a~zxfrq,
        ztpp_216a~zxfsj,
        ztpp_216a~zxfr,
        ztpp_216a~zisxs
    FROM ztpp_216a
    INNER JOIN mara ON mara~matnr = ztpp_216a~matnr
    LEFT JOIN makt  ON makt~matnr = ztpp_216a~matnr
    WHERE ztpp_216a~werks = @p_werks
      AND ztpp_216a~matnr IN @s_matnr
      AND mara~matkl IN @s_matkl
      AND mara~zzl1 IN @s_zzl1
      AND ztpp_216a~zxfrq IN @s_zxfrq
    INTO CORRESPONDING FIELDS OF TABLE @itab.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .
  AUTHORITY-CHECK OBJECT 'ZWSD06'
  ID 'ZWMKZ' FIELD '01'.
  IF sy-subrc <> 0.
    DATA(clear) = 'X'.
  ENDIF.

*（销售订单创建日期VBAK-ERDAT-当前日期大于100天的排除）
  DATA lv_gt100date TYPE sy-datum.

  lv_gt100date = sy-datum - 180.

*根据取到的物料号到MARA-MATNR取，产品组MARA-SPART,
*是否等于=“A0、B0、D0”若不等于则排除
  RANGES ra_spart FOR mara-spart.
  RANGES ra_mvgr2 FOR vbap-mvgr2.
  RANGES ra_mvgr3 FOR vbap-mvgr3.

  ra_spart[] = VALUE #( sign = 'I' option = 'EQ'
                         ( low = 'A0' high = '')
                         ( low = 'B0' high = '')
                         ( low = 'D0' high = '')
                         ).
*待创建报表增加判断逻辑只取自产的   VBAP-MVGR2  不等于Z02   Z03   Z04的类型
  ra_mvgr2[] = VALUE #( sign = 'E' option = 'EQ'
                       ( low = 'Z02' high = '')
                       ( low = 'Z03' high = '')
                       ( low = 'Z04' high = '')
                       ).

*需要在待创建报表加个判断工厂的逻辑，VBAP--MVGR3
*Z01-3000工厂、Z02-3002工厂、Z04-3008工厂、
*Z15-3060工厂、Z16-3062工厂、Z22-3090工厂
  CASE p_werks.
    WHEN '3000'.
      ra_mvgr3-low = 'Z01'.

    WHEN '3002'.
      ra_mvgr3-low = 'Z02'.

    WHEN '3008'.
      ra_mvgr3-low = 'Z04'.

    WHEN '3060'.
      ra_mvgr3-low = 'Z15'.

    WHEN '3062'.
      ra_mvgr3-low = 'Z16'.

    WHEN '3090'.
      ra_mvgr3-low = 'Z22'.

  ENDCASE.

  IF ra_mvgr3-low NE ''.
    ra_mvgr3-sign = 'I'.
    ra_mvgr3-option = 'EQ'.

    APPEND ra_mvgr3.
    CLEAR ra_mvgr3.

  ENDIF.

  SELECT
    vbap~vbeln,
    vbap~posnr,
    vbap~werks,
    vbap~matnr,
    mara~zzl1,
    vbak~vdatu,
    vbak~erdat,
    vbak~erzet,
    vbak~zywy,
    mara~matkl,
    makt~maktx,
    kna1~name1 AS zxfr,
    vbak~zhtly
    INTO TABLE @DATA(lt_vbap)
    FROM vbap
    INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
    INNER JOIN mara ON mara~matnr = vbap~matnr
    LEFT JOIN makt  ON makt~matnr = vbap~matnr AND makt~spras = 1
    LEFT JOIN kna1  ON kna1~kunnr = vbak~zhtly
    WHERE (   ( vbap~werks = @p_werks AND vbap~mvgr3 = '' )
           OR ( vbap~werks = @p_werks AND vbap~mvgr3 IN @ra_mvgr3 AND vbap~mvgr3 NE '' )
           )
      AND vbap~matnr IN @s_matnr
      AND vbap~mvgr2 IN @ra_mvgr2
      AND vbap~abgru EQ '' "拒绝原因
      AND vbak~vbtyp EQ 'G' "合同
      AND vbak~gbstk NE 'C' "未完成
*      AND vbak~erdat GT @lv_gt100date
      AND mara~matkl IN @s_matkl
      AND mara~matnr IN @s_matnr
      AND mara~zzl1 IN @s_zzl1
      AND mara~spart IN @ra_spart

      AND NOT EXISTS (
          SELECT
            'X'
            FROM ztpp_216a
            WHERE ztpp_216a~werks = vbap~werks
              AND ztpp_216a~vbeln = vbap~vbeln
              AND ztpp_216a~posnr = vbap~posnr
              AND ztpp_216a~matnr = vbap~matnr
          )
    .

  REFRESH itab.
  LOOP AT lt_vbap INTO DATA(ls_vbap).
    itab-werks = p_werks.
    itab-vbeln = ls_vbap-vbeln.
    itab-posnr = ls_vbap-posnr.
    itab-matkl  = ls_vbap-matkl.
    itab-matnr  = ls_vbap-matnr.
    itab-maktx  = ls_vbap-maktx.
    itab-zzl1  = ls_vbap-zzl1.
    itab-zxfrq  = sy-datlo.
    itab-zxfsj  = sy-timlo.
    itab-zxfr = ls_vbap-zxfr.
    APPEND itab.
    CLEAR itab.
  ENDLOOP.
  DELETE itab WHERE ( werks = '3000' OR werks = '3060' ) AND matkl = 'A0300'.
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
  REFRESH fldct.
  IF p_sel = 'X'.
    PERFORM catlg_set TABLES fldct
                  USING:
   'MES '  '消息    '  ''  'CHAR20'.
  ENDIF.
  PERFORM catlg_set TABLES fldct
                    USING:
   'WERKS '  '工厂    '  'ZTPP_216A'  'WERKS',
   'VBELN '  '合同编号'  'ZTPP_216A'  'VBELN',
   'POSNR '  '合同行号'  'ZTPP_216A'  'POSNR',
   'MATKL '  '物料组  '  'MARA     '  'MATKL',
   'MATNR '  '物料编码'  'ZTPP_216A'  'MATNR',
   'MAKTX '  '物料描述'  'MAKT     '  'MAKTX',
   'ZZL1  '  '品名    '  'MARA     '  'ZZL1 ',
   'ZXFRQ '  '下发日期'  'ZTPP_216A'  'ZXFRQ',
   'ZXFSJ '  '下发时间'  'ZTPP_216A'  'ZXFSJ',
   'ZXFR  '  '下发人  '  'ZTPP_216A'  'ZXFR ',
   'ZISXS '  '是否显示'  'ZTPP_216A'  'ZISXS'.

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
      i_grid_title             = i_title
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
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
    WHEN 'ZXFRQ' OR 'ZXFSJ'.
      IF p_sel = 'X'.
        ls_fldct-edit = 'X'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  APPEND ls_fldct TO fldcattab .
  CLEAR ls_fldct .
ENDFORM.

FORM set_status USING rt_extab TYPE slis_t_extab.
  CLEAR rt_extab.
  REFRESH rt_extab.
  IF p_sel = ''.
    APPEND 'BUT01' TO rt_extab.
  ENDIF.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab .

ENDFORM.

FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  DATA: lr_grid   TYPE REF TO cl_gui_alv_grid,
        ls_layout TYPE lvc_s_layo.
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
    WHEN 'BUT01'. "无合同剪切板导入保存
      IF NOT line_exists( itab[ sel = 'X' mes = '' ] ).
        MESSAGE s024 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM but01.
  ENDCASE.
  "修改后alv宽度自适应
  CALL METHOD lr_grid->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.
  ls_layout-cwidth_opt = 'X'.    "最优宽
  CALL METHOD lr_grid->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.
  CALL METHOD lr_grid->check_changed_data.

  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.
FORM setfldct.
  REFRESH fldct.
  PERFORM catlg_set TABLES fldct USING :
    'WERKS' '工厂'    'ZTPP_216A'  'WERKS',
    'MATNR' '物料编码' 'ZTPP_216A' 'MATNR',
    'ZXFRQ' '下发日期' 'ZTPP_216A' 'ZXFRQ',
    'ZXFSJ' '下发时间' 'ZTPP_216A' 'ZXFSJ',
    'ZXFR ' '下发人'   'ZTPP_216A' 'ZXFR '.
ENDFORM.
FORM but01.
  DATA:lt_216a TYPE TABLE OF ztpp_216a WITH HEADER LINE.
  LOOP AT itab WHERE sel = 'X' AND ( zxfrq IS INITIAL OR zxfsj IS INITIAL ).
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE s004 WITH '下发日期和下发时间不能为空' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  LOOP AT itab WHERE sel = 'X'.
    CLEAR lt_216a.
    lt_216a-werks = itab-werks.
    lt_216a-matnr = itab-matnr.
    lt_216a-zxfrq = itab-zxfrq.
    lt_216a-zxfsj = itab-zxfsj.
    lt_216a-zxfr = itab-zxfr.
    APPEND lt_216a.
  ENDLOOP.
  IF lt_216a[] IS NOT INITIAL.
    SELECT
        *
    FROM ztpp_216a
    FOR ALL ENTRIES IN @lt_216a
    WHERE vbeln IS INITIAL
      AND posnr IS INITIAL
      AND werks = @lt_216a-werks
      AND matnr = @lt_216a-matnr
      ORDER BY PRIMARY KEY
    INTO TABLE @DATA(lt_216a_old).
  ENDIF.
  LOOP AT lt_216a.
    READ TABLE lt_216a_old INTO DATA(lv_216a_old) WITH KEY werks = lt_216a-werks matnr = lt_216a-matnr.
    IF sy-subrc = 0.
      READ TABLE itab WITH KEY sel = 'X' werks = lt_216a-werks matnr = lt_216a-matnr.
      IF sy-subrc = 0.
        itab-mes = '表ZTPP_216A已存在'.
        MODIFY itab INDEX sy-tabix TRANSPORTING mes.
      ENDIF.
      DELETE lt_216a.
    ELSE.
      PERFORM getnum CHANGING lt_216a-znum.
      MODIFY lt_216a.
    ENDIF.
  ENDLOOP.
  INSERT ztpp_216a FROM TABLE lt_216a.
  IF sy-subrc = 0.
    COMMIT WORK.
    LOOP AT itab WHERE sel = 'X' AND mes = ''.
      itab-mes = '保存成功'.
      MODIFY itab TRANSPORTING mes.
    ENDLOOP.
  ELSE.
    LOOP AT itab WHERE sel = 'X' AND mes = ''.
      itab-mes = '保存失败'.
      MODIFY itab TRANSPORTING mes.
    ENDLOOP.
  ENDIF.
ENDFORM.
"后台运行时自动保存到ztpp_216a表
FORM autosave.
  DATA:lt_216a     TYPE TABLE OF ztpp_216a WITH HEADER LINE,
       lt_216a_old TYPE TABLE OF ztpp_216a WITH HEADER LINE.
  TYPES:BEGIN OF ty_vbeln,
          werks TYPE werks_d,
          vbeln TYPE ztpp_216a-vbeln,
          posnr TYPE ztpp_216a-posnr,
        END OF ty_vbeln.
  DATA:lt_vbeln TYPE TABLE OF ty_vbeln WITH KEY werks vbeln posnr,
       lv_vbeln LIKE LINE OF lt_vbeln.
  LOOP AT itab.
    lv_vbeln-vbeln = itab-vbeln.
    lv_vbeln-posnr = itab-posnr.
    lv_vbeln-werks = itab-werks.
    COLLECT lv_vbeln INTO lt_vbeln.
  ENDLOOP.
  SELECT
    z216a~*
  FROM ztpp_216a AS z216a
  INNER JOIN @lt_vbeln AS vb ON vb~vbeln  = z216a~vbeln
                             AND vb~posnr = z216a~posnr
                             AND vb~werks = z216a~werks
  WHERE z216a~vbeln IS NOT INITIAL
    AND z216a~posnr IS NOT INITIAL
  ORDER BY z216a~werks,z216a~vbeln,z216a~posnr
  INTO CORRESPONDING FIELDS OF TABLE @lt_216a_old.

  LOOP AT itab.
    READ TABLE lt_216a_old WITH KEY werks = itab-werks vbeln = itab-vbeln posnr = itab-posnr BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE FROM ztpp_216a WHERE werks = itab-werks AND vbeln = itab-vbeln AND posnr = itab-posnr.
      COMMIT WORK.
    ENDIF.
    MOVE-CORRESPONDING itab TO lt_216a.
    PERFORM getnum CHANGING lt_216a-znum.
    IF lt_216a-matnr = 'B0205233596'. "折弯件单一料号不要重复搭建BOM
      lt_216a-zisxs = 'X'.
    ENDIF.
    APPEND lt_216a.
  ENDLOOP.
  INSERT ztpp_216a FROM TABLE lt_216a.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.
ENDFORM.
FORM getnum CHANGING p_num TYPE ze_num.
  DATA:znumber  TYPE ztnums_update-znumber10,
       werks_bs TYPE char20.
  "生配送任务流水号
  werks_bs = 'PPBOM' .
  CALL FUNCTION 'ZNUMS_UPDATE'
    EXPORTING
      repid    = sy-repid
*     TCODE    = SY-