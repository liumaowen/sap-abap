*&---------------------------------------------------------------------*
*& 包含               ZRPP201_F01
*&---------------------------------------------------------------------*

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD on_close.
    CALL METHOD sender->free.
    FREE: gv_con_diabox, gv_alv_popup.
  ENDMETHOD.

  METHOD handle_data_changed_fini.
    PERFORM frm_data_changed_fini
      USING e_modified
            et_good_cells
            .
  ENDMETHOD.                    "handle_data_changed_

  METHOD handle_data_changed.
    PERFORM frm_data_changed
      USING er_data_changed
            e_onf4
            e_onf4_before
            e_onf4_after
            e_ucomm
            .
  ENDMETHOD.                    "handle_data_changed_
ENDCLASS.


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

*    WHEN '3002'.
*      ra_mvgr3-low = 'Z02'.
*
*    WHEN '3008'.
*      ra_mvgr3-low = 'Z04'.

    WHEN '3060'.
      ra_mvgr3-low = 'Z15'.

*    WHEN '3062'.
*      ra_mvgr3-low = 'Z16'.
*
*    WHEN '3090'.
*      ra_mvgr3-low = 'Z22'.

  ENDCASE.

  IF ra_mvgr3-low NE ''.
    ra_mvgr3-sign = 'I'.
    ra_mvgr3-option = 'EQ'.

    APPEND ra_mvgr3.
    CLEAR ra_mvgr3.

  ENDIF.

  IF p_ht EQ 'X'.
    SELECT
      vbap~vbeln,
      vbap~posnr,
      vbap~werks,
      vbap~matnr,
      vbak~kunnr,
      vbak~vdatu,
      vbak~erdat,
      vbak~erzet,
      vbak~zywy,
      mara~matkl,
      makt~maktx,
      kna1~name1,
      vbak~zhtly,
      kna1~kukla
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbap
      FROM vbap
      INNER JOIN vbak ON vbak~vbeln = vbap~vbeln

      INNER JOIN mara ON mara~matnr = vbap~matnr

      LEFT JOIN makt  ON makt~matnr = vbap~matnr AND makt~spras = 1

      LEFT JOIN kna1  ON kna1~kunnr = vbak~kunnr

      WHERE vbap~vbeln IN @s_vbeln
        AND ( ( vbap~werks = @p_werks
        AND vbap~mvgr3 = '' )
        OR ( vbap~werks = @p_werks
        AND vbap~mvgr3 IN @ra_mvgr3
        AND vbap~mvgr3 NE '' ) )
        AND vbap~matnr IN @s_matnr
        AND vbap~mvgr2 IN @ra_mvgr2
        AND vbap~abgru EQ '' "拒绝原因
        AND vbak~vbtyp EQ 'G' "合同
        AND vbak~gbstk NE 'C' "未完成
*        AND vbak~erdat GT @lv_gt100date  "#28770 程志龙让把时间限制去掉
        AND mara~matkl IN @s_matkl
        AND mara~spart IN @ra_spart

        AND EXISTS (
            SELECT
              'X'
              FROM mast
              WHERE mast~werks = vbap~werks
                AND mast~matnr = vbap~matnr
            )
      .
  ELSEIF p_bom1 EQ 'X'.
    SELECT
        mast~werks,
        mast~matnr,
        mara~matkl,
        makt~maktx
    FROM mast
    INNER JOIN mara ON mara~matnr = mast~matnr
    LEFT JOIN makt  ON makt~matnr = mast~matnr AND makt~spras = 1
    WHERE mast~werks = @p_werks
      AND mast~matnr IN @s_matnr
      AND mara~matkl IN @s_matkl
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbap
      .
  ELSE.
    SELECT
      a~werks,
      a~matnr,
      mara~matkl,
      makt~maktx
    FROM marc AS a
    INNER JOIN mara ON mara~matnr = a~matnr
    LEFT JOIN makt  ON makt~matnr = a~matnr AND makt~spras = 1
    WHERE a~werks = @p_werks
    AND a~matnr IN @s_matnr
    AND mara~mtart IN ( 'Z001','Z002', 'Z004' )
    AND mara~matkl IN @s_matkl
    INTO CORRESPONDING FIELDS OF TABLE @lt_vbap
.
  ENDIF.
  IF lt_vbap IS INITIAL.
    MESSAGE '无数据'
    TYPE 'S'
    DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.

  ENDIF.

  SELECT SINGLE
    name1
    INTO @DATA(lv_werkstxt)
    FROM t001w
    WHERE werks = @p_werks.

  SELECT SINGLE
    zlogic
    INTO @gv_zlogic
    FROM ztpp_260a4
    WHERE werks = @p_werks.
  IF sy-subrc = 0.

  ELSE.
    MESSAGE '请配置工厂逻辑区分表[ZTPP_260A4]'
    TYPE 'S'
    DISPLAY LIKE 'E'.

    LEAVE LIST-PROCESSING.

  ENDIF.

  SELECT
    matkl,
    wgbez
    INTO TABLE @DATA(lt_t023t)
    FROM t023t
    .
  IF sy-subrc = 0.
    SORT lt_t023t BY matkl.

  ENDIF.

  LOOP AT lt_vbap INTO DATA(ls_vbap).
    gs_list-kukla = ls_vbap-kukla.
    gs_list-werks = p_werks.
    gs_list-werks_txt = lv_werkstxt.

    gs_list-vbeln = ls_vbap-vbeln.
    gs_list-posnr = ls_vbap-posnr.
    gs_list-kunnr = ls_vbap-kunnr.
    gs_list-kunnr_txt = ls_vbap-name1.
    SELECT SINGLE name1
      INTO gs_list-zhtly_txt
      FROM kna1
      WHERE kunnr = ls_vbap-zhtly.
    SELECT SINGLE name1
      INTO gs_list-zywy_txt
      FROM kna1
      WHERE kunnr = ls_vbap-zywy.
    gs_list-matnr = ls_vbap-matnr.
    gs_list-maktx = ls_vbap-maktx.
    gs_list-erdat = ls_vbap-erdat.
    gs_list-erzet = ls_vbap-erzet.
    gs_list-vdatu = ls_vbap-vdatu.
    gs_list-matkl = ls_vbap-matkl.
    IF p_ht EQ 'X'.
      gs_list-cj_zhijin = sy-datum - gs_list-erdat .
    ENDIF.

    IF gs_list-vdatu IS NOT INITIAL.
      gs_list-shengyujh = gs_list-vdatu - sy-datum .
      gs_list-shengyujh = abs( gs_list-shengyujh ).

    ENDIF.

    READ TABLE lt_t023t
    INTO DATA(ls_t023t)
    WITH KEY matkl = gs_list-matkl
    BINARY SEARCH.
    IF sy-subrc = 0.
      gs_list-wgbez = ls_t023t-wgbez.

    ENDIF.

*    gs_list-zlogic = lv_zlogic.
    " 外贸客户判定
    IF gs_list-kukla = '03' AND clear = 'X'.
      CLEAR gs_list-kunnr_txt.
    ENDIF.
    APPEND gs_list TO gt_list.
    CLEAR gs_list.

  ENDLOOP.
  SORT gt_list BY werks matnr.
  DELETE ADJACENT DUPLICATES FROM gt_list COMPARING werks matnr.
  DELETE gt_list WHERE ( werks = '3000' OR werks = '3006' ) AND matkl = 'A0300'.
  gs_ctr-strname = 'GS_LIST'.
  gs_ctr-tabname = 'GT_LIST'.
  ASSIGN (gs_ctr-tabname) TO <fs_list>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_data .

  PERFORM frm_set_layout.
  PERFORM frm_set_fieldcat.
  PERFORM frm_show_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_layout .
  gs_layout-zebra = 'X'."zebra design
*  gs_layout-no_toolbar = 'X'."
  gs_layout-sel_mode = 'D'.".selection mode per field
*  gs_layout-box_fname = 'CK'.".selection mode per field
  gs_layout-col_opt = 'X'.".selection mode per field
  gs_layout-no_rowmark = 'X'.".selection mode per field
*  gs_layout-ctab_fname = 'CLFED'.".selection mode per field
*  gs_layout-cwidth_opt = 'X'.".always optimise columns

*  gs_layout-stylefname = 'STYLE'. "指定保存编辑状态的字段名

  gs_disvariant-report = sy-repid.
  gs_disvariant-handle = 'A'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  CHECK <fs_list> IS ASSIGNED.

  CLEAR:gs_fieldcat, gt_fieldcat[],
         gs_fieldcat_alv, gt_fieldcat_alv[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = gs_ctr-strname
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = gt_fieldcat_alv
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here

    PERFORM frm_leave_process.
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = gt_fieldcat_alv
*     IT_SORT_ALV     =
*     IT_FILTER_ALV   =
*     IS_LAYOUT_ALV   =
    IMPORTING
      et_fieldcat_lvc = gt_fieldcat
*     ET_SORT_LVC     =
*     ET_FILTER_LVC   =
*     ES_LAYOUT_LVC   =
    TABLES
      it_data         = <fs_list>
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    PERFORM frm_leave_process.
  ENDIF.

*如果有必要就循环字段属性表来调整属性
  FIELD-SYMBOLS <fs_field> LIKE LINE OF gt_fieldcat.
*  gs_fieldcat-fieldname = 'ZYXJE'.

  LOOP AT gt_fieldcat
   ASSIGNING <fs_field>.
    <fs_field>-col_opt = 'A'.
    <fs_field>-key = ''.

    IF <fs_field>-fieldname = 'CK'.
      <fs_field>-coltext = '选择'.
      <fs_field>-checkbox = 'X'.

      <fs_field>-edit = 'X'.


    ELSEIF <fs_field>-fieldname = 'WERKS_TXT'.
      PERFORM frm_write_text
      USING TEXT-002 "工厂描述
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'KUNNR_TXT'.
      PERFORM frm_write_text
      USING TEXT-003 "客户名称
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'MATNR'.
      PERFORM frm_write_text
      USING TEXT-004 "成品物料号
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'MAKTX'.
      PERFORM frm_write_text
      USING TEXT-005 "成品物料描述
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'ERDAT'.
      PERFORM frm_write_text
      USING TEXT-006 "销售订单创建日期
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'ERZET'.
      PERFORM frm_write_text
      USING TEXT-007 "销售订单创建时间
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'CJ_ZHIJIN'.
      PERFORM frm_write_text
      USING TEXT-008 "时间差
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'VDATU'.
      PERFORM frm_write_text
      USING TEXT-009 "销售订单交货日期
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'SHENGYUJH'.
      PERFORM frm_write_text
      USING TEXT-010 "剩余交货时间
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'MATKL'.
      PERFORM frm_write_text
      USING TEXT-011 "成品物料组
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'WGBEZ'.
      PERFORM frm_write_text
      USING TEXT-012 "成品物料组描述
      CHANGING <fs_field>
      .
    ELSEIF <fs_field>-fieldname = 'ZHTLY_TXT'.
      PERFORM frm_write_text
      USING TEXT-022 "合同履约
      CHANGING <fs_field>
      .
    ELSEIF <fs_field>-fieldname = 'ZYWY_TXT'.
      PERFORM frm_write_text
      USING TEXT-023 "业务员
      CHANGING <fs_field>
      .
    ELSEIF <fs_field>-fieldname = 'ZLOGIC'.
      <fs_field>-tech = 'X'.

    ENDIF.

  ENDLOOP.

*
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_show_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_alv .

  DATA: lt_event_exit TYPE slis_t_event_exit,
        ls_event_exit LIKE LINE OF lt_event_exit.

  CHECK <fs_list> IS ASSIGNED.

  ls_event_exit-ucomm = '&REFRESH'.
  ls_event_exit-after = 'X'.
  APPEND ls_event_exit TO lt_event_exit.

  ls_event_exit-ucomm = '&ALL'.
*  ls_event_exit-before = 'X'.
  ls_event_exit-after = 'X'.
  APPEND ls_event_exit TO lt_event_exit.

  ls_event_exit-ucomm = '&SAL'.
*  ls_event_exit-before = 'X'.
  ls_event_exit-after = 'X'.
  APPEND ls_event_exit TO lt_event_exit.

*  ls_event_exit-ucomm = '&SAL'.
**  ls_event_exit-before = 'X'.
*  ls_event_exit-after = 'X'.
*  APPEND ls_event_exit TO lt_event_exit.

  PERFORM alv_add_events USING:
    'PF_STATUS_SET',
    'USER_COMMAND'.

  CLEAR gv_exit_caused_by_user.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program     = sy-repid
      is_layout_lvc          = gs_layout
      it_fieldcat_lvc        = gt_fieldcat
      it_events              = gt_events
      it_sort_lvc            = gt_sort
      i_save                 = 'A'
      is_variant             = gs_disvariant
*     it_excluding           = it_excluding
      it_event_exit          = lt_event_exit
    IMPORTING
      es_exit_caused_by_user = gv_exit_caused_by_user
    TABLES
      t_outtab               = <fs_list>
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

*  BREAK ey_cuijh..

*  IF gv_exit_caused_by_user IS NOT INITIAL.
*    LOOP AT gt_list INTO gs_list.
*      PERFORM frm_unlock.
*
*    ENDLOOP.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_leave_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_leave_process .
  DATA mtext TYPE bapi_msg.

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          INTO mtext
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  MESSAGE mtext TYPE 'S' DISPLAY LIKE sy-msgty.

  LEAVE LIST-PROCESSING.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALLER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM caller_exit USING e_grid TYPE slis_data_caller_exit.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = alv_grid.

  CALL METHOD alv_grid->register_edit_event     "注册GRID事件
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified "事件：回车
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CREATE OBJECT gv_event_receiver.
*  SET HANDLER gv_event_receiver->handle_data_changed_fini FOR alv_grid.
  SET HANDLER gv_event_receiver->handle_data_changed FOR alv_grid.
*

ENDFORM.                    "CALLER_EXIT

*&---------------------------------------------------------------------*
*&      Form  ALV_ADD_EVENTS
*&---------------------------------------------------------------------*
FORM alv_add_events  USING    p_event_name TYPE slis_alv_event-name.

  gs_events-name = p_event_name.
  gs_events-form = p_event_name.
  APPEND gs_events TO gt_events.
  CLEAR gs_events.

ENDFORM.                    " ALV_ADD_EVENTS

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM pf_status_set  USING rt_extab TYPE slis_t_extab.

  DATA: BEGIN OF ls_fcode ,
          fcode LIKE rsmpe-func,
        END OF ls_fcode,
        lt_fcode LIKE TABLE OF ls_fcode.


  CLEAR lt_fcode[].

  IF gs_disvariant-handle = 'B'.
    ls_fcode-fcode = '&ZBOMLIST'.
    APPEND ls_fcode TO lt_fcode.

    ls_fcode-fcode = '&REFRESH'.
    APPEND ls_fcode TO lt_fcode.
*    ls_fcode-fcode = '&INSERT'.
*    APPEND ls_fcode TO lt_fcode.
*    ls_fcode-fcode = '&DEL_LINE'.
*    APPEND ls_fcode TO lt_fcode.
    SET PF-STATUS 'ZSTATUS' EXCLUDING lt_fcode.
  ELSEIF gs_disvariant-handle = 'A'.
    ls_fcode-fcode = '&MDM_POST'.
    APPEND ls_fcode TO lt_fcode.

    ls_fcode-fcode = '&INSERT'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode-fcode = '&DEL_LINE'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode-fcode = '&ZSAVE'.
    APPEND ls_fcode TO lt_fcode.

    SET PF-STATUS 'ZSTATUS' EXCLUDING lt_fcode.
  ELSEIF gs_disvariant-handle = 'C'.
*    ls_fcode-fcode = '&ZBOMLIST'.
*    APPEND ls_fcode TO lt_fcode.
*
*    ls_fcode-fcode = '&ZSAVE'.
*    APPEND ls_fcode TO lt_fcode.

    SET PF-STATUS 'ZSTATUS_POP' EXCLUDING lt_fcode.
  ENDIF.



ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command  USING r_ucomm TYPE sy-ucomm
                         ls_selfield TYPE slis_selfield.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid          = alv_grid
      et_fieldcat_lvc = gt_fieldcat.

  CALL METHOD alv_grid->check_changed_data.
  CASE r_ucomm.

    WHEN '&REFRESH'.
      PERFORM frm_refresh_process.


    WHEN '&DEL_LINE'.
      PERFORM frm_delete_line.

    WHEN '&INSERT'.
      PERFORM frm_insert.

    WHEN '&MDM_POST'.
      PERFORM frm_call_mdm.

    WHEN '&ZBOMLIST'.
      PERFORM frm_zbomlist.
      PERFORM frm_show_zbomlist_alv.

    WHEN '&ALL'.
      PERFORM frm_all.

    WHEN '&SAL'.
      PERFORM frm_sal.

    WHEN '&ZSAVE'.
*      PERFORM frm_crt_bom2.
      PERFORM frm_crt_bom.
*      PERFORM frm_save.

    WHEN '&IC1'.
      PERFORM frm_double_click
      USING ls_selfield.

  ENDCASE.

  ls_selfield-row_stable = 'X'.
  ls_selfield-col_stable = 'X'.
  ls_selfield-refresh = 'X'.

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*& Form frm_check_auth
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_auth .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*& Form frm_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_screen .

  LOOP AT SCREEN.

    IF screen-group1 = 'WRK'
      .
      screen-input = '0'.

      MODIFY SCREEN.
    ELSE.

    ENDIF.
    IF ( p_bom1 EQ 'X' OR p_all EQ 'X' ) AND screen-group1 = 'VB'.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_save
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_save .

*  DATA: ls_save  TYPE ztpp_260z,
*        lt_save  LIKE TABLE OF ls_save,
*        lt_check LIKE TABLE OF ls_save.
*
*  LOOP AT gt_bom
*    INTO gs_bom
*    WHERE ck = 'X'.
*
*    MOVE-CORRESPONDING gs_bom TO ls_save.
*    APPEND ls_save TO lt_save.
*    CLEAR ls_save.
*
*  ENDLOOP.
*  IF sy-subrc NE 0.
*    MESSAGE TEXT-013 "'请选择行'
*    TYPE 'S'
*    DISPLAY LIKE 'E'.
*
*    RETURN.
*  ENDIF.
*
*  CHECK lt_save[] IS NOT INITIAL.
**检查是否已存在数据
*  DATA lv_answ TYPE c.
*
*  SELECT
*    *
*    INTO TABLE lt_check
*    FROM ztpp_260z
*    FOR ALL ENTRIES IN lt_save
*    WHERE werks = lt_save-werks
*      AND matnr = lt_save-matnr
*      AND zxt = lt_save-zxt
*      AND zindex = lt_save-zindex
*    .
*  IF sy-subrc = 0.
*
*    CLEAR lt_check[].
*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        text_question  = TEXT-016 "选择的数据已存在，是否继续覆盖？
*        text_button_1  = TEXT-017
*        text_button_2  = TEXT-018
*      IMPORTING
*        answer         = lv_answ
**     TABLES
**       PARAMETER      =
*      EXCEPTIONS
*        text_not_found = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*    CHECK lv_answ = '1'.
*
*    DELETE ztpp_260z
*    FROM TABLE lt_check.
*    COMMIT WORK.
*  ELSE.
*
*  ENDIF.
*
*
*
*
*
*  IF lt_save[] IS NOT INITIAL.
*
*    ls_save-erdat = sy-datum.
*    ls_save-erzet = sy-uzeit.
*    ls_save-ernam = sy-uname.
*
*    MODIFY lt_save FROM ls_save
*    TRANSPORTING erdat erzet ernam
*    WHERE ernam = ''.
*
*    MODIFY ztpp_260z FROM TABLE lt_save.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*      MESSAGE TEXT-014 "'保存成功'
*      TYPE 'S'.
*
*    ELSE.
*      ROLLBACK WORK.
*      MESSAGE TEXT-015 "'保存失败'
*      TYPE 'S'
*      DISPLAY LIKE 'E'.
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_disabled
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_set_disabled
  USING
    VALUE(p_field).


  gs_style-fieldname = p_field.
  gs_style-style = cl_gui_alv_grid=>mc_style_disabled."设置为不可输入
  INSERT gs_style INTO TABLE gt_style.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init .

  DATA ls_functxt TYPE smp_dyntxt.

*  BREAK cuijunhu.           ,

*  ls_functxt-icon_id    = icon_export.
*  ls_functxt-quickinfo  = TEXT-001.
*  ls_functxt-icon_text  = TEXT-001.
*  sscrfields-functxt_01 = ls_functxt.

*  button1 = TEXT-002.
*  button2 = TEXT-003.
*  button3 = TEXT-004.
*  CONCATENATE TEXT-002 '(' tcode1 ')' INTO button1.
*  CONCATENATE TEXT-003 '(' tcode2 ')' INTO button2.
*  CONCATENATE TEXT-004 '(' tcode3 ')' INTO button3.
  %_p_ht_%_app_%-text = '有合同有BOM'.
  %_p_bom1_%_app_%-text = '有BOM'.
  %_p_all_%_app_%-text = '所有'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_process_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_process_data
           .
*  DATA oref TYPE REF TO cx_root.
*  DATA text TYPE string.
*  DATA ls_excel_tab TYPE zzalsmex_tabline.
*
*  FIELD-SYMBOLS: <fs_field> TYPE any.
*
*  DATA lv_col TYPE i.
*
*  LOOP AT gt_excel_tab INTO ls_excel_tab.
*    lv_col = ls_excel_tab-col + 4.
*
*    ASSIGN COMPONENT lv_col OF STRUCTURE gs_list
*      TO <fs_field>.
*    IF <fs_field> IS ASSIGNED.
*      IF ls_excel_tab-col = 11.
*        REPLACE ALL OCCURRENCES OF ','
*        IN ls_excel_tab-value
*        WITH space.
*
*        CONDENSE ls_excel_tab-value.
*
*      ENDIF.
*
*      TRY .
*          <fs_field> = ls_excel_tab-value.
*        CATCH  cx_root INTO oref.
*          text = oref->get_text( ).
*
**          gs_list-message = text.
*          CONCATENATE gs_list-message
*                      text
*                 INTO gs_list-message
*         SEPARATED BY '|'.
*
*          CONTINUE.
*      ENDTRY.
*
**      IF lv_col = 4.
**        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
**          EXPORTING
**            input  = <fs_field>
**          IMPORTING
**            output = <fs_field>.
**
**      ENDIF.
*
*      UNASSIGN <fs_field>.
*    ENDIF.
*
*    AT END OF row.
*      gs_list-gjahr = p_gjahr.
*      gs_list-monat = p_monat.
*      APPEND gs_list TO gt_list.
*      CLEAR gs_list.
*    ENDAT.
*  ENDLOOP.
*
*  IF gt_list[] IS NOT INITIAL.
*
*  ELSE.
*    MESSAGE '无数据' TYPE 'S'
*    DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING .
*
*  ENDIF.
*
*  gs_ctr-strname = 'gs_list'.
*  gs_ctr-tabname = 'GT_LIST'.
*  ASSIGN (gs_ctr-tabname) TO <fs_list>.
*
**  PERFORM frm_check.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_unlock
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_unlock .
*  CALL FUNCTION 'ENQUEUE_Ezmmt057'
*    EXPORTING
**     MODE_zmmt057   = 'E'
*      mandt          = sy-mandt
*      rsnum          = gs_list-rsnum
*      rspos          = gs_list-rspos
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_lock
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_lock .


*  CALL FUNCTION 'ENQUEUE_Ezmmt057'
*    EXPORTING
*      mandt          = sy-mandt
*      rsnum          = gs_list-rsnum
*      rspos          = gs_list-rspos
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      INTO gs_list-message
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    gs_list-style[] = gt_style[].
*  ELSE.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_global
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_global .

*  SELECT
*    prctr,
*    ktext
*    INTO TABLE @gt_cepct
*    FROM cepct
*    WHERE spras = @sy-langu
*      AND datbi >= @sy-datum
*      AND kokrs = '1000'
*    .
*
*  SORT gt_cepct BY prctr.
*
*  SELECT
*    mvgr1,
*    bezei
*    INTO TABLE @gt_tvm1t
*    FROM tvm1t
*    WHERE spras = @sy-langu
*    .
*
*  SORT gt_tvm1t BY mvgr1.
*
*  SELECT
*    ww001,
*    bezek
*    INTO TABLE @gt_t25a0
*    FROM t25a0
*    WHERE spras = @sy-langu
*    .
*
*  SORT gt_t25a0 BY ww001.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_enabled
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_set_enabled
   USING
    VALUE(p_field).

  gs_style-fieldname = p_field.
  gs_style-style = cl_gui_alv_grid=>mc_style_enabled."设置为不可输入
  INSERT gs_style INTO TABLE gt_style.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_popup
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_POP
*&---------------------------------------------------------------------*
FORM frm_get_popup   CHANGING pv_flag.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'Y'
      textline1      = '是否确定删除？'
      textline2      = ' '
      titel          = 'SAP'
      start_column   = 25
      start_row      = 6
      cancel_display = 'X'
    IMPORTING
      answer         = pv_flag.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_data_changed_fini
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_MODIFIED
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM frm_data_changed_fini
  USING e_modified
        et_good_cells TYPE lvc_t_modi.

  DATA: ls_modi LIKE LINE OF et_good_cells,
        lt_modi LIKE TABLE OF ls_modi.

*  CHECK e_modified = 'X'.
*
*  BREAK ey_cuijh.
*
*  APPEND LINES OF et_good_cells TO lt_modi.
*
*  SORT lt_modi BY row_id.
*  DELETE ADJACENT DUPLICATES FROM lt_modi
*  COMPARING row_id.
*
*  CLEAR gs_list.
*
*  MODIFY GT_LIST FROM gs_list
*  TRANSPORTING message
*  WHERE message NE ''.
*
*  DATA(lt_list01) = GT_LIST[].
*
*  LOOP AT lt_modi INTO ls_modi.
*
*    READ TABLE GT_LIST
*    ASSIGNING <fs_line>
*    INDEX ls_modi-row_id.
*    CHECK sy-subrc = 0.
*
*    DELETE lt_list01 INDEX ls_modi-row_id.
*
**    MODIFY lt_list01 FROM gs_list
**    TRANSPORTING message
**    WHERE zqzljm = gs_list-zqzljm
**      AND matnr = gs_list-matnr
**      AND zjgqj = gs_list-zjgqj
**      .
*    READ TABLE lt_list01
*    TRANSPORTING NO FIELDS
*    WITH KEY gjahr = <fs_line>-gjahr
*             monat = <fs_line>-monat
*             prctr = <fs_line>-prctr
*             mvgr1 = <fs_line>-mvgr1
*             ww001 = <fs_line>-ww001
*             .
*
*    IF sy-subrc = 0.
*
*      <fs_line>-message = '主键不得相同'.
*    ELSE.
*
*    ENDIF.
*
*  ENDLOOP.
*
*  PERFORM frm_refresh.
*
*  CLEAR lt_list01[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_refresh
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh .

  DATA: ls_stale TYPE lvc_s_stbl.
  ls_stale-row = 'X'.
  ls_stale-col = 'X'.
  CALL METHOD alv_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stale.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_make_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_make_data .

*  LOOP AT gt_list
*    ASSIGNING <fs_line>.
*
*    READ TABLE gt_cepct
*    INTO gs_cepct
*    WITH KEY prctr = <fs_line>-prctr
*    BINARY SEARCH.
*    IF sy-subrc = 0.
*      <fs_line>-ktext = gs_cepct-ktext.
*
*    ENDIF.
*
*    READ TABLE gt_tvm1t
*    INTO gs_tvm1t
*    WITH KEY mvgr1 = <fs_line>-mvgr1
*    BINARY SEARCH.
*    IF sy-subrc = 0.
*      <fs_line>-bezei = gs_tvm1t-bezei.
*
*    ENDIF.
*
*    READ TABLE gt_t25a0
*    INTO gs_t25a0
*    WITH KEY ww001 = <fs_line>-ww001
*    BINARY SEARCH.
*    IF sy-subrc = 0.
*      <fs_line>-bezek = gs_t25a0-bezek.
*
*    ENDIF.
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&      --> E_ONF4
*&      --> E_ONF4_BEFORE
*&      --> E_ONF4_AFTER
*&      --> E_UCOMM
*&---------------------------------------------------------------------*
FORM frm_data_changed
  USING
    er_data_changed TYPE REF TO	cl_alv_changed_data_protocol
    e_onf4  TYPE  char01
    e_onf4_before  TYPE  char01
    e_onf4_after  TYPE  char01
    e_ucomm  TYPE  sy-ucomm
  .

  DATA lv_error TYPE c.

  FIELD-SYMBOLS: <fs_s1> TYPE lvc_s_modi.

*  BREAK ey_cuijh.
  DATA(lt_modi) = er_data_changed->mt_good_cells[].
*
*****变更单元格内容一起变更描述
  LOOP AT lt_modi
    ASSIGNING <fs_s1>.

    READ TABLE gt_bom
    ASSIGNING <fs_bom>
    INDEX <fs_s1>-row_id.

    IF sy-subrc = 0.
      CLEAR lv_error.

      IF <fs_s1>-fieldname = 'IDNRK'."
*
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = <fs_s1>-row_id    "单元格的ID
            i_fieldname = <fs_s1>-fieldname "单元格的字段
          IMPORTING
            e_value     = <fs_s1>-value. "单元格的字段值

        <fs_bom>-idnrk = <fs_s1>-value.
*
        SELECT SINGLE
          maktx
          INTO <fs_bom>-idnrk_maktx
          FROM makt
          WHERE matnr = <fs_bom>-idnrk
            AND spras = '1'
          .
        IF sy-subrc NE 0.
          CLEAR <fs_bom>-idnrk_maktx.

        ENDIF.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = <fs_s1>-row_id    "单元格的ID
            i_fieldname = 'IDNRK_MAKTX' "单元格的字段
            i_value     = <fs_bom>-idnrk_maktx. "单元格的字段值

      ELSEIF <fs_s1>-fieldname = 'MEINS'."
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = <fs_s1>-row_id    "单元格的ID
            i_fieldname = <fs_s1>-fieldname "单元格的字段
          IMPORTING
            e_value     = <fs_s1>-value. "单元格的字段值

        <fs_bom>-meins = <fs_s1>-value.
*
        SELECT SINGLE
          mseht
          INTO @<fs_bom>-mseht
          FROM t006a
          WHERE spras = '1'
            AND msehi = @<fs_bom>-meins.
        IF sy-subrc = 0.

        ELSE.
          CLEAR <fs_bom>-mseht.

        ENDIF.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = <fs_s1>-row_id    "单元格的ID
            i_fieldname = 'MSEHT' "单元格的字段
            i_value     = <fs_bom>-mseht. "单元格的字段值

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_check_input
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_input .

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
   ID 'ACTVT' DUMMY
   ID 'WERKS' FIELD p_werks.
  IF sy-subrc <> 0.
* Implement a suitable exception handling here
    MESSAGE '对此工厂无权限'
    TYPE 'E'
    DISPLAY LIKE 'S'.

    LEAVE LIST-PROCESSING.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_write_text
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_002
*&      <-- <FS_FIELD>
*&---------------------------------------------------------------------*
FORM frm_write_text
  USING VALUE(pu_text)
  CHANGING pc_fieldcat STRUCTURE gs_fieldcat.

  pc_fieldcat-reptext = pc_fieldcat-scrtext_l
                      = pc_fieldcat-scrtext_m
*                      = pc_fieldcat-scrtext_s
                      = pc_fieldcat-coltext
*                      = pc_fieldcat-seltext
                      = pu_text
                      .

  pc_fieldcat-colddictxt = 'L'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_double_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_double_click
  USING pu_selfield TYPE slis_selfield.

  IF pu_selfield-fieldname = 'IDNRKLISTID'.

    READ TABLE gt_bom
    INTO gs_bom INDEX pu_selfield-tabindex.

    CHECK sy-subrc = 0
    AND gs_bom-idnrklist[] IS NOT INITIAL.


    PERFORM frm_set_layout_pop.
    PERFORM frm_set_fieldcat_pop
    TABLES gs_bom-idnrklist.
    PERFORM frm_show_alv_pop
    TABLES gs_bom-idnrklist.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_all .


  DATA: lt_filter TYPE lvc_t_fidx.

*获取被过滤掉的数据行数
  CLEAR lt_filter[].
  CALL METHOD alv_grid->get_filtered_entries
    IMPORTING
      et_filtered_entries = lt_filter.

  IF lt_filter[] IS INITIAL.

    IF gs_disvariant-handle = 'A'.
      gs_list-ck = 'X'.

      MODIFY gt_list
      FROM gs_list
      TRANSPORTING ck
      WHERE ck = ''.

    ELSEIF gs_disvariant-handle = 'B'.
      gs_bom-ck = 'X'.

      MODIFY gt_bom
      FROM gs_bom
      TRANSPORTING ck
      WHERE ck = ''.

    ENDIF.

  ELSE.

    IF gs_disvariant-handle = 'A'.

      LOOP AT gt_list
        ASSIGNING <fs_line>
        WHERE ck = ''.

        DATA(lv_tabix) = sy-tabix.
        READ TABLE lt_filter
        TRANSPORTING NO FIELDS
        WITH KEY table_line = lv_tabix.
        IF sy-subrc = 0.

        ELSE.

          <fs_line>-ck = 'X'.

        ENDIF.
      ENDLOOP.

    ELSEIF gs_disvariant-handle = 'B'.

      LOOP AT gt_bom
        ASSIGNING <fs_bom>
        WHERE ck = ''.

        lv_tabix = sy-tabix.
        READ TABLE lt_filter
        TRANSPORTING NO FIELDS
        WITH KEY table_line = lv_tabix.
        IF sy-subrc = 0.

        ELSE.

          <fs_bom>-ck = 'X'.

        ENDIF.
      ENDLOOP.

    ENDIF.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_sal
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_sal .

  IF gs_disvariant-handle = 'A'.
    gs_list-ck = ''.

    MODIFY gt_list
    FROM gs_list
    TRANSPORTING ck
    WHERE ck NE ''.

  ELSEIF gs_disvariant-handle = 'B'.
    gs_bom-ck = ''.

    MODIFY gt_bom
    FROM gs_bom
    TRANSPORTING ck
    WHERE ck NE ''.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_zbomlist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_zbomlist .
  DATA: lt_indata   TYPE zttpp_228_input,
        ls_indata   LIKE LINE OF lt_indata,
        lt_retdata1 TYPE zttpp_228_output,
        lt_retdata2 TYPE zttpp_228_output,
        lt_retdata  TYPE zttpp_228_output,
        lv_type     TYPE bapi_mtype,
        lv_message  TYPE bapi_msg.
  DATA:BEGIN OF wa_key,
         tdobject TYPE  stxl-tdobject,
         tdname   TYPE  stxl-tdname,
         tdid     TYPE  stxl-tdid,
         tdspras  TYPE  stxl-tdspras,
       END OF wa_key,
       t_text TYPE TABLE OF tline WITH HEADER LINE.
  DATA:lt_stpo TYPE STANDARD TABLE OF stpo_api02 WITH HEADER LINE,
       lt_stko TYPE TABLE OF stko_api02.
  DATA lv_fname TYPE rs38l-name.

  CASE gv_zlogic.
    WHEN 'A'.
      lv_fname = 'ZFM_GP_BUILD_BOM_STRUCTURE_A_N'.

    WHEN 'B'.
      lv_fname = 'ZFM_GP_BUILD_BOM_STRUCTURE_B'.

  ENDCASE.
  " 排除已经存在主材成品BOM，组件料号前3位为E01和E02，其中一种有两条组件的则排除  2023-12-06 14:16:51 by lmw
  REFRESH:it_bomparams,it_bomdet,it_bomold.
  PERFORM getbomdel USING gt_list it_bomparams it_bomdet it_bomold[].

  LOOP AT gt_list INTO gs_list WHERE ck = 'X'.
    READ TABLE it_bomparams INTO wa_bomparams WITH KEY werks = gs_list-werks matnr = gs_list-matnr del = 'X'.
    IF sy-subrc EQ 0.
      CONTINUE. "排除内板和外板(E01/E02出现两次以上的)
    ENDIF.
    MOVE-CORRESPONDING gs_list TO ls_indata.
    APPEND ls_indata TO lt_indata.
    CLEAR ls_indata.

  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE TEXT-013 "'请选择行'
    TYPE 'E'
    DISPLAY LIKE 'S'.

    RETURN.

  ENDIF.

  SORT lt_indata BY werks matnr .
  DELETE ADJACENT DUPLICATES FROM lt_indata
  COMPARING werks matnr.

  CLEAR gt_bom[].
  CALL FUNCTION lv_fname
    EXPORTING
      it_data    = lt_indata
    IMPORTING
      e_type     = lv_type
      e_message  = lv_message
      et_retdata = lt_retdata.

  IF lt_retdata[] IS NOT INITIAL.
    SELECT
      *
      INTO TABLE @DATA(lt_t006a)
      FROM t006a
      WHERE spras = '1'.
    IF sy-subrc = 0.
      SORT lt_t006a BY msehi.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.


  LOOP AT lt_retdata INTO DATA(ls_retdata).
    gs_bom-werks = ls_retdata-werks.
    gs_bom-matnr = ls_retdata-matnr.
    gs_bom-maktx = ls_retdata-maktx.
    gs_bom-matkl = ls_retdata-matkl.
    gs_bom-wgbez = ls_retdata-wgbez.
*物料长描述
    CLEAR wa_key.
    wa_key-tdobject   = 'MATERIAL'.
    wa_key-tdname     = gs_bom-matnr.
    wa_key-tdid       = 'GRUN'.
    wa_key-tdspras    = sy-langu.
    CLEAR:t_text[].
    IMPORT tline = t_text[] FROM DATABASE stxl(tx) ID wa_key.
    DATA: longtext TYPE string,
          result   TYPE string.
    CLEAR longtext.
    LOOP AT t_text.
      CONCATENATE longtext t_text-tdline  INTO longtext.
    ENDLOOP.
    IF  longtext IS NOT INITIAL.
      IF strlen(  longtext ) <= 128.
        CLEAR result.
        result = substring( val = longtext off = 0 len = strlen( longtext ) ).
        gs_bom-maktx1 = result.
      ELSE.
        CLEAR result.
        result = substring( val = longtext off = 0 len = 128 ).
        gs_bom-maktx1 = result.
        CLEAR result.
        result = substring( val = longtext off = 128 len = ( strlen( longtext ) - 128 ) ).
        gs_bom-maktx2 = result.
      ENDIF.
    ENDIF.
    IF ls_retdata-data[] IS NOT INITIAL.
      DATA(lt_idnrk) = ls_retdata-data[].

      DELETE lt_idnrk WHERE idnrk = ''.
      IF lt_idnrk[] IS NOT INITIAL.
        SORT lt_idnrk BY idnrk.
        DELETE ADJACENT DUPLICATES FROM lt_idnrk
        COMPARING idnrk.

*获取 组件物料描述
        SELECT
          matnr,
          maktx
          INTO TABLE @DATA(lt_makt)
          FROM makt
          FOR ALL ENTRIES IN @lt_idnrk
          WHERE matnr = @lt_idnrk-idnrk
            AND spras = '1'.
        IF sy-subrc = 0.
          SORT lt_makt BY matnr.

        ENDIF.
      ENDIF.
    ENDIF.


    LOOP AT ls_retdata-data INTO DATA(ls_data).
*用量 0 的 过滤
      IF ls_data-idnrk NE ''
        AND ls_data-zyl EQ 0.
        CONTINUE.
      ENDIF.


      READ TABLE it_bomdet INTO wa_bomdet WITH KEY werks = ls_retdata-werks
                                                      matnr = ls_retdata-matnr
                                                      stlal = ls_data-stlal BINARY SEARCH.
      IF sy-subrc EQ 0 AND wa_bomdet-del EQ 'X'.
        CONTINUE.
      ELSE.
        gs_bom-stlnr = wa_bomdet-stlnr.
      ENDIF.
      " bom组件中是否存在  2023-12-07 13:05:58 by lmw
      READ TABLE it_bomold WITH KEY werks = ls_retdata-werks
                                    matnr = ls_retdata-matnr
                                    stlal = ls_data-stlal
                                    idnrk = ls_data-idnrk TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        gs_bom-bom = 'X'.
      ELSE.
        gs_bom-bom = ''.
      ENDIF.
      " bom组件中是否存在此备选  2023-12-18 08:05:58 by lmw
      READ TABLE it_bomold WITH KEY werks = ls_retdata-werks
                                    matnr = ls_retdata-matnr
                                    stlal = ls_data-stlal TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        gs_bom-isstlal = 'X'.
      ELSE.
        gs_bom-isstlal = ''.
      ENDIF.

      gs_bom-stlal = ls_data-stlal.
      gs_bom-zindex = ls_data-zindex.
      gs_bom-zxt = ls_data-zxt.
      gs_bom-zclass = ls_data-zclass.
      gs_bom-idnrk = ls_data-idnrk.
      IF gs_bom-idnrk IS NOT INITIAL.
        SELECT SINGLE zzl1
          INTO gs_bom-zpmid
          FROM mara
          WHERE matnr = gs_bom-idnrk.
      ELSE.
        gs_bom-zpmid = ls_data-zpmid.
      ENDIF.
      gs_bom-zpmms = ls_data-zpmms.
      gs_bom-auspconds[] = ls_data-auspconds[].
      CONDENSE gs_bom-idnrk NO-GAPS .
      gs_bom-idnrk_matkl = ls_data-idnrk_matkl.
      READ TABLE lt_makt INTO DATA(ls_makt)
      WITH KEY matnr = gs_bom-idnrk
      BINARY SEARCH.
      IF sy-subrc = 0.
        gs_bom-idnrk_maktx = ls_makt-maktx.
      ENDIF.

      gs_bom-zyl = ls_data-zyl.
      gs_bom-meins = ls_data-meins.
      gs_bom-message = ls_data-message.
      IF ls_data-type = 'E'.
        gs_bom-id = icon_led_red.

      ELSE.
        gs_bom-id = icon_led_green.

      ENDIF.
      IF gs_bom-meins NE ''.
        READ TABLE lt_t006a INTO DATA(ls_t006a)
        WITH KEY msehi = gs_bom-meins
        BINARY SEARCH.
        IF sy-subrc = 0.
          gs_bom-mseht = ls_t006a-mseht.
        ENDIF.
      ENDIF.

      IF gs_bom-zindex < 5.
        gs_bom-clr = 'C200'. "
      ELSE.
        gs_bom-clr = 'C300'. "浅黄
      ENDIF.


      IF ls_data-idnrklist[] IS NOT INITIAL.
        APPEND LINES OF ls_data-idnrklist TO gs_bom-idnrklist[].
        gs_bom-idnrklistid = icon_list.

      ENDIF.
      APPEND gs_bom TO gt_bom.

      CLEAR: gs_bom-idnrklist[],
             gs_bom-idnrklistid,
             gs_bom-mseht,
             gs_bom-meins,
             gs_bom-zpmid,
             gs_bom-zpmms,
             gs_bom-idnrk_maktx,
             gs_bom-clr.

    ENDLOOP.
    CLEAR gs_bom.

  ENDLOOP.

  SORT gt_bom BY werks matnr matkl zxt zindex idnrk.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form getbomdel
*&---------------------------------------------------------------------*
*& 查询是否在BOM中存在
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getbomdel USING p_gt_list      LIKE gt_list
                     p_it_bomparams LIKE it_bomparams
                     p_it_bomdet    LIKE it_bomdet
                     p_it_bomold    LIKE it_bomold[].

  DATA:BEGIN OF wa_e01_e02,
         e01 TYPE i,
         e02 TYPE i,
       END OF wa_e01_e02.
  DATA:bom_cp_matnr TYPE char1. "成品BOM物料号第一位字母
  LOOP AT p_gt_list INTO gs_list WHERE ck = 'X'.
    CLEAR wa_bomparams.
    wa_bomparams-werks = gs_list-werks.
    wa_bomparams-matnr = gs_list-matnr.
    COLLECT wa_bomparams INTO p_it_bomparams.
  ENDLOOP.
  SELECT
      mast~werks,
      mast~matnr,
      mast~stlal,
      mast~stlnr,
      stpo~stlty,
      stpo~stlkn,
      stpo~stpoz,
      stpo~posnr,
      stpo~idnrk
  FROM mast
  LEFT JOIN stas ON stas~stlnr = mast~stlnr AND stas~stlal = mast~stlal
  LEFT JOIN stpo ON stpo~stlkn = stas~stlkn AND stpo~stlnr = stas~stlnr
  INNER JOIN @p_it_bomparams AS b ON  mast~werks = b~werks
                                AND mast~matnr = b~matnr
  ORDER BY mast~werks,mast~matnr,mast~stlal
  INTO TABLE @p_it_bomold.

  LOOP AT p_it_bomold INTO DATA(wa_bomold).
    CLEAR wa_bomdet.
    wa_bomdet-werks = wa_bomold-werks.
    wa_bomdet-matnr = wa_bomold-matnr.
    wa_bomdet-stlal = wa_bomold-stlal.
    wa_bomdet-stlnr = wa_bomold-stlnr.
    COLLECT wa_bomdet INTO p_it_bomdet.
  ENDLOOP.
  SORT p_it_bomdet BY werks matnr stlal.

  LOOP AT p_it_bomdet ASSIGNING FIELD-SYMBOL(<fs_bomdet>).
    CLEAR: bom_cp_matnr,wa_e01_e02.
    bom_cp_matnr = <fs_bomdet>-matnr+0(1).
    LOOP AT p_it_bomold INTO wa_bomold WHERE werks = <fs_bomdet>-werks
                                       AND matnr = <fs_bomdet>-matnr
                                       AND stlal = <fs_bomdet>-stlal.
      IF wa_bomold-idnrk+0(3) EQ 'E01'.
        wa_e01_e02-e01 += 1.
      ENDIF.
      IF wa_bomold-idnrk+0(3) EQ 'E02'.
        wa_e01_e02-e02 += 1.
      ENDIF.
      IF bom_cp_matnr EQ 'B'.
        IF wa_e01_e02-e01 > 0 OR wa_e01_e02-e02 > 0.
          EXIT.
        ENDIF.
      ELSE.
        IF wa_e01_e02-e01 > 1 OR wa_e01_e02-e02 > 1.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF bom_cp_matnr EQ 'B'.
      IF wa_e01_e02-e01 > 0 OR wa_e01_e02-e02 > 0.
        <fs_bomdet>-del = 'X'.
      ENDIF.
    ELSE.
      IF wa_e01_e02-e01 > 1 OR wa_e01_e02-e02 > 1.
        <fs_bomdet>-del = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DATA: num  TYPE i,
        num1 TYPE i.
  LOOP AT p_it_bomparams INTO wa_bomparams.
    READ TABLE p_it_bomdet WITH KEY  werks = wa_bomparams-werks matnr = wa_bomparams-matnr TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    CLEAR: num,num1.
    LOOP AT p_it_bomdet INTO wa_bomdet WHERE werks = wa_bomparams-werks
                                       AND matnr = wa_bomparams-matnr .
      num += 1.
    ENDLOOP.
    LOOP AT p_it_bomdet INTO wa_bomdet WHERE werks = wa_bomparams-werks
                                       AND matnr = wa_bomparams-matnr
                                       AND del EQ 'X' .
      num1 += 1.
    ENDLOOP.
    IF num = num1.
      wa_bomparams-del = 'X'.
      MODIFY p_it_bomparams FROM wa_bomparams TRANSPORTING del.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_show_zbomlist_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_zbomlist_alv .

  IF gt_bom[] IS INITIAL.
    MESSAGE '无法生成待创建BOM清单。'
    TYPE 'I'.

    RETURN.

  ENDIF.

  PERFORM frm_set_layout_bom.
  PERFORM frm_set_fieldcat_bom.
  PERFORM frm_show_alv_bom.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_layout_bom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_layout_bom .
  gs_layout-zebra = 'X'."zebra design
*  gs_layout-no_toolbar = 'X'."
  gs_layout-sel_mode = 'D'.".selection mode per field
*  gs_layout-box_fname = 'CK'.".selection mode per field
  gs_layout-col_opt = 'X'.".selection mode per field
*  gs_layout-ctab_fname = 'CLFED'.".selection mode per field
*  gs_layout-cwidth_opt = 'X'.".always optimise columns
  gs_layout-info_fname = 'CLR'.  "行颜色代码的字段
*  gs_layout-stylefname = 'STYLE'. "指定保存编辑状态的字段名

  gs_disvariant-report = sy-repid.
  gs_disvariant-handle = 'B'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat_bom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat_bom .
  CHECK gt_bom[] IS NOT INITIAL.

  CLEAR:gs_fieldcat, gt_fieldcat_bom[],
         gs_fieldcat_alv, gt_fieldcat_alv[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'GS_BOM'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = gt_fieldcat_alv
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here

    PERFORM frm_leave_process.
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = gt_fieldcat_alv
*     IT_SORT_ALV     =
*     IT_FILTER_ALV   =
*     IS_LAYOUT_ALV   =
    IMPORTING
      et_fieldcat_lvc = gt_fieldcat_bom
*     ET_SORT_LVC     =
*     ET_FILTER_LVC   =
*     ES_LAYOUT_LVC   =
    TABLES
      it_data         = gt_bom
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    PERFORM frm_leave_process.
  ENDIF.

*如果有必要就循环字段属性表来调整属性
  FIELD-SYMBOLS <fs_field> LIKE LINE OF gt_fieldcat.
*  gs_fieldcat-fieldname = 'ZYXJE'.

  LOOP AT gt_fieldcat_bom
   ASSIGNING <fs_field>.
    <fs_field>-col_opt = 'A'.
    <fs_field>-key = ''.

    IF <fs_field>-fieldname = 'CK'.
      <fs_field>-coltext = '选择'.
      <fs_field>-fix_column = 'X'.
      <fs_field>-checkbox = 'X'.

      <fs_field>-edit = 'X'.

    ELSEIF <fs_field>-fieldname = 'ID'.
      <fs_field>-fix_column = 'X'.

    ELSEIF <fs_field>-fieldname = 'BOM'.
      PERFORM frm_write_text
      USING TEXT-024 "BOM
      CHANGING <fs_field>
      .
    ELSEIF <fs_field>-fieldname = 'ISSTLAL'.
      PERFORM frm_write_text
      USING TEXT-025 "是否存在备选清单列
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'WERKS_TXT'.
      PERFORM frm_write_text
      USING TEXT-002 "工厂描述
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'KUNNR_TXT'.
      PERFORM frm_write_text
      USING TEXT-003 "客户名称
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'MATNR'.
      PERFORM frm_write_text
      USING TEXT-004 "成品物料号
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'MAKTX'.
      PERFORM frm_write_text
      USING TEXT-005 "成品物料描述
      CHANGING <fs_field>
      .
    ELSEIF <fs_field>-fieldname = 'MAKTX1'.
      PERFORM frm_write_text
      USING TEXT-005 "组件物料描述
      CHANGING <fs_field>
        .
    ELSEIF <fs_field>-fieldname = 'MAKTX2'.
      PERFORM frm_write_text
      USING TEXT-005 "组件物料描述
      CHANGING <fs_field>
        .
    ELSEIF <fs_field>-fieldname = 'ERDAT'.
      PERFORM frm_write_text
      USING TEXT-006 "销售订单创建日期
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'ERZET'.
      PERFORM frm_write_text
      USING TEXT-007 "销售订单创建时间
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'CJ_ZHIJIN'.
      PERFORM frm_write_text
      USING TEXT-008 "时间差
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'VDATU'.
      PERFORM frm_write_text
      USING TEXT-009 "销售订单交货日期
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'SHENGYUJH'.
      PERFORM frm_write_text
      USING TEXT-010 "剩余交货时间
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'IDNRK_MAKTX'.
      PERFORM frm_write_text
      USING TEXT-021 "组件物料描述
      CHANGING <fs_field>
        .

    ELSEIF <fs_field>-fieldname = 'IDNRK_MATKL'.
      PERFORM frm_write_text
      USING TEXT-020 "组件物料组
      CHANGING <fs_field>
        .
      <fs_field>-tech = 'X'.


    ELSEIF <fs_field>-fieldname = 'MATKL'.
      PERFORM frm_write_text
      USING TEXT-011 "成品物料组
      CHANGING <fs_field>
      .

    ELSEIF <fs_field>-fieldname = 'IDNRKLISTID'.
      PERFORM frm_write_text
      USING TEXT-019 "查看清单
      CHANGING <fs_field>
      .
      <fs_field>-hotspot = 'X'.
      <fs_field>-icon = 'X'.

    ELSEIF <fs_field>-fieldname = 'IDNRKLIST'.
      <fs_field>-tech = 'X'.

    ELSEIF <fs_field>-fieldname = 'ZINDEX'.
      <fs_field>-tech = 'X'.

    ELSEIF <fs_field>-fieldname = 'ZYL'.
      <fs_field>-edit = 'X'.

    ELSEIF <fs_field>-fieldname = 'MEINS'.
      <fs_field>-edit = 'X'.

    ELSEIF <fs_field>-fieldname = 'IDNRK'.
      <fs_field>-edit = 'X'.

    ELSEIF <fs_field>-fieldname = 'WGBEZ'.
      PERFORM frm_write_text
      USING TEXT-012 "成品物料组描述
      CHANGING <fs_field>
      .
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_show_alv_bom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_alv_bom .

  DATA: lt_event_exit TYPE slis_t_event_exit,
        ls_event_exit LIKE LINE OF lt_event_exit.

  CHECK gt_bom[] IS NOT INITIAL
  AND gt_fieldcat_bom[] IS NOT INITIAL.

  ls_event_exit-ucomm = '&ALL'.
*  ls_event_exit-before = 'X'.
  ls_event_exit-after = 'X'.
  APPEND ls_event_exit TO lt_event_exit.

  ls_event_exit-ucomm = '&SAL'.
*  ls_event_exit-before = 'X'.
  ls_event_exit-after = 'X'.
  APPEND ls_event_exit TO lt_event_exit.

*  ls_event_exit-ucomm = '&SAL'.
**  ls_event_exit-before = 'X'.
*  ls_event_exit-after = 'X'.
*  APPEND ls_event_exit TO lt_event_exit.

  PERFORM alv_add_events USING:
    'PF_STATUS_SET',
    'USER_COMMAND',
    'CALLER_EXIT'.

  CLEAR gv_exit_caused_by_user.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program     = sy-repid
      is_layout_lvc          = gs_layout
      it_fieldcat_lvc        = gt_fieldcat_bom
      it_events              = gt_events
      it_sort_lvc            = gt_sort
      is_variant             = gs_disvariant
      i_save                 = 'A'
*     it_excluding           = it_excluding
      it_event_exit          = lt_event_exit
    IMPORTING
      es_exit_caused_by_user = gv_exit_caused_by_user
    TABLES
      t_outtab               = gt_bom
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

*  BREAK ey_cuijh..

  IF gv_exit_caused_by_user IS NOT INITIAL.
    gs_disvariant-handle = 'A'.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_layout_pop
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_layout_pop .
  gs_layout-zebra = 'X'."zebra design
  gs_layout-sel_mode = 'D'.".selection mode per field
  gs_layout-col_opt = 'X'.".selection mode per field

  gs_disvariant-report = sy-repid.
  gs_disvariant-handle = 'C'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_show_alv_pop
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_alv_pop
  TABLES
    pt_pop TYPE STANDARD TABLE.

  CHECK pt_pop[] IS NOT INITIAL.

*  DATA: lt_event_exit TYPE slis_t_event_exit,
*        ls_event_exit LIKE LINE OF lt_event_exit.

  PERFORM alv_add_events USING:
    'PF_STATUS_SET',
    'USER_COMMAND'.

  CLEAR gv_exit_caused_by_user.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program     = sy-repid
      is_layout_lvc          = gs_layout
      it_fieldcat_lvc        = gt_fieldcat_pop
      it_events              = gt_events
      i_save                 = 'A'
*     it_sort_lvc            = gt_sort
      is_variant             = gs_disvariant
*     it_excluding           = it_excluding
*     it_event_exit          = lt_event_exit
      i_screen_start_column  = 30
      i_screen_start_line    = 5
      i_screen_end_column    = 100
      i_screen_end_line      = 20
    IMPORTING
      es_exit_caused_by_user = gv_exit_caused_by_user
    TABLES
      t_outtab               = pt_pop
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

*  BREAK ey_cuijh..

  IF gv_exit_caused_by_user IS NOT INITIAL.
    gs_disvariant-handle = 'B'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat_pop
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat_pop
  TABLES
    pt_pop TYPE STANDARD TABLE.

  CHECK pt_pop[] IS NOT INITIAL.

  CLEAR:gs_fieldcat, gt_fieldcat_pop[],
         gs_fieldcat_alv, gt_fieldcat_alv[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
*     i_internal_tabname     = 'GS_BOM'
      i_structure_name       = 'ZSPP_228_IDNRKLIST'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = gt_fieldcat_alv
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here

    PERFORM frm_leave_process.
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = gt_fieldcat_alv
*     IT_SORT_ALV     =
*     IT_FILTER_ALV   =
*     IS_LAYOUT_ALV   =
    IMPORTING
      et_fieldcat_lvc = gt_fieldcat_pop
*     ET_SORT_LVC     =
*     ET_FILTER_LVC   =
*     ES_LAYOUT_LVC   =
    TABLES
      it_data         = pt_pop
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    PERFORM frm_leave_process.
  ENDIF.

*如果有必要就循环字段属性表来调整属性
  FIELD-SYMBOLS <fs_field> LIKE LINE OF gt_fieldcat.
*  gs_fieldcat-fieldname = 'ZYXJE'.

  LOOP AT gt_fieldcat_pop
   ASSIGNING <fs_field>.
    <fs_field>-col_opt = 'A'.
    <fs_field>-key = ''.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_call_mdm
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_call_mdm .

*  IF sy-uname NE 'CUIJUNHU'.
*
*    MESSAGE '开发中。。。'
*    TYPE 'I'
*    .
*
*    RETURN.
*
*  ENDIF.

  DATA lv_type TYPE bapi_mtype.

*MDM分类映射关系
  SELECT
    *
    INTO TABLE @DATA(lt_ztmm201)
    FROM ztmm_201.
  IF sy-subrc = 0.
    SORT lt_ztmm201 BY atnam.

  ENDIF.

  DATA ls_mara TYPE mara.

  BREAK cuijunhu.

  LOOP AT gt_bom
    ASSIGNING <fs_bom>
    WHERE ck = 'X'
    .

    IF <fs_bom>-idnrk NE ''.
      <fs_bom>-id = icon_led_red.
      <fs_bom>-message = '已存在组件料号，请勿重复创建'.

      CONTINUE.

    ENDIF.

    IF <fs_bom>-zpmid EQ ''.
      <fs_bom>-id = icon_led_red.
      <fs_bom>-message = '无法确定品名ID，无法创建'.

      CONTINUE.

    ENDIF.

*MDM接口函数
    ls_mara-matkl = <fs_bom>-idnrk_matkl.
    ls_mara-zzl1 = <fs_bom>-zpmid.
    ls_mara-zzl = <fs_bom>-zpmms.
    ls_mara-meins = <fs_bom>-meins.
*    ls_mara-ernam = sy-uname.
*    MOVE-CORRESPONDING <fs_bom>-char_t TO <fs_bom>-char_t.

    CALL FUNCTION 'ZFM_GP_MM_MDM_MATERIAL_SAVE'
      EXPORTING
        is_mara      = ls_mara
        it_auspconds = <fs_bom>-auspconds[]
      IMPORTING
        rtype        = lv_type
        rtmsg        = <fs_bom>-message
        rt_matnr     = <fs_bom>-idnrk
      TABLES
*       t_char       = <fs_bom>-zttpp_231_input
        t_ztmm201    = lt_ztmm201
*       T_MARM       =
      .

    IF lv_type = 'S'.
      <fs_bom>-id = icon_led_green.
      SELECT SINGLE
        maktx
        INTO <fs_bom>-idnrk_maktx
        FROM makt
        WHERE matnr = <fs_bom>-idnrk
          AND spras = '1'.

    ENDIF.

  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '请选择行'
    TYPE 'S'
    DISPLAY LIKE 'E'.

    RETURN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_crt_bom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_crt_bom .

  DATA : lt_messages TYPE TABLE OF messages.
  DATA:it_stpo TYPE TABLE OF stpo,
       wa_stpo LIKE LINE OF it_stpo.
  DATA:
    lv_message TYPE bapi_msg,
    lv_warning TYPE capiflag-flwarning,
    lv_bom_no  TYPE stko_api02-bom_no.

  DATA:o_stko      TYPE stko_api02,
       lt_stko     TYPE TABLE OF stko_api02,
       ls_stko     TYPE stko_api02,
       is_stko_add TYPE stko_api01,
*       lt_stpo_add TYPE TABLE OF stpo_api03,
*       ls_stpo     LIKE LINE OF lt_stpo_add,
       lt_stpo     TYPE STANDARD TABLE OF stpo_api02 WITH HEADER LINE.
  TYPES: BEGIN OF ty_stpo_add.
           INCLUDE TYPE stpo_api03.
  TYPES:   zclass TYPE stpo-zclass,
         END OF ty_stpo_add.
  DATA:lt_stpo_add TYPE TABLE OF ty_stpo_add WITH HEADER LINE,
       it_stpo_add TYPE TABLE OF ty_stpo_add WITH EMPTY KEY.
  DATA: lv_stlal TYPE csap_mbom-stlal,
        lv_usage TYPE csap_mbom-stlan,
        lv_datuv TYPE csap_mbom-datuv.

  DATA:lv_posnr TYPE stpo-posnr.

  DATA(lt_header) = gt_bom[].
  DELETE lt_header WHERE ck = ''.
  IF lt_header[] IS INITIAL.
    MESSAGE TEXT-013 "请选择行
    TYPE 'S'
    DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  SORT lt_header BY werks matnr matkl zxt.

  DELETE ADJACENT DUPLICATES FROM lt_header COMPARING werks matnr matkl stlal.

  lv_usage = '1'.
  lv_datuv = sy-datum.

  LOOP AT lt_header INTO DATA(ls_header).
    IF ls_header-stlnr IS INITIAL.
      SELECT SINGLE
        stlnr
        INTO ls_header-stlnr
        FROM mast
        WHERE matnr = ls_header-matnr
          AND werks = ls_header-werks
          AND stlan = lv_usage
          AND stlal = ls_header-stlal
        .
    ENDIF.

*    IF ls_header-stlnr IS NOT INITIAL.
*
*      gs_msg-msgid = 'ZWSDGP'.
*      gs_msg-msgno = '000'.
*      gs_msg-msgty = 'E'.
*      CONCATENATE ls_header-matnr ls_header-werks
*                  ls_header-stlal
*             INTO gs_msg-msgv1
*             SEPARATED BY space.
*      gs_msg-msgv2 = 'BOM已存在，请点击新增组件按钮'.
*
*      APPEND gs_msg TO gt_msg.
*      CLEAR gs_msg.
*
*      CONTINUE.
*
*    ENDIF.
    CLEAR:lt_stpo,lt_stpo[],lt_stko,is_stko_add,lt_stpo_add[].
*&读取bom原始数据
    CALL FUNCTION 'CSAP_MAT_BOM_READ'
      EXPORTING
        material    = ls_header-matnr
        plant       = ls_header-werks
        bom_usage   = lv_usage
        alternative = ls_header-stlal
      TABLES
        t_stpo      = lt_stpo
        t_stko      = lt_stko
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc EQ 0.
      READ TABLE lt_stko INTO ls_stko INDEX 1.
      MOVE-CORRESPONDING ls_stko TO is_stko_add.
      SELECT
      SINGLE
      MAX( stpo~posnr )
      FROM mast
      INNER JOIN stas ON stas~stlnr = mast~stlnr AND stas~stlal = mast~stlal AND stas~stlty = 'M'
      INNER JOIN stpo ON stpo~stlkn = stas~stlkn AND stpo~stlnr = stas~stlnr AND stpo~stlty = 'M'
      WHERE mast~werks = @ls_header-werks
        AND mast~matnr = @ls_header-matnr
        AND mast~stlan = @lv_usage
        AND mast~stlal = @ls_header-stlal
      INTO @lv_posnr.
    ELSE.
      is_stko_add-bom_status = '01'.
    ENDIF.


    lv_stlal = ls_header-stlal.

    LOOP AT gt_bom INTO gs_bom WHERE werks = ls_header-werks
                                 AND matnr = ls_header-matnr
                                 AND matkl = ls_header-matkl
                                 AND stlal = lv_stlal
                                 AND ck = 'X'
      .
      lv_posnr = lv_posnr + 10.
      CLEAR lt_stpo_add.
      lt_stpo_add-item_categ = 'L'.
      lt_stpo_add-item_no = lv_posnr.
      lt_stpo_add-component = gs_bom-idnrk.
      lt_stpo_add-comp_qty = gs_bom-zyl.
      lt_stpo_add-comp_unit = gs_bom-meins.
      lt_stpo_add-rel_prod = abap_true."与生产相关
      lt_stpo_add-rec_allowd = abap_true."递归
      lt_stpo_add-recursive = abap_true."递归
      "ls_stpo-rel_cost = abap_true."成本核算
      lt_stpo_add-zclass = gs_bom-zclass.

      APPEND lt_stpo_add.
      CLEAR lt_stpo_add.

    ENDLOOP.

    "记录消息日志
    CALL FUNCTION 'CALO_INIT_API'
      EXPORTING
        flag_db_log_on           = 'X'
        flag_msg_on              = 'X'
        flag_api_api_call_on     = ' '
        flag_collect_msg_on      = ' '
        external_log_no          = 'API'
        del_log_after_days       = '10'
        data_reset_sign          = '!'
      EXCEPTIONS
        log_object_not_found     = 1
        log_sub_object_not_found = 2
        OTHERS                   = 3.
    " 创建bom
    DATA: lv_bomcreate TYPE csdata-xfeld,
          lv_complete  TYPE csdata-xfeld,
          lv_commit    TYPE capiflag-comm_wait.

    DATA: fl_warning TYPE  capiflag-flwarning.

    lv_bomcreate = 'X'.
    lv_complete = 'X'.
    lv_commit = 'X'.

    DATA:wa_memoenh TYPE zsenh_001.
    wa_memoenh-dzbs = 'MEMOXMD_CREATEBOM'.
    wa_memoenh-flag = 'X'.
    EXPORT memowa = wa_memoenh TO MEMORY ID 'MEMOXMD_CREATEBOM'.

    CLEAR: fl_warning, o_stko.

    CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
      EXPORTING
        material           = ls_header-matnr
        plant              = ls_header-werks
        bom_usage          = lv_usage
        alternative        = lv_stlal
        valid_from         = lv_datuv
        i_stko             = is_stko_add
        fl_commit_and_wait = lv_commit
        fl_bom_create      = lv_bomcreate
        fl_new_item        = 'X'
        fl_complete        = lv_complete
        fl_default_values  = 'X'
*       FL_RECURSIVE       = 'X'
      IMPORTING
        fl_warning         = fl_warning
        o_stko             = o_stko
      TABLES
        t_stpo             = lt_stpo_add
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      "拷贝日志
      CALL FUNCTION 'CALO_MSG_APPEND_DB_LOG'
        EXCEPTIONS
          log_object_not_found    = 1
          log_subobject_not_found = 2
          log_internal_error      = 3
          OTHERS                  = 4.
      "读取日志
      CALL FUNCTION 'CALO_LOG_READ_MESSAGES'
        TABLES
          messages_and_parameters = lt_messages
        EXCEPTIONS
          OTHERS                  = 1.
      CLEAR lv_message .
      LOOP AT lt_messages INTO DATA(ls_message) WHERE msg_type CA 'AEX' .
        lv_message = lv_message && ls_message-msg_txt.
      ENDLOOP.

      IF lv_message NE ''.
        gs_msg-msgty = 'E'.

      ENDIF.
    ELSE .
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
      IF o_stko-bom_no IS INITIAL.
        DO 10 TIMES.
          SELECT SINGLE
            stlnr
            INTO o_stko-bom_no
            FROM mast
            WHERE matnr = ls_header-matnr
              AND werks = ls_header-werks
              AND stlan = lv_usage
              AND stlal = lv_stlal
            .
          IF sy-subrc EQ 0.
            EXIT.
          ELSE.
            WAIT UP TO '0.1' SECONDS .
          ENDIF.
        ENDDO.

      ENDIF.
      lv_message = '创建成功:' &&  o_stko-bom_no .
      gs_msg-msgty = 'S'.
      ls_header-stlnr = o_stko-bom_no.
      REFRESH it_stpo.
      it_stpo_add = CORRESPONDING #( lt_stpo_add[] ).
      LOOP AT it_stpo_add INTO DATA(wa_stpo_add).
        wa_stpo_add-item_no = |{ wa_stpo_add-item_no ALPHA = IN }|.
        MODIFY it_stpo_add FROM wa_stpo_add.
      ENDLOOP.
      SELECT a~*
      FROM stpo AS a
      INNER JOIN stas ON stas~stlkn = a~stlkn AND stas~stlnr = a~stlnr AND stas~stlty = 'M'
      INNER JOIN @it_stpo_add AS c ON a~idnrk = c~component AND a~posnr = c~item_no
      WHERE stas~stlnr = @ls_header-stlnr AND stas~stlal = @lv_stlal
      INTO TABLE @it_stpo.
      LOOP AT it_stpo INTO wa_stpo.
        READ TABLE it_stpo_add INTO wa_stpo_add WITH KEY component = wa_stpo-idnrk item_no = wa_stpo-posnr.
        IF sy-subrc EQ 0.
          wa_stpo-zclass = wa_stpo_add-zclass.
          MODIFY it_stpo FROM wa_stpo.
        ENDIF.
      ENDLOOP.
      MODIFY stpo FROM TABLE it_stpo.
      COMMIT WORK.
*    LOOP AT gt_bom
*      INTO gs_bom
*      WHERE werks = ls_header-werks
*        AND matnr = ls_header-matnr
*        AND matkl = ls_header-matkl
*        AND stlal = lv_stlal
*        AND ck = 'X'
*      .

      ls_header-message = lv_message.
      MODIFY gt_bom
      FROM ls_header
      TRANSPORTING message stlnr
      WHERE werks = ls_header-werks
        AND matnr = ls_header-matnr
        AND matkl = ls_header-matkl
        AND stlal = lv_stlal
        AND ck = 'X'
        .

    ENDIF.

*
    CONCATENATE ls_header-matnr ls_header-werks
                lv_stlal
                lv_message
           INTO lv_message
           SEPARATED BY space.

    gs_msg-msgid = 'ZWSDGP'.
    gs_msg-msgno = '000'.
    gs_msg-msgv1 = lv_message(50).
    gs_msg-msgv2 = lv_message+50(50).
    gs_msg-msgv3 = lv_message+100(50).
    gs_msg-msgv4 = lv_message+150(50).

    APPEND gs_msg TO gt_msg.
    CLEAR gs_msg.

    CLEAR: lt_messages[], lt_stpo[], ls_stko,
           lv_warning, lv_bom_no, lv_posnr.

  ENDLOOP.


  CHECK gt_msg[] IS NOT INITIAL.
  DATA e_s_exit_command TYPE bal_s_excm.

  CALL FUNCTION 'RSDC_SHOW_MESSAGES_POPUP'
    EXPORTING
      i_t_msg           = gt_msg
      i_txt             = '消息'
      i_with_s_on_empty = ''
      i_one_msg_direct  = ''
      i_one_msg_type_s  = ''
    IMPORTING
      e_s_exit_command  = e_s_exit_command.

  CLEAR gt_msg[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_insert
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_insert .

  DATA(lt_sel) = gt_bom[].

  DELETE lt_sel WHERE ck = ''.
  IF lt_sel[] IS INITIAL.

    MESSAGE '请选择行'
    TYPE 'S'
    DISPLAY LIKE 'E'.

    RETURN.

  ENDIF.

  DESCRIBE TABLE lt_sel LINES DATA(lv_line).

  IF lv_line > 1.
    MESSAGE '只能选择一行'
    TYPE 'S'
    DISPLAY LIKE 'E'.

    RETURN.

  ENDIF.


  READ TABLE lt_sel INTO gs_bom INDEX 1.

  APPEND gs_bom TO gt_bom.
  CLEAR gs_bom.
  SORT gt_bom BY werks matnr matkl zxt zindex idnrk.

  CLEAR lt_sel[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_delete_line
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_delete_line .

  READ TABLE gt_bom
  TRANSPORTING NO FIELDS
  WITH KEY ck = 'X'.
  IF sy-subrc NE 0.
    MESSAGE '请选择行'
    TYPE 'S'
    DISPLAY LIKE 'E'.

    RETURN.

  ENDIF.

  DATA lv_ans TYPE c.

*决定是否真的删除
  PERFORM frm_get_popup
  CHANGING lv_ans.

  CHECK lv_ans = 'J'.

  DELETE gt_bom WHERE ck = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_crt_bom2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_crt_bom2 .

  DATA:gt_bomgroup          TYPE bapi1080_bgr_c OCCURS 0 WITH HEADER LINE, "
       gt_variants          TYPE bapi1080_bom_c OCCURS 0 WITH HEADER LINE,  "
       gt_items             TYPE bapi1080_itm_c OCCURS 0 WITH HEADER LINE,    "
       gt_subitems          TYPE bapi1080_itm_c OCCURS 0 WITH HEADER LINE,
       gt_materialrelations TYPE bapi1080_mbm_c OCCURS 0 WITH HEADER LINE,
       gt_itemassignments   TYPE bapi1080_rel_itm_bom_c OCCURS 0 WITH HEADER LINE, "
       gt_return            TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
       gs_return            TYPE bapiret2,
*        texts             TYPE bapi1080_txt_c OCCURS 0 WITH HEADER LINE.
       gw_subitemas         TYPE bapi1080_rel_sui_itm_c,
       gt_subitemas         TYPE TABLE OF bapi1080_rel_sui_itm_c.
  DATA:item_no    TYPE i,
       lv_check   TYPE char1,
       lv_message TYPE char200,
       lv_text    TYPE char120 VALUE ''.



  DATA(lt_header) = gt_bom[].
  DELETE lt_header
  WHERE ck = ''.
  IF lt_header[] IS INITIAL.
    MESSAGE TEXT-013 "请选择行
    TYPE 'S'
    DISPLAY LIKE 'E'.

    RETURN.

  ENDIF.

  SORT lt_header BY werks
                    matnr
                    matkl
                    zxt
                    .

  DELETE ADJACENT DUPLICATES FROM lt_header
  COMPARING werks matnr matkl stlal.

*  lv_usage = '1'.
*  lv_datuv = sy-datum.
  DATA lv_posnr TYPE n LENGTH 4.

  SELECT
    matnr,
    meins
    INTO TABLE @DATA(lt_mara)
    FROM mara
    FOR ALL ENTRIES IN @lt_header
    WHERE matnr = @lt_header-matnr
    .
  IF sy-subrc = 0.
    SORT lt_mara BY matnr.

  ENDIF.

  LOOP AT lt_header INTO DATA(ls_header).
    AT END OF stlal.
      DATA(lv_end) = 'X'.
    ENDAT.
*    lv_stlal = ls_header-stlal.

    CLEAR: item_no, lv_posnr.
    LOOP AT gt_bom
      INTO gs_bom
      WHERE werks = ls_header-werks
        AND matnr = ls_header-matnr
        AND matkl = ls_header-matkl
        AND stlal = ls_header-stlal
        AND ck = 'X'
      .
      lv_posnr = lv_posnr + 10.
*
*      ls_stpo-item_categ = 'L'.
*      ls_stpo-item_no = lv_posnr.
*      ls_stpo-component = gs_bom-idnrk.
*      ls_stpo-comp_qty = gs_bom-zyl.
*      ls_stpo-comp_unit = gs_bom-meins.
*
*      APPEND ls_stpo TO lt_stpo.
*      CLEAR ls_stpo.

      "取父物料的子件信息
      item_no = item_no + 1.
      CLEAR gt_items.
      gt_items-bom_group_identification = 'BAPI_SMP_COL1'.  "可以为空
      gt_items-object_type = 'ITM'.
      gt_items-object_id = item_no.
      gt_items-item_no = lv_posnr."项目
      gt_items-item_cat = 'L'."项目类别
      gt_items-component_long = gs_bom-idnrk."组件编码
      gt_items-comp_qty = gs_bom-zyl."组件数量
*gt_items-alt_item_group = gs_data-alpgr."项目替代组
      gt_items-prod_rel = 'X'."标识：与生产相关项目
      gt_items-cost_rel = ''."成本核算相关项的标识符


      gt_items-comp_unit = gs_bom-meins."组件计量单位
*      gt_items-item_text1 = lv_text+0(40)."项目长文本
*      gt_items-item_text2 = lv_text+40(80).
      gt_items-ltxt_lang = sy-langu.  "这个参数一定要填
      APPEND gt_items.
      CLEAR gt_itemassignments.
      gt_itemassignments-bom_group_identification = 'BAPI_SMP_COL1'. " 可以为空
      gt_itemassignments-sub_object_type = 'ITM'.
      gt_itemassignments-sub_object_id = item_no.
      gt_itemassignments-super_object_type = 'BOM'.
      gt_itemassignments-super_object_id = 'SIMPLE1'.
      gt_itemassignments-valid_from_date = sy-datum.
      gt_itemassignments-function = 'NEW'.
      APPEND gt_itemassignments.

    ENDLOOP.

    IF lv_end = 'X'.

      CLEAR lv_end.
*    "         HEADER DETAILS OF THE DIFFERENT gt_variants
      CLEAR gt_variants.
      gt_variants-bom_group_identification = 'BAPI_SMP_COL1'.
      gt_variants-object_type = 'BOM'.
      gt_variants-object_id = 'SIMPLE1'.
      gt_variants-alternative_bom = ls_header-stlal."备选物料清单 此处要填两位数字，不够补零
      gt_variants-bom_status = '01'.  "BOM状态
      gt_variants-base_qty = '1'."基本数量
      READ TABLE lt_mara
      INTO DATA(ls_mara)
      WITH KEY matnr = ls_header-matnr
      BINARY SEARCH.
      IF sy-subrc = 0.
        gt_variants-base_unit = ls_mara-meins.

      ENDIF.
      "    gt_variants-base_unit = lt_data-bmein."计量单位
      gt_variants-valid_from_date = sy-datum."生效日期
      "    gt_variants-alt_text = lt_data-stktx."可选文本
      gt_variants-ltxt_lang = sy-langu.
      gt_variants-langu_iso = sy-langu.
      gt_variants-function = 'NEW'.  "NEW表示新建.
      APPEND gt_variants.

      CLEAR gt_materialrelations.
      gt_materialrelations-bom_group_identification = 'BAPI_SMP_COL1'.  "可以为空
      gt_materialrelations-material_long = ls_header-matnr.  "物料号
      gt_materialrelations-plant = ls_header-werks."工厂
      gt_materialrelations-bom_usage = '1'."BOM用途
      gt_materialrelations-alternative_bom = ls_header-stlal."可选的BOM 此处要填两位数字，不够补零
      APPEND gt_materialrelations.



      "以父物料汇总
      CLEAR gt_bomgroup.
      gt_bomgroup-bom_group_identification = 'BAPI_SMP_COL1'.  "可以为空
      gt_bomgroup-object_type = 'BGR'.
      gt_bomgroup-object_id =  'SIMPLE1'..
      gt_bomgroup-bom_usage = '1'. "用途
      gt_bomgroup-ltxt_lang = sy-langu.
      gt_bomgroup-technical_type = 'M'. "多重BOM
      gt_bomgroup-bom_group = ''.  "可以为空
      gt_bomgroup-created_in_plant = ls_header-werks."工厂
      "    gt_bomgroup-bom_text = lt_data-bom_text."BOM文本
      APPEND gt_bomgroup.

      "BOM操作
      CALL FUNCTION 'BAPI_MATERIAL_BOM_GROUP_CREATE'
        EXPORTING
*         TESTRUN           = 'X'
          all_error         = 'X'
        TABLES
          bomgroup          = gt_bomgroup
          variants          = gt_variants
          items             = gt_items
          materialrelations = gt_materialrelations
          itemassignments   = gt_itemassignments
*         subitemassignments = gt_subitemas
          return            = gt_return.
      lv_check = 'S'.

      LOOP AT gt_return INTO gs_return WHERE type = 'A' OR type = 'E'.

        IF lv_check = 'S'.
          lv_check = 'E'.
          gs_msg-msgty = 'E'.
          gs_msg-msgid = 'ZWSDGP'.
          gs_msg-msgno = '000'.
          CONCATENATE ls_header-matnr ls_header-werks
                      '备选' ls_header-stlal
                      INTO gs_msg-msgv1
                      SEPARATED BY space.


          gs_msg-msgv2 = 'BOM创建失败'.
          APPEND gs_msg TO gt_msg.
          CLEAR gs_msg.
        ENDIF.

*        lv_message = lv_message && gs_return-message.
        gs_msg-msgty = 'E'.
        gs_msg-msgid = 'ZWSDGP'.
        gs_msg-msgno = '000'.
        gs_msg-msgv1 = gs_return-message(50).
        gs_msg-msgv2 = gs_return-message+50(50).
        gs_msg-msgv3 = gs_return-message+100(50).
        gs_msg-msgv4 = gs_return-message+150(50).

        APPEND gs_msg TO gt_msg.
        CLEAR gs_msg.

      ENDLOOP.
      IF lv_check = 'S'.
*        LOOP AT gt_return INTO gs_return.
        gs_msg-msgty = lv_check.
        gs_msg-msgid = 'ZWSDGP'.
        gs_msg-msgno = '000'.
        CONCATENATE ls_header-matnr ls_header-werks
                    '备选' ls_header-stlal
                    INTO gs_msg-msgv1
                    SEPARATED BY space.
*          gs_msg-msgv1 = gs_return-message(50).
        gs_msg-msgv2 = 'BOM创建成功'.
        gs_msg-msgv3 = gs_return-message+100(50).
        gs_msg-msgv4 = gs_return-message+150(50).

        APPEND gs_msg TO gt_msg.
        CLEAR gs_msg.

*        ENDLOOP.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
*  WRITE:GV_NUMBER.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

    ENDIF.

    CLEAR: gt_return[], gt_bomgroup[