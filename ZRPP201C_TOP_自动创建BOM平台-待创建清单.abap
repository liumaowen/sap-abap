*&---------------------------------------------------------------------*
*& 包含               ZRPP201_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields.

TABLES: t023, marc, vbak.
*&---------------------------------------------------------------------*
*&全局变量
*&---------------------------------------------------------------------*
CONSTANTS gv_sec TYPE p DECIMALS 2 VALUE '0.5'.

*输出ALV - 无BOM成品清单
DATA: BEGIN OF gs_list,
        ck        TYPE c,
        message   LIKE bapireturn-message,

        werks     LIKE t001w-werks,
        werks_txt LIKE t001w-name1,
        vbeln     LIKE vbap-vbeln,
        posnr     LIKE vbap-posnr,
        kunnr     LIKE vbak-kunnr,
        kunnr_txt LIKE kna1-name1,
        matnr     LIKE vbap-matnr,
        maktx     LIKE makt-maktx,
        erdat     LIKE vbak-erdat,
        erzet     LIKE vbak-erzet,
        cj_zhijin TYPE i,
        vdatu     LIKE vbak-vdatu,
        shengyujh TYPE i,
        matkl     LIKE mara-matkl,
        wgbez     LIKE t023t-wgbez,
        zhtly_txt LIKE kna1-name1,
        zywy_txt  LIKE kna1-name1,

*        zlogic    LIKE ztpp_260a4-zlogic,
        kukla     TYPE kukla,
      END OF gs_list,
      gt_list LIKE TABLE OF gs_list.

*输出ALV - 待创建BOM组件清单
DATA: BEGIN OF gs_bom,
        ck          TYPE c,
        bom         TYPE c,
        isstlal     TYPE c,
        id          LIKE icon-id,
        message     LIKE bapireturn-message,

        stlnr       LIKE stko-stlnr,
        werks       LIKE t001w-werks,
        matnr       LIKE vbap-matnr,
        maktx       LIKE makt-maktx,
        maktx1(128) TYPE c,
        maktx2(128) TYPE c,
        matkl       LIKE mara-matkl,
        wgbez       LIKE t023t-wgbez,
        stlal       LIKE stko-stlal,
        zxt         LIKE zspp_228_retdata_main-zxt,
        zindex      LIKE zspp_228_retdata_main-zindex,
        zclass      LIKE zspp_228_retdata_main-zclass,
        zpmid       LIKE zspp_228_retdata_main-zpmid,
        zpmms       LIKE zspp_228_retdata_main-zpmms,


        idnrk       LIKE zspp_228_retdata_main-idnrk,
        idnrk_maktx LIKE makt-maktx,

        idnrk_matkl LIKE zspp_228_retdata_main-idnrk_matkl,
        zyl         LIKE zspp_228_retdata_main-zyl,
        meins       LIKE stpo-meins,
        mseht       LIKE t006a-mseht,

        idnrklistid LIKE icon-id,
        idnrklist   TYPE zttpp_228_idnrklist,
*        char_t      TYPE char_allocation_tt,
        auspconds   TYPE zttpp_231_input,
        clfed       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
        clr         TYPE char4,

      END OF gs_bom,
      gt_bom   LIKE TABLE OF gs_bom,
      gt_bom_a LIKE TABLE OF gs_bom,
      wa_bom_a LIKE LINE OF gt_bom_a.


*消息弹窗
DATA: gt_msg TYPE rs_t_msg,
      gs_msg LIKE LINE OF gt_msg.

DATA gv_zlogic LIKE ztpp_260a4-zlogic.

**输出ALV - 弹窗显示组件清单
*DATA: BEGIN OF gs_pop,
**        ck          TYPE c,
**        id          LIKE icon-id,
**        message     LIKE bapireturn-message,
*
*        idnrk LIKE zspp_228_retdata_main-idnrk,
*        maktx LIKE makt-maktx,
*
*      END OF gs_pop,
*      gt_pop LIKE TABLE OF gs_pop.

FIELD-SYMBOLS <fs_line> LIKE LINE OF gt_list.
FIELD-SYMBOLS <fs_bom> LIKE LINE OF gt_bom.
FIELD-SYMBOLS <fs_list> TYPE STANDARD TABLE.

DATA  values_tab TYPE TABLE OF dd07v.

DATA: gs_style TYPE lvc_s_styl,
      gt_style LIKE TABLE OF gs_style.

CONSTANTS:tcode1 TYPE sy-tcode VALUE 'TCODE1',
          tcode2 TYPE sy-tcode VALUE 'TCODE2',
          tcode3 TYPE sy-tcode VALUE 'TCODE3'.

*&---------------------------------------------------------------------*
*&BDC
*&---------------------------------------------------------------------*
DATA: v_ctu_params TYPE ctu_params,
      bdcdata      LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
      messtab      LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& ALV 控件
*&---------------------------------------------------------------------*
DATA: gs_layout       TYPE lvc_s_layo, "布局结构定义
      gs_disvariant   TYPE disvariant,
      gt_sort         TYPE lvc_t_sort, "LVC 控件字段属性结构定义
      gs_sort         LIKE LINE OF gt_sort,
      gt_fieldcat     TYPE lvc_t_fcat, "LVC 控件字段属性结构定义
      gt_fieldcat_bom TYPE lvc_t_fcat, "LVC 控件字段属性结构定义
      gt_fieldcat_pop TYPE lvc_t_fcat, "LVC 控件字段属性结构定义
      gs_fieldcat     LIKE LINE OF gt_fieldcat,
      gt_fieldcat_alv TYPE slis_t_fieldcat_alv, "非LVC 控件字段属性结构定义
      gs_fieldcat_alv LIKE LINE OF gt_fieldcat_alv
      .

DATA: BEGIN OF gs_ctr,
        strname TYPE slis_tabname,
        tabname TYPE slis_tabname,
      END OF gs_ctr.

DATA: gt_events TYPE  slis_t_event,
      gs_events TYPE  slis_alv_event.

DATA: g_container        TYPE scrfname VALUE 'RESULT_LIST',
      alv_grid           TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container.


DATA: gv_bdate TYPE sy-datum,
      gv_edate TYPE sy-datum.

DATA gv_exit_caused_by_user TYPE slis_exit_by_user.

DATA: g_con01  TYPE REF TO cl_gui_custom_container.
DATA: g_grid01 TYPE REF TO cl_gui_alv_grid.
CONSTANTS: g_cus01 TYPE scrfname VALUE 'CONT'.

DATA: gs_layout02   TYPE lvc_s_layo, "布局结构定义
      gt_fieldcat02 TYPE lvc_t_fcat, "LVC 控件字段属性结构定义
      gs_fieldcat02 LIKE LINE OF gt_fieldcat02.

DATA gv_con_diabox TYPE REF TO cl_gui_dialogbox_container. "定义容器
DATA gv_alv_popup TYPE REF TO cl_gui_alv_grid.             "定义弹出的ALV

*定义事件：

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_close
        FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.
    METHODS handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        er_data_changed
        e_onf4
        e_onf4_before
        e_onf4_after
        e_ucomm
      .
    METHODS handle_data_changed_fini
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        e_modified
        et_good_cells
      .
ENDCLASS.


DATA gv_event_receiver TYPE REF TO lcl_event_receiver.

TYPES:BEGIN OF ty_bomparams,
        werks TYPE werks_d,
        matnr TYPE matnr,
        del   TYPE char1, "此物料号整个BOM是否要排除的
      END OF ty_bomparams.
TYPES:BEGIN OF ty_bomdet,
        werks TYPE werks_d,
        matnr TYPE matnr,
        stlal TYPE stlal,
        stlnr TYPE mast-stlnr,
        del   TYPE char1, "BOM中某个备选是否要排除的
      END OF ty_bomdet.
DATA:it_bomparams TYPE TABLE OF ty_bomparams WITH KEY werks matnr del,
     wa_bomparams LIKE LINE OF it_bomparams,
     it_bomdet    TYPE TABLE OF ty_bomdet WITH KEY werks matnr stlal stlnr del,
     wa_bomdet    LIKE LINE OF it_bomdet.
DATA:BEGIN OF it_bomold OCCURS 0,
       werks TYPE mast-werks,
       matnr TYPE mast-matnr,
       stlal TYPE mast-stlal,
       stlnr TYPE mast-stlnr,
       stlty TYPE stpo-stlty,
       stlkn TYPE stpo-stlkn,
       stpoz TYPE stpo-stpoz,
       posnr TYPE stpo-posnr,
       idnrk TYPE stpo-idnrk,
     END OF it_bomold,
     wa_bomold LIKE LINE OF it_bomold.

TYPES: BEGIN OF ty_vbap,
         vbeln TYPE vbap-vbeln,
         posnr TYPE vbap-posnr,
         werks TYPE vbap-werks,
         matnr TYPE vbap-matnr,
         kunnr TYPE vbak-kunnr,
         vdatu TYPE vbak-vdatu,
         erdat TYPE vbak-erdat,
         erzet TYPE vbak-erzet,
         zywy  TYPE vbak-zywy,
         matkl TYPE mara-matkl,
         maktx TYPE makt-maktx,
         name1 TYPE kna1-name1,
         zhtly 