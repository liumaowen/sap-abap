*&---------------------------------------------------------------------*
*& Report ZFIR204
*&---------------------------------------------------------------------*
*& 长期股权投资报表
*&---------------------------------------------------------------------*
REPORT zfir204.

TABLES:sscrfields,t001l,afko,acdoca.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_seg,
        name    TYPE fagl_segmt-name,
        segment TYPE fagl_segm-segment,
      END OF ty_seg.
DATA:gt_seg TYPE TABLE OF ty_seg WITH KEY name segment,
     gs_seg LIKE LINE OF gt_seg.
DATA:BEGIN OF itab OCCURS 0,
       name     TYPE fagl_segmt-name,
       segment  TYPE fagl_segm-segment,
       vsl_3000 TYPE acdoca-vsl,
       vsl_3010 TYPE acdoca-vsl,
       vsl_3020 TYPE acdoca-vsl,
       vsl_3050 TYPE acdoca-vsl,
       vsl_3060 TYPE acdoca-vsl,
       vsl_3062 TYPE acdoca-vsl,
       vsl_3080 TYPE acdoca-vsl,
       vsl_3090 TYPE acdoca-vsl,
       vsl_3100 TYPE acdoca-vsl,
       vsl_3110 TYPE acdoca-vsl,
       vsl_3130 TYPE acdoca-vsl,
       vsl_3200 TYPE acdoca-vsl,
       vsl_3210 TYPE acdoca-vsl,
       vsl_3230 TYPE acdoca-vsl,
       vsl_3980 TYPE acdoca-vsl,
       vsl_3990 TYPE acdoca-vsl,
       sel,
     END OF itab.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS:
    p_gjahr TYPE acdoca-gjahr DEFAULT sy-datum+0(4) OBLIGATORY, "财年
    p_poper TYPE acdoca-poper DEFAULT sy-datum+4(2) OBLIGATORY. "期间
  SELECT-OPTIONS:s_rbukrs FOR acdoca-rbukrs."公司代码
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '功能选择'.
  %_p_gjahr_%_app_%-text = '年度'.
  %_p_poper_%_app_%-text = '期间'.
  %_s_rbukrs_%_app_%-text = '公司'.


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
  DATA:lv_field TYPE char10.
  REFRESH itab.

  SELECT
      b~name,
      a~segment
  FROM fagl_segm AS a
  INNER JOIN fagl_segmt AS b ON  a~segment = b~segment
                             AND b~langu   = @sy-langu
  WHERE a~segment LIKE '0000003%'
  ORDER BY a~segment
  INTO TABLE @gt_seg.

  SELECT
    a~segment,
    a~psegment,
    SUM( a~vsl ) AS vsl
  FROM acdoca AS a
  INNER JOIN @gt_seg AS b ON a~segment = b~segment
  WHERE a~gjahr = @p_gjahr
  AND a~poper = @p_poper
  AND a~rbukrs IN @s_rbukrs
  AND racct = '1511010000'
  GROUP BY a~segment,a~psegment
  ORDER BY a~segment,a~psegment
  INTO TABLE @DATA(lt_acdoca).

  LOOP AT gt_seg INTO gs_seg.
    CLEAR:itab.
    itab-name = gs_seg-name.
    itab-segment = gs_seg-segment.
    LOOP AT gt_seg INTO gs_seg.
    ENDLOOP.
    LOOP AT lt_acdoca INTO DATA(ls_acdoca) WHERE segment = itab-segment.
      lv_field = 'VSL_' && |{ ls_acdoca-psegment ALPHA = OUT }|.
      ASSIGN COMPONENT lv_field OF STRUCTURE itab TO FIELD-SYMBOL(<fs>).
      IF <fs> IS ASSIGNED.
        <fs> = ls_acdoca-vsl.
      ENDIF.
    ENDLOOP.
    APPEND itab.
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
  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."控制用户布局

  PERFORM catlg_set TABLES fldct
                    USING:
'NAME '  '产业公司'  'FAGL_SEGMT'  'NAME',
'VSL_3000'  '山东围护'  'ACDOCA'  'VSL',
'VSL_3010'  '山东钢构'  'ACDOCA'  'VSL',
'VSL_3020'  '山东洁净'  'ACDOCA'  'VSL',
'VSL_3050'  '北京围护'  'ACDOCA'  'VSL',
'VSL_3060'  '江苏围护'  'ACDOCA'  'VSL',
'VSL_3062'  '江苏洁净'  'ACDOCA'  'VSL',
'VSL_3080'  '厂景科技'  'ACDOCA'  'VSL',
'VSL_3090'  '山东冷链'  'ACDOCA'  'VSL',
'VSL_3100'  '西南洁净'  'ACDOCA'  'VSL',
'VSL_3110'  '山东得默'  'ACDOCA'  'VSL',
'VSL_3130'  '江苏电子'  'ACDOCA'  'VSL',
'VSL_3200'  '广东围护'  'ACDOCA'  'VSL',
'VSL_3210'  '马来钢构'  'ACDOCA'  'VSL',
'VSL_3230'  '美国洁净'  'ACDOCA'  'VSL',
'VSL_3980'  '工业板块'  'ACDOCA'  'VSL',
'VSL_3990'  '钢品股份'  'ACDOCA'  'VSL'.

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
  E