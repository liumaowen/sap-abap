*&---------------------------------------------------------------------*
*& Report ZFIR204A
*&---------------------------------------------------------------------*
*& �ڲ��������׵�����
*&---------------------------------------------------------------------*
REPORT zfir204a.

TABLES:acdoca.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_seg,
        segment TYPE fagl_segm-segment,
      END OF ty_seg.
DATA:gt_seg TYPE TABLE OF ty_seg WITH KEY segment,
     gs_seg LIKE LINE OF gt_seg.
DATA:BEGIN OF itab OCCURS 0,
       month     TYPE kmonth,         "����
       segment   TYPE fagl_segm-segment,
       segmentl  TYPE txt50,
       psegment  TYPE acdoca-psegment,
       psegmentl TYPE txt50,
       csl       TYPE acdoca-csl,   "������
       zcbje     TYPE fins_vccur12, "�ɱ����
       zchwsxje  TYPE fins_vccur12, "���δʵ�ֽ��
       sel,
     END OF itab.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS:
    p_gjahr TYPE acdoca-gjahr DEFAULT sy-datum+0(4) OBLIGATORY, "����
    p_poper TYPE acdoca-poper DEFAULT sy-datum+4(2) OBLIGATORY. "�ڼ�
  SELECT-OPTIONS:s_segmen FOR acdoca-segment."��˾����
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '����ѡ��'.
  %_p_gjahr_%_app_%-text = '���'.
  %_p_poper_%_app_%-text = '�ڼ�'.
  %_s_segmen_%_app_%-text = '��ҵ��˾'.


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
  REFRESH: itab,gt_seg.


  SELECT
    a~segment,
    a~psegment,
    SUM( a~csl ) AS csl
  FROM acdoca AS a
  WHERE a~gjahr = @p_gjahr
  AND a~poper = @p_poper
  AND a~segment IN @s_segmen
  AND racct = '6001010000'
  AND kunnr LIKE '000090%'
  GROUP BY a~segment,a~psegment
  ORDER BY a~segment,a~psegment
  INTO CORRESPONDING FIELDS OF TABLE @itab.

  LOOP AT itab.
    gs_seg-segment = itab-segment.
    COLLECT gs_seg INTO gt_seg.
    gs_seg-segment = itab-psegment.
    COLLECT gs_seg INTO gt_seg.
  ENDLOOP.

  SELECT
      a~name,
      a~segment
  FROM fagl_segmt AS a
  INNER JOIN @gt_seg AS b ON  a~segment = b~segment
  WHERE a~langu   = @sy-langu
  ORDER BY a~segment
  INTO TABLE @DATA(lt_segt).

  LOOP AT itab.
    itab-month = p_gjahr && p_poper+1(2).
    itab-zcbje = itab-csl.
    READ TABLE lt_segt INTO DATA(ls_segt) WITH KEY segment = itab-segment BINARY SEARCH.
    IF sy-subrc = 0.
      itab-segmentl = ls_segt-name.
    ENDIF.
    READ TABLE lt_segt INTO ls_segt WITH KEY segment = itab-psegment BINARY SEARCH.
    IF sy-subrc = 0.
      itab-psegmentl = ls_segt-name.
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
  slayt-colwidth_optimize = 'X'. "  colwidth_optimize
  slayt-zebra             = 'X'.
  slayt-box_fieldname     = 'SEL'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1."�����û�����

  PERFORM catlg_set TABLES fldct
                    USING:
'MONTH     '  '����      '      'ISELLIST  '    'MONTH   ',
'SEGMENT   '  '������    '      'FAGL_SEGMT'    'SEGMENT',
'SEGMENTL  '  '����������'      'FAGL_SEGMT'    'NAME    ',
'PSEGMENT  '  '�ջ���    '      'FAGL_SEGMT'    'SEGMENT',
'PSEGMENTL '  '�ջ�������'      'FAGL_SEGMT'    'NAME    ',
'CSL       '  '������  '      'ACDOCA    '    'CSL     ',
'ZCBJE     '  '�ɱ����  '      'ACDOCA    '    'CSL     ',
'ZCHWSXJE  '  '���Ϊʵ�ֽ��'   'ACDOCA    '    'CSL     '.

  i_title               = lines( itab ) .
  CONDENSE i_title.
  CONCATENATE '��Ŀ��:' i_title INTO i_title.

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
    WHEN '&IC1'. "˫��
      CHECK rs_selfield-tabindex <> 0 . "С�����ܼ���ʲô�ĺ���
      READ TABLE itab INTO wa INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN ''.

        WHEN OTHERS.
      ENDCASE.
  ENDCASE.
  rs_s