*&---------------------------------------------------------------------*
*& Report ZRSD218
*&---------------------------------------------------------------------*
*& ��Ŀ�����ƻ�
*&---------------------------------------------------------------------*
REPORT zrsd218 MESSAGE-ID zgp_msg.
TABLES:vbkd,mara,but000,prps.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0,
       bstkd  TYPE vbkd-bstkd,  "OCP��ͬ��
       posnr  TYPE vbap-posnr,  "��ͬ��ϸ��
       matnr  TYPE vbap-matnr,  "���ϱ���
       zywy   TYPE vbak-zywy,   "ҵ��Ա����
       zywyt  TYPE kna1-name1,  "ҵ��Ա����
       zhtly  TYPE vbak-zhtly,  "��ԼרԱ����
       zhtlyt TYPE kna1-name1,  "��ԼרԱ����
       kunnr  TYPE kna1-kunnr,  "�ͻ����Ʊ���
       name1  TYPE kna1-name1,  "�ͻ���������
       post1  TYPE prps-post1,  "��Ŀ����
       zzl1   TYPE mara-zzl1,   "Ʒ��
       werks  TYPE vbap-werks,  "��������
       werkst TYPE string,  "������������
       zbx    TYPE atwrt,       "����
       zbk    TYPE atwrt,       "���
       zbh    TYPE atwrt,       "���
       zxcrz  TYPE atwrt,       "о������
       zxccd  TYPE atwrt,       "о�Ĳ���
       zxczl  TYPE atwrt,       "о������
       zrsdj  TYPE atwrt,       "ȼ�յȼ�
       zwbcms TYPE atwrt,       "��峤����
       znbcms TYPE atwrt,       "�ڰ峤����
       zwbxz  TYPE atwrt,       "�����״
       zwbfm  TYPE atwrt,       "��帲Ĥ
       zwbcd  TYPE atwrt,       "������
       zwbhd  TYPE atwrt,       "�����
       zwbys  TYPE atwrt,       "�����ɫ
       zwbtc  TYPE atwrt,       "���Ϳ������
       zwbdc  TYPE atwrt,       "���Ʋ�
       zwbqd  TYPE atwrt,       "���ǿ��
       znbfm  TYPE atwrt,       "�ڰ帲Ĥ
       znbxz  TYPE atwrt,       "�ڰ���״
       znbcd  TYPE atwrt,       "�ڰ����
       znbhd  TYPE atwrt,       "�ڰ���
       znbys  TYPE atwrt,       "�ڰ���ɫ
       znbtc  TYPE atwrt,       "�ڰ�Ϳ������
       znbdc  TYPE atwrt,       "�ڰ�Ʋ�
       znbqd  TYPE atwrt,       "�ڰ�ǿ��
       guige  TYPE string,       "���
       mvgr2  TYPE vbap-mvgr2,   "������Դ
       sel,
     END OF itab.
DATA:it_kunnr TYPE TABLE OF wkunnr WITH HEADER LINE.
DATA:BEGIN OF it_kna1 OCCURS 0,
       kunnr TYPE kna1-kunnr,
       name1 TYPE kna1-name1,
     END OF it_kna1.
DATA:it_matnr  TYPE TABLE OF ccvx_matnr WITH HEADER LINE,
     outtab001 TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
DATA: BEGIN OF it_wlcms OCCURS 0,
        matnr TYPE matnr,
        wlcms TYPE string,
      END OF it_wlcms.
"����001����
DATA:str_atnam TYPE string VALUE 'ZBX,ZBK,ZBH,ZXCRZ,ZXCCD,ZXCZL,ZRSDJ,ZWBXZ,ZWBFM,ZWBCD,ZWBHD,ZWBYS,ZWBTC,ZWBDC,ZWBQD,ZNBXZ,ZNBFM,ZNBCD,ZNBYS,ZNBHD,ZNBTC,ZNBDC,ZNBQD',
     r_atnam   TYPE RANGE OF atnam WITH HEADER LINE,
     tabix     TYPE sy-tabix.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS: s_bstkd FOR vbkd-bstkd,
                  s_posid FOR prps-posid MATCHCODE OBJECT zf4_posid,
                  s_post1 FOR prps-post1,
                  s_matnr FOR mara-matnr,
                  s_zhtly FOR but000-partner.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '����ѡ��'.
  %_s_bstkd_%_app_%-text = 'OCP��ͬ��'.
  %_s_posid_%_app_%-text = '��Ŀ����'.
  %_s_post1_%_app_%-text = '��Ŀ����'.
  %_s_matnr_%_app_%-text = '���ϱ���'.
  %_s_zhtly_%_app_%-text = '��ԼרԱ'.


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
   b~bstkd,
   c~posnr,
   c~matnr,
   a~zywy,
   a~zhtly,
   a~kunnr,
   prps~post1,
   mara~zzl1,
   c~mvgr2,
   CASE WHEN c~mvgr2 = 'Z03' THEN c~mvgr3 ELSE c~werks END AS werks
  FROM vbak AS a
  INNER JOIN vbkd AS b ON a~vbeln = b~vbeln AND b~posnr = '000000'
  INNER JOIN vbap AS c ON a~vbeln = c~vbeln
  INNER JOIN mara ON c~matnr = mara~matnr
  INNER JOIN prps ON a~ps_psp_pnr = prps~pspnr
  WHERE b~bstkd    IN @s_bstkd
    AND prps~posid IN @s_posid
    AND prps~post1 IN @s_post1
    AND a~zhtly    IN @s_zhtly
    AND c~matnr    IN @s_matnr
  INTO CORRESPONDING FIELDS OF TABLE @itab.
  IF itab[] IS INITIAL.
    MESSAGE s004 WITH '������' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  REFRESH: it_kunnr,it_matnr,it_wlcms,r_atnam.
  LOOP AT itab.
    it_kunnr-kunnr = itab-kunnr.
    COLLECT it_kunnr.
    it_kunnr-kunnr = itab-zhtly.
    COLLECT it_kunnr.
    it_kunnr-kunnr = itab-zywy.
    COLLECT it_kunnr.
    it_matnr-matnr = itab-matnr.
    COLLECT it_matnr.
    it_wlcms-matnr = itab-matnr.
    COLLECT it_wlcms.
  ENDLOOP.
  DELETE it_kunnr WHERE kunnr IS INITIAL.
  IF it_kunnr[] IS NOT INITIAL.
    AUTHORITY-CHECK OBJECT 'ZWSD06'
    ID 'ZWMKZ' FIELD '01'.
    IF sy-subrc = 0.
      SELECT kna1~kunnr
             kna1~name1
        INTO TABLE it_kna1
        FROM kna1
        FOR ALL ENTRIES IN it_kunnr
        WHERE kunnr = it_kunnr-kunnr.
    ELSE.
      SELECT kna1~kunnr
             kna1~name1
        INTO TABLE it_kna1
        FROM kna1
        FOR ALL ENTRIES IN it_kunnr
        WHERE kunnr = it_kunnr-kunnr AND kukla NE '03'."��Ȩ�ޣ���ȡ��ó�ͻ�
    ENDIF.
    SORT it_kna1 BY kunnr.
  ENDIF.

  SELECT werks,name1 FROM t001w INTO TABLE @DATA(it_t001w).
  SELECT mvgr3,bezei FROM tvm3t WHERE spras = @sy-langu INTO TABLE @DATA(it_tvm3t).

  "���ϳ�����
  PERFORM getlongtextpl IN PROGRAM zpubform TABLES it_wlcms.
  SORT it_wlcms BY matnr.
  IF it_matnr[] IS NOT INITIAL.
    "ȡ001����
    PERFORM get001 IN PROGRAM zpubform TABLES it_matnr outtab001 USING str_atnam.
    SORT outtab001 BY matnr.
  ENDIF.
  PERFORM splitstr(zpubform) TABLES r_atnam USING str_atnam ','.
  LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
    READ TABLE it_kna1 WITH KEY kunnr = <fs_itab>-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_itab>-name1 = it_kna1-name1.
    ENDIF.
    READ TABLE it_kna1 WITH KEY kunnr = <fs_itab>-zhtly BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_itab>-zhtlyt = it_kna1-name1.
    ENDIF.
    READ TABLE it_kna1 WITH KEY kunnr = <fs_itab>-zywy BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_itab>-zywyt = it_kna1-name1.
    ENDIF.
    READ TABLE it_wlcms WITH KEY matnr = <fs_itab>-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_itab>-guige = it_wlcms-wlcms.
    ENDIF.
    READ TABLE outtab001 WITH KEY matnr = <fs_itab>-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      tabix = sy-tabix.
      "ZBX,ZBK,ZBH,ZXCRZ,ZXCCD,ZXCZL,ZRSDJ,ZWBXZ,ZNBXZ,ZWBFM,ZNBFM
      LOOP AT outtab001 FROM tabix.
        IF outtab001-matnr NE <fs_itab>-matnr.
          EXIT.
        ENDIF.
        LOOP AT r_atnam.
          IF outtab001-atnam EQ r_atnam-low.
            ASSIGN COMPONENT r_atnam-low OF STRUCTURE <fs_itab> TO  FIELD-SYMBOL(<fs_value>).
            IF sy-subrc = 0.
              <fs_value> = outtab001-atwrt.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      <fs_itab>-guige = it_wlcms-wlcms.
    ENDIF.
    IF <fs_itab>-znbfm IS NOT INITIAL.
      <fs_itab>-znbcms = |{ <fs_itab>-znbcd }_{ <fs_itab>-znbhd }_{ <fs_itab>-znbys }_{ <fs_itab>-znbtc }_{ <fs_itab>-znbdc }_{ <fs_itab>-znbqd }_{ <fs_itab>-znbxz }_{ <fs_itab>-znbfm }|.
    ENDIF.
    IF <fs_itab>-zwbfm IS NOT INITIAL..
      <fs_itab>-zwbcms = |{ <fs_itab>-zwbcd }_{ <fs_itab>-zwbhd }_{ <fs_itab>-zwbys }_{ <fs_itab>-zwbtc }_{ <fs_itab>-zwbdc }_{ <fs_itab>-zwbqd }_{ <fs_itab>-zwbxz }_{ <fs_itab>-zwbfm }|.
    ENDIF.
    IF <fs_itab>-mvgr2 EQ 'Z03'.
      READ TABLE it_tvm3t INTO DATA(wa_tvm3t) WITH KEY mvgr3 = <fs_itab>-werks.
      IF sy-subrc EQ 0.
        <fs_itab>-werkst = wa_tvm3t-bezei.
      ENDIF.
    ELSE.
      READ TABLE it_t001w INTO DATA(wa_t001w) WITH KEY werks = <fs_itab>-werks.
      IF sy-subrc EQ 0.
        <fs_itab>-werkst = wa_t001w-name1.
      ENDIF.
    ENDIF.
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
'BSTKD  '  'OCP��ͬ�� '     'VBKD'  'BSTKD',
'POSNR  '  '��ͬ��ϸ��  '   'VBAP'  'POSNR',
'MATNR  '  '���ϱ���   '    'VBAP'  'MATNR',
'ZYWYT  '  'ҵ��Ա    '     'KNA1'  'NAME1',
'ZHTLYT '  '��ԼרԱ���� '  'KNA1'  'NAME1',
'NAME1  '  '�ͻ��������� '  'KNA1'  'NAME1',
'POST1  '  '��Ŀ����   '    'PRPS'  'POST1',
'ZZL1   '  'Ʒ��     '     'MARA'  'ZZL1',
'WERKS  '  '��������   '   'VBAP'  'WERKS',
'WERKST'   '������������'  '    '  '      ',
'ZBX    '  '����     '     'AUSP'  'ATWRT',
'ZBK    '  '���     '     'AUSP'  'ATWRT',
'ZBH    '  '���     '     'AUSP'  'ATWRT',
'ZXCRZ  '  'о������   '   'AUSP'  'ATWRT',
'ZXCCD  '  'о�Ĳ���   '   'AUSP'  'ATWRT',
'ZXCZL  '  'о������   '   'AUSP'  'ATWRT',
'ZRSDJ  '  'ȼ�յȼ�   '   'AUSP'  'ATWRT',
'ZWBXZ  '  '�����״   '   'AUSP'  'ATWRT',
'ZNBXZ  '  '�ڰ���״   '   'AUSP'  'ATWRT',
'ZWBCMS '  '��峤����  '  'AUSP'  'ATWRT',
'ZNBCMS '  '�ڰ峤����  '  'AUSP'  'ATWRT',
'ZWBFM  '  '��帲Ĥ   '   'AUSP'  'ATWRT',
'ZNBFM  '  '�ڰ帲Ĥ   '   'AUSP'  'ATWRT',
'ZZL1   '  'Ʒ��     '     'MARA'  'ZZL1',
'GUIGE  '  '���       '   '    '  '    '.

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
    WHEN '&IC1'. "˫�