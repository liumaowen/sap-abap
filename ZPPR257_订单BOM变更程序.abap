*&---------------------------------------------------------------------*
*& Report ZPPR257
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr257 MESSAGE-ID zmsg_gp.

TABLES:sscrfields,t001l,afko,mara.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0,
       werks     TYPE ztpp_206-werks,       "����
       aufnr     TYPE ztpp_206-aufnr,       "�������
       zbgxh     TYPE ztpp_320c-zbgxh,      "������
       zwllx     TYPE ztpp_206-zwllx,       "��������
       matnr     TYPE ztpp_206-matnr,       "���ϱ���
       maktx     TYPE makt-maktx,           "��������
       zzl1      TYPE mara-zzl1,            "Ʒ��
       zdate     TYPE ztpp_206-zdate,       "������������
       zjhdshzt  TYPE ztpp_206-zjhdshzt,    "�������״̬
       andat     TYPE mast-andat,           "BOM��������
       ziscg_sap TYPE ztpp_320c-ziscg_sap,  "SAP�Ƿ����ɹ�
       zfhjg_sap TYPE ztpp_320c-zfhjg_sap,  "SAP������ؽ��
       ziscg_mes TYPE ztpp_320c-ziscg_mes,  "MES�Ƿ����ɹ�
       zfhjg_mes TYPE ztpp_320c-zfhjg_mes,  "MES������ؽ��
       sel,
     END OF itab.
DATA:BEGIN OF ltab OCCURS 0,
       werks     TYPE ztpp_206-werks,       "����
       aufnr     TYPE ztpp_206-aufnr,       "�������
       zbgxh     TYPE ztpp_320c-zbgxh,      "������
       zdate     TYPE ztpp_320c-zdate,      "��������
       ztime     TYPE ztpp_320c-ztime,      "����ʱ��
       zname     TYPE ztpp_320c-zname,      "������
       ziscg_sap TYPE ztpp_320c-ziscg_sap,  "SAP�Ƿ����ɹ�
       zfhjg_sap TYPE ztpp_320c-zfhjg_sap,  "SAP������ؽ��
       ziscg_mes TYPE ztpp_320c-ziscg_mes,  "MES�Ƿ����ɹ�
       zfhjg_mes TYPE ztpp_320c-zfhjg_mes,  "MES������ؽ��
       sel,
     END OF ltab.
TYPES:BEGIN OF ty_aufnr,
        werks TYPE ztpp_206-werks,
        aufnr TYPE ztpp_206-aufnr,
      END OF ty_aufnr.
TYPES:BEGIN OF ty_matnr,
        werks TYPE ztpp_206-werks,
        matnr TYPE ztpp_206-matnr,
      END OF ty_matnr.
DATA:gt_aufnr TYPE TABLE OF ty_aufnr WITH KEY werks aufnr,
     gv_aufnr LIKE LINE OF gt_aufnr.
DATA:gt_matnr TYPE TABLE OF ty_matnr WITH KEY werks matnr,
     gv_matnr LIKE LINE OF gt_matnr.
FIELD-SYMBOLS:<itab> TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:s_werks FOR t001l-werks OBLIGATORY NO INTERVALS NO-EXTENSION,
                 s_matkl FOR mara-matkl MODIF ID m1,
                 s_zzl1 FOR mara-zzl1 MODIF ID m1,
                 s_matnr FOR mara-matnr MODIF ID m1,
                 s_aufnr FOR mara-matnr.
  PARAMETERS:p_log TYPE char1 AS CHECKBOX USER-COMMAND singleclick.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '����ѡ��'.
  %_s_werks_%_app_%-text = '����'.
  %_s_matkl_%_app_%-text = '��Ʒ������'.
  %_s_zzl1_%_app_%-text = '��ƷƷ��'.
  %_s_matnr_%_app_%-text = '��Ʒ����'.
  %_p_log_%_app_%-text = '�鿴��־'.
  %_s_aufnr_%_app_%-text = '��Ʒ����'.


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.
  CASE 'X'.
    WHEN p_log.
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
  IF p_log = 'X'.
    ASSIGN ltab[] TO <itab>.
    CLEAR :<itab>,ltab[].
    SELECT
        *
    FROM ztpp_320c
    WHERE werks IN @s_werks
      AND aufnr IN @s_aufnr
    INTO CORRESPONDING FIELDS OF TABLE @ltab.
  ELSE.
    ASSIGN itab[] TO <itab>.
    CLEAR :<itab>,itab[].
    SELECT
        z206~werks,
        z206~aufnr
    FROM ztpp_206 AS z206
    INNER JOIN mara AS ma ON z206~matnr = ma~matnr
    WHERE z206~del = ''
      AND z206~aufnr IS NOT INITIAL
      AND z206~werks IN @s_werks
      AND z206~matnr IN @s_matnr
      AND ma~matkl   IN @s_matkl
      AND ma~zzl1    IN @s_zzl1
      AND z206~aufnr IN @s_aufnr
      AND z206~zwllx = '��Ʒ'
      AND NOT EXISTS ( SELECT 1 FROM aufm
                        WHERE aufm~werks = z206~werks
                          AND aufm~aufnr = z206~aufnr )
    GROUP BY z206~werks,z206~aufnr
    INTO TABLE @gt_aufnr.

    IF gt_aufnr IS INITIAL.
      MESSAGE s005 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT
        z206~werks,
        z206~aufnr,
        z206~zwllx,
        z206~matnr,
        mk~maktx,
        ma~zzl1,
        z206~zdate,
        z206~zjhdshzt
    FROM ztpp_206 AS z206
    INNER JOIN @gt_aufnr AS au ON z206~werks = au~werks AND left( z206~aufnr,9 ) = au~aufnr
    INNER JOIN mara AS ma ON z206~matnr = ma~matnr
    INNER JOIN makt AS mk ON mk~matnr = z206~matnr AND mk~spras = @sy-langu
    WHERE z206~del = ''
      AND z206~werks IN @s_werks
      AND NOT EXISTS ( SELECT 1 FROM aufm
                        WHERE aufm~werks = z206~werks
                          AND aufm~aufnr = z206~aufnr )
    INTO CORRESPONDING FIELDS OF TABLE @itab.
    SORT itab BY werks aufnr.
    DELETE ADJACENT DUPLICATES FROM itab COMPARING werks aufnr.

    REFRESH: gt_aufnr,gt_matnr.
    LOOP AT itab.
      gv_aufnr-werks = itab-werks.
      gv_aufnr-aufnr = itab-aufnr.
      COLLECT gv_aufnr INTO gt_aufnr.
      gv_matnr-werks = itab-werks.
      gv_matnr-matnr = itab-matnr.
      COLLECT gv_matnr INTO gt_matnr.
    ENDLOOP.
    SELECT
        z320c~werks,
        z320c~aufnr,
        MAX( z320c~zbgxh ) AS zbgxh
    FROM ztpp_320c AS z320c
    INNER JOIN @gt_aufnr AS au ON z320c~aufnr = au~aufnr AND z320c~werks = au~werks
    GROUP BY z320c~werks,z320c~aufnr
    ORDER BY z320c~werks,z320c~aufnr
    INTO TABLE @DATA(lt_320c).
    SELECT
        mast~matnr,
        mast~andat
    FROM mast
    INNER JOIN @gt_matnr AS mat ON mast~werks = mat~werks AND mast~matnr = mat~matnr
    GROUP BY mast~matnr,mast~andat
    ORDER BY mast~matnr
    INTO TABLE @DATA(lt_mast).

    LOOP AT itab.
      READ TABLE lt_320c INTO DATA(lv_320c) WITH KEY werks = itab-werks aufnr = itab-aufnr BINARY SEARCH.
      IF sy-subrc = 0.
        itab-zbgxh = lv_320c-zbgxh + 1.
      ELSE.
        itab-zbgxh = 1.
      ENDIF.
      READ TABLE lt_mast INTO DATA(lv_mast) WITH KEY matnr = itab-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        itab-andat = lv_mast-andat.
      ENDIF.
      MODIFY itab.
    ENDLOOP.
  ENDIF.
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

  IF p_log = 'X'.
    PERFORM catlg_set TABLES fldct
                      USING:
      'ZISCG_SAP '  'SAP�Ƿ����ɹ�'  'ZTPP_320C'  'ZISCG_SAP',
      'ZFHJG_SAP '  'SAP������ؽ��'  'ZTPP_320C'  'ZFHJG_SAP',
      'ZISCG_MES '  'MES�Ƿ����ɹ�'  'ZTPP_320C'  'ZISCG_MES',
      'ZFHJG_MES '  'MES������ؽ��'  'ZTPP_320C'  'ZFHJG_MES',
      'WERKS     '  '����          '  'ZTPP_320C'  'WERKS    ',
      'AUFNR     '  '�������     '  'ZTPP_320C'  'AUFNR    ',
      'ZBGXH     '  '������     '  'ZTPP_320C'  'ZBGXH    ',
      'ZDATE     '  '��������     '  'ZTPP_320C'  'ZDATE    ',
      'ZTIME     '  '����ʱ��     '  'ZTPP_320C'  'ZTIME    ',
      'ZNAME     '  '������      '  'ZTPP_320C'  'ZNAME    '.
  ELSE.
    PERFORM catlg_set TABLES fldct
                      USING:
      'ZISCG_SAP '  'SAP�Ƿ����ɹ�'  'ZTPP_320C'  'ZISCG_SAP',
      'ZFHJG_SAP '  'SAP������ؽ��'  'ZTPP_320C'  'ZFHJG_SAP',
      'ZISCG_MES '  'MES�Ƿ����ɹ�'  'ZTPP_320C'  'ZISCG_MES',
      'ZFHJG_MES '  'MES������ؽ��'  'ZTPP_320C'  'ZFHJG_MES',
      'WERKS    '  '����      '     'ZTPP_206'  'WERKS   ',
      'AUFNR    '  '�������    '   'ZTPP_206'   'AUFNR   ',
      'ZBGXH    '  '������    '   'ZTPP_320C'  'ZBGXH   ',
      'ZWLLX    '  '��������    '   'ZTPP_206'   'ZWLLX   ',
      'MATNR    '  '���ϱ���    '   'ZTPP_206'   'MATNR   ',
      'MAKTX    '  '��������    '   'MAKT    '   'MAKTX   ',
      'ZZL1     '  'Ʒ��        '   'MARA    '   'ZZL1    ',
      'ZDATE    '  '������������  '  'ZTPP_206'   'ZDATE   ',
      'ZJHDSHZT '  '�������״̬  '  'ZTPP_206'   'ZJHDSHZT',
      'ANDAT    '  'BOM��������   '  'MAST    '  'ANDAT   '.

  ENDIF.

  i_title               = lines( <itab> ) .
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
      t_outtab                 = <itab>
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
    WHEN 'AUFNR'.
      ls_fldct-hotspot = 'X'.
    WHEN OTHERS.
  ENDCASE.

  APPEND ls_fldct TO fldcattab .
  CLEAR ls_fldct .
ENDFORM.

FORM set_status USING rt_extab TYPE slis_t_extab.
  CLEAR rt_extab.
  REFRESH rt_extab.
  IF p_log = 'X'.
    APPEND 'ZEDIT' TO rt_extab.
  ELSE.
    APPEND 'RESENDMES' TO rt_extab.
  ENDIF.
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
        WHEN 'AUFNR'.
          IF wa-aufnr IS NOT INITIAL.
            PERFORM co03(zpubform) USING wa-aufnr.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZEDIT'.
      IF NOT line_exists( itab[ sel = 'X' ] ).
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      LOOP AT itab WHERE sel = 'X'.
        IF itab-ziscg_sap NE '' OR itab-ziscg_mes NE ''.
          MESSAGE '�������ظ��޸�' TYPE 'E'.
          EXIT.
        ENDIF.
      ENDLOOP.
      PERFORM edit.
    WHEN 'RESENDMES'.
      IF NOT line_exists( ltab[ sel = 'X' ziscg_mes = 'E' ] ).
        MESSAGE '��ѡ��MES���ʧ�ܵ���ϸ' TYPE 'E'.
        EXIT.
      ENDIF.
      PERFORM resendmes.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form edit
*&---------------------------------------------------------------------*
*& ����BOM��������MES
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM edit.
  DATA: number           TYPE bapi_order_key-order_number,
        orderdata        TYPE bapi_pp_order_change,
        orderdatax       TYPE bapi_pp_order_changex,
        master_data_read TYPE char1,
        return           TYPE bapiret2.
  TYPES:BEGIN OF ty_aufnr,
          aufnr TYPE ztpp_206-aufnr,
        END OF ty_aufnr.
  DATA:gt_aufnr TYPE TABLE OF ty_aufnr WITH KEY aufnr,
       gv_aufnr LIKE LINE OF gt_aufnr.
  DATA: rtype TYPE bapi_mtype,
        rtmsg TYPE bapi_msg.
  DATA:lt_320c TYPE TABLE OF ztpp_320c WITH HEADER LINE.

  LOOP AT itab WHERE sel = 'X'.
    gv_aufnr-aufnr = itab-aufnr.
    COLLECT gv_aufnr INTO gt_aufnr.
  ENDLOOP.
  SELECT
    afpo~aufnr,
    afpo~verid
  FROM afpo
  INNER JOIN @gt_aufnr AS au ON afpo~aufnr = au~aufnr
  GROUP BY afpo~aufnr,afpo~verid
  ORDER BY afpo~aufnr
  INTO TABLE @DATA(lt_afpo).

  LOOP AT itab WHERE sel = 'X'.
    CLEAR:orderdata,orderdatax,return,number,master_data_read,lt_320c.
    IF itab-zwllx = '��Ʒ'.
      number = itab-aufnr.
      READ TABLE lt_afpo INTO DATA(lv_afpo) WITH KEY aufnr = itab-aufnr BINARY SEARCH.
      IF sy-subrc = 0.
        orderdata-prod_version = lv_afpo-verid.
      ENDIF.
      orderdata-explosion_date = sy-datlo.
      orderdata-explode_new = 'X'.
      orderdatax-prod_version = 'X'.
      orderdatax-explosion_date = 'X'.
      orderdatax-routing ='X'.
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'BAPI_PRODORD_CHANGE'             "BAPI: Change Production Order
        EXPORTING
          number           = number                   " bapi_order_key-order_number  Production Order Number
          orderdata        = orderdata                " bapi_pp_order_change  Transfer Structure for Changing Production Orders
          orderdatax       = orderdatax               " bapi_pp_order_changex  Transfer Structure for Changing Production Orders
        IMPORTING
          return           = return                   " bapiret2      Return Parameters
          master_data_read = master_data_read.
      IF return-type CA 'AEX' OR master_data_read IS INITIAL.
        itab-ziscg_sap = 'E'.
        itab-zfhjg_sap = return-message.
        MODIFY itab TRANSPORTING ziscg_sap zfhjg_sap.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        itab-ziscg_sap = 'S'.
        itab-zfhjg_sap = '���³ɹ�'.
        MODIFY itab TRANSPORTING ziscg_sap zfhjg_sap.
        "�ɹ���������MES
        IF itab-werks = '3060'.
          CALL FUNCTION 'ZFM_GP_PP_MES_ORDERBOMCHANGE'
            EXPORTING
              aufnr = itab-aufnr
            IMPORTING
              rtype = rtype
              rtmsg = rtmsg.
          itab-ziscg_mes = rtype.
          itab-zfhjg_mes = rtmsg.
          MODIFY itab TRANSPORTING ziscg_mes zfhjg_mes.
        ENDIF.
      ENDIF.
      lt_320c-werks = itab-werks.
      lt_320c-aufnr = itab-aufnr.
      lt_320c-zbgxh  = itab-zbgxh .
      lt_320c-ziscg_sap = itab-ziscg_sap.
      lt_320c-zfhjg_sap = itab-zfhjg_sap.
      lt_320c-ziscg_mes = itab-ziscg_mes.
      lt_320c-zfhjg_mes = itab-zfhjg_mes.
      lt_320c-zdate = sy-datlo.
      lt_320c-ztime = sy-timlo.
      lt_320c-zname = sy-uname.
      APPEND lt_320c.
    ELSE.
      "������MES
      IF itab-werks = '3060'.
        CALL FUNCTION 'ZFM_GP_PP_MES_ORDERBOMCHANGE'
          EXPORTING
            aufnr = itab-aufnr
          IMPORTING
            rtype = rtype
            rtmsg = rtmsg.
        itab-ziscg_mes = rtype.
        itab-zfhjg_mes = rtmsg.
        MODIFY itab TRANSPORTING ziscg_mes zfhjg_mes.
        lt_320c-werks = itab-werks.
        lt_320c-aufnr = itab-aufnr.
        lt_320c-zbgxh  = itab-zbgxh .
        lt_320c-ziscg_sap = itab-ziscg_sap.
        lt_320c-zfhjg_sap = itab-zfhjg_sap.
        lt_320c-ziscg_mes = itab-ziscg_mes.
        lt_320c-zfhjg_mes = itab-zfhjg_mes.
        lt_320c-zdate = sy-datlo.
        lt_320c-ztime = sy-timlo.
        lt_320c-zname = sy-uname.
        APPEND lt_320c.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_320c[] IS NOT INITIAL.
    INSERT ztpp_320c FROM TABLE lt_320c.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form resendmes
*&---------------------------------------------------------------------*
*& ����������MES
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM resendmes.
  DATA: rtype TYPE bapi_mtype,
        rtmsg TYPE bapi_msg.
  DATA: ltab1    TYPE TABLE OF ztpp_320c WITH HEADER LINE.

  LOOP AT ltab WHERE sel = 'X' AND ziscg_mes = 'E'.
    MOVE-CORRESPONDING ltab TO ltab1.
    APPEND ltab1.
  ENDLOOP.
  LOOP AT ltab1.
    CALL FUNCTION 'ZFM_GP_PP_MES_ORDERBOMCHANGE'
      EXPORTING
 