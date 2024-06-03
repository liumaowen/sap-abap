*&---------------------------------------------------------------------*
*& Report ZPPR255
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr255 MESSAGE-ID zgp_msg.

TABLES:likp.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:hour TYPE p DECIMALS 1.
TYPES:BEGIN OF ty_itab,
        vbeln         TYPE vbak-vbeln,             "��������
        zysfzr        TYPE vbak-zysfzr,            "������
        zlyrqsj       TYPE char19,                 "��Լ�ύ
        zbjrq         TYPE vbak-zbjrq,             "��������
        zbjsj         TYPE vbak-zbjsj,             "����ʱ��
        zbjrqsj       TYPE char19,                 "����ʱ��
        zbj_ly        TYPE hour,                   "����ʱЧ = ����ʱ��-��Լ�ύʱ��
        zzbrqsj       TYPE char19,                 "�б�ʱ��
        zzb_bj        TYPE hour,                   "����ʱЧ = �б�ʱ��-����ʱ��
        zzhrq         TYPE vbak-zzhrq,             "Ԥ��װ������
        zzhsj         TYPE vbak-zzhsj,             "Ԥ��װ��ʱ��
        zzhrqsj       TYPE char19,                 "Ԥ��װ��ʱ��
        zrcrq         TYPE vbak-zrcrq,             "��������
        zrcsj         TYPE vbak-zrcsj,             "����ʱ��
        zrcrqsj       TYPE char19,                 "��������
        zzhrq_start   TYPE vbak-zzhrq,             "װ����ʼ����
        zzhsj_start   TYPE vbak-zzhsj,             "װ����ʼʱ��
        zzhrqsj_start TYPE char19,                 "װ����ʼ
        zzhrq_end     TYPE vbak-zzhrq,             "װ����������
        zzhsj_end     TYPE vbak-zzhsj,             "װ������ʱ��
        zzhrqsj_end   TYPE char19,                 "װ������
        zwait         TYPE hour,                   "�����ȴ�ʱ�� = װ����ʼ-��������
        zzhsx         TYPE hour,                   "װ��ʱЧ = װ������-װ����ʼ
        zxhrq         TYPE vbak-zxhrq,             "Ԥ��ж������
        zxhsj         TYPE vbak-zxhsj,             "Ԥ��ж��ʱ��
        zxhrqsj       TYPE char19,                 "Ԥ��ж��
        zccrq         TYPE vbak-zccrq,             "��������
        zccsj         TYPE vbak-zccsj,             "����ʱ��
        zccrqsj       TYPE char19,                 "����ʱ��
        zbgrq         TYPE datum,                  "���ܳ�������
        zbgsj         TYPE tims,                   "���ܳ���ʱ��
        zbgrqsj       TYPE char19,                 "���ܳ���
        zcc_bg        TYPE hour,                   "����ͣ��ʱ�� = ��������ʱ��-���ܳ���ʱ��
        zhdrq         TYPE datum,                  "�ص��ϴ�����
        zhdsj         TYPE tims,                   "�ص��ϴ�ʱ��
        zhdrqsj       TYPE char19,                 "�ص��ϴ�
        zhd_cc        TYPE hour,                   "ʵ������ʱЧ = �ص��ϴ�-��������ʱ��
        zyjxh_zh      TYPE hour,                   "Ԥ������ʱЧ = Ԥ��ж��ʱ��-Ԥ��װ��ʱ��
        sel,
      END OF ty_itab.
DATA: itab    TYPE TABLE OF ty_itab WITH EMPTY KEY,
      wa_itab LIKE LINE OF itab.
* ��̬�ڱ�
DATA: lo_struct   TYPE REF TO cl_abap_structdescr,
      lo_element  TYPE REF TO cl_abap_elemdescr,
      lo_new_type TYPE REF TO cl_abap_structdescr,
      lo_new_tab  TYPE REF TO cl_abap_tabledescr,
      lo_data     TYPE REF TO data,
      dyn_wa      TYPE REF TO data,
      lt_comp     TYPE cl_abap_structdescr=>component_table,
      lt_tot_comp TYPE cl_abap_structdescr=>component_table,
      la_comp     LIKE LINE OF lt_comp.
* ���ʶ�̬����ֶη���
FIELD-SYMBOLS: <f_tab>  TYPE table,
               <f_line> TYPE any.
TYPES:BEGIN OF ty_vbeln,
        vbeln TYPE vbak-vbeln,
      END OF ty_vbeln.
DATA:gt_vbeln TYPE TABLE OF ty_vbeln WITH NON-UNIQUE KEY vbeln,
     gv_vbeln LIKE LINE OF gt_vbeln.
DATA: BEGIN OF gt_tp OCCURS 0,
        type  TYPE string,
        text  TYPE string,
        matkl TYPE string,
      END OF gt_tp.
DATA: BEGIN OF gt_fname OCCURS 0,
        fname TYPE string,
        ftext TYPE string,
      END OF gt_fname.
DATA:r_matkl   TYPE RANGE OF matkl WITH HEADER LINE.
DATA:BEGIN OF gt_malips OCCURS 0,
       vbeln  TYPE lips-vbeln,
       zerdat TYPE lips-zerdat,
       zertim TYPE lips-zertim,
     END OF gt_malips.
DATA:fieldname TYPE string,
     fieldtext TYPE string.
DATA:zerdat_s   TYPE ze_erdat, "��ʼ����
     zertim_s   TYPE ze_ertim, "��ʼʱ��
     zerdat_e   TYPE ze_erdat, "����+ʱ��
     zertim_e   TYPE ze_ertim, "��������
     zdat_tim_s TYPE char19,   "��������
     zdat_tim_e TYPE char19,   "����+ʱ��
     cha        TYPE hour.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS:
*                 s_werks FOR t001l-werks OBLIGATORY,
*                 s_posid FOR prps-posid,
*                 s_post1 FOR prps-post1,"��Ŀ����
                 s_erdat FOR likp-erdat,     "�������Ƶ�����
                 s_ckdat FOR likp-wadat_ist. "���ⵥ��������
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t1 = '����ѡ��'.
  %_s_erdat_%_app_%-text = '�Ƶ�����'.
  %_s_ckdat_%_app_%-text = '��������'.

  gt_fname[] = VALUE #( ( fname = 'ZHRQSJ_START' ftext = 'װ����ʼ'  )
                        ( fname = 'ZHRQSJ_END'   ftext = 'װ������'   )
                        ( fname = 'ZHSX'         ftext = 'װ��ʱЧ' )
                      ).
  gt_tp[] = VALUE #( ( type = 'LC'  text = '����'   matkl = 'A0100,A0200,A0300,A0400'  )
                     ( type = 'ZWJ' text = '�����' matkl = 'B0201,B0202,B0203,B0204,B0205,B0206,B0207,B0208' )
                     ( type = 'MQ'  text = 'Ļǽ'  matkl = 'A0500'  )
                     ( type = 'LT'  text = '����'  matkl = 'B0106'  )
                     ( type = 'YW'  text = 'ѹ��'  matkl = 'B0101'  )
                     ( type = 'DM'  text = '��Ĭ'  matkl = 'A1601,A1602,B0206'  )
                   ).


AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
  PERFORM get_dynamic_table. "��ȡ��̬�ڱ�
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
      a~vbeln,
      a~zysfzr,
      a~zzhrq,
      a~zzhsj,
      a~zrcrq,
      a~zrcsj,
      a~zxhrq,
      a~zxhsj,
      a~zbjrq,
      a~zbjsj,
      a~zccrq,
      a~zccsj
  FROM vbak AS a
  INNER JOIN lips AS b ON b~vgbel = a~vbeln
  INNER JOIN likp AS c ON c~vbeln = b~vbeln
  WHERE left( a~vbeln,2 ) EQ 'FH'
   AND  a~zjhfs = 'B'
   AND  a~erdat     IN @s_erdat
   AND  c~wadat_ist IN @s_ckdat
  ORDER BY a~vbeln,c~wadat_ist,c~spe_wauhr_ist
  INTO CORRESPONDING FIELDS OF TABLE @itab.
  DELETE ADJACENT DUPLICATES FROM itab COMPARING vbeln.
  IF itab IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH: gt_vbeln.
  LOOP AT itab INTO wa_itab.
    gv_vbeln-vbeln = wa_itab-vbeln.
    COLLECT gv_vbeln INTO gt_vbeln.
  ENDLOOP.

  "��Լ�ύʱ�䣨���һ�Σ�
  SELECT
    b~vbeln,
    a~zdate,
    a~ztime
  FROM ztsdzdlog AS a
  INNER JOIN @gt_vbeln AS b ON a~sapno = b~vbeln
  WHERE a~fieldname = 'ZTTZT'
  AND  a~value_o = 'A'
  AND  a~value_n = 'B'
  ORDER BY vbeln,zdate DESCENDING,ztime DESCENDING
  INTO TABLE @DATA(it_kfdate).
  DELETE ADJACENT DUPLICATES FROM it_kfdate COMPARING vbeln.
  "�б�ʱ�䣨���һ�Σ�
  SELECT
    b~vbeln,
    a~zdate,
    a~ztime
  FROM ztsdzdlog AS a
  INNER JOIN @gt_vbeln AS b ON a~sapno = b~vbeln
  WHERE a~fieldname = 'ZTTZT'
  AND  a~value_o = 'B'
  AND  a~value_n = 'C'
  ORDER BY vbeln,zdate DESCENDING,ztime DESCENDING
  INTO TABLE @DATA(it_zbdate).
  DELETE ADJACENT DUPLICATES FROM it_zbdate COMPARING vbeln.
*  ��һ��ɨ������
  SELECT
    a~vgbel AS vbeln,
    a~zerdat,
    a~zertim
  FROM lips AS a
  INNER JOIN @gt_vbeln AS b ON a~vgbel = b~vbeln
  WHERE a~zckfs IN ('1','2')
  ORDER BY vbeln, zerdat, zertim
  INTO TABLE @DATA(sm_first).
  DELETE ADJACENT DUPLICATES FROM sm_first COMPARING vbeln.
*  ���һ��ɨ������
  SELECT
    a~vgbel AS vbeln,
    a~zerdat,
    a~zertim
  FROM lips AS a
  INNER JOIN @gt_vbeln AS b ON a~vgbel = b~vbeln
  WHERE a~zckfs IN ('1','2')
  ORDER BY vbeln, zerdat DESCENDING, zertim DESCENDING
  INTO TABLE @DATA(sm_last).
  DELETE ADJACENT DUPLICATES FROM sm_last COMPARING vbeln.
*�ص��ϴ�ʱ��
  SELECT
    b~vgbel AS vbeln,
    CASE WHEN a~zhcjrq IS INITIAL THEN a~zyjrq ELSE a~zhcjrq END AS zhdrq
  FROM ztsdurl AS a
  INNER JOIN lips AS b ON a~vbeln = b~vbeln
  INNER JOIN @gt_vbeln AS c ON b~vgbel = c~vbeln
  WHERE a~zmk = 'POD'
  ORDER BY vbeln,zhdrq
  INTO TABLE @DATA(gt_sdurl).
  DELETE ADJACENT DUPLICATES FROM gt_sdurl COMPARING vbeln.
*���ܳ���
  SELECT
    c~vgbel AS vbeln,
    a~wadat_ist AS dat,
    a~spe_wauhr_ist AS tim
  FROM likp AS a
  INNER JOIN lips AS c ON a~vbeln = c~vbeln
  INNER JOIN @gt_vbeln AS b ON c~vgbel = b~vbeln
  ORDER BY vbeln, dat, tim
  INTO TABLE @DATA(gt_bgck).
  DELETE ADJACENT DUPLICATES FROM gt_bgck COMPARING vbeln.
*��������װ��ʱ��
  SELECT
    a~vgbel AS vbeln,
    a~zerdat,
    a~zertim,
    a~matkl
  FROM lips AS a
  INNER JOIN @gt_vbeln AS b ON a~vgbel = b~vbeln
  WHERE a~zckfs IN ('1','2')
  ORDER BY vbeln
  INTO TABLE @DATA(lt_matdat).


*��ֵalv�ڱ�
  LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
    "��Լ�ύ
    READ TABLE it_kfdate INTO DATA(wa_kfdate) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      PERFORM getdatzet USING    wa_kfdate-zdate
                                 wa_kfdate-ztime
                        CHANGING <fs_itab>-zlyrqsj.
    ENDIF.
    "����ʱ��
    PERFORM getdatzet USING    <fs_itab>-zbjrq
                               <fs_itab>-zbjsj
                      CHANGING <fs_itab>-zbjrqsj.
    "����ʱЧ
    PERFORM getcha    USING    wa_kfdate-zdate
                               <fs_itab>-zbjrq
                               wa_kfdate-ztime
                               <fs_itab>-zbjsj
                      CHANGING <fs_itab>-zbj_ly.
    "�б�ʱ��
    READ TABLE it_zbdate INTO DATA(wa_zbdate) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      PERFORM getdatzet  USING     wa_zbdate-zdate
                                   wa_zbdate-ztime
                         CHANGING  <fs_itab>-zzbrqsj.
      "����ʱЧ
      PERFORM getcha     USING     <fs_itab>-zbjrq
                                   wa_zbdate-zdate
                                   <fs_itab>-zbjsj
                                   wa_zbdate-ztime
                         CHANGING  <fs_itab>-zzb_bj.
    ENDIF.
    "Ԥ��װ��ʱ��
    PERFORM getdatzet   USING   <fs_itab>-zzhrq
                                <fs_itab>-zzhsj
                       CHANGING <fs_itab>-zzhrqsj.
    "�����볧
    PERFORM getdatzet   USING   <fs_itab>-zrcrq
                                <fs_itab>-zrcsj
                       CHANGING <fs_itab>-zrcrqsj.
    "װ����ʼ
    READ TABLE sm_first INTO DATA(wa_first) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      <fs_itab>-zzhrq_start = wa_first-zerdat.
      <fs_itab>-zzhsj_start = wa_first-zertim.
      PERFORM getdatzet USING    <fs_itab>-zzhrq_start
                                 <fs_itab>-zzhsj_start
                        CHANGING <fs_itab>-zzhrqsj_start.
    ENDIF.
    "װ������
    READ TABLE sm_last INTO DATA(wa_last) WITH KEY vbeln = <fs_itab>-vbeln.
    IF sy-subrc = 0.
      <fs_itab>-zzhrq_end = wa_last-zerdat.
      <fs_itab>-zzhsj_end = wa_last-zertim.
      PERFORM getdatzet USING    <fs_itab>-zzhrq_end
                                 <fs_itab>-zzhsj_end
                        CHANGING <fs_itab>-zzhrqsj_end.
    ENDIF.
    "�����ȴ�ʱ��
    PERFORM getcha USING    <fs_itab>-zrcrq
                            <fs_itab>-zzhrq_start
                            <fs_itab>-zrcsj
                            <fs_itab>-zzhsj_start
                   CHANGING <fs_itab>-zwait.
    "װ��ʱЧ
    PERFORM getcha USING    <fs_itab>-zzhrq_start
                            <fs_itab>-zzhrq_end
                            <fs_itab>-zzhsj_start
                            <fs_itab>-zzhsj_end
                   CHANGING <fs_itab>-zzhsx.
    "Ԥ��ж��
    PERFORM getdatzet USING    <fs_itab>-zxhrq
                               <fs_itab>-zxhsj
                      CHANGING <fs_itab>-zxhrqsj.
    "Ԥ������ʱЧ
    PERFORM getcha    USING    <fs_itab>-zzhrq
                               <fs_itab>-zxhrq
                               <fs_itab>-zzhsj
                               <fs_itab>-zxhsj
                      CHANGING <fs_itab>-zyjxh_zh.
    "�ص��ϴ�����
    READ TABLE gt_sdurl INTO DATA(gv_sdurl) WITH KEY vbeln =  <fs_itab>-vbeln.
    IF sy-subrc = 0.
      <fs_itab>-zhdrq = gv_sdurl-zhdrq.
      PERFORM getdatzet USING    <fs_itab>-zhdrq
                                 <fs_itab>-zhdsj
                        CHANGING <fs_itab>-zhdrqsj.
    ENDIF.
    "��������
    PERFORM getdatzet USING    <fs_itab>-zccrq
                               <fs_itab>-zccsj
                      CHANGING <fs_itab>-zccrqsj.
    "ʵ������ʱЧ
    PERFORM getcha    USING    <fs_itab>-zccrq
                               <fs_itab>-zhdrq
                               <fs_itab>-zccsj
                               <fs_itab>-zhdsj
                      CHANGING <fs_itab>-zhd_cc.
    "���ܳ���
    READ TABLE gt_bgck INTO DATA(gv_bgck) WITH KEY vbeln =  <fs_itab>-vbeln.
    IF sy-subrc = 0.
      PERFORM getdatzet USING    gv_bgck-dat
                                 gv_bgck-tim
                        CHANGING <fs_itab>-zbgrqsj.
      PERFORM getcha    USING    gv_bgck-dat
                                 <fs_itab>-zccrq
                                 gv_bgck-tim
                                 <fs_itab>-zccsj
                        CHANGING <fs_itab>-zcc_bg.
    ENDIF.
  ENDLOOP.
  MOVE-CORRESPONDING itab TO <f_tab>.
  "��������ʱ�丳ֵ
  LOOP AT <f_tab> ASSIGNING <f_line>.
    ASSIGN COMPONENT 'VBELN' OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_vbeln_value>).
    LOOP AT gt_tp.
      REFRESH: r_matkl,gt_malips.
      CLEAR:zerdat_s,zertim_s,zerdat_e,zertim_e,zdat_tim_s,zdat_tim_e,cha.
      PERFORM splitstr(zpubform) TABLES r_matkl USING gt_tp-matkl ','.
      LOOP AT lt_matdat INTO DATA(wa_matdat) WHERE vbeln = <f_vbeln_value> AND matkl IN r_matkl.
        CLEAR gt_malips.
        MOVE-CORRESPONDING wa_matdat TO gt_malips.
        APPEND gt_malips.
      ENDLOOP.
      IF gt_malips[] IS NOT INITIAL.
        SORT gt_malips BY zerdat zertim.
        zerdat_s = gt_malips[ 1 ]-zerdat.
        zertim_s = gt_malips[ 1 ]-zertim.
        SORT gt_malips BY zerdat DESCENDING zertim DESCENDING.
        zerdat_e = gt_malips[ 1 ]-zerdat.
        zertim_e = gt_malips[ 1 ]-zertim.
        LOOP AT gt_fname.
          DATA(compname) = |{ gt_tp-type }{ gt_fname-fname }|.
          ASSIGN COMPONENT compname OF STRUCTURE <f_line> TO FIELD-SYMBOL(<f_dat_value>).
          CLEAR <f_dat_value>.
          CASE sy-tabix.
            WHEN 1.
              PERFORM getdatzet USING    zerdat_s
                                         zertim_s
                                CHANGING <f_dat_value>.
            WHEN 2.
              PERFORM getdatzet USING    zerdat_e
                                         zertim_e
                                CHANGING <f_dat_value>.

            WHEN 3.
              PERFORM getcha    USING    zerdat_s
                                         zerdat_e
                                         zertim_s
                                         zertim_e
                                CHANGING <f_dat_value>.
            WHEN OTHERS.
              MESSAGE s004 WITH 'gt_fname���ֵ�4���ˣ����޸��߼�' DISPLAY LIKE 'E'.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  REFRESH itab.
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
'VBELN         '  '����֪ͨ����                '  'VBAK'  'VBELN',
'ZYSFZR        '  '������                    '  'VBAK'  'ZYSFZR',
'ZLYRQSJ       '  '��Լ�ύ                   '  ''  '',
'ZBJRQSJ       '  '����ʱ�� '                   ''  '',
'ZBJ_LY        '  '����ʱЧ              '  ''  '',
'ZZBRQSJ       '  '�б�ʱ��                      '  ''  '',
'ZZB_BJ        '  '����ʱЧ                      '  ''  '',
'ZZHRQSJ       '  'Ԥ��װ��ʱ��    '                    ''  '',
'ZRCRQSJ       '  '��������ʱ��       '                   '' '',
'ZZHRQSJ_START '  'װ����ʼ '                      ''  '',
'ZZHRQSJ_END   '  'װ������                         '  ''  '',
'ZWAIT         '  '�����ȴ�ʱ��               '       ''  '',
'ZZHSX         '  'װ��ʱЧ'                  ''  '',
'ZBGRQSJ       '  '���ܳ���                       '  ''  '',
'ZCCRQSJ       '  '��������                        '  ''  '',
'ZCC_BG       '  '����ͣ��ʱ��                    '  ''  '',
'ZHDRQSJ       '  '�ص��ϴ�                       '  ''  '',
'ZXHRQSJ       '  'Ԥ��ж��                    '  ''  '',
'ZHD_CC       '  'ʵ������ʱЧ                  '  ''  '',
'ZYJXH_ZH     '  'Ԥ������ʱЧ                  '  ''  ''.
  LOOP AT gt_tp.
    LOOP AT gt_fname.
      fieldname = gt_tp-type && gt_fname-fname.
      fieldtext = gt_tp-text && gt_fname-ftext.
      PERFORM catlg_set TABLES fldct USING
             fieldname  fieldtext  ''  ''.
    ENDLOOP.
  ENDLOOP.

  i_title               = lines( <f_tab> ) .
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
    WHEN 'VIEW'. "�鿴��ϸ
      DATA(num) = 0.
      CLEAR num.
      MOVE-CORRESPONDING <f_tab> TO itab.
      LOOP AT itab INTO wa_itab WHERE sel = 'X'.
        num += 1.
      ENDLOOP.
      IF num NE 1.
        MESSAGE s022 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM view USING wa_itab-vbeln.
  ENDCASE.
  "����Ҫˢ��
*  rs_selfield-row_stable = 'X'.
*  rs_selfield-col_stable = 'X'.
*  rs_selfield-refresh    = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form getdatzet
*&---------------------------------------------------------------------*
*& ����ʱ��ƴ��
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdatzet USING s_dat TYPE sy-datum
                     s_zet TYPE sy-timlo
               CHANGING s_datzet.
  IF s_dat IS NOT INITIAL.
    s_datzet = |{ s_dat+0(4) }-{ s_dat+4(2) }-{ s_dat+6(2) } { s_zet+0(2) }:{ s_zet+2(2) }:{ s_zet+4(2) }|.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getcha
*&---------------------------------------------------------------------*
*& ��ȡ����ʱ������Сʱ��λ
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getcha USING    p_d1 TYPE d
                     p_d2 TYPE d
                     p_t1 TYPE t
                     p_t2 TYPE t
            CHANGING p_m TYPE hour .
  DATA:p_hour TYPE i.
  CALL FUNCTION 'DELTA_TIME_DAY_HOUR'
    EXPORTING
      t1      = p_t1
      t2      = p_t2
      d1      = p_d1
      d2      = p_d2
    IMPORTING
      minutes = p_hour.
  p_m = p_hour / 60.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_dynamic_table
*&---------------------------------------------------------------------*
*& ��ȡ��̬�ڱ�
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_dynamic_table.
* 1. ���������ͻ�ȡ���
  lo_struct ?= cl_abap_typedescr=>describe_by_name( 'TY_ITAB' ).
  lt_comp  = lo_struct->get_components( ).
  APPEND LINES OF lt_comp TO lt_tot_comp.
  LOOP AT gt_tp.
*
*   Ԫ������
    LOOP AT gt_fname.
      IF sy-tabix = 3.
        lo_element ?= cl_abap_elemdescr=>describe_by_name( 'HOUR' ).
        CONCATENATE gt_tp-type gt_fname-fname INTO la_comp-name.
        la_comp-type = cl_abap_elemdescr=>get_p(
          p_length   = lo_element->length
          p_decimals = lo_element->decimals ).
      ELSE.
        lo_element ?= cl_abap_elemdescr=>describe_by_name( 'CHAR19' ).
*   �ֶ�����
        CONCATENATE gt_tp-type gt_fname-fname INTO la_comp-name.
*
*   �ֶ�����
        la_comp-type = cl_abap_elemdescr=>get_c(
          p_length = lo_element->length ).
      ENDIF.
*
*   ��д�����
      APPEND la_comp TO lt_tot_comp.
      CLEAR: la_comp.
    ENDLOOP.

  ENDLOOP.
* 3. ����������
  lo_new_type = cl_abap_structdescr=>create( lt_tot_comp ).
*
* 4. �½��������
  lo_new_tab = cl_abap_tabledescr=>create(
    p_line_type  = lo_new_type
    p_table_kind = cl_abap_tabledescr=>tablekind_std
    p_unique     = abap_false ).
*
* 5. ���ڴ����±����͵�����
  CREATE DATA lo_data TYPE HANDLE lo_new_tab.
*
* 6. �ֶ��е����ڲ������
  ASSIGN lo_data->* TO <f_tab>.
* 7. ������̬�������ṹ
  CREATE DATA dyn_wa LIKE LINE OF <f_tab>.
* 8. ������̬������
  ASSIGN dyn_wa->* TO <f_line>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form view
*&---------------------------------------------------------------------*
*& �鿴��ϸ
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM view USING p_vbeln TYPE vbak-vbeln.
  DATA:fieldcat TYPE slis_t_fieldcat_alv.
  DATA:gt_dtab LIKE TABLE OF zspp222 WITH HEADER LINE.
  CALL FUNCTION 'ZFM_GP_OCP_PP_GETPHXX'
    EXPORTING
      vbeln   = p_vbeln
    TABLES
      out_tab = gt_dtab[].

  PERFORM catlg_set TABLES fieldcat
                    USING:
    'ZPHZ ' '���վ      '  ''  '',
    'ZLXR ' '��ϵ��     '  ''  '',
    'ZLXDH ' '��ϵ�绰  '  ''  '',
    'ZPRICE ' '����     '