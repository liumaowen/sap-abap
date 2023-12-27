*&---------------------------------------------------------------------*
*& Report ZSDB008
*&---------------------------------------------------------------------*
*&�ͻ�����������
*&---------------------------------------------------------------------*
REPORT zsdb008 MESSAGE-ID zxmd_msg.
TABLES:sscrfields,t001l,afko.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
TYPES:BEGIN OF ty_kunnr,
        ktokd TYPE  kna1-ktokd,    "����
        kunnr TYPE  kna1-kunnr,    "�ͻ�����
        bukrs TYPE  knb1-bukrs,    "��˾����
        akont TYPE  knb1-akont,    "ͳԦ��Ŀ
        vkorg TYPE  knvv-vkorg,    "������֯
        vtweg TYPE  knvv-vtweg,    "��������
        spart TYPE  knvv-spart,    "��Ʒ��
        awahr TYPE  knvv-awahr,    "����������
        waers TYPE  knvv-waers,    "����
        vsbed TYPE  knvv-vsbed,    "װ������
        podkz TYPE  knvv-podkz,    "POD
        kalks TYPE  knvv-kalks,    "Cust.Pric.����
        taxkd TYPE  knvi-taxkd,    "˰����
        ktgrd TYPE  knvv-ktgrd,    "�ͻ���Ŀ������
        zterm TYPE  knvv-zterm,    "��������
      END OF ty_kunnr.
DATA:it_kunnr TYPE TABLE OF ty_kunnr,
     wa_kunnr LIKE LINE OF it_kunnr.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE   TYPE  ty_kunnr.
DATA:  memo(200),                  "��Ϣ�ı�
       sel,
     END OF itab.
TYPES:BEGIN OF ty_kna1,
        kunnr      TYPE kna1-kunnr,
        bahne      TYPE kna1-bahne,
        land1      TYPE kna1-land1,
        ort01      TYPE kna1-ort01,
        regio      TYPE kna1-regio,
        name4      TYPE kna1-name4,
        str_suppl3 TYPE adrc-str_suppl3,
        landx      TYPE t005t-landx,
        bezei      TYPE t005u-bezei,
        bezei20    TYPE t005u-bezei,
      END OF ty_kna1.
DATA:it_but000 TYPE TABLE OF but000 WITH HEADER LINE.
DATA:it_but020 TYPE TABLE OF but020 WITH HEADER LINE.
DATA: it_kna1 TYPE TABLE OF ty_kna1,
      wa_kna1 TYPE ty_kna1.
DATA: it_kna2 TYPE TABLE OF ty_kna1,
      wa_kna2 TYPE ty_kna1.
DATA:it_knvv TYPE TABLE OF knvv,
     wa_knvv TYPE knvv.
DATA:it_tb003i TYPE TABLE OF tb003i WITH HEADER LINE.
DATA: it_but0bk TYPE TABLE OF but0bk,
      wa_but0bk TYPE but0bk.
DATA : ls_partner  TYPE bus_ei_extern,
       ls_customer TYPE cmds_ei_extern.
DATA: lt_company TYPE cmds_ei_company_t,
      ls_company TYPE cmds_ei_company,
      lt_tax_ind TYPE cmds_ei_tax_ind_t,
      ls_tax_ind TYPE cmds_ei_tax_ind,
      lt_taxs    TYPE cmds_ei_tax_ind_t,
      ls_tax     TYPE cmds_ei_tax_ind,
      lt_sales   TYPE cmds_ei_sales_t,
      ls_sales   TYPE cmds_ei_sales.
DATA : ls_header       TYPE cmds_ei_header,
       ls_central_data TYPE cmds_ei_central_data,
       ls_company_data TYPE cmds_ei_cmd_company,
       ls_sales_data   TYPE cmds_ei_cmd_sales.
DATA : ls_data   TYPE  cvis_ei_extern,
       lt_data   TYPE  cvis_ei_extern_t,
       lt_return TYPE  bapiretm,
       ls_return TYPE  bapireti,
       ls_msg    TYPE bapiretc.
DATA:mdm_intab  TYPE TABLE OF zssd_010 WITH HEADER LINE,
     mdm_outtab TYPE TABLE OF zfm_sd_mdm_custom_s1 WITH HEADER LINE.
DATA:mdm_rtype TYPE bapi_mtype,
     mdm_rtmsg TYPE bapi_msg.
DATA:it_DFKKBPTAXNUM TYPE TABLE OF dfkkbptaxnum WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:p_sel  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY :1.

INITIALIZATION.
  sscrfields-functxt_01 = '@14@����ģ��'.
  %_p_sel_%_app_%-text = '���Ƶ����а壨���Ʊ��⣩'.


AT SELECTION-SCREEN.
  CLEAR:fldct.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      REFRESH fldct.
      PERFORM copyfiled USING fldct.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
  IF p_sel NE 'X'.
    MESSAGE s004 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  PERFORM cliptoitab(zpubform) TABLES it_kunnr.
*  LOOP AT it_kunnr INTO wa_kunnr.
*    AUTHORITY-CHECK OBJECT 'F_KNA1_BUK'
*      ID 'ACTVT' FIELD '03'
*      ID 'BUKRS' FIELD wa_kunnr-bukrs.
*    IF sy-subrc NE 0.
*      MESSAGE s000(oo) WITH 'û�й�˾' wa_kunnr-bukrs '��Ȩ��'.
*      STOP.
*    ENDIF.
*
*    AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
*       ID 'ACTVT' FIELD '03'
*       ID 'VKORG' FIELD wa_kunnr-vkorg.
*    IF sy-subrc NE 0.
*      MESSAGE s000(oo) WITH 'û��������֯' wa_kunnr-vkorg '��Ȩ��'.
*      STOP.
*    ENDIF.
*  ENDLOOP.
  IF it_kunnr IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
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
  LOOP AT it_kunnr INTO wa_kunnr.
    MOVE-CORRESPONDING wa_kunnr TO itab.
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
  varnt-handle = 1."�����û�����

  PERFORM catlg_set TABLES fldct
                    USING:
'MEMO '  '������Ϣ    '   '   '  '     ' ,
'KTOKD'  '����        '  'KNA1'  'KTOKD' ,
'KUNNR'  '�ͻ�����    '  'KNA1'  'KUNNR' ,
'BUKRS'  '��˾����    '  'KNB1'  'BUKRS' ,
'AKONT'  'ͳԦ��Ŀ    '  'KNB1'  'AKONT' ,
'VKORG'  '������֯    '  'KNVV'  'VKORG' ,
'VTWEG'  '��������    '  'KNVV'  'VTWEG' ,
'SPART'  '��Ʒ��      '  'KNVV'  'SPART' ,
'AWAHR'  '����������  '  'KNVV'  'AWAHR' ,
'WAERS'  '����        '  'KNVV'  'WAERS' ,
'VSBED'  'װ������    '  'KNVV'  'VSBED' ,
'PODKZ'  'POD         '  'KNVV'  'PODKZ' ,
'KALKS'  'Cust.Pric.��'  'KNVV'  'KALKS' ,
'TAXKD'  '˰����      '  'KNVI'  'TAXKD' ,
'KTGRD'  '�ͻ���Ŀ������' 'KNVV'  'KTGRD' ,
'ZTERM'  '��������'       'KNVV'  'ZTERM' .

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
    WHEN 'BUT03'.
      PERFORM but03.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.

FORM copyfiled USING p_fldct LIKE fldct.
  PERFORM init_fieldcat(zpubform) TABLES p_fldct
  USING :'' '����' '' '' '' '',
         '' '�ͻ�����' '' '' '' '',
         '' '��˾����' '' '' '' '',
         '' 'ͳԦ��Ŀ' '' '' '' '',
         '' '������֯' '' '' '' '',
         '' '��������' '' '' '' '',
         '' '��Ʒ��' '' '' '' '',
         '' '����������' '' '' '' '',
         '' '����' '' '' '' '',
         '' 'װ������' '' '' '' '',
         '' 'POD' '' '' '' '',
         '' 'Cust.Pric.����' '' '' '' '',
         '' '˰����' '' '' '' '',
         '' '�ͻ���Ŀ������' '' '' '' '',
         '' '��������' '' '' '' ''
        .
  PERFORM itabstructoclip(zpubform) USING p_fldct '' ''.
ENDFORM.

FORM but03.
  DATA : lv_msg TYPE string.
  DATA : lv_msg1 TYPE string.

  DATA:lt_return_ukmbp1 TYPE ukm_t_monitor_return,
       lt_return_frg1   TYPE TABLE OF bapiret2 WITH HEADER LINE.



*��ǰ����
  LOOP AT it_kunnr INTO wa_kunnr.
    PERFORM addzero(zpubform) CHANGING wa_kunnr-kunnr.
    MODIFY it_kunnr FROM wa_kunnr TRANSPORTING kunnr.
  ENDLOOP.

  IF it_kunnr IS NOT INITIAL.

    CLEAR:it_but000,it_but020.
    SELECT partner
           partner_guid
           name_org1
        INTO CORRESPONDING FIELDS OF TABLE it_but000
        FROM but000
      FOR ALL ENTRIES IN it_kunnr
      WHERE partner = it_kunnr-kunnr
      .


    SELECT kunnr
           eikto
           vsort
      INTO CORRESPONDING FIELDS OF TABLE it_knvv
      FROM knvv
      FOR ALL ENTRIES IN it_kunnr
      WHERE kunnr = it_kunnr-kunnr.

    SELECT
        kna1~kunnr
        kna1~land1
        kna1~regio
        t005t~landx
        t005u~bezei AS bezei20
        kna1~ort01
        kna1~telf1
        adrc~str_suppl3
        kna1~name4
      INTO CORRESPONDING FIELDS OF TABLE it_kna1
      FROM kna1
      INNER JOIN t005t ON kna1~land1 = t005t~land1
      INNER JOIN t005u ON kna1~regio = t005u~bland AND t005u~spras = sy-langu
                       AND kna1~land1 = t005u~land1
      INNER JOIN adrc ON adrc~addrnumber = kna1~adrnr
      FOR ALL ENTRIES IN it_kunnr
      WHERE kunnr = it_kunnr-kunnr
      AND t005t~spras = sy-langu
      .

    SELECT
      kunnr
      INTO CORRESPONDING FIELDS OF TABLE it_kna2
      FROM kna1
      FOR ALL ENTRIES IN it_kunnr
      WHERE kunnr = it_kunnr-kunnr.

  ENDIF.

  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE it_but020
   FROM but020
 WHERE partner = itab-kunnr.

  SELECT *
     INTO TABLE it_tb003i
    FROM tb003i
    WHERE rltgr = 'ZWSDCU'.

  CLEAR:it_but0bk.
  SELECT bankl
         bankn
         partner
         banks
         bkext
         accname
    INTO CORRESPONDING FIELDS OF TABLE it_but0bk
    FROM but0bk.



  LOOP AT itab.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'   " �ͻ����벹��
      EXPORTING
        input  = itab-kunnr
      IMPORTING
        output = itab-kunnr.

    SORT it_but000.
    READ TABLE it_but000  WITH KEY partner = itab-kunnr.
    IF sy-subrc = 0.
      ls_partner-header-object_task = 'U'.
      ls_partner-header-object_instance-bpartnerguid = it_but000-partner_guid.
      ls_partner-header-object_instance-bpartner = it_but000-partner.

      ls_customer-header-object_instance-kunnr = ''.
      ls_customer-header-object_task = 'U'.

    ENDIF.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_but020
    FROM but020
      WHERE partner = itab-kunnr.

    "��չ��ɫ
    LOOP AT it_tb003i.
      PERFORM create_role USING itab-kunnr it_tb003i-role CHANGING itab-memo.
    ENDLOOP.

    IF itab-ktokd IS NOT INITIAL.
      ls_partner-central_data-common-data-bp_control-grouping = itab-ktokd.  " ����
    ENDIF.


    ls_partner-central_data-common-data-bp_centraldata-partnerlanguage = sy-langu.
    ls_partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.


****���¹�˾��������************************************************
    CLEAR:ls_company,lt_company.
    IF itab-bukrs IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'   " ͳԦ��Ŀ����
        EXPORTING
          input  = itab-akont
        IMPORTING
          output = itab-akont.

      ls_company-task = 'M'.

      ls_company-data_key-bukrs = itab-bukrs. " ��˾����
      ls_company-data-akont = itab-akont. " ͳԦ��Ŀ

      ls_company-datax-akont = abap_true.
      ls_company-datax-zuawa = abap_true.
      ls_company-datax-zterm = abap_true.
      APPEND ls_company TO lt_company.
      ls_company_data-company = lt_company.
      ls_customer-company_data = ls_company_data.

    ENDIF.

****˰����*******************************************************
    CLEAR:ls_tax_ind,lt_tax_ind.
    ls_tax_ind-task = 'M'.
    ls_tax_ind-data_key-aland = 'CN'.
    ls_tax_ind-data_key-tatyp = 'MWST'.
    ls_tax_ind-data-taxkd = itab-taxkd.
    ls_tax_ind-datax-taxkd = 'X'.
    APPEND ls_tax_ind TO  lt_tax_ind.

    CLEAR:ls_central_data.
    ls_central_data-tax_ind-tax_ind = lt_tax_ind.
    ls_customer-central_data = ls_central_data.


*****��������****************************************************

    CLEAR:ls_sales,lt_sales.
    IF  itab-vkorg IS NOT INITIAL
        AND itab-vtweg IS NOT INITIAL
        AND itab-spart IS NOT INITIAL.
      ls_sales-task = 'M'.
      ls_sales-data_key-vkorg = itab-vkorg.
      ls_sales-data_key-vtweg = itab-vtweg.
      ls_sales-data_key-spart = itab-spart.
      ls_sales-data-waers = itab-waers.
      ls_sales-data-kalks = itab-kalks.
      ls_sales-data-vsbed = itab-vsbed.
      ls_sales-data-pltyp = '01'.
      ls_sales-data-podkz = itab-podkz.
      ls_sales-data-ktgrd = itab-ktgrd.
      ls_sales-data-zterm = itab-zterm.
      ls_sales-data-inco1 = 'EXW'.
      ls_sales-data-inco2_l = '����'.
      ls_sales-datax-inco1 = abap_true.
      ls_sales-datax-inco2_l = abap_true.

      ls_sales-datax-waers = abap_true.
      ls_sales-datax-kalks = abap_true.
      ls_sales-datax-vsbed = abap_true.
      ls_sales-datax-pltyp = abap_true.
      ls_sales-datax-podkz = abap_true.
      ls_sales-datax-konda = abap_true.
      ls_sales-datax-vsort = abap_true.
      ls_sales-datax-eikto = abap_true.
      ls_sales-datax-ktgrd = abap_true.
      ls_sales-datax-zterm = abap_true.

      APPEND ls_sales TO lt_sales.
      ls_sales_data-sales = lt_sales.
      ls_customer-sales_data = ls_sales_data.
    ENDIF.

    CLEAR:ls_data,lt_data.
    ls_data-partner = ls_partner.
    ls_data-customer = ls_customer.

    APPEND ls_data TO lt_data.

    CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
      EXPORTING
        i_data   = lt_data
*       I_EXT_DATA       =
      IMPORTING
        e_return = lt_return.

    CLEAR lv_msg.
    LOOP AT lt_return INTO ls_return.

      LOOP AT ls_return-object_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.

        CONCATENATE lv_msg ls_msg-message INTO lv_msg.

      ENDLOOP.

    ENDLOOP.

    IF lv_msg IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      CONCATENATE '�ͻ�' it_but000-partner '���³ɹ���' INTO itab-memo.
      MODIFY itab.
    ELSE.
      itab-memo = lv_msg.
      MODIFY itab.
      EXIT.

    ENDIF.

    "����MDM��ȡBPCODE
    "����
    CLEAR :mdm_intab[].
    mdm_intab-kunnr = itab-kunnr.
    READ TABLE it_but000  WITH KEY partner = itab-kunnr.
    IF sy-subrc = 0.
      mdm_intab-name1 = it_but000-name_org1.
    ENDIF.

    mdm_intab-kdkg1 = ''.
    mdm_intab-kdkg2 = ''.
    mdm_intab-zxtly = ''.
    mdm_intab-bukrs = itab-bukrs.  "��˾����
    mdm_intab-butxt = ''.    "��˾����
    mdm_intab-zcontactstatus = ''.
    "ȡ�������ϵ�˼��绰
    READ TABLE it_knvv INTO wa_knvv WITH KEY kunnr = itab-kunnr.
    IF sy-subrc = 0.
      mdm_intab-bahne = wa_knvv-vsort.
      mdm_intab-bahns = wa_knvv-eikto.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = itab-kunnr.
    IF sy-subrc = 0.

      mdm_intab-land1 = wa_kna1-land1.     "����
      mdm_intab-regio = wa_kna1-regio.   "ʡ
      mdm_intab-ort01 = wa_kna1-ort01.  "������
      mdm_intab-name4 = wa_kna1-name4.   "����
      mdm_intab-landx = wa_kna1-landx .     "��������
      mdm_intab-bezei20 = wa_kna1-bezei20.    "ʡ����
      mdm_intab-str_suppl3 = wa_kna1-str_suppl3.
    ENDIF.

*    MDM_INTAB-BAHNS = ''.

    mdm_intab-telf1 = ''.
    "  "��ϸ��ַ
    mdm_intab-found_dat = sy-datum.


    mdm_intab-zcitycode = ''.  "�б���

    mdm_intab-counc = ''.   "�ش���
    mdm_intab-zbankstatus = ''.  "���й��Ҵ���
    READ TABLE it_but0bk INTO wa_but0bk WITH KEY partner = itab-kunnr.
    IF sy-subrc = 0.
      mdm_intab-banks = wa_but0bk-banks.  "���й��Ҵ���
      mdm_intab-bankl = wa_but0bk-bankl.  "���б��
      mdm_intab-banka = ''.
      mdm_intab-ebpp_accname = wa_but0bk-accname.
      mdm_intab-bankn = wa_but0bk-bankn.
    ENDIF.

    APPEND mdm_intab.

    CLEAR:mdm_rtype,mdm_rtmsg.
    CALL FUNCTION 'ZFM_SD_MDM_CUSTOM'
      IMPORTING
        rtype    = mdm_rtype
        rtmsg    = mdm_rtmsg
      TABLES
        in_tab   = mdm_intab
        t_result = mdm_outtab.
    IF mdm_outtab[] IS NOT INITIAL.
    ELSE.
      "MESSAGE '����MDMϵͳBP���ʧ��' TYPE 'E'.
      itab-memo = '����MDMϵͳBP���ʧ��'.
      MODIFY itab .
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form create_role
*&---------------------------------------------------------------------*
*& ����BP��ɫ
*&---------------------------------------------------------------------*
FORM create_role  USING    partner
                           role
                  CHANGING lv_msg.

  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE,
       wa_but100 TYPE but100.

  CLEAR:lv_msg.

  SELECT SINGLE *
    INTO wa_but100
    FROM but100
   WHERE partner = partner
     AND rltyp = role.

  IF sy-subrc <> 0.
    CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
      EXPORTING
        businesspartner     = partner
        businesspartnerrole = role
        validfromdate       = sy-datum
      TABLES
        return              = lt_return.
