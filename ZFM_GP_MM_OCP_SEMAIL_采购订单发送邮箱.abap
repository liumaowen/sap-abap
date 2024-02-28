FUNCTION zfm_gp_mm_ocp_semail.
*"----------------------------------------------------------------------
*"*"���ؽӿڣ�
*"  IMPORTING
*"     VALUE(EBELN) TYPE  EBELN
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(P_OUTPUT) TYPE  STRING
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_GP_MM_OCP_SEMAIL'.
  zfmdatasave2 'B'.
  COMMIT WORK.
***********************************************
  TYPES:BEGIN OF ty_ekko,
          zmblj TYPE string,             "ģ������
          zfjr  TYPE ekko-zzlms,         "������
          zfjyx TYPE ztmm252-smtp_addr,  "�������ʼ�
          zfjmm TYPE string,             "�����ʼ�����
          lifnr TYPE ekko-lifnr,         "
          zsjr  TYPE lfa1-name1,         "�ռ���
          zsjyx TYPE ztmm252-smtp_addr,  "�ռ�������
        END OF ty_ekko.
  DATA:gs_ekko TYPE ty_ekko.
  DATA:mes_workplancreate_url   TYPE string,
       mes_workplancreate_str   TYPE string,
       mes_workplancreatere_str TYPE string,
       mes_workplancreate_msg   TYPE string,
       mes_workplancreate_sta   TYPE i.
  TYPES: BEGIN OF t_token_re,
           status_code TYPE i,
           msg         TYPE bapi_msg,
         END OF t_token_re.
  DATA: itokenre TYPE t_token_re.
  CONSTANTS zerpbh TYPE ztsd219-zerpbh VALUE '866'. "���Э��PDFģ��id
  IF ebeln IS INITIAL.
    fillmsgr 'E' '����ebelnΪ��'.
  ENDIF.

*url��Ϊ�����ã�zmm000
  PERFORM getdata(zpub_data) USING 'ZFM_GP_MM_OCP_SEMAIL' CHANGING mes_workplancreate_url.
  IF mes_workplancreate_url IS INITIAL.
    fillmsgr 'E' '�������ӿ�urlΪ�գ�ZMM000������url'.
  ENDIF.

  SELECT
  SINGLE
  e~zzlms AS zfjr,
  f~smtp_addr AS zfjyx,
  f~zpass AS zfjmm,
  e~lifnr,
  l~name1 AS zsjr,
  a~smtp_addr AS zsjyx
  FROM ekko AS e
  LEFT JOIN ztmm252 AS f ON f~ekorg = e~ekorg
  INNER JOIN lfa1 AS l ON l~lifnr = e~lifnr
  LEFT JOIN adr6 AS a ON a~addrnumber = l~adrnr
  WHERE ebeln = @ebeln
  INTO CORRESPONDING FIELDS OF @gs_ekko.
  IF sy-subrc EQ 0.
    PERFORM getpdfurl(zmmd200) USING    ebeln
                                        zerpbh
                               CHANGING gs_ekko-zmblj
                                        rtype
                                        rtmsg.
    IF rtype EQ 'E'.
      fillmsgr 'E' rtmsg.
    ENDIF.
    IF gs_ekko-zmblj IS INITIAL.
      fillmsgr 'E' 'ģ�����ӻ�ȡΪ��'.
    ENDIF.
    "����OCP�������ӿ�
    mes_workplancreate_str = /ui2/cl_json=>serialize( data = gs_ekko compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    p_output = mes_workplancreate_str.
    PERFORM replace(zpubform) USING 'https' 'http' CHANGING mes_workplancreate_url .

    CALL FUNCTION 'ZFMS_15_HTTP'
      EXPORTING
        input     = mes_workplancreate_str
        url       = mes_workplancreate_url
        reqmethod = 'POST' "HTTP ����
        http1_1   = 'X' "Э��1.1/1.0
      IMPORTING
        output    = mes_workplancreatere_str "����JSON����
        rtmsg     = mes_workplancreate_msg "��Ϣ
        status    = mes_workplancreate_sta "HTTP״̬
      EXCEPTIONS
        OTHERS    = 1.
    CLEAR itokenre.
    /ui2/cl_json=>deserialize( EXPORTING json = mes_workplancreatere_str pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = itokenre ).
    rtmsg = itokenre-msg.
    IF mes_workplancreate_sta = '200'.
      IF itokenre-status_code = '200'.
        rtype = 'S'.
        rtmsg = '�������ͳɹ�,' && rtmsg.
      ELSE.
        rtype = 'E'.
        rtmsg = '��������ʧ��,' && rtmsg.
      ENDIF.
    ELSE.
      rtype = 'E'.
      rtmsg = '���������������ӳ��ִ���,' && rtmsg