FUNCTION zfm_gp_ocp_pp_pcotd.
*"----------------------------------------------------------------------
*"*"���ؽӿڣ�
*"  IMPORTING
*"     REFERENCE(ZZDEL) TYPE  ZE_DEL OPTIONAL
*"  EXPORTING
*"     REFERENCE(RTYPE) TYPE  BAPI_MTYPE
*"     REFERENCE(RTMSG) TYPE  BAPI_MSG
*"     REFERENCE(P_OUTPUT) TYPE  STRING
*"  TABLES
*"      INTAB STRUCTURE  ZSBI_PCD
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_GP_OCP_PP_PCOTD'.
  zfmdatasave2 'B'.
  COMMIT WORK.
***********************************************
  DATA:intab1    TYPE TABLE OF zsbi_pcd WITH KEY zpcdh,
       wa_intab1 LIKE LINE OF intab1.
  DATA:BEGIN OF gt_tab OCCURS 0,
         zpcdh    TYPE ztpp_205-zpcdh,     "�Ų�����
         zpcdhh   TYPE ztpp_205a-zpcdhh,   "�Ų����к�
         vbeln    TYPE ztpp_205-vbeln,     "SAP��ͬ��
         posnr    TYPE ztpp_205-posnr,     "��ͬ�к�
         zjsxdid  TYPE ztpp_205a-zjsxdid,  "�굥����
         zjsxddid TYPE ztpp_205a-zjsxddid, "�굥�к�
         kunnr    TYPE ztpp_205-kunnr,     "�ͻ�����
         name1    TYPE ztpp_205-name1,     "�ͻ�����
         bstkd    TYPE ztpp_205-bstkd,     "OCP�ͻ�
         werks    TYPE ztpp_205-werks,     "��������
         posid    TYPE ztpp_205-posid,     "��Ŀ����
         post1    TYPE ztpp_205-post1,     "��Ŀ����
         zzks     TYPE ztpp_205a-zks,      "�ܿ���
         zzms     TYPE ztpp_205a-zms,      "������
         zks      TYPE ztpp_205a-zks,      "����
         zms      TYPE ztpp_205a-zms,      "����
         zdel     TYPE ztpp_205a-del,      "�Ƿ�ɾ��
         zzdel    TYPE ztpp_205a-del,      "�Ų��������Ƿ�ɾ��
       END OF gt_tab.
  DATA:zzks TYPE ztpp_205a-zks,
       zzms TYPE ztpp_205a-zms.
  DATA:mes_workplancreate_url   TYPE string,
       mes_workplancreate_str   TYPE string,
       mes_workplancreatere_str TYPE string,
       mes_workplancreate_msg   TYPE string,
       mes_workplancreate_sta   TYPE i.
  TYPES: BEGIN OF t_token_re,
           flag    TYPE char2, "�ɹ�/ʧ��
           message TYPE bapi_msg,
         END OF t_token_re.
  DATA: itokenre TYPE t_token_re.

  IF intab[] IS INITIAL.
    fillmsgr 'E' '�ڱ�intabΪ��'.
  ENDIF.
*url��Ϊ�����ã�zmm000
  PERFORM getdata(zpub_data) USING 'ZFM_GP_OCP_PP_PCOTD' CHANGING mes_workplancreate_url.
  IF mes_workplancreate_url IS INITIAL.
    fillmsgr 'E' '�Ų����ش�OTD�ӿ�urlΪ�գ�ZMM000������url'.
  ENDIF.
  intab1 = intab[].
  IF zzdel = 'X'.
    SELECT
        c~zpcdh,
        b~zpcdhh,
        a~vbeln,
        a~posnr,
        b~zjsxdid,
        b~zjsxddid,
        a~kunnr,
        a~name1,
        a~bstkd,
        a~werks,
        a~posid,
        a~post1,
        b~zks,
        b~zms,
        b~del AS zdel,
        @zzdel AS zzdel
    FROM @intab1 AS c
    LEFT JOIN ztpp_205 AS a ON a~zpcdh = c~zpcdh
    LEFT JOIN ztpp_205a AS b ON a~zpcdh = b~zpcdh
    ORDER BY a~zpcdh
    INTO CORRESPONDING FIELDS OF TABLE @gt_tab.
  ELSE.
    SELECT
        a~zpcdh,
        b~zpcdhh,
        a~vbeln,
        a~posnr,
        b~zjsxdid,
        b~zjsxddid,
        a~kunnr,
        a~name1,
        a~bstkd,
        a~werks,
        a~posid,
        a~post1,
        b~zks,
        b~zms,
        b~del AS zdel,
        @zzdel AS zzdel
    FROM ztpp_205 AS a
    INNER JOIN ztpp_205a AS b ON a~zpcdh = b~zpcdh
    INNER JOIN @intab1 AS c ON a~zpcdh = c~zpcdh
    ORDER BY a~zpcdh
    INTO CORRESPONDING FIELDS OF TABLE @gt_tab.
  ENDIF.


  LOOP AT gt_tab INTO DATA(gs_tab) GROUP BY ( zpcdh = gs_tab-zpcdh size = GROUP SIZE
                                                 index = GROUP INDEX )
                                                 ASCENDING ASSIGNING FIELD-SYMBOL(<group>).
    CLEAR:zzks,zzms.
    LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<fs_meb>).
      zzks += <fs_meb>-zks.
      zzms += <fs_meb>-zms.
    ENDLOOP.
    <fs_meb>-zzks = zzks.
    <fs_meb>-zzms = zzms.
    MODIFY gt_tab FROM <fs_meb> TRANSPORTING zzks zzms WHERE zpcdh = <fs_meb>-zpcdh.
  ENDLOOP.

  "����mes�������󴴽��ӿ�
  mes_workplancreate_str = /ui2/cl_json=>serialize( data = gt_tab[] compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-none ).
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
  rtmsg = itokenre-message.
  IF mes_workplancreate_sta = '200'.
    IF itokenre-flag = '�ɹ�'.
      rtype = 'S'.
      rtmsg = '�Ų����ش�OTD���ͳɹ�'.
    ELSE.
      rtype = 'E'.
      rtmsg = '�Ų����ش�OTD����ʧ��,' && rtmsg.
    ENDIF.
  ELSE.
    rtype