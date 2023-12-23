FUNCTION zfm_gp_mm_ocp_dbgz.
*"----------------------------------------------------------------------
*"*"���ؽӿڣ�
*"  IMPORTING
*"     VALUE(ZDBDH) TYPE  ZTMM220-ZDBDH
*"  TABLES
*"      RETURN2 STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_GP_MM_OCP_DBGZ'.
  zfmdatasave2 'B'.
  COMMIT WORK.
*************************************
  DATA: wa_220 TYPE ty_220,
        it_221 TYPE TABLE OF ty_221 WITH HEADER LINE.
  DATA:rtype1  TYPE bapi_mtype.
  IF zdbdh IS INITIAL.
    PERFORM inmsg(zpubform) TABLES return2 USING 'ZGP_MSG' 'E' '001' '����zdbdh' '' '' ''.
    RETURN.
  ENDIF.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF @wa_220 FROM ztmm220 WHERE zdbdh = @zdbdh.
  IF sy-subrc NE 0.
    PERFORM inmsg(zpubform) TABLES return2 USING 'ZGP_MSG' 'E' '004' '�鲻��������̧ͷ��Ϣ' '' '' ''.
    RETURN.
  ENDIF.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE @it_221 FROM ztmm221 WHERE zdbdh = @zdbdh.
  IF sy-subrc NE 0.
    PERFORM inmsg(zpubform) TABLES return2 USING 'ZGP_MSG' 'E' '004' '�鲻����������ϸ��Ϣ' '' '' ''.
    RETURN.
  ENDIF.
*����

  PERFORM migo311 IN PROGRAM zmmd201 TABLES it_221 return2 CHANGING wa_220 rtype1.

*************************************
  zf