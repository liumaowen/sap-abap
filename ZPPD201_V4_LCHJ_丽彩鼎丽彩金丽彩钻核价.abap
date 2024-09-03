*&---------------------------------------------------------------------*
*& Include zppd201_v4_lchj
*& ���ʶ����ʽ�������˼�
*&---------------------------------------------------------------------*


**********************************************************************
*& �����ʺ˼���Ļ
**********************************************************************
FORM openlchj USING p_zpcdh TYPE ztpp_205-zpcdh.
  CLEAR: gv_matkl,gv_reset.
  gv_matkl = wa_ggzd-matnr+0(5).
  CASE gv_matkl.
    WHEN 'A0100' OR 'A0200' OR 'A0400'.
      PERFORM gethead USING gv_reset.
      CALL SCREEN 9200.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM  gethead
*&---------------------------------------------------------------------*
*& ��ȡ̧ͷ���ݺ�һЩ���õ�Ĭ��ֵ
*&---------------------------------------------------------------------*
FORM gethead USING p_reset TYPE char1.

  CLEAR: wa_lchj.
  wa_lchj-matnr = wa_ggzd-matnr.
  wa_lchj-zwlcms = lv_wlcms.
  wa_lchj-zzsl = wa_ggzd-zzsl.
  wa_lchj-meins = wa_ggzd-meins.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZBH'.
  IF sy-subrc EQ 0.
    wa_lchj-zbh = outtab001-atwrt.
    wa_lchj-zxchd = outtab001-atwrt.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZBK'.
  IF sy-subrc EQ 0.
    wa_lchj-zbk = outtab001-atwrt.
    "ϵ�� = ��� / 1000
    wa_lchj-zxs = wa_lchj-zbk / 1000.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZBX'.
  IF sy-subrc EQ 0.
    wa_lchj-zbx = outtab001-atwrt.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZWBXZ'.
  IF sy-subrc EQ 0.
    wa_lchj-zwbxz = outtab001-atwrt.
  ENDIF.
  READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr atnam = 'ZWBTC'.
  IF sy-subrc EQ 0.
    wa_lchj-zwbtc = outtab001-atwrt.
  ENDIF.
  IF wa_ggzd-zzks <> 0.
    wa_lchj-zbc = wa_ggzd-zzms / wa_ggzd-zzks.
  ENDIF.
  "�˷�
  READ TABLE it_pcd INDEX 1.
  IF sy-subrc = 0.
    wa_lchj-zyf = it_pcd-zyf.
    wa_lchj-zpr0 = it_pcd-zyhtdj.
    IF wa_lchj-zpr0 IS INITIAL. "����ȡԭ��ͬ���ۣ���Ϊ�գ���ȡ����
      wa_lchj-zpr0 = it_pcd-zpr0.
    ENDIF.
  ENDIF.
  "����ȡ��־ztpp_205_hjlog ���е�����
  SELECT * FROM ztpp_205_hjlog WHERE zpcdh = @wa_ggzd-zpcdh ORDER BY zdate DESCENDING,ztime DESCENDING INTO TABLE @DATA(lt_205_hjlog).
  IF sy-subrc = 0 AND p_reset = ''.
    MOVE-CORRESPONDING lt_205_hjlog[ 1 ] TO wa_lchj.
  ELSE.
    PERFORM getdefault.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM  getdefault
*&---------------------------------------------------------------------*
*& һЩ���õ�Ĭ��ֵ
*&---------------------------------------------------------------------*
FORM getdefault.
  DATA:lv_zwbjsfm TYPE ztpp_205j4-zwbjsfm,
       lv_znbjsfm TYPE ztpp_205j4-znbjsfm.
  DATA:isvalue TYPE char1.

  CLEAR: lv_zwbjsfm,lv_znbjsfm.
  "���� ������+���޷����+��װ�� ���+������+����+��װ��
  SELECT * FROM ztpp_205j3 WHERE werks = @wa_ggzd-werks
                             AND matkl = @gv_matkl
                             AND zbx   = @wa_lchj-zbx
  INTO TABLE @DATA(lt_205j3).
  LOOP AT lt_205j3 INTO DATA(ls_205j3).
    IF wa_lchj-zbh > ls_205j3-zhdxx AND wa_lchj-zbh <= ls_205j3-zhdsx.
      wa_lchj-zwl = ls_205j3-zwl.
      CASE gv_matkl.
        WHEN 'A0200'."���ʽ�
          wa_lchj-zjc_ym_bz  = ls_205j3-zfb.
        WHEN OTHERS.
          wa_lchj-zfb_hm_jd_bz = ls_205j3-zfb.
      ENDCASE.
      isvalue = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  "û��ȡ��ֵ������ = ��� / 10 + 1
  IF isvalue = ''.
    CASE gv_matkl.
      WHEN 'A0200'."���ʽ�
        wa_lchj-zjc_ym_bz  = wa_lchj-zbh / 10 + 1.
      WHEN OTHERS.
        wa_lchj-zfb_hm_jd_bz = wa_lchj-zbh / 10 + 1.
    ENDCASE.
  ENDIF.
  "��ˮ-��Ĥ
  SELECT * FROM ztpp_205j4 WHERE werks = @wa_ggzd-werks AND matkl = @gv_matkl INTO TABLE @DATA(lt_205j4).
  SORT lt_205j4 BY zwbtc.
  READ TABLE lt_205j4 INTO DATA(ls_205j4) WITH KEY zwbtc = wa_lchj-zwbtc BINARY SEARCH.
  IF sy-subrc = 0.
    lv_zwbjsfm = ls_205j4-zwbjsfm.
    lv_znbjsfm = ls_205j4-znbjsfm.
  ELSE.
    READ TABLE lt_205j4 INTO ls_205j4 WITH KEY zwbtc = '����' BINARY SEARCH.
    IF sy-subrc = 0.
      lv_zwbjsfm = ls_205j4-zwbjsfm.
      lv_znbjsfm = ls_205j4-znbjsfm.
    ENDIF.
  ENDIF.
  IF wa_ggzd-zwbfmyq = '06'. "����Ĥ
    lv_zwbjsfm = lv_zwbjsfm - '1.2'.
  ENDIF.
  IF wa_ggzd-znbfmyq = '08'. "��Ĥ
    lv_zwbjsfm = lv_zwbjsfm + lv_znbjsfm.
  ENDIF.
  "��ˮ+��Ĥ: ���� * ��� / 1000
  wa_lchj-zjs_fm = lv_zwbjsfm * wa_lchj-zbk / 1000.
  "������� ����Ӽ� / ĸ��������� ���Ӱ���μӹ���
  PERFORM getzzfy.
  "ת�ǰ����/�̰��и�
  PERFORM getzjb.
  "��׼ë�� = �ܿ����� * ë������(0.59)
  SELECT
    SINGLE kwmeng
  FROM vbap
  WHERE vbeln = @wa_ggzd-vbeln
    AND posnr = @wa_ggzd-posnr
  INTO @DATA(lv_kwmeng). "��ͬ����

  IF lv_kwmeng > 0.
    SELECT * FROM ztpp_205j8
     WHERE werks = @wa_ggzd-werks
       AND matkl = @gv_matkl
       AND zbx   = @wa_lchj-zbx
    INTO TABLE @DATA(lt_205j8).
    LOOP AT lt_205j8 INTO DATA(ls_205j8).
      IF lv_kwmeng > ls_205j8-zddlxx AND lv_kwmeng <= ls_205j8-zddlsx.
        wa_lchj-zjcml = ls_205j8-zgklr * ls_205j8-zmlbl.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9200 OUTPUT.
  DATA lchj_extab TYPE slis_t_extab.
  CLEAR lchj_extab.
  REFRESH lchj_extab.

  LOOP AT SCREEN.
    CASE screen-group1.
*        "A1:WA_LCHJ-ZFB_HM_JD_BZ(���+������+����+��װ��)
*      WHEN 'A1'.
*        IF lv_matkl = 'A0200'."���ʽ�
*          screen-active = 0.
*        ENDIF.
*        "A2:WA_LCHJ-ZJC_YM_BZ(������+���޷����+��װ��)
*      WHEN 'A2'.
*        IF lv_matkl <> 'A0200'."���ʽ�
*          screen-active = 0.
*        ENDIF.
        "B1:WA_LCHJ-ZMBZZFY(ĸ���������) WA_LCHJ-ZHZBJGF(���Ӱ���μӹ���)
        "B2:WA_LCHJ-ZMBYLCB(ĸ�����ϳɱ�) WA_LCHJ-ZMBSH(ĸ�����2%) WA_LCHJ-ZHZBSH(���Ӱ��ͷ�и����0.05M)
        "   WA_LCHJ-ZHZBJGSH(���Ӱ�ӹ����0.4%) WA_LCHJ-ZDTHB(��ͷ����-�Ұ�ȼ۸�)
      WHEN 'B1' OR 'B2'.
        IF gv_matkl <> 'A0400'."������
          screen-input = 0.
        ENDIF.
        "C1:WA_LCHJ-ZZZFY(�������)  WA_LCHJ-ZZZJJ(����Ӽ�)  WA_LCHJ-ZZZSH(�������)  WA_LCHJ-ZYLCB(���ϳɱ�)
      WHEN 'C1'.
        IF gv_matkl = 'A0400'."������
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

  SET PF-STATUS 'STA9200' EXCLUDING  lchj_extab.
  SET TITLEBAR 'TIT9200' WITH '���ʲ�Ʒ�˼�'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  DATA:nborwb TYPE i. "�ڰ�-1 ���-2
  DATA:wa_ztpp_205_hjlog TYPE ztpp_205_hjlog.

  save_ok = sy-ucomm..

  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'QX'.
      LEAVE TO SCREEN 0.
    WHEN 'NBYY'. " �ڰ�����
      nborwb = 1.
      PERFORM qgyy USING    nborwb
                            wa_lchj-zbk
                            wa_lchj-zbh
                            wa_lchj-zbx
                            ''
                   CHANGING wa_lchj-znbycms
                            wa_lchj-znbcm
                            wa_lchj-znbdj
                            wa_lchj-znbsh
                            wa_lchj-znbzj
                            wa_lchj-znbct.
    WHEN 'WBYY'. " �������
      nborwb = 2.
      PERFORM qgyy USING    nborwb
                            wa_lchj-zbk
                            wa_lchj-zbh
                            wa_lchj-zbx
                            wa_lchj-zwbxz
                   CHANGING wa_lchj-zwbycms
                            wa_lchj-zwbcm
                            wa_lchj-zwbdj
                            wa_lchj-zwbsh
                            wa_lchj-zwbzj
                            wa_lchj-zwbct.
    WHEN 'HJJS'.
      PERFORM calc.
    WHEN 'RESET'.
      PERFORM reset.
    WHEN 'KCYY'.
      PERFORM kcyy.
    WHEN 'CON'.
      IF wa_lchj-zwbdj IS INITIAL.
        MESSAGE s004 WITH '���ּ�δ��д' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      IF wa_lchj-znbdj IS INITIAL.
        MESSAGE s004 WITH '�ڰ�ּ�δ��д' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      LOOP AT it_pcd.
        it_pcd-zgkcb = wa_lchj-zgkcb.
        it_pcd-zjcml = wa_lchj-zjcml.
        it_pcd-zmlj = wa_lchj-zygml.
        MODIFY it_pcd TRANSPORTING zgkcb zjcml zmlj.
      ENDLOOP.
      CLEAR wa_ztpp_205_hjlog.
      MOVE-CORRESPONDING wa_lchj TO wa_ztpp_205_hjlog.
      PERFORM getnum(zpubform) USING 'PCDLCHJLOG' 'D' 4 wa_ztpp_205_hjlog-znum."����������ˮ�� 4λ
      wa_ztpp_205_hjlog-zpcdh = wa_ggzd-zpcdh.
      wa_ztpp_205_hjlog-zdate = sy-datlo.
      wa_ztpp_205_hjlog-ztime = sy-timlo.
      wa_ztpp_205_hjlog-zname = sy-uname.
      INSERT ztpp_205_hjlog FROM wa_ztpp_205_hjlog.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& FORM  kcyy
*&---------------------------------------------------------------------*
*& �������
*&---------------------------------------------------------------------*
FORM kcyy.
  DATA:BEGIN OF wa_modal,
         nb_z01   TYPE char20,
         nb_charg TYPE char20,
         wb_z01   TYPE char20,
         wb_charg TYPE char20,
       END OF wa_modal.
  DATA:lt_z84    TYPE tt_z84,           "ԭ�ɹ����Ա��
       ls_z84    LIKE LINE OF lt_z84,
       lt_charg  TYPE tt_charg,         "���κ�
       lt_charg1 TYPE tt_charg,         "���κ�
       ls_charg  LIKE LINE OF lt_charg,
       lt_238    TYPE tt_238,           "���ʹ�ü�¼
       lt_238_tp TYPE tt_238,           "���ʹ�ü�¼_��ʱ
       ls_238    LIKE LINE OF lt_238.
  TYPES:BEGIN OF ty_ebeln,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
        END OF ty_ebeln.
  DATA:lt_ebeln TYPE TABLE OF ty_ebeln WITH KEY ebeln ebelp,
       ls_ebeln LIKE LINE OF lt_ebeln.
  DATA:zkd      TYPE decfloat16,
       lv_zsfzj TYPE ztpp_205b-zsfnbzj. "������Ƿ��ݼ�

  DEFINE addfields.
    CLEAR:fields.
    fields-tabname = &1.    " �ֶνṹ
    fields-fieldname = &2.  " �ֶ���
    fields-fieldtext = &3.  " ��ʾ�ı�
    fields-value = &4.      " Ĭ��ֵ
    fields-field_obl = &5.  " �Ƿ�������X
    fields-field_attr = &6. " �ܲ����޸ģ�02-���ɵ� ''-�ɵ�
    APPEND fields.
  End-OF-DEFINITION.

  CLEAR:fields[],returncode.
  CLEAR:fields.

  addfields: 'ZSPP_234'   'WB_Z01'      '����Ա��'   ''   ''    ''  ,
             'ZSPP_234'   'WB_CHARG'    '������κ�'   ''   ''    ''  ,
             'ZSPP_234'   'NB_Z01'      '�ڰ��Ա��'   ''   ''    ''  ,
             'ZSPP_234'   'NB_CHARG'    '�ڰ����κ�'   ''   ''    ''  .
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = '�������'
    IMPORTING
      returncode      = returncode
    TABLES
      fields          = fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF returncode IS INITIAL.
    LOOP AT fields.
      CONDENSE fields-value NO-GAPS.
      ASSIGN COMPONENT fields-fieldname OF STRUCTURE wa_modal TO <fs>.
      IF sy-subrc EQ 0.
        <fs> = fields-value.
      ENDIF.
    ENDLOOP.
    IF wa_modal-nb_z01 IS NOT INITIAL.
      ls_z84-z84 = wa_modal-nb_z01.
      COLLECT ls_z84 INTO lt_z84.
    ENDIF.
    IF wa_modal-nb_charg IS NOT INITIAL.
      ls_charg-charg = wa_modal-nb_charg.
      COLLECT ls_charg INTO lt_charg.
    ENDIF.
    PERFORM kcyycal USING    lt_charg
                             lt_z84
                             wa_ggzd-zsfnbzj
                    CHANGING wa_lchj-znbycms
                             wa_lchj-znbcm
                             wa_lchj-znbdj
                             wa_lchj-znbsh
                             wa_lchj-znbzj
                             wa_lchj-znbct.
    REFRESH:lt_z84,lt_charg.
    CLEAR:ls_z84,ls_charg.
    IF wa_modal-wb_z01 IS NOT INITIAL.
      ls_z84-z84 = wa_modal-wb_z01.
      COLLECT ls_z84 INTO lt_z84.
    ENDIF.
    IF wa_modal-wb_charg IS NOT INITIAL.
      ls_charg-charg = wa_modal-wb_charg.
      COLLECT ls_charg INTO lt_charg.
    ENDIF.
    PERFORM kcyycal USING    lt_charg
                             lt_z84
                             wa_ggzd-zsfwbzj
                    CHANGING wa_lchj-zwbycms
                             wa_lchj-zwbcm
                             wa_lchj-zwbdj
                             wa_lchj-zwbsh
                             wa_lchj-zwbzj
                             wa_lchj-zwbct.

  ENDIF.
ENDFORM.
*������õļ���
FORM kcyycal USING    t_charg  TYPE tt_charg
                      t_z84    TYPE tt_z84
                      p_zsfzj TYPE ztpp_205b-zsfwbzj
             CHANGING p_zycms TYPE zspp225_nb-znbycms
                      p_zcm   TYPE zspp225_nb-znbcm
                      p_zdj   TYPE zspp225_nb-znbdj
                      p_zsh   TYPE zspp225_nb-znbsh
                      p_zzj   TYPE zspp225_nb-znbzj
                      p_zct   TYPE zspp225_nb-znbct.
  DATA:lt_z84    TYPE tt_z84,           "ԭ�ɹ����Ա��
       lt_charg  TYPE tt_charg,         "���κ�
       lt_238    TYPE tt_238,           "���ʹ�ü�¼
       lt_238_tp TYPE tt_238,           "���ʹ�ü�¼_��ʱ
       ls_238    LIKE LINE OF lt_238.
  TYPES:BEGIN OF ty_ebeln,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
        END OF ty_ebeln.
  DATA:lt_ebeln TYPE TABLE OF ty_ebeln WITH KEY ebeln ebelp,
       ls_ebeln LIKE LINE OF lt_ebeln.
  DATA:zkd      TYPE decfloat16.

  IF t_charg IS NOT INITIAL OR t_z84 IS NOT INITIAL.
    PERFORM getkcjl USING t_z84 t_charg lt_238_tp.
    " ���ԭ�ļ۸�Ϊ�յĸ������κŲ�ѯ  2024-07-31 14:33:01 by lmw
    LOOP AT lt_238_tp INTO ls_238 WHERE zycjg IS INITIAL.
      ls_ebeln-ebeln = ls_238-ebeln.
      ls_ebeln-ebelp = ls_238-ebelp.
      COLLECT ls_ebeln INTO lt_ebeln.
    ENDLOOP.
    IF lt_ebeln IS NOT INITIAL.
      SELECT
        mg~charg
      FROM mseg AS mg
      INNER JOIN @lt_ebeln AS en ON  mg~ebeln = en~ebeln
                                 AND mg~ebelp = en~ebelp
      INNER JOIN ekko      AS eo ON  eo~ebeln = en~ebeln
      WHERE eo~bsart = 'UB'
        AND mg~bwart = '351'
        AND mg~xauto = ''
        AND NOT EXISTS ( SELECT * FROM  m_mbmps   "δ������
                                  WHERE m_mbmps~sjahr = mg~mjahr AND
                                        m_mbmps~smbln = mg~mblnr AND
                                        m_mbmps~smblp = mg~zeile )
      GROUP BY mg~charg
      INTO TABLE @lt_charg.
    ENDIF.
    lt_238 = lt_238_tp.
    IF lt_charg IS NOT INITIAL.
      REFRESH: lt_238_tp,lt_z84.
      PERFORM getkcjl USING lt_z84 lt_charg lt_238_tp.
      APPEND LINES OF lt_238_tp TO lt_238.
    ENDIF.
    LOOP AT lt_238 INTO ls_238.
      ls_238-zycjg = ls_238-zycjg + ls_238-zysjg.
      SELECT SINGLE maktx FROM makt WHERE matnr = @ls_238-matnr AND spras = @sy-langu INTO @ls_238-zwlcms.
      MODIFY lt_238 FROM ls_238 TRANSPORTING zycjg zwlcms.
    ENDLOOP.
    DELETE lt_238 WHERE zycjg IS INITIAL.
    READ TABLE lt_238 INTO ls_238 INDEX 1.
    IF sy-subrc = 0.
      p_zdj = ls_238-zycjg.
      p_zcm = ls_238-ztm.
      p_zycms = ls_238-zwlcms.
      zkd = CONV decfloat16( ls_238-zkd ).
      "��ȡ��������
      PERFORM getct USING    wa_lchj-zwbxz wa_lchj-zbx wa_lchj-zbh wa_lchj-zbk zkd
                    CHANGING p_zct.
      "��ȡ���������ݼ�
      PERFORM getshzj USING    ls_238-zzl1 p_zsfzj
                      CHANGING p_zsh
                               p_zzj.
      PERFORM calc.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM  qgyy
*&---------------------------------------------------------------------*
*& ����������빺��
*&---------------------------------------------------------------------*
*& -->  p_zycms    �ڰ�/���ԭ������
*& -->  p_zcm      �ڰ�/������
*& -->  p_zdj      �ڰ�/���ּ�
*& -->  p_zsh      �ڰ�/������
*& -->  p_zzj      �ڰ�/����ݼ�
*& -->  p_zct      �ڰ�/������
*& -->  p_zmj      �ڰ�/����׼�
*& <--  p_nw       �ڰ�/���  �ڰ�-1 ���-2
*& <--  p_zbk      ���
*& <--  p_zbh      ���
*& <--  p_zbx      ����
*& <--  p_zwbxz    �����״
*----------------------------------------------------------------------*
FORM qgyy USING    p_nw    TYPE i
                   p_zbk   TYPE zebckd
                   p_zbh   TYPE zebchd
                   p_zbx   TYPE zbx
                   p_zwbxz TYPE zwbxz
          CHANGING p_zycms TYPE zspp225_nb-znbycms
                   p_zcm   TYPE zspp225_nb-znbcm
                   p_zdj   TYPE zspp225_nb-znbdj
                   p_zsh   TYPE zspp225_nb-znbsh
                   p_zzj   TYPE zspp225_nb-znbzj
                   p_zct   TYPE zspp225_nb-znbct.

  DATA:lv_nwbkd TYPE decfloat16,         "ԭ�Ŀ��
       lv_zsfzj TYPE ztpp_205b-zsfnbzj   "������Ƿ��ݼ�
       .
  DATA:it_matnr_nwb  TYPE TABLE OF ccvx_matnr WITH HEADER LINE, "����־�-�����
       outtab001_nwb TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
  DATA:lvv_zwbxz TYPE zwbxz.

  CLEAR:gs_lchj_operate,lv_qgd.

  CASE p_nw.
    WHEN 1.
      gs_lchj_operate-nbyy = 'X'.
      lv_zsfzj = wa_ggzd-zsfnbzj.
    WHEN 2.
      lv_zsfzj = wa_ggzd-zsfwbzj.
      gs_lchj_operate-wbyy = 'X'.
  ENDCASE.
  PERFORM pick. "���빺������
  IF lv_qgd-matnr IS NOT INITIAL. "�빺��ѡ����ϸ��
    p_zycms = lv_qgd-zwlcms.
    p_zcm = floor( lv_qgd-zcm ).
    it_matnr_nwb-matnr = lv_qgd-matnr.
    APPEND it_matnr_nwb.
    "ȡ001����
    PERFORM get001 IN PROGRAM zpubform TABLES it_matnr_nwb outtab001_nwb USING 'ZKD'.
    SORT outtab001_nwb BY matnr atnam.
    READ TABLE outtab001_nwb WITH KEY matnr = lv_qgd-matnr atnam = 'ZKD' BINARY SEARCH.
    IF sy-subrc = 0.
      lv_nwbkd = outtab001_nwb-atwrt.
    ENDIF.
    p_zdj = lv_qgd-zdj.
    "��ȡ��������/�ݼ�
    PERFORM getshzj USING    lv_qgd-zzl1 lv_zsfzj
                    CHANGING p_zsh
                             p_zzj.
    "��ȡ��������
    PERFORM getct USING    p_zwbxz p_zbx p_zbh p_zbk lv_nwbkd
                  CHANGING p_zct.
    PERFORM calc.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getct
*&---------------------------------------------------------------------*
*& ��ȡ��������/�ݼ�
*&---------------------------------------------------------------------*
FORM getshzj USING    p_zzl1  TYPE mara-zzl1
                      p_zsfzj TYPE ztpp_205b-zsfnbzj
             CHANGING p_zsh   TYPE zspp225_nb-znbsh
                      p_zzj   TYPE zspp225_nb-znbzj.
  SELECT * FROM ztpp_205j1 INTO TABLE @DATA(lt_205j1).
  SORT lt_205j1 BY werks matkl zynam.

  READ TABLE lt_205j1 INTO DATA(ls_205j1) WITH KEY werks = wa_ggzd-werks matkl = wa_ggzd-matnr+0(5) zynam = p_zzl1 BINARY SEARCH.
  IF sy-subrc = 0.
    p_zsh = ls_205j1-zshjj.
    IF p_zsfzj = '1'.
      p_zzj = ls_205j1-zzjjj.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getct
*&---------------------------------------------------------------------*
*& ��ȡ�ڰ�/������
*&---------------------------------------------------------------------*
FORM getct USING    p_zwbxz  TYPE zwbxz
                    p_zbx    TYPE zbx
                    p_zbh    TYPE zebchd
                    p_zbk    TYPE zebckd
                    lv_nwbkd TYPE decfloat16
           CHANGING p_zct TYPE zspp225_nb-znbct.

  RANGES ra_bh FOR ztpp_260c-zhd.
  DATA:lvv_zwbxz TYPE zwbxz.
  DATA:lv_nwbjl TYPE ztpp_260c-znbzkjl.  "�ڰ�/������

  IF p_zwbxz <> 'V8����'.
    lvv_zwbxz = ''.
  ENDIF.
  SELECT * FROM ztpp_260c WHERE werks = @wa_ggzd-werks AND matkl = @gv_matkl INTO TABLE @DATA(lt_260C).
  LOOP AT lt_260C INTO DATA(ls_260c) WHERE zbx   = p_zbx
                                       AND zwbxz = lvv_zwbxz
                                       .
*���û������ �Ͳ�������Ա�
    IF p_zbh IS NOT INITIAL.
      IF ls_260c-zoption = ''.

      ELSE.
        ra_bh-sign = 'I'.
        ra_bh-option = ls_260c-zoption.
        ra_bh-low = ls_260c-zhd.
        APPEND ra_bh.
      ENDIF.
      IF p_zbh IN ra_bh.
        lv_nwbjl = ls_260c-znbzkjl.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  "���� = ԭ�Ŀ�� / �����+��������
  IF ( p_zbk + lv_nwbjl ) <> 0.
    p_zct = floor( lv_nwbkd / ( p_zbk + lv_nwbjl ) ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  CALCULATE  INPUT
*&---------------------------------------------------------------------*
*       �Զ�����
*----------------------------------------------------------------------*
MODULE calculate INPUT.
  PERFORM calc.
ENDMODULE.
FORM calc.
  DATA:lv_zhjxs  TYPE ztpp_205j2-zhjxs, "о�ĺ˼�ϵ��
       lv_zbkjjl TYPE ztpp_205j2-zjjl.  "���Ӽ���

  "�ڰ��׼� = ���ּ�+���+�ݼ���/ ���� / ����
  IF wa_lchj-znbct <> 0 AND wa_lchj-znbcm <> 0.
    wa_lchj-znbmj = ( wa_lchj-znbdj + wa_lchj-znbsh + wa_lchj-znbzj ) / wa_lchj-znbcm / wa_lchj-znbct.
  ENDIF.

  "����׼� = ���ּ�+���+�ݼ���/ ���� / ����
  IF wa_lchj-zwbct <> 0 AND wa_lchj-zwbcm <> 0.
    wa_lchj-zwbmj = ( wa_lchj-zwbdj + wa_lchj-zwbsh + wa_lchj-zwbzj ) / wa_lchj-zwbcm / wa_lchj-zwbct.
  ENDIF.

  SELECT * FROM ztpp_205j2
  WHERE werks = @wa_ggzd-werks
    AND zbx   = @wa_lchj-zbx
    AND matkl = @gv_matkl
  INTO TABLE @DATA(lt_205j2).
  SORT lt_205j2 BY werks zbx matkl zhdxx zhdsx.
  LOOP AT lt_205j2 INTO DATA(ls_205j2).
    IF wa_lchj-zxchd > ls_205j2-zhdxx AND wa_lchj-zxchd <= ls_205j2-zhdsx.
      lv_zhjxs = ls_205j2-zhjxs.
      lv_zbkjjl = ls_205j2-zjjl.
      EXIT.
    ENDIF.
  ENDLOOP.
  "о���׼� = о�ļ۸�*1.065*���/1000*(���-90)/1000
  wa_lchj-zxcmj = wa_lchj-zxcjg * lv_zhjxs * wa_lchj-zxchd / 1000 * ( wa_lchj-zbk + lv_zbkjjl ) / 1000.
  "�������
  PERFORM getzzzsh.
  "�ܿسɱ� = ���ϳɱ�+������+ת�ǰ�+�̰��и�+�˷�
  wa_lchj-zgkcb = wa_lchj-zzylcb + wa_lchj-zkjf + wa_lchj-zzjbfy + wa_lchj-zdbqg + wa_lchj-zyf.
  "ë���� = ����ͬ�۸�-�ܳɱ���* 0.59
  wa_lchj-zygml = ( wa_lchj-zpr0 - wa_lchj-zgkcb ) * '0.59'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getzzfy
*&---------------------------------------------------------------------*
*& ��ȡ�������
*&---------------------------------------------------------------------*
FORM getzzfy.
  DATA:r_zbx TYPE RANGE OF zbx WITH HEADER LINE,
       r_zbk TYPE RANGE OF zbk WITH HEADER LINE,
       r_zbh TYPE RANGE OF zbh WITH HEADER LINE.
  DATA:lv_zsftjbx TYPE ztpp_205j6-zsftjbx.

  SELECT
      *
  FROM ztpp_205j5
  WHERE werks = @wa_ggzd-werks
  INTO TABLE @DATA(lt_205j5).
  LOOP AT lt_205j5 INTO DATA(ls_205j5).
    CASE ls_205j5-ztjbx.
      WHEN '����'.
        r_zbx-sign = 'I'.
        r_zbx-option = 'EQ'.
        r_zbx-low = ls_205j5-zbxkh.
        CONDENSE r_zbx-low NO-GAPS.
        COLLECT r_zbx.
      WHEN '���'.
        r_zbk-sign = 'I'.
        r_zbk-option = 'EQ'.
        r_zbk-low = ls_205j5-zbxkh.
        CONDENSE r_zbk-low NO-GAPS.
        COLLECT r_zbk.
      WHEN '���'.
        r_zbh-sign = 'I'.
        r_zbh-option = 'EQ'.
        r_zbh-low = ls_205j5-zbxkh.
        CONDENSE r_zbh-low NO-GAPS.
        COLLECT r_zbh.

    ENDCASE.
  ENDLOOP.
  "�ж��Ƿ��Ƽ�����
  IF wa_lchj-zbx IN r_zbx AND wa_lchj-zbk IN r_zbk AND wa_lchj-zbh IN r_zbh.
    lv_zsftjbx = '��'.
  ELSE.
    lv_zsftjbx = '��'.
  ENDIF.
  IF gv_matkl = 'A0400'. "������
    SELECT * FROM ztpp_205j7
    WHERE werks   = @wa_ggzd-werks
      AND matkl   = @gv_matkl
      AND zbx     = @wa_lchj-zbx
      AND zsftjbx = @lv_zsftjbx
    INTO TABLE @DATA(lt_205j7).
    LOOP AT lt_205j7 INTO DATA(ls_205j7).
      IF wa_ggzd-zzms > ls_205j7-zpcmsxx AND wa_ggzd-zzms <= ls_205j7-zpcmssx.
        "ĸ���������
        wa_lchj-zmbzzfy = ls_205j7-zmbzzfy.
        "���Ӱ���μӹ���
        wa_lchj-zhzbjgf = ls_205j7-zhzbjgf / wa_lchj-zbc.
        wa_lchj-zmbshl = ls_205j7-zmbshl / 100. "ĸ�������%
        wa_lchj-zhzbshl = ls_205j7-zhzbsh. "���Ӱ��ͷ�и����(M/��)
        wa_lchj-zhzbjgshl = ls_205j7-zhzbjgsh / 100. "���Ӱ�ӹ����(%)
        "��ͷ����-�Ұ�ȼ۸�  = (���ñ���)��ͷ����-�Ұ�ȼ۸�*����� / 50��/ƽ���峤
        wa_lchj-zdthb = ls_205j7-zdthb * ( wa_lchj-zbh / 50 ) / wa_lchj-zbc.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    SELECT * FROM ztpp_205j6
    WHERE werks   = @wa_ggzd-werks
      AND matkl   = @gv_matkl
      AND zsftjbx = @lv_zsftjbx
    INTO TABLE @DATA(lt_205j6).
    LOOP AT lt_205j6 INTO DATA(ls_205j6).
      IF wa_ggzd-zzms > ls_205j6-zpcmsxx AND wa_ggzd-zzms <= ls_205j6-zpcmssx.
        "�������
        wa_lchj-zzzfy = ls_205j6-zzzfy.
        wa_lchj-zylcbsh = ls_205j6-zylcbsh / 100. "�����%
        "����Ӽ�
        IF wa_lchj-zbh >= 150.
          wa_lchj-zzzjj  = ls_205j6-zzzfyjj .
        ENDIF.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF wa_lchj-zwbxz = '����ѹ��'.
      wa_lchj-zzzjj += 2 .
    ENDIF.
  ENDIF.
  "�������
  PERFORM getzzzsh.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getzzzsh
*&---------------------------------------------------------------------*
*& ��ȡ�������ģ�����
*&---------------------------------------------------------------------*
FORM getzzzsh.
  DATA: lv_zfy   TYPE p DECIMALS 2,
        lv_zfc   TYPE p DECIMALS 2,
        lv_zzzfy TYPE zspp225_zz-zzzfy.

  CASE gv_matkl.
    WHEN 'A0200'. "���ʽ�
      lv_zfc = wa_lchj-zwl + wa_lchj-zjc_ym_bz + wa_lchj-zjs_fm.
    WHEN OTHERS.
      lv_zfc = wa_lchj-zwl + wa_lchj-zfb_hm_jd_bz + wa_lchj-zjs_fm.
  ENDCASE.

  CASE gv_matkl.
    WHEN 'A0400'. "������
      lv_zzzfy = wa_lchj-zmbzzfy.
    WHEN OTHERS.
      lv_zzzfy = wa_lchj-zzzfy + wa_lchj-zzzjj.
  ENDCASE.

  lv_zfy = wa_lchj-zwbmj + wa_lchj-znbmj + wa_lchj-zxcmj
         + lv_zfc
         + lv_zzzfy.

  CASE gv_matkl.
    WHEN 'A0400'. "������
      "ĸ�����ϳɱ�
      wa_lchj-zmbylcb =  lv_zfy.
      "ĸ�����2%
      wa_lchj-zmbsh =  wa_lchj-zmbylcb * wa_lchj-zmbshl.
      "���Ӱ��ͷ�и����0.05M = ���Ӱ��ͷ�и������ / ƽ���峤 *ĸ�����ϳɱ�
      wa_lchj-zhzbsh = wa_lchj-zhzbshl / wa_lchj-zbc * wa_lchj-zmbylcb.
      "���Ӱ�ӹ����0.4% = ���Ӱ�ӹ���� * ĸ�����ϳɱ�
      wa_lchj-zhzbjgsh = wa_lchj-zhzbjgshl * wa_lchj-zmbylcb.
      "�����ϳɱ� = �����Ӱ���μӹ���+ĸ�����ϳɱ�+ĸ�����+���Ӱ��ͷ�и����+���Ӱ�ӹ����+��ͷ����-�Ұ�ȼ۸�+��������ѣ�/ϵ��
      wa_lchj-zzylcb = ( wa_lchj-zhzbjgf + wa_lchj-zmbylcb + wa_lchj-zmbsh + wa_lchj-zhzbsh + wa_lchj-zhzbjgsh + wa_lchj-zdthb + wa_lchj-zjsfwf ) / wa_lchj-zxs.
    WHEN OTHERS.
      "�������
      wa_lchj-zzzsh = lv_zfy * wa_lchj-zylcbsh.
      "���ϳɱ�
      wa_lchj-zylcb = lv_zfy + wa_lchj-zzzsh.
      "�����ϳɱ� = �����ϳɱ� +��������ѣ�/ϵ��
      wa_lchj-zzylcb = ( wa_lchj-zylcb + wa_lchj-zjsfwf ) / wa_lchj-zxs.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getzjb
*&---------------------------------------------------------------------*
*& ��ȡת�ǰ����/�̰��и�
*&---------------------------------------------------------------------*
FORM getzjb.
  DATA:lv_je  TYPE p DECIMALS 3,
       lv_zks TYPE ztpp_205a-zks.

  DATA(lv_ZAZFS) = wa_ggzd-zazfs. "01-��װ 02-��װ
  LOOP AT it_pcd.
    IF wa_ggzd-zjglx = 'B02'.
      CASE it_pcd-zzjblx.
        WHEN 'L��ת�ǰ�'.
          CASE lv_ZAZFS.
            WHEN '01'.
              lv_je = lv_je + it_pcd-zyyks * 80.
      