*&---------------------------------------------------------------------*
*& Report ZFIR004F
*&---------------------------------------------------------------------*
*& �ʲ��䶯��
*&---------------------------------------------------------------------*
REPORT zfir004f MESSAGE-ID zgp_msg.

TABLES: anla, anlz, anlb, anlbza,t001,ccss,csks,tfkbt,
        fagl_segmt,
        aufk,
        anlc,
        anlp,
        lfa1,
        bkpf.

DATA:
  g_firstdate LIKE sy-datum,      "���µ�һ��
  g_lastdate  LIKE sy-datum,      "�������һ��
  g_anhwsl    LIKE t090na-anhwsl, "��ֵֹ��
  g_ahproz    LIKE t091p-ahproz,  "��ֹת��ʱ�İٷ���
  g_str       LIKE anlc-kansw.

DATA      g_aibn1 TYPE string.
DATA: BEGIN OF it_list OCCURS 0,
        bukrs         LIKE anep-bukrs,   "��˾����
        anlue         LIKE anla-anlue,   "�ʲ����
*  add �ʲ��������
        txk20         LIKE ankt-txk20,   "�ʲ��������
        anln1         LIKE anep-anln1,   "�ʲ�����
        anln2         LIKE anep-anln2,   "�ʲ��Ӻ���
        txt50         LIKE anla-txt50,   "�ʲ�����
        txa50         LIKE anla-txa50,   "���/���ض�
        sernr         LIKE anla-sernr,   "���к�
        invnr         LIKE anla-invnr,   "�����/��ϵͳ�ʲ����
        menge         LIKE anla-menge,   "����
*        zqingdun LIKE anla-menge,"���������
        anlkl         LIKE anla-anlkl,        "�ʲ�����
        zzndjb        LIKE anlb-ndper,  "��ʹ���ڼ�
        meins         LIKE anla-meins,   "������λ
        ivdat         LIKE anla-ivdat,   "ԭʼ��������
        invzu         LIKE anla-invzu,   "���λ��
        zugdt         LIKE anla-zugdt,   "�ʱ�������
        aktiv         LIKE anla-aktiv,   "�ʱ�������
        deakt         LIKE anla-deakt,   "�������
        gsber         LIKE anlz-gsber,   "ҵ��Χ
        gtext         LIKE tgsbt-gtext,   "ҵ��������      ADD  TANZHIM
        kostl         LIKE anlz-kostl,   "�ɱ�����
        ltext         LIKE cskt-ltext,   "�ɱ�����˵��
        func_area     LIKE csks-func_area, "���ܷ�Χ
        fkbtx         LIKE tfkbt-fkbtx, "���ܷ�Χ����
        ktext         TYPE auftext,       " ��Ŀ����
        kostlv        LIKE anlz-kostlv, "���γɱ�����
        caufn         LIKE anlz-caufn,   "�ڲ�������
        raumn         LIKE anlz-raumn,   "�����
        kfzkz         LIKE anlz-kfzkz,   "ִ���ƺ�
        ord41         LIKE t087t-ordtx,  "���ӷ�ʽ
        ord42         LIKE t087t-ordtx,  "�ʲ���;
        ord43         LIKE t087t-ordtx,  "�Ƿ��Ѻ
        ord44         LIKE t087t-ordtx,  "�Ƿ��Ѻ
        gdlgrp        LIKE t087s-gdlgrp_txt, "�ʽ���Դ
        lifnr         LIKE anla-lifnr,    "��Ӧ�̱���
        herst         LIKE anla-herst,   "������
        vbund         LIKE anla-vbund,   "ó�׻��
        typbz         LIKE anla-typbz,   "������
        zcyz          LIKE anlc-kansw,   "ԭֵ
        jzzb          LIKE anlc-kaufw,   "��ֵ׼��
        ljzj_zc       LIKE anlc-knafa,   "�ۼ��۾ɣ�������
        ljzj_jhw      LIKE anlc-knafa,   "�ۼ��۾ɣ��ƻ��⣩
        zcjz          LIKE anlc-knafa,   "��ֵ
        "schrw LIKE anlb-schrw,   "��ֵ
        schrw         LIKE anlc-kansw,   "��ֵ
*        ZSCHRW        LIKE ANLB-SCHRW,  "��ֵ��
        ahproz        LIKE t091p-ahproz,  "��ֵ��
        zljzj_zc      LIKE  anlc-knafa, "��������۾�
*        add  �������۾ɶ�͡������۾ɶ
        month_1       LIKE anlp-nafaz,  "�����۾�
        month_2       LIKE anlp-nafaz,  "�����۾�
        zmonthje1(13) TYPE p DECIMALS 2, "���۾ɶ�
        year_1        LIKE anlp-nafag,  "��ǰ����۾�    Added by Oliver Yuan
        zmonthje      LIKE anlb-schrw, "���۾ɶ�
        afasl         LIKE anlb-afasl,   "�۾���
        zzndja        LIKE anlb-ndper,  "�ƻ�ʹ���ڼ�
        afabg         LIKE anlb-afabg,   "�۾ɿ�ʼ����
        afabe         LIKE anlc-afabe,   "�۾ɷ�Χ
        "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩�����޸� Start
        zsysyqj       LIKE anlb-ndper,  "ʣ��ʹ���ڼ�
        "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩�����޸� End

        "modify by zhangzq 2012-10-27 ���� ��Ӧ������ ԭ���ֶ�
        name1	        LIKE lfa1-name1,    "��Ӧ������
        zcje          LIKE anlc-knafa,   "����

        zcybh         TYPE string,        "�ʲ�ԭ���
        eaufn         LIKE anla-eaufn,    "Ͷ�ʶ���
        ktext1        TYPE auftext,       " Ͷ�ʶ�������
        anlhtxt       LIKE anlh-anlhtxt, "�豸��
        segment       TYPE anlz-segment,
        aibn1         TYPE anla-aibn1,
        aibn2         TYPE anla-aibn2,
        urjhr         TYPE anla-urjhr,
        urwrt         TYPE anla-urwrt,
        antei         TYPE anla-antei,
        segmt         TYPE fagl_segmt-name,
        eknam         TYPE t024-eknam,
*        VBUND         TYPE ANLA-VBUND,
      END OF it_list.
TYPES: BEGIN OF ty_sk,
         anln1 TYPE anln1,
         eknam TYPE eknam,
       END OF ty_sk.
DATA: enam TYPE STANDARD TABLE OF ty_sk WITH EMPTY KEY.
DATA:BEGIN OF it_sel1 OCCURS 0,
       bukrs TYPE anla-bukrs,
       anln1 TYPE anla-anln1,
       anln2 TYPE anla-anln2,
     END OF it_sel1,
     BEGIN OF it_kostl OCCURS 0,
       kostl TYPE cskt-kostl,
     END OF it_kostl,
     BEGIN OF it_aufnr OCCURS 0,
       aufnr TYPE aufk-aufnr,
     END OF it_aufnr,
     BEGIN OF it_anln1 OCCURS 0,
       anln1 TYPE anla-anln1,
     END OF it_anln1,
     it_tfkbt  TYPE TABLE OF tfkbt WITH HEADER LINE,
     it_lifnr  TYPE TABLE OF lfa1_key WITH HEADER LINE,
     it_t091p  TYPE TABLE OF t091p WITH HEADER LINE,
     it_t090na TYPE TABLE OF t090na WITH HEADER LINE.
DATA:tabix TYPE sy-tabix..
FIELD-SYMBOLS: <it_list> LIKE it_list.
DATA: BEGIN OF it_afapl OCCURS 0,
        bukrs LIKE t093c-bukrs,  "��˾����
        afapl LIKE t093c-afapl,  "�й��ʲ��������۾ɱ�
      END OF it_afapl.
DATA:r_month TYPE RANGE OF sy-datum WITH HEADER LINE.
DATA:jsrq  TYPE sy-datum,
     low   TYPE syst_datum,
     p_mon TYPE bkpf-monat, "�����ڼ� ���㱾���۾ɶ�&�����۾ɶ�
     zrq   TYPE char5.
DATA:msg TYPE bapi_msg.

DATA: BEGIN OF it_anla OCCURS 0,              "�ʲ�����¼
        bukrs   LIKE anla-bukrs,   "��˾����
        anlkl   LIKE anla-anlkl,   "�ʲ�����
        anln1   LIKE anla-anln1,   "�ʲ�����
        anln2   LIKE anla-anln2,   "�ʲ��Ӻ���
        anlue   LIKE anla-anlue,   "�ʲ����
        txt50   LIKE anla-txt50,   "�ʲ�����
        txa50   LIKE anla-txa50,   "���/���ض�
*        txk20 like ankt-txk20,   "�ʲ��������
        sernr   LIKE anla-sernr,   "���к�
        invnr   LIKE anla-invnr,   "�����/��ϵͳ�ʲ����
        menge   LIKE anla-menge,   "����
        meins   LIKE anla-meins,   "������λ
        ivdat   LIKE anla-ivdat,   "�������     Added by Oliver 20120413
        invzu   LIKE anla-invzu,   "���λ��
        zugdt   LIKE anla-zugdt,   "�ʱ�������
        aktiv   LIKE anla-aktiv,   "�ʱ�������
        deakt   LIKE anla-deakt,   "�������
        ord41   LIKE anla-ord41,   "���ӷ�ʽ
        ord42   LIKE anla-ord42,   "�ʲ���;
        ord43   LIKE anla-ord43,   "�Ƿ��Ѻ
        ord44   LIKE anla-ord43,   "�Ƿ��Ѻ
        gdlgrp  LIKE anla-gdlgrp, "�ʽ���Դ
        lifnr   LIKE anla-lifnr,   "��Ӧ�̱���
        herst   LIKE anla-herst,   "������
        vbund   LIKE anla-vbund,   "ó�׻��
        typbz   LIKE anla-typbz,   "������

        name1	  LIKE lfa1-name1,   "��Ӧ������
        gsber   LIKE anlz-gsber,   "ҵ��Χ
        kostl   LIKE anlz-kostl,   "�ɱ�����
        kostlv  LIKE anlz-kostlv, "���γɱ�����
        caufn   LIKE anlz-caufn,   "�ڲ�����
        raumn   LIKE anlz-raumn,   "�����
        kfzkz   LIKE anlz-kfzkz,   "ִ���ƺţ��������պţ�

        eaufn   LIKE anla-eaufn,    "Ͷ�ʶ���
        anlhtxt LIKE anlh-anlhtxt,
        segment TYPE anlz-segment,
        aibn1   TYPE anla-aibn1,
        aibn2   TYPE anla-aibn2,
        urjhr   TYPE anla-urjhr,
        urwrt   TYPE anla-urwrt,
        antei   TYPE anla-antei,
      END OF it_anla.

*DATA: BEGIN OF it_anlz OCCURS 0,              "ʱ������ʲ�����
*        bukrs LIKE anlz-bukrs,   "��˾����
*        anln1 LIKE anlz-anln1,   "�ʲ�����
*        anln2 LIKE anlz-anln2,   "�ʲ��Ӻ���
*        gsber LIKE anlz-gsber,   "ҵ��Χ
*        kostl LIKE anlz-kostl,   "�ɱ�����
*        kostlv LIKE anlz-kostlv, "���γɱ�����
*        caufn LIKE anlz-caufn,   "�ڲ�����
*        raumn LIKE anlz-raumn,   "�����
*        kfzkz LIKE anlz-kfzkz,   "ִ���ƺţ��������պţ�
*      END OF it_anlz.

DATA: BEGIN OF it_anek OCCURS 0,            "�ʲ�����ƾ̧֤ͷ
        bukrs LIKE anek-bukrs,   "��˾����
        anln1 LIKE anek-anln1,   "�ʲ�����
        anln2 LIKE anek-anln2,   "�ʲ��Ӻ���
        gjahr LIKE anek-gjahr,   "������
        lnran LIKE anek-lnran,   "������ʲ�����Ŀ�����
        budat LIKE anek-budat,   "��������
        belnr LIKE anek-belnr,   "�ο�ƾ֤���
        buzei LIKE anek-buzei,   "���ƾ֤�е�����Ŀ��
      END OF it_anek.

DATA: BEGIN OF it_anep OCCURS 0,          "�ʲ�����Ŀ
        bukrs  LIKE anep-bukrs,   "��˾����
        anln1  LIKE anep-anln1,   "�ʲ�����
        anln2  LIKE anep-anln2,   "�ʲ��Ӻ���
        gjahr  LIKE anep-gjahr,   "������
        lnran  LIKE anep-lnran,   "������ʲ�����Ŀ�����
        budat  LIKE anek-budat,   "��������
        belnr  LIKE anek-belnr,   "�ο�ƾ֤���
        buzei  LIKE anek-buzei,   "���ƾ֤�е�����Ŀ��
        afabe  LIKE anep-afabe,   "�۾ɷ�Χ
        bzdat  LIKE anep-bzdat,   "�ʲ���ֵ��
        bwasl  LIKE anep-bwasl,   "�ʲ�ҵ������
        anbtr  LIKE anep-anbtr,   "���ʽ��
        lnsan  LIKE anep-lnsan,   "�������ʲ���������к�
        xawbt  LIKE anep-xawbt,   "��ǣ���ͬ���ʽ������
        anbtra LIKE anep-anbtr,  "�ʲ�ԭֵ
        anbtrb LIKE anep-anbtr,  "��ֵ׼��
        anbtrc LIKE anep-anbtr,  "�ۼ��۾ɣ�������
        anbtrd LIKE anep-anbtr,  "�ۼ��۾ɣ�δ�ƻ���
      END OF it_anep.

DATA: BEGIN OF it_anea OCCURS 0,        "����ֵ���ʲ�����Ŀ
        bukrs LIKE anea-bukrs,   "��˾����
        anln1 LIKE anea-anln1,   "�ʲ�����
        anln2 LIKE anea-anln2,   "�ʲ��Ӻ���
        gjahr LIKE anea-gjahr,   "������
        lnran LIKE anea-lnran,   "������ʲ�����Ŀ�����
        afabe LIKE anea-afabe,   "�۾ɷ�Χ
        invzv LIKE anea-invzv,   "�����ۻ�Ͷ����Ȩ
        aufwv LIKE anea-aufwv,   "�й��滻ֵ�ı����ۻ��ع�
        aufwl LIKE anea-aufwl,   "�����й��滻ֵ�ı����ع�
        nafav LIKE anea-nafav,   "�����ۻ������۾�
        safav LIKE anea-safav,   "�����ۼ��ر��۾�
        aafav LIKE anea-aafav,   "�������ۻ��ƻ����۾�
        invzl LIKE anea-invzl,   "�йش���ı���Ͷ����Ȩ
        nafal LIKE anea-nafal,   "����ı��������۾�
        safal LIKE anea-safal,   "����ı����ر��۾�
        aafal LIKE anea-aafal,   "����ı����ƻ����۾�
      END OF it_anea.

DATA: BEGIN OF it_anlp OCCURS 0,          "�ʲ��ڼ��ֵ
        bukrs   LIKE anlp-bukrs,   "��˾����
        gjahr   LIKE anlp-gjahr,   "������
        peraf   LIKE anlp-peraf,   "�۾ɼ�����
        anln1   LIKE anlp-anln1,   "�ʲ�����
        anln2   LIKE anlp-anln2,   "�ʲ��Ӻ���
        afaber  LIKE anlp-afaber, "�۾ɷ�Χ
        bwasl   LIKE anep-bwasl,   "�ʲ�ҵ������
        aufwz   LIKE anlp-aufwz,   "�����ʵ�����ֵ�ع�
        nafaz   LIKE anlp-nafaz,   "���ʵ������۾�
        safaz   LIKE anlp-safaz,   "�����ʵ������۾�
        aafaz   LIKE anlp-aafaz,   "�����ʵļƻ����۾�
        belnr   LIKE anlp-belnr,   "ƾ֤���
*        anbtrc like anlp-nafaz,
        zanbtrb LIKE anlp-nafaz,
        zanbtrc LIKE anlp-nafaz,
        zanbtrd LIKE anlp-nafaz,
        budat   LIKE bkpf-budat,
        nafag   LIKE anlp-nafag,   "���ʵ������۾�
        safag   LIKE anlp-safag,   "�����ʵ������۾�
        aafag   LIKE anlp-aafag,   "�����ʵļƻ����۾�
      END OF it_anlp.

DATA: BEGIN OF it_t087t OCCURS 0,         "����������
        ordnr LIKE t087t-ordnr,   "����С���
        ord4x LIKE t087t-ord4x,                             "����С��1-4
        ordtx LIKE t087t-ordtx,   "����
      END OF it_t087t.

DATA: BEGIN OF it_t087s OCCURS 0,                           "������8����
        gdlgrp     LIKE t087s-gdlgrp,         "�ʽ���Դ
        gdlgrp_txt LIKE t087s-gdlgrp_txt,                   "������8λ�ı�
      END OF it_t087s.

DATA: BEGIN OF it_ankt OCCURS 0,        "�ʲ��������
        anlkl LIKE ankt-anlkl,
        txk20 LIKE ankt-txk20,
      END OF it_ankt.

DATA: BEGIN OF it_tgsbt OCCURS 0,     "ҵ��������
        gsber LIKE tgsbt-gsber,
        gtext LIKE tgsbt-gtext,
      END OF it_tgsbt.

DATA: BEGIN OF it_anlc OCCURS 0,     "�ʲ�ֵ�ֶ�
        bukrs LIKE anlc-bukrs,   "��˾����
        anln1 LIKE anlc-anln1,   "�ʲ�����
        anln2 LIKE anlc-anln2,   "�ʲ��Ӻ���
        gjahr LIKE anlc-gjahr,   "������
        afabe LIKE anlc-afabe,   "�۾ɷ�Χ
        kansw LIKE anlc-kansw,   "�ۻ����ú���������
        kaufw LIKE anlc-kaufw,   "���ü�ֵ���ۼ��ع�
        knafa LIKE anlc-knafa,   "�ۼ������۾�
        ksafa LIKE anlc-ksafa,   "�ۼ������۾�
        kaafa LIKE anlc-kaafa,   "�ۻ��ƻ����۾�
        answl LIKE anlc-answl,   "�����Ӱ���ʲ�ֵ��ҵ��
        aufwb LIKE anlc-aufwb,   "���ü�ֵ���ع�����
        nafag LIKE anlc-nafag,   "�����ڵ�ǰ��������۾�
        safag LIKE anlc-safag,   "�ڵ�ǰ�ƻ�����еļ��ʵ��ر��۾�
        aafag LIKE anlc-aafag,   "�й���ļƻ����۾ɼ���
        aufwl LIKE anlc-aufwl,   "�����й��滻ֵ�ı����ع�
        nafal LIKE anlc-nafal,   "����ı��������۾�
        safal LIKE anlc-safal,   "����ı����ر��۾�
        aafal LIKE anlc-aafal,   "����ı����ƻ����۾�
        aufwv LIKE anlc-aufwv,   "�й��滻ֵ�ı����ۻ��ع�
        nafav LIKE anlc-nafav,   "�����ۻ������۾�
        safav LIKE anlc-safav,   "�����ۼ��ر��۾�
        aafav LIKE anlc-aafav,   "�������ۻ��ƻ����۾�
        aufwp LIKE anlc-aufwp,   "����ֵ�ļƻ��ع�
        nafap LIKE anlc-nafap,   "�����Ѽƻ������۾�
        safap LIKE anlc-safap,   "�����Ѽƻ��ر��۾�
        aafap LIKE anlc-aafap,   "������Ԥ����δ�ƻ��۾�
        aufng LIKE anlc-aufng,   "�й��ۻ������۾ɵļ�������
        zusna LIKE anlc-zusna,   "�������۾��ϵļ�ֵ����
        zussa LIKE anlc-zussa,   "���ر��۾��ϵļ�ֵ����
        zusaa LIKE anlc-zusaa,   "���޼ƻ��۾��ϵļ�ֵ����
      END OF it_anlc.

DATA: BEGIN OF it_anlb OCCURS 0,          "�۾�����
        bukrs LIKE anlb-bukrs,   "��˾����
        anln1 LIKE anlb-anln1,   "�ʲ�����
        anln2 LIKE anlb-anln2,   "�ʲ��Ӻ���
        afabe LIKE anlb-afabe,   "�۾ɷ�Χ
        schrw LIKE anlb-schrw,   "�ʲ���ֵ
        afasl LIKE anlb-afasl,   "�۾���
        ndjar LIKE anlb-ndjar,   "�ƻ���ʹ����
        ndper LIKE anlb-ndper,   "�ƻ�ʹ���ڼ�
        afabg LIKE anlb-afabg,   "�۾ɼ��㿪ʼ����
      END OF it_anlb.

DATA: BEGIN OF it_anlbza OCCURS 0,   "ʱ������۾�����
        bukrs LIKE anlbza-bukrs,   "��˾����
        anln1 LIKE anlbza-anln1,   "�ʲ�����
        anln2 LIKE anlbza-anln2,   "�ʲ��Ӻ���
        afabe LIKE anlbza-afabe,   "�۾ɷ�Χ
        schrw LIKE anlbza-schrw,   "�ʲ���ֵ
        afasl LIKE anlbza-afasl,   "�۾���
        ndjar LIKE anlbza-ndjar,   "�ƻ���ʹ����
        ndper LIKE anlbza-ndper,   "�ƻ�ʹ���ڼ�
        afabg LIKE anlb-afabg,     "�۾ɼ��㿪ʼ����
      END OF it_anlbza.

DATA: it_peraf1 TYPE peraf,
      it_peraf2 TYPE peraf.
*RANGES s_bukrs FOR t001-bukrs.

DATA: l_monat    LIKE bkpf-monat,
      l_budat(8) TYPE c,
      g_count    TYPE i.                                          "��¼����
DATA tp_msg(50).
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECT-OPTIONS: s_bukrs FOR t001-bukrs MEMORY ID obligatory. "��˾����
  PARAMETERS: p_gjahr LIKE anek-gjahr DEFAULT sy-datum+0(4) OBLIGATORY.    "������
  SELECT-OPTIONS: s_mon   FOR bkpf-monat DEFAULT sy-datum+4(2) NO-EXTENSION OBLIGATORY.    "�ڼ�
SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.


*& PBO
AT SELECTION-SCREEN OUTPUT.
  t1 = 'ɸѡ����'.
  %_s_bukrs_%_app_%-text = '��˾����'.
  %_p_gjahr_%_app_%-text = '������'.
  %_s_mon_%_app_%-text = '�ڼ�'.

AT SELECTION-SCREEN.

  PERFORM auth_check .
  PERFORM check_data.

START-OF-SELECTION.
  REFRESH: it_list, it_anla,  it_anlc, it_anek, it_anep, "it_anlz,
           it_anea, it_anlp, it_anlb, it_anlbza, it_t087t.
  PERFORM getmonth CHANGING msg.
  IF msg IS NOT INITIAL.
    MESSAGE s004 WITH msg DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM updatelog(zreplog) IF FOUND.

  PERFORM frm_alv.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       ��ȡ����������ʲ��������
*----------------------------------------------------------------------*
FORM get_data.

* ȡ����������
  SELECT ordnr ord4x ordtx INTO TABLE it_t087t
    FROM t087t
  WHERE spras = sy-langu.
  SORT it_t087t BY ordnr ord4x.

  SELECT gdlgrp gdlgrp_txt INTO TABLE it_t087s
    FROM t087s
  WHERE spras = sy-langu.
  SORT it_t087s BY gdlgrp.

  SELECT anlkl txk20 INTO TABLE it_ankt
  FROM ankt
  WHERE spras = sy-langu.
  SORT it_ankt BY anlkl.

  SELECT gsber gtext INTO TABLE it_tgsbt
  FROM tgsbt
  WHERE spras = sy-langu.
  SORT it_tgsbt BY gsber.
  SELECT * INTO TABLE it_t090na FROM t090na.
  SORT it_t090na BY afapl afasl.
  SELECT * INTO TABLE it_t091p FROM t091p.
  SORT it_t091p BY anhwsl.
  SELECT * INTO TABLE it_tfkbt FROM tfkbt WHERE spras = sy-langu.
  SORT it_tfkbt BY  fkber.
* ȡ�ʲ�����¼
  SELECT anla~bukrs
         anla~anln1
         anla~anln2
         anlkl
         anlue
         txt50
         txa50
         sernr
         invnr
         menge
         meins
         ivdat
         invzu
         aktiv
         deakt
         ord41
         ord42
         ord43
         ord44
         gdlgrp
         lifnr
         herst
         vbund
         typbz
         gsber
         kostl
         kostlv
         caufn
         raumn
         kfzkz
         eaufn
         zugdt
         aktiv
         anlh~anlhtxt
         anlz~segment
         anla~aibn1
         anla~aibn2
         anla~urjhr
         anla~urwrt
         anla~antei
    INTO CORRESPONDING FIELDS OF TABLE it_anla
    FROM anla INNER JOIN anlz
      ON anlz~bukrs = anla~bukrs
      AND anlz~anln1 = anla~anln1
      AND anlz~anln2 = anla~anln2
      INNER  JOIN anlh ON  anlh~bukrs = anla~bukrs
      AND anlh~anln1 = anla~anln1
    WHERE anla~bukrs IN s_bukrs
    AND   aktiv NE '00000000'   "�ʲ��ʱ�������
    AND   ( aktiv IN r_month OR deakt IN r_month )
*    AND   bdatu GE g_lastdate  "��Ч���ڽ���
*    AND   adatu LE g_lastdate "��Ч����ʼ����
  .
  SORT it_anla BY bukrs anln1 anln2.
  DATA(tnam) = it_anla[].
  SORT tnam BY anln1.
  DELETE ADJACENT DUPLICATES FROM tnam COMPARING anln1.
  IF tnam[] IS NOT INITIAL.
    SELECT
      a~anln1,
      c~eknam
      FROM ekkn AS a
      INNER JOIN ekko AS b ON a~ebeln = b~ebeln
      INNER JOIN t024 AS c ON b~ekgrp = c~ekgrp
      FOR ALL ENTRIES IN @tnam
      WHERE a~anln1 = @tnam-anln1
      INTO CORRESPONDING FIELDS OF TABLE @enam.
    SORT enam BY anln1 eknam.
    DELETE ADJACENT DUPLICATES FROM enam COMPARING ALL FIELDS.
  ENDIF.

** ȡʱ������ʲ�����
*  SELECT bukrs anln1 anln2 gsber kostl kostlv caufn raumn kfzkz
*    INTO TABLE it_anlz
*    FROM anlz
*    WHERE bukrs IN s_bukrs
*    AND   anln1 IN s_anln1
*    AND   anln2 IN s_anln2
**    AND   gsber IN s_gsber
*    AND   kostl IN s_kostl
*    AND   kostlv IN s_kostlv   "�ɱ����Ķ��ʲ�����
*    AND   bdatu GE g_lastdate  "��Ч���ڽ���
*    AND   adatu LE g_lastdate "��Ч����ʼ����
**    AND   caufn IN s_caufn.   "Added by Oliver 20120413
*    .
*  SORT it_anlz BY bukrs anln1 anln2.
  IF it_anla[] IS NOT INITIAL.
* ȡ�۾�����
    SELECT bukrs anln1 anln2 afabe schrw afasl ndjar ndper afabg
      INTO TABLE it_anlb
      FROM anlb
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
    AND   afabe  = '01'."in s_afabe.  ʵ���۾ɷ�Χ
    SORT it_anlb BY bukrs anln1 anln2.

* ȡ�۾�����
    SELECT bukrs anln1 anln2 afabe schrw afasl ndjar ndper
      INTO TABLE it_anlbza
      FROM anlb
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   afabe  = '01'   "in s_afabe
      AND   adatu <= g_lastdate
    AND   bdatu >= g_lastdate.
    SORT it_anlbza BY bukrs anln1 anln2.
    LOOP AT it_anlbza.
      READ TABLE it_anlb WITH KEY bukrs = it_anlbza-bukrs anln1 = it_anlbza-anln1 anln2 = it_anlbza-anln2 afabe = it_anlbza-afabe BINARY SEARCH.
      IF sy-subrc = 0.
        it_anlbza-afabg = it_anlb-afabg.
        MODIFY it_anlbza TRANSPORTING afabg.
        CLEAR it_anlb.
      ENDIF.
    ENDLOOP.

* ȡ�ʲ�ֵ�ֶ�
    SELECT bukrs anln1 anln2 gjahr afabe kansw answl kaufw aufwb
           aufwl aufwv knafa ksafa kaafa nafag safag aufng zusna
           zussa zusaa nafav safav aafav nafal safal aafal aafag
           aufwp nafap safap aafap
      INTO CORRESPONDING FIELDS OF TABLE it_anlc
      FROM anlc
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   gjahr = p_gjahr
    AND   afabe  = '01'."in s_afabe.
    SORT it_anlc BY bukrs anln1 anln2.

* ȡƾ̧֤ͷ�ʲ�����
    SELECT bukrs anln1 anln2 gjahr lnran budat belnr buzei
      INTO CORRESPONDING FIELDS OF TABLE it_anek
      FROM anek
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   gjahr = p_gjahr
    AND   monat IN s_mon.
*    AND   budat GT g_lastdate.
    SORT it_anek BY bukrs anln1 anln2 gjahr lnran.
  ENDIF.

  IF NOT it_anek[] IS INITIAL.
*   ȡ�ʲ�����Ŀ
    SELECT bukrs anln1 anln2 gjahr lnran belnr buzei afabe bzdat bwasl anbtr xawbt lnsan
      INTO CORRESPONDING FIELDS OF TABLE it_anep
      FROM anep
      FOR ALL ENTRIES IN it_anek
      WHERE bukrs = it_anek-bukrs
      AND   anln1 = it_anek-anln1
      AND   anln2 = it_anek-anln2
      AND   gjahr = it_anek-gjahr
      AND   lnran = it_anek-lnran
      AND   afabe  = '01' "in s_afabe
      AND   bwasl NE 'Z31'
    AND   bwasl NOT BETWEEN 600 AND 699.
*      and   bzdat gt g_lastdate.
    SORT it_anep BY bukrs anln1 anln2 gjahr lnran afabe.

*   ȡ����ֵ���ʲ�����Ŀ
    IF NOT it_anep[] IS INITIAL.
      SELECT bukrs anln1 anln2 gjahr lnran afabe invzv aufwv
             aufwl nafav safav aafav invzl nafal safal aafal
        INTO TABLE it_anea
        FROM anea
        FOR ALL ENTRIES IN it_anep
        WHERE bukrs = it_anep-bukrs
        AND   anln1 = it_anep-anln1
        AND   anln2 = it_anep-anln2
        AND   gjahr = it_anep-gjahr
        AND   lnran = it_anep-lnran   "������ʲ�����Ŀ�����
      AND   afabe = it_anep-afabe.
      SORT it_anea BY bukrs anln1 anln2 gjahr lnran afabe.
    ENDIF.
  ENDIF.

* ȡ�ʲ��ڼ��ֵ
  SELECT bukrs gjahr peraf anln1 anln2 afaber
         aufwz nafaz safaz aafaz belnr
         nafag safag aafag
    INTO CORRESPONDING FIELDS OF TABLE it_anlp
    FROM anlp
    WHERE bukrs IN s_bukrs
    AND   gjahr = p_gjahr
    AND   afaber  = '01'  "in s_afabe  ʵ�ʵĻ��������۾ɷ�Χ
  AND   peraf IN s_mon.
  SORT it_anlp BY bukrs anln1 anln2 peraf afaber.
ENDFORM.                                    "get_data

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       �������������������alv���ڲ���
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_nafag LIKE anlp-nafag,
        l_safag LIKE anlp-safag,
        l_aafag LIKE anlp-aafag.
  DATA: l_flag(1),
        l_x         TYPE p15_months,
        l_datetime  LIKE sy-datum,
        l_gjahrm(6) TYPE c.
  SELECT *
    INTO TABLE @DATA(it_segm)
    FROM fagl_segmt
    WHERE langu = @sy-langu.
  SORT it_segm BY segment.
  LOOP AT it_anlc.
    CLEAR it_list.
    it_list-bukrs = it_anlc-bukrs.
    it_list-anln1 = it_anlc-anln1.
    it_list-anln2 = it_anlc-anln2.
    it_list-afabe = it_anlc-afabe.
    it_list-zcyz = it_anlc-kansw + it_anlc-answl.
    it_list-jzzb = ( it_anlc-kaufw + it_anlc-aufwb + it_anlc-aufwl + it_anlc-aufwv ) * -1.

    it_list-ljzj_zc = ( it_anlc-knafa + it_anlc-nafav ) * -1.
    it_list-ljzj_jhw = ( it_anlc-aafag + it_anlc-aafal ) * -1.

    COLLECT it_list.
    PERFORM fillsel.
  ENDLOOP.

  LOOP AT it_anep.
    READ TABLE it_anek WITH KEY bukrs = it_anep-bukrs
                                anln1 = it_anep-anln1
                                anln2 = it_anep-anln2
                                gjahr = it_anep-gjahr
                                lnran = it_anep-lnran
                                BINARY SEARCH.

    it_anep-anbtra = it_anep-anbtr.

*    if it_anep-xawbt = space or it_anep-xawbt is initial.
    READ TABLE it_anea WITH KEY bukrs = it_anep-bukrs
                                anln1 = it_anep-anln1
                                anln2 = it_anep-anln2
                                gjahr = it_anep-gjahr
                                lnran = it_anep-lnran
                                afabe = it_anep-afabe
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_anep-anbtrb = - it_anea-aufwv - it_anea-aufwl.
      IF g_lastdate+4(4) EQ '1231'.
        it_anep-anbtrc = - it_anea-nafav - it_anea-nafal - it_anea-safav - it_anea-safal - it_anea-aafav - it_anea-aafal.
        it_anep-anbtrd = 0.
      ELSE.
        it_anep-anbtrc = - it_anea-nafav - it_anea-nafal - it_anea-safav - it_anea-aafav.
        it_anep-anbtrd = - it_anea-safal - it_anea-aafal.
      ENDIF.
    ENDIF.

    CLEAR it_list.
    it_list-bukrs = it_anep-bukrs.
    it_list-anln1 = it_anep-anln1.
    it_list-anln2 = it_anep-anln2.
    it_list-afabe = it_anep-afabe.


    it_list-zcyz = - it_anep-anbtra.
    it_list-jzzb = - it_anep-anbtrb.
    it_list-ljzj_zc = - it_anep-anbtrc.
    it_list-ljzj_jhw = - it_anep-anbtrd.
    COLLECT it_list.
    PERFORM fillsel.
  ENDLOOP.

  LOOP AT it_anlp.
    IF it_anlp-aufwz <> 0.
      it_anlp-bwasl = 'Z31'.
      it_anlp-zanbtrb = it_anlp-aufwz.
    ENDIF.

    IF it_anlp-nafaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrc = it_anlp-nafaz.
    ENDIF.

    IF it_anlp-safaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrd = it_anlp-safaz.
    ENDIF.

    IF it_anlp-aafaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrd = it_anlp-zanbtrd + it_anlp-aafaz.
    ENDIF.

    IF it_anlp-peraf > 12.
      l_monat = 12.
    ELSE.
      l_monat = it_anlp-peraf.
    ENDIF.
    CLEAR: l_datetime,l_gjahrm.
    CONCATENATE it_anlp-gjahr l_monat '01' INTO l_datetime.
    CONCATENATE it_anlp-gjahr it_anlp-peraf+1(2) INTO l_gjahrm.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = l_datetime
      IMPORTING
        e_date = it_anlp-budat.

    IF l_gjahrm > g_lastdate+0(6).
      CLEAR it_list.
      it_list-bukrs = it_anlp-bukrs.
      it_list-anln1 = it_anlp-anln1.
      it_list-anln2 = it_anlp-anln2.
      it_list-afabe = it_anlp-afaber.
      it_list-jzzb = it_anlp-zanbtrb.
      it_list-ljzj_zc = it_anlp-zanbtrc.
      it_list-ljzj_jhw = it_anlp-zanbtrd.
      COLLECT it_list.
      PERFORM fillsel.
    ENDIF.
  ENDLOOP.
********ADD BY DONGPZ BEGIN AT 01.03.2023 09:32:08
  LOOP AT it_anla.
    CLEAR:it_lifnr.
    it_lifnr-lifnr = it_anla-lifnr.
    COLLECT it_lifnr.
  ENDLOOP.
  DELETE it_lifnr WHERE lifnr IS INITIAL.
  IF it_lifnr[] IS NOT INITIAL.
    SORT it_lifnr BY lifnr.
    SELECT *
      INTO TABLE @DATA(it_lfa1)
      FROM lfa1
      FOR ALL ENTRIES IN @it_lifnr
      WHERE lifnr = @it_lifnr-lifnr.
    SORT it_lfa1 BY lifnr.
  ENDIF.
  IF it_sel1[] IS NOT INITIAL.
    SORT it_sel1 BY bukrs anln1 anln2.
    SELECT *
      INTO TABLE @DATA(gt_anla)
      FROM anla
      FOR ALL ENTRIES IN @it_sel1
      WHERE bukrs = @it_sel1-bukrs
      AND   anln1 = @it_sel1-anln1
      AND   anln2 = @it_sel1-anln2.
    SORT gt_anla BY bukrs anln1 anln2.
  ENDIF.

********ADD BY DONGPZ END AT 01.03.2023 09:32:08
  CLEAR:it_sel1[].
  LOOP AT it_list ASSIGNING <it_list>.
    READ TABLE it_anla WITH KEY bukrs = <it_list>-bukrs
                                anln1 = <it_list>-anln1
                                anln2 = <it_list>-anln2
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_list.
    ELSEIF sy-subrc EQ 0.
      <it_list>-anlue = it_anla-anlue.
      <it_list>-txt50 = it_anla-txt50.
      <it_list>-txa50 = it_anla-txa50.
      <it_list>-invnr = it_anla-invnr.
      <it_list>-sernr = it_anla-sernr.
      <it_list>-menge = it_anla-menge.
      <it_list>-meins = it_anla-meins.
      <it_list>-ivdat = it_anla-ivdat.  "�������
      <it_list>-invzu = it_anla-invzu.
      <it_list>-zugdt = it_anla-zugdt.  "�ʱ�������
      <it_list>-deakt = it_anla-deakt.
      <it_list>-herst = it_anla-herst.
      <it_list>-vbund = it_anla-vbund.
      <it_list>-typbz = it_anla-typbz.
      <it_list>-anlkl = it_anla-anlkl.
      <it_list>-lifnr = it_anla-lifnr.
      <it_list>-segment = it_anla-segment.
      <it_list>-aibn1   = it_anla-aibn1  .
      <it_list>-aibn2   = it_anla-aibn2  .
      <it_list>-urjhr   = it_anla-urjhr  .
      <it_list>-urwrt   = it_anla-urwrt  .
      <it_list>-antei   = it_anla-antei  .
      READ TABLE it_segm INTO fagl_segmt WITH KEY segment = <it_list>-segment BINARY SEARCH.
      IF sy-subrc EQ 0.
        <it_list>-segmt = fagl_segmt-name.
      ENDIF.

      <it_list>-eaufn = it_anla-eaufn.    "Ͷ�ʶ���
      <it_list>-anlhtxt = it_anla-anlhtxt.
      <it_list>-raumn = it_anla-raumn.
      READ TABLE it_lfa1 INTO lfa1 WITH KEY lifnr = it_anla-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-name1 = lfa1-name1.
      ELSE."���ʲ����û�ù�Ӧ�̱���ʱ���Թ̶��ʲ��������еĹ�Ӧ�������滻��LIUZEHNG
        READ TABLE gt_anla INTO anla WITH KEY bukrs = it_anla-bukrs
                                              anln1 = it_anla-anln1
                                              anln2 = it_anla-anln2
                                              BINARY SEARCH.
        IF sy-subrc EQ 0.
          <it_list>-name1 = anla-liefe.
        ENDIF.
      ENDIF.

      READ TABLE it_t087s WITH KEY gdlgrp = it_anla-gdlgrp BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-gdlgrp = it_t087s-gdlgrp_txt.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '1'
                                   ord4x = it_anla-ord41
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord41 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '2'
                                   ord4x = it_anla-ord42
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord42 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '3'
                                   ord4x = it_anla-ord43
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord43 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '4'
                                   ord4x = it_anla-ord44
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-ord44 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_ankt WITH  KEY anlkl = it_anla-anlkl BINARY SEARCH.
      IF sy-subrc = 0.
        <it_list>-txk20 = it_ankt-txk20.
      ENDIF.

*      READ TABLE it_anlz WITH KEY bukrs = <it_list>-bukrs
*                             anln1 = <it_list>-anln1
*                             anln2 = <it_list>-anln2
*                             BINARY SEARCH.
*      IF sy-subrc EQ 0.
      <it_list>-gsber = it_anla-gsber.
      <it_list>-kostl = it_anla-kostl.
      <it_list>-kostlv = it_anla-kostlv.
      <it_list>-raumn = it_anla-raumn.
      <it_list>-kfzkz = it_anla-kfzkz.
      <it_list>-caufn = it_anla-caufn.
*      ENDIF.

      "12��  �����û�����������BUKRS��ANLN1�� ANLN2��AFABE��GJAHR��ZMONTHH��ѡ��ANLBZAȡBUKRS��
      "ANLN1�� ANLN2��AFASL��NDJAR��NDPER��SCHRW��AFABG����ZANEPTEMP2��ȡZANEPTEMP2�����ֶ�+
      "AFASL��NDJAR��NDPER��SCHRW��AFABG�����ɱ�ZANEPTEMP3�����δ�ҵ�ANLBZA��¼��������û���
      "��������BUKRS��ANLN1�� ANLN2��AFABE����ANLB�����ɱ�ZANEPTEMP3
      READ TABLE it_anlbza WITH KEY bukrs = <it_list>-bukrs
                            anln1 = <it_list>-anln1
                            anln2 = <it_list>-anln2
                            afabe = <it_list>-afabe
                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF it_anlbza-schrw <> 0.
          <it_list>-schrw = it_anlbza-schrw.
        ELSE.
          READ TABLE it_afapl WITH KEY bukrs = <it_list>-bukrs BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR:g_anhwsl,g_ahproz.
            READ TABLE it_t090na WITH KEY afapl = it_afapl-afapl
                                          afasl = it_anlbza-afasl
                                          BINARY SEARCH.
            IF sy-subrc EQ 0.
              g_anhwsl = it_t090na-anhwsl.
              READ TABLE it_t091p WITH KEY anhwsl = g_anhwsl BINARY SEARCH.
              IF sy-subrc EQ 0.
                g_ahproz = it_t091p-ahproz.
*                <IT_LIST>-SCHRW = ( <IT_LIST>-ZCYZ - <IT_LIST>-JZZB - <IT_LIST>-SCHRW ) * G_AHPROZ / 100.

                <it_list>-schrw =  <it_list>-zcyz * g_ahproz / 100.
*                <IT_LIST>-SCHRW =  <IT_LIST>-SCHRW / 100.
                <it_list>-ahproz = g_ahproz.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        <it_list>-afasl = it_anlbza-afasl.
        <it_list>-afabg = it_anlbza-afabg.
        <it_list>-zzndja = it_anlbza-ndjar * 12 + it_anlbza-ndper.

        CLEAR l_x.
        IF <it_list>-afabg >= g_lastdate.
          CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
            EXPORTING
              begda  = g_lastdate
              endda  = <it_list>-afabg
            IMPORTING
              months = l_x.
        ELSE.
          CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
            EXPORTING
              begda  = <it_list>-afabg
              endda  = g_lastdate
            IMPORTING
              months = l_x.
        ENDIF.
        <it_list>-zzndjb = l_x.

        IF <it_list>-zzndjb > <it_list>-zzndja.
          <it_list>-zzndjb = <it_list>-zzndja.
        ENDIF.
      ELSE.
        READ TABLE it_anlb WITH KEY bukrs = <it_list>-bukrs
                              anln1 = <it_list>-anln1
                              anln2 = <it_list>-anln2
                              afabe = <it_list>-afabe
                              BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF it_anlb-schrw <> 0.
            <it_list>-schrw = it_anlb-schrw.
          ELSE.
            READ TABLE it_afapl WITH KEY bukrs = <it_list>-bukrs BINARY SEARCH.
            IF sy-subrc = 0.
              CLEAR:g_anhwsl,g_ahproz.
              READ TABLE it_t090na WITH KEY afapl = it_afapl-afapl
                                            afasl = it_anlb-afasl
                                            BINARY SEARCH.
              IF sy-subrc EQ 0.
                g_anhwsl = it_t090na-anhwsl.
                READ TABLE it_t091p WITH KEY anhwsl = g_anhwsl BINARY SEARCH.
                IF sy-subrc EQ 0.
                  g_ahproz = it_t091p-ahproz.
                  " <it_list>-schrw = ( <it_list>-zcyz - <it_list>-jzzb ) * g_ahproz / 100.
                  <it_list>-schrw = <it_list>-zcyz * g_ahproz / 100.
                  <it_list>-ahproz = g_ahproz.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          <it_list>-afasl = it_anlb-afasl.
          <it_list>-afabg = it_anlb-afabg.
          <it_list>-zzndja = it_anlb-ndjar * 12 + it_anlb-ndper.

          CLEAR l_x.
          IF <it_list>-afabg >= g_lastdate.
            CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
              EXPORTING
                begda  = g_lastdate
                endda  = <it_list>-afabg
              IMPORTING
                months = l_x.
          ELSE.
            CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
              EXPORTING
                begda  = <it_list>-afabg
                endda  = g_lastdate
              IMPORTING
                months = l_x.
          ENDIF.
          <it_list>-zzndjb = l_x.

          IF <it_list>-zzndjb > <it_list>-zzndja.
            <it_list>-zzndjb = <it_list>-zzndja.
          ENDIF.
        ENDIF.
      ENDIF.

      IF g_lastdate+4(4) EQ '1231'.
        <it_list>-ljzj_zc = <it_list>-ljzj_zc + <it_list>-ljzj_jhw.
        <it_list>-ljzj_jhw = 0.
      ENDIF.

      IF <it_list>-zzndjb = <it_list>-zzndja.  "��ʹ���ڼ� = �ƻ�ʹ���ڼ�
        <it_list>-zmonthje1 = 0.
      ELSE.
        <it_list>-zmonthje1 = ( <it_list>-zcyz - <it_list>-jzzb - <it_list>-ljzj_zc - <it_list>-schrw ) / ( <it_list>-zzndja - <it_list>-zzndjb ).
        "  �����۾� = ( zcyz "ԭֵ - jzzb "��ֵ׼�� - ljzj_zc "�ۼ��۾ɣ�������- schrw ��ֵ )  / ( �ƻ�ʹ���ڼ� - ʵ��ʹ���ڼ� )
      ENDIF.

      <it_list>-zmonthje = <it_list>-zmonthje1. "�����۾�
      "modify by zhangzq 2012-10-27
      "<it_list>-zcjz = <it_list>-zcyz - <it_list>-jzzb - <it_list>-ljzj_zc - <it_list>-ljzj_jhw."��ֵ = ԭֵ - ��ֵ׼�� - �ۼ��۾ɣ��ƻ��ڣ� - �ۼ��۾ɣ��ƻ��⣩
      <it_list>-zcjz = <it_list>-zcyz - <it_list>-ljzj_zc."��ֵ = ԭֵ - �ۼ��۾ɣ��ƻ��ڣ�
      <it_list>-zcje = <it_list>-zcyz - <it_list>-ljzj_zc."���� = ԭֵ - �ۼ��۾ɣ��ƻ��ڣ�
*        <IT_LIST>-ZSCHRW = ( <IT_LIST>-SCHRW / <IT_LIST>-ZCYZ ) * 100 . "��ֵ��
      <it_list>-zljzj_zc = <it_list>-zcyz - <it_list>-ljzj_zc - <it_list>-schrw."��������۾�

      "ENDIF. "LUZHENG 20230505
      CLEAR:it_kostl,it_aufnr,it_sel1.
      it_sel1-bukrs = <it_list>-bukrs.
      it_sel1-anln1 = <it_list>-anln1.
      it_sel1-anln2 = <it_list>-anln2.
      it_kostl-kostl = <it_list>-kostl.
      it_aufnr-aufnr = <it_list>-caufn.
      it_anln1-anln1 = <it_list>-anln1.
      COLLECT:it_aufnr,it_sel1,it_anln1,it_kostl.
      it_aufnr-aufnr = <it_list>-eaufn.
      COLLECT:it_aufnr.
    ENDIF.
  ENDLOOP.
  DELETE it_kostl WHERE kostl IS INITIAL.
  IF it_kostl[] IS NOT INITIAL.
    SORT it_kostl BY kostl.
    SELECT csks~kostl,
           csks~func_area,
           cskt~ktext,
           cskt~ltext
      INTO TABLE @DATA(it_cskt)
      FROM csks INNER JOIN cskt ON csks~kostl = cskt~kostl
                               AND csks~kokrs = cskt~kokrs
                               AND csks~datbi = cskt~datbi
      FOR ALL ENTRIES IN @it_kostl
      WHERE csks~kostl = @it_kostl-kostl
      AND   cskt~spras = @sy-langu
      AND   csks~kokrs = 'WISD'.
    SORT it_cskt BY kostl.
  ENDIF.
  DELETE it_aufnr WHERE aufnr IS INITIAL.
  IF it_aufnr[] IS NOT INITIAL.
    SORT it_aufnr BY aufnr.
    SELECT *
      INTO TABLE @DATA(it_aufk)
      FROM aufk
      FOR ALL ENTRIES IN @it_aufnr
      WHERE aufnr = @it_aufnr-aufnr.
    SORT it_aufk BY aufnr.
  ENDIF.
  IF it_sel1[] IS NOT INITIAL.
    SORT it_sel1 BY bukrs anln1 anln2.
    SELECT *
      INTO TABLE @DATA(it_anlp)
      FROM anlp
      FOR ALL ENTRIES IN @it_sel1
      WHERE bukrs = @it_sel1-bukrs
      AND   anln1 = @it_sel1-anln1
      AND   anln2 = @it_sel1-anln2
      AND   gjahr = @p_gjahr.
*      AND   PERAF = @IT_PERAF1.  "�۾ɼ�����
    SORT it_anlp BY bukrs anln1 anln2 peraf afaber.
    SELECT *
      INTO TABLE @DATA(gt_anlc)
      FROM anlc
      FOR ALL ENTRIES IN @it_sel1
      WHERE bukrs = @it_sel1-bukrs
      AND   anln1 = @it_sel1-anln1
      AND   anln2 = @it_sel1-anln2
      AND   gjahr = @p_gjahr
*      AND   PERAF = @IT_PERAF1.  "�۾ɼ�����
      AND   afabe = '01'.
    SORT gt_anlc BY bukrs anln1 anln2.
  ENDIF.
*��ԭʼ�ʲ����
  DELETE it_anln1 WHERE anln1 IS INITIAL.
*  IF IT_ANLN1[] IS NOT INITIAL.
*    SORT IT_ANLN1 BY ANLN1.
*    SELECT *
*      INTO TABLE GT_ANLA
*      FROM ANLA
*      FOR ALL ENTRIES IN IT_ANLN1
*      WHERE ANLN1 = IT_ANLN1-ANLN1
*      AND   BUKRS IN S_BUKRS.
*    SORT GT_ANLA BY ANLN1.
*  ENDIF.
  "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩

  LOOP AT it_list ASSIGNING <it_list>.
*    IF <it_list>-zcyz = 0 AND
*       <it_list>-jzzb = 0 AND
*       <it_list>-ljzj_zc = 0 AND
*       <it_list>-ljzj_jhw = 0 AND
*       <it_list>-zcjz = 0.
*      " DELETE it_list.
*    ELSE.
    READ TABLE enam INTO DATA(wa_nam) WITH KEY anln1 = <it_list>-anln1 BINARY SEARCH.
    IF sy-subrc = 0.
      <it_list>-eknam = wa_nam-eknam.
    ENDIF.
    CLEAR wa_nam.
*  ȡ��Ŀ���� aufk
    READ TABLE it_cskt INTO DATA(wa_cskt) WITH KEY kostl = <it_list>-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-ltext = wa_cskt-ltext.
      <it_list>-func_area = wa_cskt-func_area.
    ENDIF.
    READ TABLE it_tfkbt WITH KEY fkber = <it_list>-func_area BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-fkbtx = it_tfkbt-fkbtx.
    ENDIF.
    READ TABLE it_aufk INTO aufk WITH KEY aufnr = <it_list>-caufn BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-ktext = aufk-ktext.
    ENDIF.
    READ TABLE it_aufk INTO aufk WITH KEY aufnr = <it_list>-eaufn BINARY SEARCH.
    IF sy-subrc EQ 0.
      <it_list>-ktext1 = aufk-ktext.
    ENDIF.

    READ TABLE it_tgsbt WITH  KEY gsber = <it_list>-gsber BINARY SEARCH.
    IF sy-subrc = 0.
      <it_list>-gtext = it_tgsbt-gtext.
    ENDIF.
    <it_list>-zsysyqj = <it_list>-zzndja - <it_list>-zzndjb.
*    ENDIF.

*����۾�����

    IF <it_list>-zugdt IS INITIAL.
      <it_list>-zugdt = <it_list>-aktiv.
    ENDIF.
*&
*    CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE '
*      EXPORTING
*        MONTHS  = 1
*        OLDDATE = <IT_LIST>-ZUGDT
*      IMPORTING
*        NEWDATE = <IT_LIST>-AFABG.
    "�����۾�&�����۾�
    IF <it_list>-bukrs IN s_bukrs.
      it_peraf1 = p_mon .
      it_peraf2 = p_mon - 1.

*    SELECT SINGLE nafaz INTO <IT_LIST>-month_1 FROM anlp
*      WHERE  bukrs = <IT_LIST>-bukrs "= <<IT_LIST>>-bukrs
*         AND anln1 = <IT_LIST>-anln1
*         AND anln2 = <IT_LIST>-anln2
*         AND gjahr = p_gjahr
*         AND peraf = it_peraf1.  "�۾ɼ�����
      READ TABLE it_anlp INTO anlp WITH KEY bukrs = <it_list>-bukrs
                                            anln1 = <it_list>-anln1
                                            anln2 = <it_list>-anln2
                                            peraf = it_peraf1
                                            afaber = '01'
                                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        tabix = sy-tabix.
        LOOP AT it_anlp INTO anlp FROM tabix.
          IF anlp-bukrs NE <it_list>-bukrs
            OR anlp-anln1 NE <it_list>-anln1
            OR anlp-anln2 NE <it_list>-anln2
            OR anlp-peraf NE it_peraf1
            OR anlp-afaber NE '01'.
            EXIT.
          ENDIF.
          <it_list>-month_1 = <it_list>-month_1 + anlp-nafaz.
        ENDLOOP.
      ENDIF.
      READ TABLE it_anlp INTO anlp WITH KEY bukrs = <it_list>-bukrs
                                            anln1 = <it_list>-anln1
                                            anln2 = <it_list>-anln2
                                            peraf = it_peraf2
                                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        <it_list>-month_2 = anlp-nafaz.
      ENDIF.

      <it_list>-month_1 = 0 - <it_list>-month_1.
      <it_list>-month_2 = 0 - <it_list>-month_2.
*      READ TABLE gt_anlc INTO anlc WITH KEY bukrs = <it_list>-bukrs
*                                            anln1 = <it_list>-anln1
*                                            anln2 = <it_list>-anln2
*                                            BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        tabix = sy-tabix.
*        LOOP AT gt_anlc INTO anlc FROM tabix.
*          IF anlc-bukrs NE <it_list>-bukrs
*            OR anlc-anln1 NE <it_list>-anln1
*            OR anlc-anln2 NE <it_list>-anln2.
*            EXIT.
*          ENDIF.
*          <it_list>-year_1 = anlc-nafag + anlc-safag + anlc-aafag.
*          <it_list>-year_1 = 0 - <it_list>-year_1.
*        ENDLOOP.
*      ENDIF.
      READ TABLE it_anlp INTO anlp WITH KEY bukrs = <it_list>-bukrs
                                      anln1 = <it_list>-anln1
                                      anln2 = <it_list>-anln2
                                      afaber = '01'
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        tabix = sy-tabix.
        LOOP AT it_anlp INTO anlp FROM tabix.
          IF anlp-bukrs NE <it_list>-bukrs
            OR anlp-anln1 NE <it_list>-anln1
            OR anlp-anln2 NE <it_list>-anln2
            OR anlp-peraf NE it_peraf1
            OR anlp-afaber NE '01'.
            EXIT.
          ENDIF.
          <it_list>-year_1 = <it_list>-year_1 + anlp-nafap.
        ENDLOOP.
        <it_list>-year_1 = 0 - <it_list>-year_1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "�����۾�&�����۾�
*  LOOP AT IT_LIST WHERE BUKRS IN S_BUKRS.
*    IT_PERAF1 = P_MONAT .
*    IT_PERAF2 = P_MONAT - 1.
*
**    SELECT SINGLE nafaz INTO it_list-month_1 FROM anlp
**      WHERE  bukrs = it_list-bukrs "= <it_list>-bukrs
**         AND anln1 = it_list-anln1
**         AND anln2 = it_list-anln2
**         AND gjahr = p_gjahr
**         AND peraf = it_peraf1.  "�۾ɼ�����
*    READ TABLE IT_ANLP INTO ANLP WITH KEY BUKRS = IT_LIST-BUKRS
*                                          ANLN1 = IT_LIST-ANLN1
*                                          ANLN2 = IT_LIST-ANLN2
*                                          AFABER = '01'
*                                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      TABIX = SY-TABIX.
*      LOOP AT IT_ANLP INTO ANLP FROM TABIX.
*        IF ANLP-BUKRS NE IT_LIST-BUKRS
*          OR ANLP-ANLN1 NE IT_LIST-ANLN1
*          OR ANLP-ANLN2 NE IT_LIST-ANLN2
*          OR ANLP-AFABER NE '01'.
*          EXIT.
*        ENDIF.
*        IT_LIST-MONTH_1 = IT_LIST-MONTH_1 + ANLP-NAFAZ.
*      ENDLOOP.
*    ENDIF.
*    READ TABLE IT_ANLP INTO ANLP WITH KEY BUKRS = IT_LIST-BUKRS
*                                          ANLN1 = IT_LIST-ANLN1
*                                          ANLN2 = IT_LIST-ANLN2
*                                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      IT_LIST-MONTH_2 = ANLP-NAFAZ.
*    ENDIF.
*
*    IT_LIST-MONTH_1 = 0 - IT_LIST-MONTH_1.
*    IT_LIST-MONTH_2 = 0 - IT_LIST-MONTH_2.
*    READ TABLE GT_ANLC INTO ANLC WITH KEY BUKRS = IT_LIST-BUKRS
*                                          ANLN1 = IT_LIST-ANLN1
*                                          ANLN2 = IT_LIST-ANLN2
*                                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      TABIX = SY-TABIX.
*      LOOP AT GT_ANLC INTO ANLC FROM TABIX.
*        IF ANLC-BUKRS NE IT_LIST-BUKRS
*          OR ANLC-ANLN1 NE IT_LIST-ANLN1
*          OR ANLC-ANLN2 NE IT_LIST-ANLN2.
*          EXIT.
*        ENDIF.
*        IT_LIST-YEAR_1 = ANLC-NAFAG + ANLC-SAFAG + ANLC-AAFAG.
*        IT_LIST-YEAR_1 = 0 - IT_LIST-YEAR_1.
*      ENDLOOP.
*    ENDIF.
*    MODIFY IT_LIST TRANSPORTING MONTH_1 MONTH_2 YEAR_1.
*  ENDLOOP.
*&------�����ʲ�ԭ���----MODIFY BY JW_ZHANGZQ---BEGIN-------------------*
  LOOP AT it_list.
    CLEAR g_aibn1.
    PERFORM frm_get_aibn1 USING it_list-anln1 it_list-bukrs.
    it_list-zcybh = g_aibn1.

    IF NOT g_aibn1 IS INITIAL.
      MODIFY it_list.
    ENDIF.
  ENDLOOP.
*&------�����ʲ�ԭ���----MODIFY BY JW_ZHANGZQ---END---------------------*

  "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩�����޸� End
ENDFORM.                                    "process_data

*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       ����û�����Ĳ�ѯ����
*----------------------------------------------------------------------*
FORM check_data.
  DATA: l_bukrs     LIKE t001-bukrs,
        l_anlkl     LIKE ankb-anlkl,
        l_rcomp     LIKE t880-rcomp,
        l_datec(10) TYPE c,
        l_afaber    LIKE t093-afaber.

* ��鹫˾����
  SELECT SINGLE bukrs INTO l_bukrs
    FROM t001
  WHERE bukrs IN s_bukrs.

  IF sy-subrc NE 0.
*   MESSAGE����˾���벻����
    MESSAGE e009(zxmd_msg_fico).
  ENDIF.


* ����۾ɷ�Χ
  "  if not s_afabe is initial.
  SELECT bukrs afapl INTO TABLE it_afapl
    FROM t093c
  WHERE bukrs IN s_bukrs.
  SORT it_afapl BY bukrs.

  IF NOT it_afapl[] IS INITIAL.
    SELECT afaber INTO l_afaber
      FROM t093
      FOR ALL ENTRIES IN it_afapl
      WHERE afapl = it_afapl-afapl
      AND   xstore = 'X'
      AND   afaber = '01'."in s_afabe.
    ENDSELECT.
  ENDIF.

  IF sy-subrc NE 0.
*     MESSAGE���۾ɷ�Χ������
    MESSAGE e011(zxmd_msg_fico).
  ENDIF.
  " endif.

* ����ڼ�
*  IF p_mon GT 16 OR p_mon LT 1.
**   MESSAGE���ڼ�Ӧ��1��16��Χ��
*    MESSAGE e012(zxmd_msg_fico).
*  ENDIF.

* �����ʲ�����ת�ʵ���Ϣ��
  CLEAR: l_datec, g_firstdate, g_lastdate.
  "ZZTR028�ʲ��嵥�����޸��Ƶ���������ZZTR117һ�£�Start
*  concatenate p_gjahr p_monat '01' into l_datec.
  IF p_mon > 12.
    CONCATENATE p_gjahr  '1201' INTO l_datec.
  ELSE.
    CONCATENATE p_gjahr p_mon '01' INTO l_datec.
  ENDIF.
  " ZZTR028�ʲ��嵥�����޸��Ƶ���������ZZTR117һ�£�End
  g_firstdate = l_datec.

  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = g_firstdate
    IMPORTING
      e_date = g_lastdate.

  DATA: BEGIN OF ta_bukrs OCCURS 0.
          INCLUDE STRUCTURE t093c.
  DATA: END OF ta_bukrs.

  REFRESH ta_bukrs.
  CLEAR ta_bukrs.

  SELECT bukrs datum
    INTO CORRESPONDING FIELDS OF TABLE ta_bukrs
    FROM t093c
    WHERE bukrs IN s_bukrs
  AND datum < g_firstdate.
  IF sy-subrc <> 0.
*    MESSAGE e016.
*    EXIT.
  ELSE.
    IF ta_bukrs[] IS INITIAL.
*      MESSAGE e016.
*      EXIT.
    ELSE.
      REFRESH s_bukrs.
      CLEAR s_bukrs.
      LOOP AT ta_bukrs.
        s_bukrs-sign = 'I'.
        s_bukrs-option = 'EQ'.
        s_bukrs-low = ta_bukrs-bukrs.
        APPEND s_bukrs.
      ENDLOOP.
      REFRESH ta_bukrs.
      CLEAR ta_bukrs.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_data

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AIBN1
*&---------------------------------------------------------------------*
*       text  ����ԭʼ�ʲ� ADD by jw_zhangzq 20130227
*----------------------------------------------------------------------*
FORM frm_get_aibn1 USING aibn1 bukrs.
  DATA: i_aibn1 TYPE aibn1, i_bukrs TYPE bukrs.
  CLEAR: i_aibn1 ,i_bukrs.

  SELECT SINGLE aibn1
    INTO i_aibn1
    FROM anla
    WHERE anln1 = aibn1
     AND bukrs = bukrs.
  IF i_aibn1 IS INITIAL.
    EXIT.
  ENDIF.
  IF g_aibn1 IS NOT INITIAL.
    CONCATENATE i_aibn1 '/' g_aibn1 INTO g_aibn1.
  ELSE.
    g_aibn1 = i_aibn1.
  ENDIF.
  IF i_aibn1 = aibn1.
    EXIT.
  ENDIF.
  PERFORM frm_get_aibn1 USING i_aibn1 bukrs.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM auth_check .
  DATA: BEGIN OF tab_bukrs OCCURS 0,
          bukrs LIKE t001-bukrs,
        END OF tab_bukrs.
  DATA: l_message TYPE string.
*��鹫˾����
  IF NOT s_bukrs IS INITIAL .
    SELECT bukrs
      INTO TABLE tab_bukrs
        FROM t001            "  TVKOT
          WHERE
    bukrs IN s_bukrs.
    IF tab_bukrs[] IS NOT INITIAL.
      LOOP AT tab_bukrs.
        AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                 ID 'BUKRS' FIELD tab_bukrs-bukrs
                 ID 'ACTVT' FIELD '03'.
        IF sy-subrc <> 0.
          "  MESSAGE e019(zfi_message) WITH tab_bukrs-bukrs.
          MESSAGE '��Ȩ�Դ˹�˾����' TYPE 'E'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " AUTH_CHECK


*&---------------------------------------------------------------------*
*&      Form  FRM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_alv." ���ALV��ʽ

  DATA:
    ls_layout   TYPE  slis_layout_alv,
    it_fieldcat TYPE  slis_t_fieldcat_alv,
    wa_fieldcat TYPE  slis_fieldcat_alv.

  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra             = 'X'.
  ls_layout-detail_popup = 'X'.

  DEFINE  make_output_title.
    CLEAR wa_fieldcat.
    wa_fieldcat-fieldname   = &1.
    wa_fieldcat-seltext_s   = &2.
    wa_fieldcat-seltext_m   = &2.
    wa_fieldcat-seltext_l   = &2.
    wa_fieldcat-reptext_ddic = &2.
    "WA_FIELDCAT-TECH = &3.
    wa_fieldcat-emphasize  = &3.  "����ɫ
    IF &1 = 'PERNR' OR &1 = 'ENAME'.
      wa_fieldcat-fix_column = 'X'.
    ENDIF.
    wa_fieldcat-fix_column = &4.

    APPEND wa_fieldcat TO it_fieldcat.
  END-OF-DEFINITION.

*  CASE SY-TCODE .
*    WHEN 'ZFI002'.
  make_output_title 'BUKRS' '��˾����' 'C410' 'X'.
  make_output_title 'ANLKL' '�ʲ�����' 'C410' 'X'.
  make_output_title 'TXK20' '�ʲ���������' 'C410' 'X'.
  make_output_title 'ANLN1' '�ʲ����' 'C410' 'X'.
  make_output_title 'TXT50' '�ʲ�����' 'C410' 'X'.
  make_output_title 'SERNR' '���к�' 'C410' 'X'.
  make_output_title 'ANLN2' '�ʲ��ӱ��' ' ' ' '.
  make_output_title 'TXA50' 'ʵ����' ' ' ''.
  make_output_title 'MENGE' '����' ' ' ''.
  make_output_title 'MEINS' '������λ' ' ' ''.
  make_output_title 'KOSTL' '�ɱ�����' ' ' ''.
  make_output_title 'LTEXT' '�ɱ���������' ' ' ''.
  make_output_title 'FKBTX' '��������' ' ' ''.
  "  make_output_title 'KOSTLV' '���γɱ�����' ' ' ''.
  make_output_title 'CAUFN' '�ڲ�����' ' ' ''.
*  MAKE_OUTPUT_TITLE 'KTEXT' '�ڲ���������' ' ' ''.
  make_output_title 'EAUFN' 'Ͷ�ʶ���' ' ' ''.
  make_output_title 'KTEXT1' 'Ͷ�ʶ�������' ' ' ''.
  "      make_output_title 'RAUMN' '����' ' ' ''.

  make_output_title 'ORD41' '�ʲ�С��' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD41T' '���ӷ�ʽ' ' '.
  make_output_title 'ORD42' 'ʹ��״̬' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD42T' '�ʽ���Դ' ' '.
*  make_output_title 'ORD43' 'ʹ��״̬' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD43T' 'ʹ��״̬' ' '.
*      make_output_title 'ORD44' '�ʲ�ϸ��' ' ' ''.
  "MAKE_OUTPUT_TITLE 'ORD44T' '�ʲ�ϸ��' ' '.
*      make_output_title 'KFZKZ' '�������պ�' ' ' ''.

  make_output_title 'LIFNR' '��Ӧ��' ' ' ''.
  make_output_title 'NAME1' '��Ӧ������' ' ' ''.
  "      make_output_title 'HERST' '������' ' ' ''.

  "MAKE_OUTPUT_TITLE 'AIBN1' 'ԭʼ�ʲ�����' ' '.
  make_output_title 'ZUGDT' '�ʱ�������' ' ' ''.
  make_output_title 'DEAKT' '�������' ' ' ''.
  make_output_title 'AFASL' '�۾���' ' ' ''.
  make_output_title 'AFABG' '��ʼ�����۾�����' ' ' ''.
  "MAKE_OUTPUT_TITLE 'NDJAR' '�ƻ�ʹ������' ' '.
  "MAKE_OUTPUT_TITLE 'NDPER' '�ƻ�ʹ���ڼ�' ' '.
  make_output_title 'ZZNDJA' '�ƻ�ʹ���ڼ�' ' ' ''.
  make_output_title 'ZCYZ' 'ԭֵ' ' ' ''.

  make_output_title 'JZZB' '��ֵ׼��' ' ' ''.
  make_output_title 'LJZJ_ZC' '�ۼ��۾�' ' ' ''.
  "MAKE_OUTPUT_TITLE 'LJZJ_JHW' '�ۼ��۾ɣ�δ�ƻ���' ' ' ''.
  make_output_title 'ZCJZ' '��ֵ' ' ' ''.
  make_output_title 'ZCJE' '����' ' ' ''.
  make_output_title 'AHPROZ' '��ֵ��%' ' ' ''.
  make_output_title 'SCHRW' 'Ԥ�Ʋ�ֵ' ' ' ''.
  make_output_title 'ZLJZJ_ZC' '��������۾�' ' ' ''.
  make_output_title 'ZZNDJB' '��ʹ���ڼ�' ' ' ''.
  make_output_title 'ZSYSYQJ' 'ʣ��ʹ���ڼ�' ' ' ''.
  make_output_title 'YEAR_1' '��ǰ������۾ɶ�' ' ' ''.
  make_output_title 'MONTH_2' '�����۾ɶ�' ' ' ''.
  make_output_title 'MONTH_1' '�����۾ɶ�' ' ' ''.
  make_output_title 'ZMONTHJE' '�����۾ɶ�' ' ' ''.
  make_output_title 'ZCYBH' '�ʲ�ԭ���' ' ' ''.
  make_output_title 'SEGMENT' '��' ' ' ''.
  make_output_title 'SEGMT' '������' ' ' ''.
  make_output_title 'AIBN1' 'ԭʼ�ʲ�' ' ' ''.
  make_output_title 'AIBN2' 'ԭʼ�ʲ�2' ' ' ''.
  make_output_title 'URJHR' 'ԭʼ�������' ' ' ''.
  make_output_title 'URWRT' 'ԭʼֵ' ' ' ''.
  make_output_title 'EKNAM' '�ɹ�Ա' ' ' ''.
  make_output_title 'ANTEI' '���ڲ�Ʒ�ٷֱ�' ' ' ''.


*    WHEN 'ZFI'.
*      MAKE_OUTPUT_TITLE 'BUKRS' '��˾����' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'ANLKL' '�ʲ�����' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'TXK20' '�ʲ���������' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'ANLN1' '�ʲ����' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'TXT50' '�ʲ�����' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'SERNR' '���к�' 'C410' 'X'.
*      MAKE_OUTPUT_TITLE 'ANLN2' '�ʲ��ӱ��' ' ' ' '.
*      MAKE_OUTPUT_TITLE 'TXA50' '�ͺŹ��' ' ' ''.
*      MAKE_OUTPUT_TITLE 'MENGE' '����' ' ' ''.
*      MAKE_OUTPUT_TITLE 'MEINS' '������λ' ' ' ''.
*      MAKE_OUTPUT_TITLE 'KOSTL' '�ɱ�����' ' ' ''.
*      MAKE_OUTPUT_TITLE 'LTEXT' '�ɱ���������' ' ' ''.
*      "  make_output_title 'KOSTLV' '���γɱ�����' ' ' ''.
*      "     make_output_title 'CAUFN' '�ڲ�����' ' ' ''.
*      "      make_output_title 'KTEXT' '�ڲ���������' ' ' ''.
*      MAKE_OUTPUT_TITLE 'EAUFN' 'Ͷ�ʶ���' ' ' ''.
*      MAKE_OUTPUT_TITLE 'KTEXT1' 'Ͷ�ʶ�������' ' ' ''.
*      "   make_output_title 'RAUMN' '����' ' ' ''.
*
*      MAKE_OUTPUT_TITLE 'ORD41' '�ʲ�С��' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD41T' '���ӷ�ʽ' ' '.
*      MAKE_OUTPUT_TITLE 'ORD42' 'Ͷ��ԭ��' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD42T' '�ʽ���Դ' ' '.
*      MAKE_OUTPUT_TITLE 'ORD43' 'ʹ��״̬' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD43T' 'ʹ��״̬' ' '.
*      "   make_output_title 'ORD44' '�ʲ�ϸ��' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'ORD44T' '�ʲ�ϸ��' ' '.
**      make_output_title 'KFZKZ' '�������պ�' ' ' ''.
*
**      make_output_title 'LIFNR' '��Ӧ��' ' ' ''.
**      make_output_title 'NAME1' '��Ӧ������' ' ' ''.
**      make_output_title 'HERST' '������' ' ' ''.
*
*      "MAKE_OUTPUT_TITLE 'AIBN1' 'ԭʼ�ʲ�����' ' '.
*      "  make_output_title 'ZUGDT' '�ʱ�������' ' ' ''.
*      "      make_output_title 'DEAKT' '�������' ' ' ''.
*      "      make_output_title 'AFASL' '�۾���' ' ' ''.
*      MAKE_OUTPUT_TITLE 'AFABG' '��ʼ�����۾�����' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'NDJAR' '�ƻ�ʹ������' ' '.
*      "MAKE_OUTPUT_TITLE 'NDPER' '�ƻ�ʹ���ڼ�' ' '.
**      make_output_title 'ZZNDJA' '�ƻ�ʹ���ڼ�' ' ' ''.
*
**
**      make_output_title 'JZZB' '��ֵ׼��' ' ' ''.
**      make_output_title 'LJZJ_ZC' '�ۼ��۾�' ' ' ''.
*      "MAKE_OUTPUT_TITLE 'LJZJ_JHW' '�ۼ��۾ɣ�δ�ƻ���' ' ' ''.
*
*      "      make_output_title 'ZCJE' '����' ' ' ''.
*
*
*      MAKE_OUTPUT_TITLE 'ZZNDJB' '��ʹ���ڼ�' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZSYSYQJ' 'ʣ��ʹ���ڼ�' ' ' ''.
**      make_output_title 'YEAR_1' '��ǰ������۾ɶ�' ' ' ''.
**      make_output_title 'MONTH_2' '�����۾ɶ�' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZCYZ' 'ԭֵ' ' ' ''.
*      MAKE_OUTPUT_TITLE 'MONTH_1' '�����۾ɶ�' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZCJZ' '��ֵ' ' ' ''.
*      MAKE_OUTPUT_TITLE 'SCHRW' 'Ԥ�Ʋ�ֵ' ' ' ''.
*      MAKE_OUTPUT_TITLE 'ZLJZJ_ZC' '��������۾�' ' ' ''.
*      MAKE_OUTPUT_TITLE 'LJZJ_ZC' '�ۼ��۾�' ' ' ''.
**      make_output_title 'ZMONTHJE' '�����۾ɶ�' ' ' ''.
**      make_output_title 'ZCYBH' '�ʲ�ԭ���' ' ' ''.
*  ENDCASE.

*    make_output_title 'BUKRS' '��˾����' 'C410' 'X'.
*    make_output_title 'ANLKL' '�ʲ�����' 'C410' 'X'.
*    make_output_title 'TXK20' '�ʲ���������' 'C410' 'X'.
*    make_output_title 'ANLN1' '�ʲ����' 'C410' 'X'.
*    make_output_title 'TXT50' '�ʲ�����' 'C410' 'X'.
*    make_output_title 'SERNR' '���к�(ԭ�ʲ����)' 'C410' 'X'.
*    make_output_title 'ANLN2' '�ʲ��ӱ��' ' ' ' '.
*    make_output_title 'TXA50' '�ͺŹ��' ' ' ''.
*    make_output_title 'MENGE' '����' ' ' ''.
*    make_output_title 'MEINS' '������λ' ' ' ''.
*    make_output_title 'KOSTL' '�ɱ�����' ' ' ''.
*    make_output_title 'LTEXT' '�ɱ���������' ' ' ''.
*    make_output_title 'KOSTLV' '���γɱ�����' ' ' ''.
*    make_output_title 'CAUFN' '�ڲ�����' ' ' ''.
*    make_output_title 'KTEXT' '�ڲ���������' ' ' ''.
  make_output_title 'RAUMN' '����' ' ' ''.
*
*    make_output_title 'ORD41' '���ӷ�ʽ' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD41T' '���ӷ�ʽ' ' '.
*    make_output_title 'ORD42' '�ʽ���Դ' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD42T' '�ʽ���Դ' ' '.
*    make_output_title 'ORD43' 'ʹ��״̬' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD43T' 'ʹ��״̬' ' '.
*    make_output_title 'ORD44' '�ʲ�ϸ��' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'ORD44T' '�ʲ�ϸ��' ' '.
*    make_output_title 'KFZKZ' '�������պ�' ' ' ''.
*
*    make_output_title 'LIFNR' '��Ӧ��' ' ' ''.
*    make_output_title 'NAME1' '��Ӧ������' ' ' ''.
  make_output_title 'HERST' '������' ' ' ''.
*
*    "MAKE_OUTPUT_TITLE 'AIBN1' 'ԭʼ�ʲ�����' ' '.
*    make_output_title 'AKTIV' '�ʱ�������' ' ' ''.
*    make_output_title 'DEAKT' '�������' ' ' ''.
*    make_output_title 'AFASL' '�۾���' ' ' ''.
*    make_output_title 'AFABG' '��ʼ�����۾�����' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'NDJAR' '�ƻ�ʹ������' ' '.
*    "MAKE_OUTPUT_TITLE 'NDPER' '�ƻ�ʹ���ڼ�' ' '.
*    make_output_title 'ZZNDJA' '�ƻ�ʹ���ڼ�' ' ' ''.
*    make_output_title 'ZCYZ' 'ԭֵ' ' ' ''.
*
*    make_output_title 'JZZB' '��ֵ׼��' ' ' ''.
*    make_output_title 'LJZJ_ZC' '�ۼ��۾�' ' ' ''.
*    "MAKE_OUTPUT_TITLE 'LJZJ_JHW' '�ۼ��۾ɣ�δ�ƻ���' ' ' ''.
*    make_output_title 'ZCJZ' '��ֵ' ' ' ''.
*    make_output_title 'ZCJE' '����' ' ' ''.
*    make_output_title 'SCHRW' 'Ԥ�Ʋ�ֵ' ' ' ''.
*
*    make_output_title 'ZZNDJB' '��ʹ���ڼ�' ' ' ''.
*    make_output_title 'ZSYSYQJ' 'ʣ��ʹ���ڼ�' ' ' ''.
*    make_output_title 'YEAR_1' '��ǰ������۾ɶ�' ' ' ''.
*    make_output_title 'MONTH_2' '�����۾ɶ�' ' ' ''.
*    make_output_title 'MONTH_1' '�����۾ɶ�' ' ' ''.
*    make_output_title 'ZMONTHJE' '�����۾ɶ�' ' ' ''.
*    make_output_title 'ZCYBH' '�ʲ�ԭ���' ' ' ''.
  make_output_title 'INVNR' '���ʲ���' ' ' ''.
  make_output_title 'ANLHTXT' '�豸��' ' ' ''.

  make_output_title ' INVZU' '���ע��' ' ' ''.

  LOOP AT it_fieldcat INTO wa_fieldcat.
    CASE wa_fieldcat-fieldname.
      WHEN 'ANLN1' OR 'ANLN2' OR 'KOSTL'
        OR 'CAUFN' OR 'EAUFN' OR 'LIFNR'
        OR 'SEGMENT' OR 'AIBN1' OR 'AIBN2'
        OR 'INVNR'.
        wa_fieldcat-no_zero = 'X'.
    ENDCASE.
    MODIFY it_fieldcat FROM wa_fieldcat.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK           = ' '
*     I_BYPASSING_BUFFER          = ' '
*     I_BUFFER_ACTIVE             = ' '
      i_callback_program          = sy-repid
*     I_CALLBACK_PF_STATUS_SET    = 'PF_STATUS_SET'
      i_callback_user_command     = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE      = ' '
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
*     I_CALLBACK_HTML_END_OF_LIST = ' '
*     I_STRUCTURE_NAME            = 'ZSJTHRR0037'
*     I_BACKGROUND_ID             = ' '
*     I_GRID_TITLE                =
*     I_GRID_SETTINGS             =
      is_layout                   = ls_layout
      it_fieldcat                 = it_fieldcat
      i_save                      = 'X'
      i_html_height_top           = '0'
    TABLES
      t_outtab                    = it_list
* EXCEPTIONS
*     PROGRAM_ERROR               = 1
*     OTHERS                      = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    "FRM_ALV

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
            rs_selfield TYPE slis_selfield.
  CLEAR it_list.
  CASE r_ucomm.
    WHEN '&IC1'.
      CASE rs_selfield-fieldname.
        WHEN 'ANLN1' OR 'ANLN2'.
          READ TABLE it_list INTO it_list INDEX rs_selfield-tabindex.
          IF sy-subrc = 0.
*            IF rs_selfield-fieldname = 'BELNR'.
*              SET PARAMETER ID 'BLN' FIELD WA_DATA1-BELNR.
*            ELSEIF rs_selfield-fieldname = 'AUGBL'.
*              SET PARAMETER ID 'BLN' FIELD WA_DATA1-AUGBL.
*            ENDIF.

            SET PARAMETER ID 'AN1' FIELD it_list-anln1.
            SET PARAMETER ID 'AN2' FIELD it_list-anln2.
            SET PARAMETER ID 'BUK' FIELD it_list-bukrs.
            CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
          ENDIF.
          "WHEN 'KOSTL'.
          "          READ TABLE IT_LIST INTO IT_LIST INDEX RS_SELFIELD-TABINDEX.
          "          IF SY-SUBRC = 0.
          "            SET PARAMETER ID 'KOS' FIELD IT_LIST-KOSTL.
          "            CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
          "          ENDIF.
          "        WHEN 'KOSTLV'.
          "          READ TABLE IT_LIST INTO IT_LIST INDEX RS_SELFIELD-TABINDEX.
          "          IF SY-SUBRC = 0.
          "            SET PARAMETER ID 'KOS' FIELD IT_LIST-KOSTLV.
          "            CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
          "          ENDIF.
        WHEN 'CAUFN'."�ڲ�����
          READ TABLE it_list INTO it_list INDEX rs_selfield-tabindex.

          IF sy-subrc = 0.
            SET PARAMETER ID 'ANR' FIELD it_list-caufn.
            CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'EAUFN'."Ͷ�ʶ���
          READ TABLE it_list INTO it_list INDEX rs_selfield-tabindex.

          IF sy-subrc = 0.
            SET PARAMETER ID 'ANR' FIELD it_list-eaufn.
            CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.                    "USER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DOCUMENT   text
*----------------------------------------------------------------------*
FORM html_top_of_page USING document TYPE REF TO cl_dd_document.

  DATA: text TYPE sdydo_text_element.
  DATA: m_p      TYPE i,
        m_buffer TYPE string.
*  CASE SY-TCODE .
*    WHEN 'ZFIR017'.
  text =  '�̶��ʲ���ϸ��'.
*    WHEN 'ZFIR018'.
*      TEXT =  '�̶��ʲ��۾ɱ�'.
*  ENDCASE.
  CALL METHOD document->add_text
    EXPORTING
      text          = text
      sap_style     = 'HEADING'               " ��ʾ���ֵ�STYLE����
      sap_color     = cl_dd_document=>list_total_int
      sap_fontsize  = cl_dd_document=>large
      sap_fontstyle = cl_dd_document=>serif
      sap_emphasis  = cl_dd_document=>emphasis.

  CALL METHOD document->new_line. "����
  CALL METHOD document->new_line.

  CALL METHOD document->add_icon     " ����ͼƬ
    EXPORTING
      sap_icon = 'ICON_DATE'.

  text = '�������� : '.
  CALL METHOD document->add_text
    EXPORTING
      text         = text
      sap_emphasis = 'Strong'.

  CALL METHOD document->add_gap
    EXPORTING
      width = 2.

  CONCATENATE p_gjahr '��' zrq '��' INTO text.
  CALL METHOD document->add_text
    EXPORTING
      text      = text
      sap_style = 'Key'.

  CALL METHOD document->add_gap
    EXPORTING
      width = 10.

  text = '������ : '.
  CALL METHOD document->add_text      "�����ı�
    EXPORTING
      text         = text
      sap_emphasis = 'Strong'.

  CALL METHOD document->add_gap      "����λ��
    EXPORTING
      width = 2.

  text = sy-uname.
  CALL METHOD document->add_text
    EXPORTING
      text      = te