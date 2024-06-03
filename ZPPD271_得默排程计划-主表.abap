*&---------------------------------------------------------------------*
*& Report ZPPD271
*&---------------------------------------------------------------------*
*& ��Ĭ�ų̼ƻ�-����
*&---------------------------------------------------------------------*
REPORT zppd271 MESSAGE-ID zmsg_gp.

TABLES:sscrfields,t001l,ztpp_205,prps.
TYPE-POOLS:slis.
DATA: fldct TYPE slis_t_fieldcat_alv,
      slayt TYPE slis_layout_alv,
      varnt LIKE disvariant,
      repid LIKE sy-repid.
DATA: i_title TYPE lvc_title.
DATA:BEGIN OF itab OCCURS 0.
       INCLUDE TYPE ztpp_270.
DATA:  zljkp_id LIKE icon-id, " ��ƽ״̬ ICON_LED_RED  ICON_LED_GREEN
       zljtc_id LIKE icon-id, " ����״̬
       zljzw_id LIKE icon-id, " ����״̬
       zljfh_id LIKE icon-id, " ����״̬
       zljkc_id LIKE icon-id, " ����״̬
       zljzz_id LIKE icon-id, " ��װ״̬
       zbx      TYPE ztpp_270a-zbx,
       zbxks    TYPE ztpp_270a-zbxks,
       sel,
     END OF itab.
DATA:t_text TYPE TABLE OF zsmm202 WITH HEADER LINE.

TYPES:BEGIN OF ty_zpcdh,
        zpcdh TYPE ztpp_205-zpcdh,
      END OF ty_zpcdh.
DATA:gt_zpcdh TYPE TABLE OF ty_zpcdh WITH KEY zpcdh,
     gv_zpcdh LIKE LINE OF gt_zpcdh.
DATA:it_matnr  TYPE TABLE OF ccvx_matnr WITH HEADER LINE,
     outtab001 TYPE TABLE OF zsfms_getpctx WITH HEADER LINE.
DATA:BEGIN OF gt_ysks OCCURS 0, "���տ���
       zpcdh TYPE ztpp_205-zpcdh,
       zzk   TYPE ztpp316-zzk,
       zwllx TYPE ztpp_206-zwllx,
     END OF gt_ysks.
DATA:BEGIN OF gt_lj OCCURS 0, "·��
       text  TYPE string, "·������
       field TYPE string, "·���ֶ� ��ZTPP_205B���ֶα���һ��
     END OF gt_lj.
DATA:BEGIN OF gt_gx OCCURS 0, "����
       text  TYPE string,          "��������
       field TYPE string,          "�����ֶ�
       zsdpl TYPE ztpp_270a-zsdpl, "��������-�ϵ�
       zxdpl TYPE ztpp_270a-zxdpl, "��������-�µ�
     END OF gt_gx.
DATA:zbxks TYPE ztpp_205a-zks.  "ÿ�����Ϳ���
DATA:BEGIN OF gt_pcks OCCURS 0, "ÿ���Ų�������
       zpcdh TYPE ztpp_270-zpcdh,
       zbxks TYPE ztpp_205a-zks,
     END OF gt_pcks.
DATA:answer TYPE char1.
DATA:zwbcms_field TYPE TABLE OF char6 WITH HEADER LINE, "��峤����
     znbcms_field TYPE TABLE OF char6 WITH HEADER LINE, "�ڰ峤����
     zdbcms_field TYPE TABLE OF char6 WITH HEADER LINE. "��Ĭ���峤����
DATA:BEGIN OF gt_bzjs OCCURS 0, "�Ų���ע����
       zpcdh   TYPE ztpp_270-zpcdh,
       zpcbzjs TYPE string,
     END OF gt_bzjs.
DATA: gt_zsfyp TYPE TABLE OF dd07v WITH HEADER LINE. "��ֵ �Ƿ���Ʒ

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  PARAMETERS    :p_werks TYPE t001l-werks DEFAULT '3009'.
  SELECT-OPTIONS:s_zpcdh FOR ztpp_205-zpcdh,
                 s_posid FOR prps-posid,
                 s_matnr FOR ztpp_205-matnr,
                 s_sydat FOR ztpp_205-sydat.
  PARAMETERS p_all TYPE char1 AS CHECKBOX USER-COMMAND singleclick.
SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.
  PERFORM getdomain(zpubform) TABLES gt_zsfyp USING 'ZD_SF'.

INITIALIZATION.
  t1 = '����ѡ��'.
  %_p_werks_%_app_%-text = '����'.
  %_s_zpcdh_%_app_%-text = '�Ų�����'.
  %_s_posid_%_app_%-text = '��Ŀ����'.
  %_s_matnr_%_app_%-text = '���ϱ���'.
  %_s_sydat_%_app_%-text = '��������'.
  %_p_all_%_app_%-text = 'ȫ��'.

  gt_lj[] = VALUE #( ( text = '��ƽ'     field = 'ZLJKP'   )
                     ( text = '����'     field = 'ZLJTC'   )
                     ( text = '�����и�' field = 'ZLJJGQG' )
                     ( text = '����'     field = 'ZLJZW'   )
                     ( text = '����'     field = 'ZLJFH'   )
                     ( text = '����'     field = 'ZLJKC'   )
                     ( text = '��װ'     field = 'ZLJZZ'   )
                     ( text = '���'     field = 'ZLJDB'   )
                    ).
  gt_gx[] = VALUE #( ( text = '��ƽ'     field = 'ZLJKP'   zsdpl = '����'        zxdpl = '����'      )
                     ( text = '����'     field = 'ZLJTC'   zsdpl = '��ƽ'        zxdpl = '����'      )
                     ( text = '����'     field = 'ZLJZW'   zsdpl = '����'        zxdpl = '����'      )
                     ( text = '����'     field = 'ZLJFH'   zsdpl = '����'        zxdpl = '����/��װ' )
                     ( text = '����'     field = 'ZLJKC'   zsdpl = '����'        zxdpl = '��װ'      )
                     ( text = '��װ'     field = 'ZLJZZ'   zsdpl = '����/����'   zxdpl = '���'      )
                    ).
  zwbcms_field[] = VALUE #( ( 'ZWBCD' ) ( 'ZWBHD' ) ( 'ZWBYS' ) ( 'ZWBTC' ) ( 'ZWBDC' ) ( 'ZWBQD' ) ).
  znbcms_field[] = VALUE #( ( 'ZNBCD' ) ( 'ZNBHD' ) ( 'ZNBYS' ) ( 'ZNBTC' ) ( 'ZNBDC' ) ( 'ZNBQD' ) ).
  zdbcms_field[] = VALUE #( ( 'ZCD' ) ( 'ZHD' ) ( 'ZYS' ) ( 'ZTCZL' ) ( 'ZDCHL' ) ( 'ZCZQD' ) ).


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
      z205~werks,
      z205~posid,
      z205~zpcdh,
      z205~matnr,
      z205a~zdetailname AS zbx,
      z205a~zms,
      z205a~zpcsl,
      z205a~zks,
      z205b~zsfyp AS zsfyp_v,
      z205b~zjhsj AS zjhrq,
      z205b~zqtrq,
      z205b~zdbh,
      z205b~zplh,
      z205b~zplkd,
      z205b~zljkp,
      z205b~zljtc,
      z205b~zljjgqg,
      z205b~zljzw,
      z205b~zljfh,
      z205b~zljkc,
      z205b~zljzz,
      z205b~zljdb,
      z205b~zjfsx,
      z270~zycbs,
      z270~zpch
  FROM ztpp_205 AS z205
  INNER JOIN ztpp_205b AS z205b ON z205b~zpcdh = z205~zpcdh
  INNER JOIN ztpp_205a AS z205a ON z205a~zpcdh = z205~zpcdh
  LEFT JOIN  ztpp_270  AS z270  ON z270~zpcdh  = z205~zpcdh
  WHERE z205~werks = @p_werks
    AND z205~zpcdh IN @s_zpcdh
    AND z205~posid IN @s_posid
    AND z205~matnr IN @s_matnr
    AND z205~sydat IN @s_sydat
    AND z205~del = ''
    AND z205a~del = ''
  INTO TABLE @DATA(lt_itab).
  IF p_all = ''.
    DELETE lt_itab WHERE zycbs = 'X'.
  ENDIF.
  IF lt_itab IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  REFRESH:gt_zpcdh,it_matnr.
  LOOP AT lt_itab INTO DATA(lv_itab).
    gv_zpcdh-zpcdh = lv_itab-zpcdh.
    COLLECT gv_zpcdh INTO gt_zpcdh.
    it_matnr-matnr    = lv_itab-matnr.
    COLLECT: it_matnr.
  ENDLOOP.

  "������
  SELECT
    z206~zpcdh,
    z206~aufnr
  FROM ztpp_206 AS z206
  INNER JOIN @gt_zpcdh AS zpc ON zpc~zpcdh = z206~zpcdh
  WHERE del = ''
  GROUP BY z206~zpcdh,z206~aufnr
  ORDER BY z206~zpcdh,z206~aufnr
  INTO TABLE @DATA(lt_aufnr).

  REFRESH gt_bzjs.
  LOOP AT gt_zpcdh INTO gv_zpcdh.
*ȡ���ı�
    CALL FUNCTION 'ZFM_DEALLONGTEXT'
      EXPORTING
        intype = 'O'
        tdid   = 'BZJS'
        sapno  = gv_zpcdh-zpcdh
        sapmk  = 'PCD'
      TABLES
        t_text = t_text.
    LOOP AT t_text.
      CONCATENATE gt_bzjs-zpcbzjs t_text-text INTO gt_bzjs-zpcbzjs.
    ENDLOOP.
    gt_bzjs-zpcdh = gv_zpcdh-zpcdh.
    APPEND gt_bzjs.
  ENDLOOP.
  SORT gt_bzjs BY zpcdh.

  "ȡ001����
  REFRESH outtab001.
  PERFORM get001 IN PROGRAM zpubform TABLES it_matnr outtab001 USING 'ZWBCD,ZWBHD,ZWBYS,ZWBTC,ZWBDC,ZWBQD,ZNBCD,ZNBHD,ZNBYS,ZNBTC,ZNBDC,ZNBQD,ZCD,ZHD,ZYS,ZTCZL,ZDCHL,ZCZQD'.
  SORT outtab001 BY matnr atnam.

  "�µ�����
  SELECT
    zpc~zpcdh,
    MAX( log~zdate ) AS zdate
  FROM ztsdzdlog AS log
  INNER JOIN @gt_zpcdh AS zpc ON  log~sapno = zpc~zpcdh
                              AND ( ( log~value_o = 'S'
                                      AND (  log~value_n = 'C' OR log~value_n = 'A' )
                                     )
                                    OR ( log~value_o = '' AND log~value_n = 'A' )
                                  )
  GROUP BY zpc~zpcdh
  ORDER BY zpc~zpcdh
  INTO TABLE @DATA(lt_log).

  "������
  ##ITAB_DB_SELECT
  SELECT
      t~zpcdh,
      SUM( t~zms ) AS zms,
      SUM( t~zpcsl ) AS zpcsl,
      SUM( 1 ) AS zpchs
  FROM @lt_itab AS t
  GROUP BY t~zpcdh
  ORDER BY t~zpcdh
  INTO TABLE @DATA(lt_ddl).

  "���տ���
  SELECT
    z206~zpcdh,
    z206~aufnr,
    CASE WHEN z206~zwllx = '��Ʒ' THEN z206~zwllx ELSE '���Ʒ' END AS zwllx
  FROM ztpp_206 AS z206
  INNER JOIN @gt_zpcdh AS zpc ON z206~zpcdh = zpc~zpcdh
  WHERE z206~del = ''
    AND z206~werks = @p_werks
    AND EXISTS ( SELECT 'X'
                FROM ztpp_afrdt
                WHERE aufnr = z206~aufnr )
  GROUP BY z206~zpcdh,z206~aufnr,zwllx
  ORDER BY z206~zpcdh,z206~aufnr,zwllx
  INTO TABLE @DATA(lt_206).

  REFRESH gt_ysks.
  SELECT
    z206~zpcdh,
    SUM( z316~zzk ) AS zzk,
    z206~zwllx
  FROM ztpp316 AS z316
  INNER JOIN @lt_206 AS z206 ON z206~aufnr = z316~aufnr
  WHERE z316~werks = @p_werks
  GROUP BY z206~zpcdh,zwllx
  ORDER BY z206~zpcdh,zwllx
  INTO TABLE @gt_ysks.

  "��������
  SELECT
    z270~zpcdh,
    z270a~zgxmc
  FROM ztpp_270a AS z270a
  INNER JOIN ztpp_270 AS z270 ON z270~zpch = z270a~zpch
  INNER JOIN @gt_zpcdh AS zpc ON z270~zpcdh = zpc~zpcdh
  WHERE z270a~zdelbs = ''
  GROUP BY z270~zpcdh,z270a~zgxmc
  ORDER BY z270~zpcdh,z270a~zgxmc
  INTO TABLE @DATA(lt_gxmc).

  CLEAR:itab,itab[].
  LOOP AT lt_itab INTO lv_itab GROUP BY ( zpcdh = lv_itab-zpcdh zbx = lv_itab-zbx
                                           size = GROUP SIZE
                                           index = GROUP INDEX )
    ASCENDING ASSIGNING FIELD-SYMBOL(<group>).
    CLEAR:zbxks.
    LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<fs_item>).
      zbxks += <fs_item>-zks.
    ENDLOOP.
    itab-zpcdh = <group>-zpcdh.
    itab-zbx   = <group>-zbx.
    itab-zbxks = zbxks.
    APPEND itab.
  ENDLOOP.
  SORT lt_itab BY zpcdh zbx.
  REFRESH gt_pcks.
  LOOP AT itab.
    gt_pcks-zpcdh = itab-zpcdh.
    gt_pcks-zbxks = itab-zbxks.
    COLLECT gt_pcks.
  ENDLOOP.
  SORT gt_pcks BY zpcdh.
  LOOP AT itab.
    READ TABLE lt_itab INTO lv_itab WITH KEY zpcdh = itab-zpcdh zbx = itab-zbx BINARY SEARCH.
    IF sy-subrc = 0.
      itab-werks = lv_itab-werks.
      itab-posid = lv_itab-posid.
      itab-matnr = lv_itab-matnr.
      READ TABLE gt_zsfyp WITH KEY domvalue_l = lv_itab-zsfyp_v BINARY SEARCH.
      IF sy-subrc = 0.
        itab-zsfyp = gt_zsfyp-ddtext.
      ENDIF.
      itab-zjhrq = lv_itab-zjhrq.
      itab-zqtrq = lv_itab-zqtrq.
      itab-zdbh  = lv_itab-zdbh.
      itab-zjfsx = lv_itab-zjfsx.
      itab-zplh = lv_itab-zplh.
      itab-zplkd = lv_itab-zplkd.
      itab-zpch = lv_itab-zpch.
      " ·��ƴ��
      LOOP AT gt_lj.
        ASSIGN COMPONENT gt_lj-field OF STRUCTURE lv_itab TO FIELD-SYMBOL(<f_lj>).
        IF sy-subrc = 0 AND <f_lj> = 'X'.
          CONCATENATE itab-zsclj '-' gt_lj-text INTO itab-zsclj.
        ENDIF.
      ENDLOOP.
      SHIFT itab-zsclj LEFT DELETING LEADING '-'.
      LOOP AT gt_gx.
        ASSIGN COMPONENT gt_gx-field OF STRUCTURE lv_itab TO FIELD-SYMBOL(<f_gx>).
        IF sy-subrc = 0.
          ASSIGN COMPONENT |{ gt_gx-field }_id| OF STRUCTURE itab TO FIELD-SYMBOL(<f_id>).
          IF sy-subrc = 0.
            IF <f_gx> = 'X'.
              <f_id> = icon_led_red.
            ELSE.
              <f_id> = icon_led_yellow.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    " ������ƴ��
    READ TABLE lt_aufnr INTO DATA(lv_aufnr) WITH KEY zpcdh = itab-zpcdh BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT lt_aufnr INTO lv_aufnr FROM sy-tabix.
        IF lv_aufnr-zpcdh NE itab-zpcdh.
          EXIT.
        ENDIF.
        CONCATENATE itab-aufnr '/' lv_aufnr-aufnr INTO itab-aufnr.
      ENDLOOP.
      SHIFT itab-aufnr LEFT DELETING LEADING '/'.
    ENDIF.
    READ TABLE lt_log INTO DATA(lv_log) WITH KEY zpcdh = itab-zpcdh BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zxdrq = lv_log-zdate.
    ENDIF.
    READ TABLE lt_ddl INTO DATA(lv_ddl) WITH KEY zpcdh = itab-zpcdh BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zddl_m = lv_ddl-zms.
      itab-zddl_m2 = lv_ddl-zpcsl.
      itab-zpchs = lv_ddl-zpchs.
    ENDIF.
    READ TABLE gt_ysks INTO DATA(gv_ysks) WITH KEY zpcdh = itab-zpcdh zwllx = '��Ʒ' BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zcpysks = gv_ysks-zzk.
    ENDIF.
    READ TABLE gt_ysks INTO gv_ysks WITH KEY zpcdh = itab-zpcdh zwllx = '���Ʒ' BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zbcpysks = gv_ysks-zzk.
    ENDIF.
    LOOP AT gt_gx.
      ASSIGN COMPONENT |{ gt_gx-field }_id| OF STRUCTURE itab TO <f_id>.
      IF sy-subrc = 0.
        READ TABLE lt_gxmc INTO DATA(lv_gxmc) WITH KEY zpcdh = itab-zpcdh zgxmc = gt_gx-text BINARY SEARCH.
        IF sy-subrc = 0.
          <f_id> = icon_led_green.
        ENDIF.
      ENDIF.
    ENDLOOP.
    " ��峤����ƴ��
    LOOP AT zwbcms_field .
      READ TABLE outtab001 WITH KEY matnr = itab-matnr atnam = zwbcms_field BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE itab-zwbcms '_' outtab001-atwrt INTO itab-zwbcms.
      ENDIF.
    ENDLOOP.
    SHIFT itab-zwbcms LEFT DELETING LEADING '_'.
    IF itab-zwbcms IS INITIAL.
      " ��Ĭ���峤����ƴ��
      LOOP AT zdbcms_field .
        READ TABLE outtab001 WITH KEY matnr = itab-matnr atnam = zdbcms_field BINARY SEARCH.
        IF sy-subrc EQ 0.
          CONCATENATE itab-zwbcms '_' outtab001-atwrt INTO itab-zwbcms.
        ENDIF.
      ENDLOOP.
      SHIFT itab-zwbcms LEFT DELETING LEADING '_'.
    ENDIF.
    " �ڰ峤����ƴ��
    LOOP AT znbcms_field .
      READ TABLE outtab001 WITH KEY matnr = itab-matnr atnam = znbcms_field BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE itab-znbcms '_' outtab001-atwrt INTO itab-znbcms.
      ENDIF.
    ENDLOOP.
    SHIFT itab-znbcms LEFT DELETING LEADING '_'.
    READ TABLE gt_bzjs WITH KEY zpcdh = itab-zpcdh BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zpcbzjs = gt_bzjs-zpcbzjs.
    ENDIF.
    READ TABLE gt_pcks WITH KEY zpcdh = itab-zpcdh BINARY SEARCH.
    IF sy-subrc = 0.
      itab-zzks = gt_pcks-zbxks.
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
'ZLJKP_ID '  '��ƽ         '  '        '  '        ',
'ZLJTC_ID '  '����         '  '        '  '        ',
'ZLJZW_ID '  '����         '  '        '  '        ',
'ZLJFH_ID '  '����         '  '        '  '        ',
'ZLJKC_ID '  '����         '  '        '  '        ',
'ZLJZZ_ID '  '��װ         '  '        '  '        ',
'WERKS    '  '����         '  'ZTPP_270'  'WERKS   ',
'POSID    '  '��Ŀ���     '  'ZTPP_270'  'POSID   ',
'ZPCDH    '  '�Ų�����     '  'ZTPP_270'  'ZPCDH   ',
'AUFNR    '  '������       '  'ZTPP_270'  'AUFNR   ',
'ZXDRQ    '  '�µ�����     '  'ZTPP_270'  'ZXDRQ   ',
'ZBX      '  '����         '  'ZTPP_270'  'ZBX     ',
'MATNR    '  '���ϱ���     '  'ZTPP_270'  'MATNR   ',
'ZWBCMS   '  '��峤����   '  'ZTPP_270'  'ZWBCMS  ',
'ZNBCMS   '  '�ڰ峤����   '  'ZTPP_270'  'ZNBCMS  ',
'ZDDL_M   '  '������(M)    '  'ZTPP_270'  'ZDDL_M  ',
'ZDDL_M2  ' '������(M2)    '  'ZTPP_270'  'ZDDL_M2 ',
'ZBXKS    '  '�������Ϳ��� '  'ZTPP_270A'  'ZBXKS  ',
'ZPCHS    '  '�Ų�������   '  'ZTPP_270'  'ZPCHS   ',
'ZSFYP    '  '�Ƿ���Ʒ     '  'ZTPP_270'  'ZSFYP   ',
'ZBCPYSKS '  '���Ʒ���տ���'  'ZTPP_270' 'ZBCPYSKS',
'ZCPYSKS  '  '��Ʒ���տ��� '  'ZTPP_270'  'ZCPYSKS ',
'ZJHRQ    '  '��������     '  'ZTPP_270'  'ZJHRQ   ',
'ZQTRQ    '  '��������     '  'ZTPP_270'  'ZQTRQ   ',
'ZDBH     '  '�����       '  'ZTPP_270'  'ZDBH    ',
'ZJFSX    '  '����˳��     '  'ZTPP_270'  'ZJFSX   ',
'ZPLH     '  '��¯��       '  'ZTPP_270'  'ZPLH    ',
'ZSCLJ    '  '·��         '  'ZTPP_270'  'ZSCLJ   ',
'ZPLKD    '  '���Ͽ��     '  'ZTPP_270'  'ZPLKD   ',
'ZPCBZJS  '  '�Ų���ע����'   'ZTPP_270'  'ZPCBZJS '.

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
    WHEN 'ZPCDH'.
      ls_fldct-hotspot = 'X'.
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
  DATA num TYPE i.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'. "˫��
      CHECK rs_selfield-tabindex <> 0 . "С�����ܼ���ʲô�ĺ���
      READ TABLE itab INTO wa INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'ZPCDH'.
          CHECK wa-zpcdh IS NOT INITIAL.
          SUBMIT zppd201_v4 WITH p_submit = 'X'
                            WITH p_werks = wa-werks
                            WITH p_zpcdh  = wa-zpcdh
                            AND RETURN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'XFGX'. "�·�����
      CLEAR num.
      LOOP AT itab WHERE sel = 'X'.
        num += 1.
      ENDLOOP.
      IF num = 0.
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF num > 1.
        MESSAGE s004 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM xfgx.
    WHEN 'HIDE'. "�����ų̵�
      CLEAR num.
      LOOP AT itab WHERE sel = 'X'.
        num += 1.
      ENDLOOP.
      IF num = 0.
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF num > 1.
        MESSAGE s004 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      PERFORM hide.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh    = 'X'.
ENDFORM.
**********************************************************************
* �·�����
**********************************************************************
FORM xfgx.
  DATA: t_spopli TYPE TABLE OF spopli  WITH HEADER LINE,
        answer   TYPE char1.
  DATA:zgxmc TYPE ztpp_270a-zgxmc.
  DATA:wa_ztpp_270  TYPE ztpp_270,
       lt_ztpp_270a TYPE TABLE OF ztpp_270a WITH HEADER LINE.
  DATA:max_pchh TYPE ztpp_270a-zpchh.

  LOOP AT itab WHERE sel = 'X'.
    MOVE-CORRESPONDING itab TO wa_ztpp_270.
  ENDLOOP.
  LOOP AT gt_gx.
    CLEAR t_spopli.
    ASSIGN COMPONENT |{ gt_gx-field }_id| OF STRUCTURE itab TO FIELD-SYMBOL(<f_id>).
    IF sy-subrc = 0.
      IF <f_id> = icon_led_red.
        t_spopli-varoption = gt_gx-text.
        APPEND t_spopli.
      ELSEIF <f_id> = icon_led_green.
        MESSAGE s000(oo) WITH '��ת��������ϸ���·�' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF t_spopli[] IS INITIAL.
    MESSAGE s000(oo) WITH 'û�п��õĹ���' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
*     CURSORLINE         = 1
*     MARK_FLAG          = ' '
*     MARK_MAX           = 1
*     START_COL          = 5
*     START_ROW          = 5
      textline1          = '��ѡ���򣺲������ظ��·�'
*     TEXTLINE2          = ' '
*     TEXTLINE3          = ' '
      titel              = '�·�����'
*     DISPLAY_ONLY       = ' '
    IMPORTING
      answer             = answer
    TABLES
      t_spopli           = t_spopli
    EXCEPTIONS
      not_enough_answers = 1
      too_much_answers   = 2
      too_much_marks     = 3
      OTHERS             = 4.

  IF answer = 'A' .
    MESSAGE s000(oo) WITH 'ȡ��' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  READ TABLE t_spopli WITH KEY selflag  = 'X'.
  IF sy-subrc = 0.
    zgxmc = t_spopli-varoption.
  ENDIF.


  IF itab-zpch IS INITIAL.
    PERFORM getnum CHANGING wa_ztpp_270-zpch.
    max_pchh = 0.
  ELSE.
    SELECT
     MAX( zpchh ) AS zpchh
    FROM ztpp_270a
    WHERE zpch = @wa_ztpp_270-zpch
    INTO @max_pchh.
  ENDIF.

  LOOP AT itab WHERE zpcdh = wa_ztpp_270-zpcdh.
    CLEAR lt_ztpp_270a.
    MOVE-CORRESPONDING itab TO lt_ztpp_270a.
    lt_ztpp_270a-zpch = wa_ztpp_270-zpch.
    lt_ztpp_270a-zgxmc = zgxmc.
    max_pchh += 10.
    lt_ztpp_270a-zpchh = max_pchh.
    lt_ztpp_270a-zgxxfrq = sy-datlo.
    lt_ztpp_270a-zgxxfusr = sy-uname.
    READ TABLE gt_gx WITH KEY text = zgxmc. "���������ϵ�  ���������µ� ��Ĭ��ֵ
    IF sy-subrc = 0.
      lt_ztpp_270a-zsdpl = gt_gx-zsdpl.
      lt_ztpp_270a-zxdpl = gt_gx-zxdpl.
    ENDIF.
    lt_ztpp_270a-zjh_m = wa_ztpp_270-zddl_m.
    lt_ztpp_270a-zjh_m2 = wa_ztpp_270-zddl_m2.
    lt_ztpp_270a-zjh_k = wa_ztpp_270-zzks.
    APPEND lt_ztpp_270a.
  ENDLOOP.
  IF itab-zpch IS INITIAL.
    INSERT ztpp_270 FROM wa_ztpp_270.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.
  MODIFY ztpp_270a FROM TABLE lt_ztpp_270a.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s059 WITH '�·�����ɹ�'.
    PERFORM getdata.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s059 WITH '����ʧ��' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
**********************************************************************
*�����ų̺�
**********************************************************************
FORM getnum CHANGING p_num TYPE ze_num.
  DATA:znumber  TYPE ztnums_update-znumber10,
       werks_bs TYPE char20.
  "������������ˮ��
  werks_bs = 'PPPCH' .
  CALL FUNCTION 'ZNUMS_UPDATE'
    EXPORTING
      repid    = sy-repid
*     TCODE    = SY-TCODE
      werks    = werks_bs
      flag     = 'D'
*     INITNUM  =
      weishu   = 4
    IMPORTING
      znumber  = znumber
    EXCEPTIONS
      overflow = 1
      OTHERS   = 2.
  IF znumber IS INITIAL.
    MESSAGE s059 WITH '�����ų̺�ʧ��' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  CONCATENATE sy-datum+2(6) znumber INTO p_num.
ENDFORM.
**********************************************************************
* �����ų̵�
**********************************************************************
FORM hide.
  DATA isgreen TYPE char1.
  DATA:wa_ztpp_270  TYPE ztpp_270.

*  LOOP AT itab WHERE sel = 'X'.
*    LOOP AT gt_gx.
*      ASSIGN COMPONENT |{ gt_gx-field }_id| OF STRUCTURE itab TO FIELD-SYMBOL(<f_id>).
*      IF sy-subrc = 0 AND <f_id> = icon_led_green.
*        isgreen = 'X'.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*  IF isgreen IS INITIAL.
*    MESSAGE s059 WITH '�Ų���' itab-zpcdh 'δ�����ų̣��޷�����' DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
  CLEAR answer.
  PERFORM confirmact(zpubfor