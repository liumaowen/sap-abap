*&---------------------------------------------------------------------*
*& 包含               ZPPD201_V4_9300_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9300 OUTPUT.
  SET PF-STATUS 'STA9300'.
  CASE mctype.
    WHEN 'M'.
      SET TITLEBAR 'TIT9300' WITH '洁净门'.
    WHEN 'C'.
      SET TITLEBAR 'TIT9300' WITH '洁净窗'.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'MC1'.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
  DATA:BEGIN OF reqtab OCCURS 0,
         field TYPE char10,
       END OF reqtab.
  DATA:curfield TYPE char30.

  reqtab[] = VALUE #( ( field = 'ZJHSJ' )
                      ( field = 'ZBZFS' )
                      ( field = 'ZQY'   )
                      ( field = 'ZBHHSF' )
                      ( field = 'ZZYYQ' )
                      ( field = 'ZSCDW1' )
                    ).
  LOOP AT reqtab.
    CLEAR curfield.
    ASSIGN COMPONENT reqtab-field OF STRUCTURE wa_mczd TO FIELD-SYMBOL(<fs_value>).
    IF sy-subrc = 0 AND <fs_value> IS INITIAL.
      curfield = 'WA_MCZD-' && reqtab-field.
      SET CURSOR FIELD curfield. "必填的时候光标
      EXIT.
    ENDIF.
  ENDLOOP.
*生产单位
  PERFORM setzscdw1_mc.
  PERFORM initlongtext USING container_mcbz editor_mcbz 'CONT_MCPCBZ' 65.
  PERFORM tab2screen USING editor_mcbz 'MCBZ'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form SETZSCDW1_MC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*生产单位取值
*&---------------------------------------------------------------------*
FORM setzscdw1_mc .
  SELECT crhd~arbpl AS zscdw1,
         crtx~ktext
    INTO TABLE @DATA(it_zscdw1)
    FROM crhd INNER JOIN crtx ON crhd~objty = crtx~objty
                             AND crhd~objid = crtx~objid
    WHERE crhd~werks = @p_werks
     AND  crhd~arbpl IN ( '30026GZM','30026LHJ','30026SCX' )
     AND  crtx~spras = @sy-langu.
  CHECK sy-subrc = 0.
  SORT it_zscdw1 BY zscdw1.
  PERFORM itabtolist(zpubform) TABLES it_zscdw1 USING 'WA_MCZD-ZSCDW1'.
  CASE mctype.
    WHEN 'M'.

    WHEN 'C'.
      READ TABLE it_zscdw1 INTO DATA(is_zscdw1) WITH KEY zscdw1 = '30026SCX'.
      IF sy-subrc = 0.
        wa_mczd-zscdw1 = is_zscdw1-zscdw1.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.

  ok_code = sy-ucomm.
  PERFORM screen2tab USING: editor_mcbz 'MCBZ'.
  PERFORM tab2screen USING: editor_mcbz 'MCBZ'.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'QX' OR 'MCCANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'MCCONF'.
      PERFORM mcconf.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.

FORM mcconf.
  DATA:BEGIN OF lt_zpcdh OCCURS 0,
         zpcdh TYPE ztpp_205-zpcdh,
       END OF lt_zpcdh.
  DATA:mc_type TYPE bapi_mtype,
       mc_msg  TYPE bapi_msg.

  IF wa_mczd IS INITIAL.
    MESSAGE s004 WITH '请填写完再确认！！' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  LOOP AT reqtab.
    ASSIGN COMPONENT reqtab-field OF STRUCTURE wa_mczd TO FIELD-SYMBOL(<fs_value>).
    IF sy-subrc = 0 AND <fs_value> IS INITIAL.
      MESSAGE s004 WITH '该字段必填！！' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.
  " 填写运费后的判断
  REFRESH:returnmsg.
  IF wa_mczd-zmcyfp IS NOT INITIAL.
    LOOP AT it_vbeln_p INTO it_vbeln.
      CLEAR:mc_type,mc_msg.
      PERFORM empty001 USING    it_vbeln-matnr
                       CHANGING mc_type mc_msg.
      IF mc_type = 'E'.
        mc_msg = '物料号：' && it_vbeln-matnr && mc_msg.
        PERFORM inmsg(zpubform) TABLES returnmsg USING 'ZMSG_GP' 'E' '059' mc_msg '' '' ''.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF returnmsg[] IS NOT INITIAL.
    PERFORM showmsg(zpubform) TABLES returnmsg.
    EXIT.
  ENDIF.
  ispl = 'X'.
  LOOP AT it_vbeln_p INTO it_vbeln.
    CLEAR:it_pcd[],it_pcd.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = '第' && sy-tabix && '个排产单创建中...'.
    PERFORM crtpcd_v4 USING it_vbeln ispl.
    MOVE-CORRESPONDING wa_mczd TO wa_ggzd.
    IF mctype = 'M'.
      PERFORM alljsxd.
    ENDIF.
    IF it_pcd[] IS INITIAL.
      CONTINUE.
    ENDIF.
    LOOP AT it_pcd.
      it_pcd-zmcyfp = wa_mczd-zmcyfp.
      IF it_pcd-zmcyfp IS NOT INITIAL.
        PERFORM calzyf USING    wa_ggzd-matnr it_pcd-zmcyfp
                       CHANGING it_pcd-zyf.
      ENDIF.
      MODIFY it_pcd TRANSPORTING zmcyfp zyf.
    ENDLOOP.
    CASE pclx.
      WHEN 'JSXD'.
        PERFORM savepcd_jsxd.
      WHEN 'JGMX'.
        PERFORM savepcd_jgmx.
      WHEN 'WHT'.
        PERFORM savepcd_wht.
    ENDCASE.
*保存冲孔要求
    PERFORM saveck.
    PERFORM savemcbz.
    it_vbeln-zpcdh =  wa_ggzd-zpcdh.
    MODIFY it_vbeln_p FROM it_vbeln TRANSPORTING zpcdh.
    lt_zpcdh-zpcdh = wa_ggzd-zpcdh.
    APPEND lt_zpcdh.
    lock 'X' wa_ggzd-zpcdh.
  ENDLOOP.
  PERFORM getdata.
  LOOP AT it_vbeln_p.
    READ TABLE it_vbeln WITH KEY vbeln = it_vbeln_p-vbeln posnr = it_vbeln_p-posnr.
    IF sy-subrc = 0.
      it_vbeln-zpcdh = it_vbeln_p-zpcdh.
      MODIFY it_vbeln INDEX sy-tabix TRANSPORTING zpcdh.
    ENDIF.
  ENDLOOP.
  DATA(dh_l) = lines( lt_zpcdh[] ).
  DATA(s_msg) = '成功创建' && dh_l && '个排产单'.
  MESSAGE s004 WITH s_msg.
  LEAVE TO SCREEN 0.
ENDFORM.
**********************************************************************
*&洁净门全引技术详单明细
**********************************************************************
FORM alljsxd.
  IF pclx = 'JSXD'.
    REFRESH it_jsxd.
    PERFORM getjsxd.
    DELETE it_jsxd WHERE zks_bcpc = 0.
    CASE wa_ggzd-zjglx.
      WHEN 'B01'.
        DELETE it_jsxd WHERE type NE '板材'.
      WHEN 'B02'.
        DELETE it_jsxd WHERE type = '板材'.
    ENDCASE.

    IF it_jsxd[] IS NOT INITIAL.
      PERFORM savemjsxd.
    ENDIF.
  ELSEIF pclx = 'JGMX'.
    REFRESH it_jgmx.
    PERFORM getjgmx.
    IF it_jgmx[] IS NOT INITIAL.
      PERFORM savejgmx.
    ENDIF.
  ENDIF.
ENDFORM.

FORM savemjsxd.
  CLEAR:it_knumv[],zpcdhhmaxtop.
  LOOP AT it_jsxd.
    it_jsxd-chbox = 'X'.
    MODIFY it_jsxd.
  ENDLOOP.
  CLEAR: it_jsxd.

  CLEAR vbap.
  SELECT SINGLE *
    FROM vbap
    WHERE vbeln = wa_205-vbeln
    AND   posnr = wa_205-posnr.
  PERFORM delqfw(zpubform) CHANGING vbap-zxishu.
  CLEAR:it_knumv[],it_knumv.
  it_knumv-vbeln = wa_205-vbeln.
  it_knumv-posnr = wa_205-posnr.
  COLLECT it_knumv.
*取价格
  PERFORM getsdjg.
  CLEAR:it_konv.
  READ TABLE it_knumv WITH KEY vbeln = wa_205-vbeln
                               posnr = wa_205-posnr
                               BINARY SEARCH.
  IF sy-subrc EQ 0.
    READ TABLE it_konv WITH KEY knumv = it_knumv-knumv
                                kposn = it_knumv-posnr
                                BINARY SEARCH.
    IF it_konv-kpein GT 0.
      " 单价要三位小数  04.03.2023 11:21:09 by kkw
      it_konv-zpr0 = it_konv-kbetr / it_konv-kpein.
    ENDIF.
  ENDIF.
  CLEAR:mara.
  SELECT SINGLE *
    FROM mara
    WHERE matnr = wa_ggzd-matnr.
  "获取vbap中的zyfft
  PERFORM getzyfft USING    wa_ggzd-vbeln
                            wa_ggzd-posnr
                   CHANGING zyfft.
********ADD BY DONGPZ BEGIN AT 30.12.2022 09:11:29
*同一技术详单行多次引用后，累加块数而不是重新计算
  SORT it_pcd BY zjsxdid zjsxddid.
  IF gcbs = 'A'.
    SORT it_jsxd BY zdtmc zlmzx zcc DESCENDING.
  ENDIF.

  LOOP AT it_jsxd WHERE chbox = 'X'.
    CLEAR it_pcd.
    MOVE-CORRESPONDING it_jsxd TO it_pcd.
    ADD 10 TO zpcdhhmaxtop.

    it_pcd-zpcdhh = zpcdhhmaxtop.
    it_pcd-chbox = ''.
    it_pcd-zmxbz = it_jsxd-zdremark.
    it_pcd-zpr0 = it_konv-zpr0.
    it_pcd-zjjlx = it_jsxd-type.
    it_pcd-zfahuo = it_jsxd-zfahuo.
    CASE mclx.
      WHEN 'A' OR 'B' OR 'C' OR 'D'.
        CLEAR:jssl,zcdmm.
        zcdmm = it_pcd-zcd.
        PERFORM transcd USING wa_ggzd-matnr CHANGING zcdmm.
        jssl = zcdmm / 1000 * it_pcd-zxishu.
        it_pcd-zjssl1 = it_pcd-zyyks * jssl.
        it_pcd-je = it_pcd-zpr0 * it_pcd-zjssl1.
    ENDCASE.
    it_pcd-zjssl = it_pcd-zjssl1.
    it_pcd-zpcsl = it_pcd-zjssl1.
    PERFORM delqfw(zpubform) CHANGING it_pcd-zjssl.
    it_pcd-zks = it_pcd-zyyks.
*去除小数位
    CLEAR:cdstr,it_knumv,c_flag.
    cdstr =  it_pcd-zcd.
    cdstrz =  it_pcd-zbckd.
    PERFORM dealcc CHANGING cdstr.
    PERFORM dealcc CHANGING cdstrz.
    CONCATENATE cdstr '*' cdstrz INTO it_pcd-zcc.
    it_pcd-matkl = mara-matkl.
    READ TABLE it_t023t WITH KEY matkl = it_pcd-matkl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_pcd-t023t = it_t023t-wgbez.
    ENDIF.
    " 单体名称和立面轴线的重新赋值  2024-05-15 09:18:36 by lmw
    IF gcbs = 'A'.
      IF it_jsxd-zdtmc NE it_jsxd-zqymc AND it_jsxd-zqymc IS NOT INITIAL.
        it_pcd-zdtmc = it_jsxd-zdtmc && ',' && it_jsxd-zqymc.
        CONDENSE it_pcd-zdtmc NO-GAPS.
      ENDIF.
      IF it_jsxd-zlmzx NE it_jsxd-zbwmc AND it_jsxd-zbwmc IS NOT INITIAL.
        it_pcd-zlmzx = it_jsxd-zlmzx && ',' && it_jsxd-zbwmc.
        CONDENSE it_pcd-zlmzx NO-GAPS.
      ENDIF.
    ENDIF.
*& kkw27006 排产单新增原合同单价获取逻辑
    it_pcd-zyhtdj = wa_ggzd-zyhtdj.
    it_pcd-zyhtje = wa_ggzd-zyhtdj * it_pcd-zpcsl.
    APPEND it_pcd.
  ENDLOOP.
********ADD BY DONGPZ END AT 30.12.2022 09:11:29
  PERFORM sortpcd USING ''.
*& kkw 针对钢品工厂单独排序，赋值排产单行号
  IF gcbs = 'A' AND p_xg NE 'X'.
    CLEAR: zpcdhhsort,flag.
    PERFORM autosort TABLES it_pcd USING it_vbeln-zzl1 CHANGING flag.
    IF flag = 'S'.
      LOOP AT it_pcd.
        CLEAR it_pcd-zpcdhh.
        ADD 10 TO zpcdhhsort.
        it_pcd-zpcdhh = zpcdhhsort.
        MODIFY it_pcd TRANSPORTING zpcdhh.
      ENDLOOP.
    ENDIF.
  ENDIF.
*计算总块数ZKS总米数ZMS总数量ZJSSL
  CLEAR:wa_ggzd-zzks,wa_ggzd-zzms,wa_ggzd-zzsl,
  wa_ggzd-zzje.
  LOOP AT it_pcd.
    wa_ggzd-zzks = wa_ggzd-zzks + it_pcd-zks.
    wa_ggzd-zzms = wa_ggzd-zzms + it_pcd-zms.
    wa_ggzd-zzsl = wa_ggzd-zzsl + it_pcd-zjssl1.
    wa_ggzd-zzje = wa_ggzd-zzje + it_pcd-je.
  ENDLOOP.
  PERFORM cal_kd_jj_xg USING ''."更新抬头数据
ENDFORM.

FORM savejgmx.
  CLEAR:zpcdhhmaxtop,it_knumv[],it_knumv.
  LOOP AT it_jgmx WHERE zysvbeln = wa_205-vbeln AND zysposnr = wa_205-posnr.
    it_jgmx-chbox = 'X'.
    MODIFY it_jgmx.
  ENDLOOP.
*取价格
  it_knumv-vbeln = wa_205-vbeln.
  it_knumv-posnr = wa_205-posnr.
  COLLECT it_knumv.
*获取单价
  PERFORM getsdjg.
  CLEAR:it_konv.
  READ TABLE it_knumv WITH KEY vbeln = wa_ggzd-vbeln
                               posnr = wa_205-posnr
                               BINARY SEARCH.
  IF sy-subrc EQ 0.
    READ TABLE it_konv WITH KEY knumv = it_knumv-knumv
                                kposn = it_knumv-posnr
                                BINARY SEARCH.
    IF it_konv-kpein GT 0.
      it_konv-zpr0 = it_konv-kbetr / it_konv-kpein.
    ENDIF.
  ENDIF.

  LOOP AT it_jgmx WHERE chbox = 'X' .
    CLEAR it_pcd.
    MOVE-CORRESPONDING it_jgmx TO it_pcd.
    ADD 10 TO zpcdhhmaxtop.
    it_pcd-zpcdhh = zpcdhhmaxtop.
    it_pcd-zpcsl = it_jgmx-zsl_wpc.
    it_pcd-zypcsl = it_jgmx-zsl_ypc.
    it_pcd-zwpcl = it_jgmx-zsl_wpc.
    IF it_jgmx-zmgd IS NOT INITIAL.
      it_pcd-zmkg = it_jgmx-zmgd.
    ELSE.
      it_pcd-zmkg = it_jgmx-zmdgd.
    ENDIF.
    IF it_jgmx-zmkd IS NOT INITIAL.
      it_pcd-zmkk = it_jgmx-zmkd.
    ELSE.
      it_pcd-zmkk = it_jgmx-zmdkd.
    ENDIF.
    it_pcd-chbox = ''.
*001字段赋值及字段对照
    LOOP AT outtab001.
      ASSIGN COMPONENT outtab001-atnam OF STRUCTURE it_pcd TO <fs>.
      IF sy-subrc EQ 0.
        <fs> = outtab001-atwrt.
      ENDIF.
    ENDLOOP.
    IF it_pcd-zptys IS INITIAL.
      READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr
                                    atnam = 'ZMSPTYS'
                                    BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_pcd-zptys = outtab001-atwrt.
      ENDIF.
    ENDIF.
    IF it_pcd-zptxg IS INITIAL.
      READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr
                                    atnam = 'ZMSPTXG'
                                    BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_pcd-zptxg = outtab001-atwrt.
      ENDIF.
    ENDIF.
    READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr
                                  atnam = 'ZMSCYB'
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_pcd-zscyb = outtab001-atwrt.
    ENDIF.
    IF it_pcd-zscyb IS INITIAL.
      READ TABLE outtab001 WITH KEY matnr = wa_ggzd-matnr
                                    atnam = 'ZSCYB'
                                    BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_pcd-zscyb = outtab001-atwrt.
      ENDIF.
    ENDIF.
    it_pcd-zks = it_jgmx-zamount.
    it_pcd-zjssl = it_pcd-zks.
    it_pcd-zjssl1 = it_pcd-zks.
    it_pcd-zpcsl = it_pcd-zjssl1.
    it_pcd-zzl1 = wa_ggzd-zzl1.
    CASE mclx.
      WHEN 'A'.
        it_pcd-zcd = it_pcd-zmkg.
        it_pcd-zbckd = it_pcd-zmkk.
      WHEN 'B'.
        it_pcd-zcd = it_pcd-zscgd.
        it_pcd-zbckd = it_pcd-zsckd.
    ENDCASE.
    it_pcd-zms = it_pcd-zks * it_pcd-zcd / 1000.
    it_pcd-zpingfangshu =  it_pcd-zbckd * it_pcd-zms / 1000.
    it_pcd-zpr0 = it_konv-zpr0.
    it_pcd-je = it_pcd-zpr0 * it_pcd-zjssl1.
*& kkw27006 排产单新增原合同单价获取逻辑
    it_pcd-zyhtdj = wa_ggzd-zyhtdj.
    it_pcd-zyhtje = wa_ggzd-zyhtdj * it_pcd-zpcsl.
    APPEND it_pcd.
  ENDLOOP.
  DELETE it_pcd WHERE zpcdhh IS INITIAL.
*计算总块数ZKS总米数ZMS总数量ZJSSL
  CLEAR:wa_ggzd-zzks,wa_ggzd-zzms,wa_ggzd-zzsl,
  wa_ggzd-zzje.
  LOOP AT it_pcd.
    wa_ggzd-zzks = wa_ggzd-zzks + it_pcd-zks.
    wa_ggzd-zzms = wa_ggzd-zzms + it_pcd-zms.
    wa_ggzd-zzsl = wa_ggzd-zzsl + it_pcd-zjssl1.
    wa_ggzd-zzje = wa_ggzd-zzje + it_pcd-je.
  ENDLOOP.
ENDFORM.

**********************************************************************
*&保存排产备注
**********************************************************************
FORM savemcbz.
*将当前停留文本更新
  PERFORM screen2tab USING: editor_mcbz 'MCBZ'.
  CLEAR:t_text[].
  LOOP AT it_text WHERE tdid = 'MCBZ'.
    CLEAR:t_text.
    t_text-text = it_text-text.
    APPEND t_text.
  ENDLOOP.
  CALL FUNCTION 'ZFM_DEALLONGTEXT'
    EXPORTING
      intype = 'I'
      tdid   = 'PCBZ'
      sapno  = wa_ggzd-zpcdh
      sapmk  = 'PCD'
    TABLES
      t_text = t_text.
ENDFORM.
**********************************************************************
*&填写运费后判断无聊属性值是否为空
**********************************************************************
FORM empty001 USING    p_matnr TYPE matnr
              CHANGING p_type TYPE bapi_mtype
                       p_msg  TYPE bapi_msg.
  DATA:lv_kd_m TYPE atwrt,
       lv_gd_m TYPE atwrt,
       lv_kd_c TYPE atwrt,
       lv_gd_c TYPE atwrt.
  CASE mctype.
    WHEN 'M'.
      READ TABLE outtab001 WITH KEY matnr = p_matnr atnam = 'ZMKK' BINARY SEARCH.
      IF sy-subrc = 0.
        lv_kd_m = outtab001-atwrt.
      ENDIF.
      READ TABLE outtab001 WITH KEY matnr = p_matnr atnam = 'ZMKG' BINARY SEARCH.
      IF sy-subrc = 0.
        lv_gd_m = outtab001-atwrt.
      ENDIF.
      IF lv_kd_m IS INITIAL.
        p_type = 'E'.
        p_msg = ' 门框宽为空!'.
      ENDIF.
      IF lv_gd_m IS INITIAL.
        p_type = 'E'.
        CONCATENATE p_msg ' 门框高为空!' INTO p_msg.
      ENDIF.
    WHEN 'C'.
      READ TABLE outtab001 WITH KEY matnr = p_matnr atnam = 'ZSCKD' BINARY SEARCH.
      IF sy-subrc = 0.
        lv_kd_c = outtab001-atwrt.
      ELSE.
        READ TABLE outtab001 WITH KEY matnr = p_matnr atnam = 'ZDKKD' BINARY SEARCH.
        IF sy-subrc = 0.
          lv_kd_c = outtab001-atwrt.
        ENDIF.
      ENDIF.
      READ TABLE outtab001 WITH KEY matnr = p_matnr atnam = 'ZSCGD' BINARY SEARCH.
      IF sy-subrc = 0.
        lv_gd_c = outtab001-atwrt.
      ELSE.
        READ TABLE outtab001 WITH KEY matnr = p_matnr atnam = 'ZDKGD' BINARY SEARCH.
        IF sy-subrc = 0.
          lv_gd_c = outtab001-atwrt.
        ENDIF.
      ENDIF.
      IF lv_kd_c IS INITIAL.
        p_type = 'E'.
        p_msg = ' 视窗宽度为空!'.
      ENDIF.
      IF lv_gd_c IS INITIAL.
        p_type = 'E'.
        CONCATENATE p_msg ' 视窗高度为空!' INTO p_msg.
      ENDIF.
  ENDCASE.
ENDFORM.
**********************************************************************
*&填写平方运费后计算樘运费
**********************************************************************
FORM calzyf   USING    p_matnr TYPE matnr
                       p_zmcyfp TYPE ztpp_205a-zmcyfp
              CHANGING p_zyf   TYPE ztpp_205a-zyf.
  DATA:lv_kd_c TYPE atwrt,
       lv_gd_c TYPE atwrt.
  CASE mctype.
    WHEN 'M'.
      READ TABLE outtab001 INTO DATA(mkk) WITH KEY matnr = p_matnr atnam = 'ZMKK' BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE outtab001 INTO DATA(mkg) WITH KEY matnr = p_matnr atnam = 'ZMKG' BINARY SEARCH.
        IF sy-subrc = 0.
          p_zyf = p_zmcyfp * ( mkk-atwrt / 1000 ) * ( mkg-atwrt / 1000 ).
        ENDIF.
      ENDIF.
    WHEN 'C'.
      READ TABLE outtab001 INTO DATA(sckd) WITH KEY matnr = p_matnr atnam = 'ZSCKD' BINARY SEARCH.
      IF sy-subrc = 0.
        lv_kd_c = sckd-atwrt.
      ELSE.
        READ TABLE outtab001 INTO sckd WITH KEY matnr = p_matnr atnam = 'ZDKKD' BINARY SEARCH.
        IF sy-subrc = 0.
          lv_kd_c = sckd-atwrt.
        ENDIF.
      ENDIF.
      READ TABLE outtab001 INTO DATA(scgd) WITH KEY matnr = p_matnr atnam = 'ZSCGD' BINARY SEARCH.
      IF sy-subrc = 0.
        lv_gd_