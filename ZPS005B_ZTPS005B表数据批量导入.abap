*&---------------------------------------------------------------------*
*& Report ZPS005B
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zps005b.
TABLES: sscrfields,ztps005b.
INCLUDE rmcs0f0m.
TYPES: BEGIN OF ty_out,
         zdh   TYPE ztps005b-zdh,
         zxh   TYPE ztps005b-zxh,
         zfphm TYPE ztps005b-zfphm,
         zfpje TYPE ztps005b-zfpje,
         zfprq TYPE ztps005b-zfprq,
         zfplx TYPE ztps005b-zfplx,
         zbz   TYPE ztps005b-zbz,
       END OF ty_out.
DATA: fieldcat  TYPE slis_t_fieldcat_alv,
      z005b     TYPE STANDARD TABLE OF ztps005b WITH HEADER LINE,
      it_upload TYPE TABLE OF ty_out WITH HEADER LINE,
      functxt   TYPE smp_dyntxt.
TYPES: BEGIN OF ty_select,
         month TYPE fti_month_year,
       END OF ty_select.
DATA: wa_select TYPE ty_select.
SELECTION-SCREEN: BEGIN OF BLOCK sandeep WITH FRAME.
  PARAMETERS: p_bukrs LIKE zvmmpo-bukrs DEFAULT '3000'.
SELECTION-SCREEN:END OF BLOCK sandeep.

SELECTION-SCREEN FUNCTION KEY:1.

INITIALIZATION.
  %_p_bukrs_%_app_%-text = '公司'.
  functxt = '@14@导出模板'.
  sscrfields-functxt_01 = functxt.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN INTO DATA(wa_screen).
    IF wa_screen-group1 = 'BL1'.
      wa_screen-input = '0'.
    ENDIF.
    MODIFY SCREEN FROM wa_screen.
  ENDLOOP.


AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      REFRESH fieldcat.
      PERFORM init_fieldcat(zpubform) TABLES fieldcat
        USING:
              '' '付款单号' '' '' '' '',
              '' 'OA流程子表序号' '' '' '' '',
              '' '发票号码' '' '' '' '',
              '' '发票金额' '' '' '' '',
              '' '发票日期' '' '' '' '',
              '' '发票类型' '' '' '' '',
              '' '备注' '' '' '' ''.
      PERFORM itabstructoclip(zpubform) USING fieldcat '' ''.
  ENDCASE.

START-OF-SELECTION.
  PERFORM cliptoitab(zpubform) TABLES it_upload.
  LOOP AT it_upload.
    IF it_upload-zdh IS INITIAL OR it_upload-zfphm IS INITIAL.
      DELETE it_upload.
    ENDIF.
    CLEAR it_upload.
  ENDLOOP.
  PERFORM alvshow.

FORM alvshow.
  REFRESH fieldcat.
  PERFORM init_fieldcat(zpubform) TABLES fieldcat USING:
        'ZDH' '项目外包服务采购付款单号' '' '' '' '',
        'ZXH' 'OA流程子表序号' '' '' '' '',
        'ZFPHM' '发票号码' '' '' '' '',
        'ZFPJE' '发票金额' '' '' '' '',
        'ZFPRQ' '发票日期' '' '' '' '',
        'ZFPLX' '发票类型' '' '' '' '',
        'ZBZ' '备注' '' '' '' ''.
  PERFORM alvfm(zpubform) TABLES it_upload fieldcat USING 'X' ''.
ENDFORM.

FORM set_status USING rt_extab TYPE slis_t_extab.
  DATA: lt_exfcode TYPE TABLE OF sy-ucomm.
  SET PF-STATUS 'STANDARD1' EXCLUDING lt_exfcode.
ENDFORM. "set_status


FORM user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&BUT80'.
      PERFORM but80.
  ENDCASE.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh = 'X'.
ENDFORM.

FORM but80.
  IF it_upload[] IS INITIAL.
    MESSAGE '没有数据' TYPE 'E'.
    RETURN.
  ENDIF.
  LOOP AT it_upload.
    MOVE-CORRESPONDING it_upload TO z005b.
    APPEND z005b.
  ENDLOOP.
  IF z005b[] IS NOT INITIAL.
    MODIFY ztps005b FROM z005b.
    IF sy-subrc = 0.
      MESSAGE '更新成功' TYPE '