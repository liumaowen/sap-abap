*&---------------------------------------------------------------------*
*& Report ZSDR022
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr022.
DATA:fieldcat TYPE slis_t_fieldcat_alv.
TYPES: BEGIN OF ty_out,
         kunnr TYPE kna1-kunnr,
         posid TYPE zefi_psid,
         kyje  TYPE ze_zhxje, "可用金额
       END OF ty_out.
DATA: itab   TYPE TABLE OF ty_out,
      ls_tab LIKE LINE OF itab.
DATA: kyje TYPE ze_kyje.

SELECTION-SCREEN BEGIN OF BLOCK ss1 WITH FRAME TITLE t01.
  PARAMETERS:s_kunnr LIKE ztsd201-kunnr OBLIGATORY,
             s_ps    LIKE cobl-ps_posid OBLIGATORY MATCHCODE OBJECT zf4_posid,
             s_vbeln LIKE lips-vbeln.
SELECTION-SCREEN END OF BLOCK ss1.

INITIALIZATION.
  %_s_kunnr_%_app_%-text = '客户编号'.
  %_s_vbeln_%_app_%-text = '出库单'.
  %_s_ps_%_app_%-text = '项目编号'.

START-OF-SELECTION.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM getdata.
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
  ls_tab-kunnr = s_kunnr.
  ls_tab-posid = s_ps.
  CALL FUNCTION 'ZFM_GP_SD_KYJE'
    EXPORTING
      kunnr      = s_kunnr
      ps_psp_pnr = s_ps
      vbeln      = s_vbeln
    IMPORTING
*     RTYPE      =
*     RTMSG      =
      kyje       = kyje.
  ls_tab-kyje = kyje.
  APPEND ls_tab TO itab.
ENDFORM.

FORM alvshow.
  PERFORM init_fieldcat TABLES fieldcat USING:
     'KUNNR' '客户编码' 'X' '' '' ''  '' '',
     'POSID' '项目编号' '' '' '' ''  '' '',
     'KYJE' '可用金额' '' '' '' ''  '' ''.
  PERFORM alvfm(zpubform) TABLES itab fieldcat USING '' 'X'.
ENDFORM.

FORM init_fieldcat TABLES outfieldcat
USING fieldname seltext nozero noout hotspot edit p_tab p_field.
  DATA:wa TYPE slis_fieldcat_alv.
  wa-fieldname = fieldname.
  wa-seltext_l = seltext.
  wa-seltext_m = seltext.
  wa-seltext_s = seltext.
  wa-reptext_ddic = seltext.
  wa-no_out = noout.
  wa-hotspot = hotspot.
  IF edit = 'X'.
    wa-edit = 'X'.
  ENDIF.
*对于数值，若有小数位但却为000，则隐藏
  CASE nozero.
    WHEN 'X'.
      wa-no_zero = nozero.
    WHEN 'Y'.
      wa-no_zero = 'X'.
      wa-qfieldname = 'MEINS'.
  ENDCASE.

  wa-ref_tabname = p_tab.
  wa-ref_fieldname = p_field.
  IF p_field IS INITIAL.
    wa-ref_fieldname = fieldname.
  ENDIF.
  IF p_field = 'N'.
    CLEAR:wa-ref_fieldname.
  ENDIF.
  APPEN