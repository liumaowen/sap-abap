REPORT zrpp201c.

INCLUDE ZRPP201C_TOP.
INCLUDE ZRPP201C_SCR.
INCLUDE ZRPP201C_F01.

INITIALIZATION.
  PERFORM frm_init.

AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*& ��Ļ����
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM frm_screen.

*&---------------------------------------------------------------------*
*& ��ʼ
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM frm_check_input.
  PERFORM frm_get_data.
*  PERFORM frm_get_global.
*  PERFORM frm_make_data.

*&---------------------------------------------------------------------*
*& ���
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM frm_sh