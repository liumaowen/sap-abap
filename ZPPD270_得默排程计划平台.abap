*&---------------------------------------------------------------------*
*& Report ZPPD270
*&---------------------------------------------------------------------*
*& ��Ĭ�ų̼ƻ�ƽ̨
*&---------------------------------------------------------------------*
REPORT zppd270.
TABLES:sscrfields.

CONSTANTS:tcode1 TYPE sy-tcode VALUE 'ZPP271',
          tcode2 TYPE sy-tcode VALUE 'ZPP272',
          tcode3 TYPE sy-tcode VALUE 'ZPP273',
          tcode4 TYPE sy-tcode VALUE 'ZPP274',
          tcode5 TYPE sy-tcode VALUE 'ZPP275',
          tcode6 TYPE sy-tcode VALUE 'ZPP276',
          tcode7 TYPE sy-tcode VALUE 'ZPP277'.
FIELD-SYMBOLS:<fs>      TYPE any.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text001.
  SELECTION-SCREEN:
  PUSHBUTTON /1(55) button1 USER-COMMAND tcode1,
  SKIP,
  PUSHBUTTON /1(55) button2 USER-COMMAND tcode2,
  SKIP,
  PUSHBUTTON /1(55) button3 USER-COMMAND tcode3,
  SKIP,
  PUSHBUTTON /1(55) button4 USER-COMMAND tcode4,
  SKIP,
  PUSHBUTTON /1(55) button5 USER-COMMAND tcode5,
  SKIP,
  PUSHBUTTON /1(55) button6 USER-COMMAND tcode6,
  SKIP,
  PUSHBUTTON /1(55) button7 USER-COMMAND tcode7.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  text001 = '����ѡ��'.
  button1 = |��Ĭ�ų̼ƻ�-����({ tcode1 })|.
  button2 = |��Ĭ�ų̼ƻ�-��ƽ({ tcode2 })|.
  button3 = |��Ĭ�ų̼ƻ�-����({ tcode3 })|.
  button4 = |��Ĭ�ų̼ƻ�-����({ tcode4 })|.
  button5 = |��Ĭ�ų̼ƻ�-����({ tcode5 })|.
  button6 = |��Ĭ�ų̼ƻ�-����({ tcode6 })|.
  button7 = |��Ĭ�ų̼ƻ�-��װ({ tcode7 })|.

AT SELECTION-SCREEN.
  CHECK sscrfields-ucomm IS NOT INITIAL.
  ASSIGN (sscrfields-ucomm) TO <fs>.