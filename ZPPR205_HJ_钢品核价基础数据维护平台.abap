*&---------------------------------------------------------------------*
*& Report ZPPR205_HJ
*&---------------------------------------------------------------------*
*& ��Ʒ�˼ۻ�������ά��ƽ̨
*&---------------------------------------------------------------------*
REPORT zppr205_hj.

TABLES: sscrfields.
CONSTANTS:btn01 TYPE se16n_tab VALUE 'ZTPP_205G',
          btn02 TYPE se16n_tab VALUE 'ZTPP_205H',
          btn03 TYPE se16n_tab VALUE 'ZTPP_205I',
          btn04 TYPE se16n_tab VALUE 'ZTPP_205J1',
          btn05 TYPE se16n_tab VALUE 'ZTPP_205J2',
          btn06 TYPE se16n_tab VALUE 'ZTPP_205J3',
          btn07 TYPE se16n_tab VALUE 'ZTPP_205J4',
          btn08 TYPE se16n_tab VALUE '',
          btn09 TYPE se16n_tab VALUE '',
          btn10 TYPE se16n_tab VALUE 'ZTPP_205J6',
          btn11 TYPE se16n_tab VALUE 'ZTPP_205J7',
          btn12 TYPE se16n_tab VALUE '',
          btn13 TYPE se16n_tab VALUE 'ZTPP_205J5',
          btn14 TYPE se16n_tab VALUE 'ZTPP_205J8'.
FIELD-SYMBOLS:<fs>      TYPE any.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  SELECTION-SCREEN:
  PUSHBUTTON /2(22) btntxt01 USER-COMMAND btn01.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t2.
  SELECTION-SCREEN:
  PUSHBUTTON /2(22) btntxt02 USER-COMMAND btn02,
  PUSHBUTTON 28(22) btntxt03 USER-COMMAND btn03.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE t3.
  SELECTION-SCREEN:
  PUSHBUTTON /2(22) btntxt04 USER-COMMAND btn04,
  SKIP,
  PUSHBUTTON /2(22) btntxt05 USER-COMMAND btn05,
  SKIP,
  PUSHBUTTON /2(22) btntxt06 USER-COMMAND btn06,
  PUSHBUTTON 28(22) btntxt07 USER-COMMAND btn07,
  PUSHBUTTON 54(22) btntxt08 USER-COMMAND btn08,
  PUSHBUTTON 79(22) btntxt09 USER-COMMAND btn09,
  SKIP,
  PUSHBUTTON /2(22) btntxt10 USER-COMMAND btn10,
  PUSHBUTTON 28(22) btntxt11 USER-COMMAND btn11,
  PUSHBUTTON 54(22) btntxt12 USER-COMMAND btn12,
  PUSHBUTTON 79(22) btntxt13 USER-COMMAND btn13,
  SKIP,
  PUSHBUTTON /2(22) btntxt14 USER-COMMAND btn14.
SELECTION-SCREEN END OF BLOCK b3.


INITIALIZATION.
  t1 = 'ѹ�Ͱ��������ά��'.
  btntxt01 = |ѹ�Ͱ�˼����ñ�({ btn01 })|.
  t2 = '�������������ά��'.
  btntxt02 = |������˼����ñ�({ btn02 })|.
  btntxt03 = |�������п�ּ����ñ�({ btn03 })|.
  t3 = '���ʻ�������ά��'.
  btntxt04 = |�����Ӽ����ñ�({ btn04 })|.
  btntxt05 = |о�����ñ�({ btn05 })|.
  btntxt06 = |���ʶ����긨�����ñ�({ btn06 })|.
  btntxt07 = |���ʶ����꽺ˮ-��Ĥ���ñ�({ btn07 })|.
  btntxt08 = |Ļǽ�������ñ�({ btn08 })|.
  btntxt09 = |�����������ñ�({ btn09 })|.
  btntxt10 = |���ʶ�������������ñ�({ btn10 })|.
  btntxt11 = |����������������ð�({ btn11 })|.
  btntxt12 = |����Ļ��������������ñ�({ btn12 })|.
  btntxt13 = |�Ƽ��������ñ�({ btn13 })|.
  btntxt14 = |�ܿ��������ñ�({ btn14 })|.


AT SELECTION-SC