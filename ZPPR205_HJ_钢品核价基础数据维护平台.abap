*&---------------------------------------------------------------------*
*& Report ZPPR205_HJ
*&---------------------------------------------------------------------*
*& 钢品核价基础数据维护平台
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
  t1 = '压型板基础数据维护'.
  btntxt01 = |压型板核价配置表({ btn01 })|.
  t2 = '折弯件基础数据维护'.
  btntxt02 = |折弯件核价配置表({ btn02 })|.
  btntxt03 = |折弯件镀锌吨价配置表({ btn03 })|.
  t3 = '丽彩基础数据维护'.
  btntxt04 = |内外板加价配置表({ btn04 })|.
  btntxt05 = |芯材配置表({ btn05 })|.
  btntxt06 = |丽彩鼎金钻辅材配置表({ btn06 })|.
  btntxt07 = |丽彩鼎金钻胶水-覆膜配置表({ btn07 })|.
  btntxt08 = |幕墙辅材配置表({ btn08 })|.
  btntxt09 = |凯撒辅材配置表({ btn09 })|.
  btntxt10 = |丽彩鼎金制造费用配置表({ btn10 })|.
  btntxt11 = |丽彩钻制造费用配置吧({ btn11 })|.
  btntxt12 = |丽彩幕凯撒制造费用配置表({ btn12 })|.
  btntxt13 = |推荐版型配置表({ btn13 })|.
  btntxt14 = |管控利润配置表({ btn14 })|.


AT SELECTION-SC