*&---------------------------------------------------------------------*
*& °üº¬               ZRPP201_SCR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& ²éÑ¯ÆÁÄ»
*&---------------------------------------------------------------------*

*SELECTION-SCREEN FUNCTION KEY: 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS:
      p_werks TYPE marc-werks OBLIGATORY
                              MEMORY ID wrk
                              MODIF ID wrk
                              .

  SELECT-OPTIONS:
      s_vbeln FOR vbak-vbeln
              NO-EXTENSION
              NO INTERVALS MODIF ID vb,

      s_matkl FOR t023-matkl
              NO-EXTENSION
              NO INTERVALS,

      s_matnr FOR marc-matnr.
  PARAMETERS: p_ht RADIOBUTTON GROUP rad3 DEFAULT 'X' USER-COMMAND singleclick,
              p_bom1 RADIOBUTTON GROUP rad3,
              p_all RADIOBUTTON GROUP rad3.

SELECTION-SCREEN END OF BLO