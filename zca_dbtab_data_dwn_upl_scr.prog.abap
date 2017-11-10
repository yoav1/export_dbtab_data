*&---------------------------------------------------------------------*
*&  Include           ZCA_DBTAB_DATA_DWN_UPL_SCR
*&---------------------------------------------------------------------*


**********************************************************************
* Selection Screen Definition
**********************************************************************
  SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.
  PARAMETERS: download RADIOBUTTON GROUP gr1 DEFAULT 'X' USER-COMMAND run_type,
              upload   RADIOBUTTON GROUP gr1.
  SELECTION-SCREEN END OF BLOCK b01.

  SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME.
  SELECT-OPTIONS: tables FOR gv_table DEFAULT 'SCARR' MODIF ID prm.
  PARAMETERS: show_res AS CHECKBOX    DEFAULT 'X' MODIF ID res,
              del_trgt AS CHECKBOX                MODIF ID res,
              insert   RADIOBUTTON GROUP gr2      MODIF ID res,
              modify   RADIOBUTTON GROUP gr2      MODIF ID res.
  SELECTION-SCREEN END OF BLOCK b02.


**********************************************************************
  AT SELECTION-SCREEN OUTPUT.
**********************************************************************
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PRM'.
          CASE abap_true.
            WHEN download.
              screen-active = 1.
            WHEN OTHERS.
              screen-active = 0.
          ENDCASE.
        WHEN 'RES'.
          CASE abap_true.
            WHEN upload.
              screen-active = 1.
            WHEN OTHERS.
              screen-active = 0.
          ENDCASE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
