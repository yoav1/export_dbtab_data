*&---------------------------------------------------------------------*
*& Report ZCA_DBTAB_DATA_DWN_UPL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zca_dbtab_data_dwn_upl.


**********************************************************************
INCLUDE:
**********************************************************************
  zca_dbtab_data_dwn_upl_top
 ,zca_dbtab_data_dwn_upl_lcl_def
 ,zca_dbtab_data_dwn_upl_scr
 ,zca_dbtab_data_dwn_upl_lcl_imp
 .

**********************************************************************
INITIALIZATION.
**********************************************************************
  go_report = lcl_app=>get_instance( ).


**********************************************************************
START-OF-SELECTION.
**********************************************************************
  go_report->main( ).
