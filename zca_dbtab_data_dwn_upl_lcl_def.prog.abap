*&---------------------------------------------------------------------*
*&  Include           ZCA_DBTAB_DATA_DWN_UPL_LCL_DEF
*&---------------------------------------------------------------------*

CLASS lcl_app DEFINITION CREATE PRIVATE.

**********************************************************************
  PUBLIC SECTION.
**********************************************************************

    DATA:
      gv_temp_dir TYPE string.

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_app.

    METHODS:
      main.

**********************************************************************
  PROTECTED SECTION.
**********************************************************************

    TYPES:
      BEGIN OF ty_header,
        table TYPE tabname,
        size  TYPE iwnolines,
      END OF ty_header,

      BEGIN OF ty_result.
        INCLUDE TYPE ty_header.
    TYPES:
      err_txt TYPE rsrqprov_errortext, "wlbwerrtxt,
      END OF ty_result,
      tt_result TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

    CONSTANTS:
      gc_default_extension TYPE string VALUE 'bin',
      gc_default_file_name TYPE string VALUE 'data_tr',
      gc_default_init_dir  TYPE string VALUE 'C:\temp'.

    DATA:
      gv_initial_directory TYPE string,
      gt_result            TYPE tt_result,
      go_alv               TYPE REF TO cl_salv_table.

    METHODS:
      download_tables_to_file,
      get_tables_data RETURNING VALUE(rv_tables_data) TYPE xstring,
      download_data_to_file IMPORTING iv_tables_data TYPE xstring,
      conv_xstring_to_binary IMPORTING iv_xstring TYPE xstring
                             EXPORTING ev_outlen  TYPE i
                                       et_binary  TYPE solix_tab,
      show_save_dialog EXPORTING ev_fullpath    TYPE string
                                 ev_user_action TYPE i,
      download_file_to_frontend IMPORTING iv_bin_filesize    TYPE i
                                          iv_filename        TYPE string
                                          VALUE(it_data_tab) TYPE solix_tab,
      upload_tab_data_from_file,
      upload_data_from_file RETURNING VALUE(rv_tables_data) TYPE xstring,
      show_open_file_dialog EXPORTING et_file_table  TYPE filetable
                                      ev_user_action TYPE i,
      upload_file_from_frontend IMPORTING iv_filename TYPE file_table-filename
                                EXPORTING ev_outlen   TYPE i
                                          et_data_tab TYPE solix_tab,
      conv_binary_to_xstring IMPORTING iv_outlen             TYPE i
                                       it_tables_data        TYPE solix_tab
                             RETURNING VALUE(rv_tables_data) TYPE xstring,
      update_tables_data IMPORTING iv_tables_data TYPE xstring,
      display_results.

**********************************************************************
  PRIVATE SECTION.
**********************************************************************

    CLASS-DATA:
      go_app TYPE REF TO lcl_app.

    METHODS:
      constructor.

ENDCLASS.
