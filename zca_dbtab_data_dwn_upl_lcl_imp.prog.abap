*&---------------------------------------------------------------------*
*&  Include           ZCA_DBTAB_DATA_DWN_UPL_LCL_IMP
*&---------------------------------------------------------------------*

CLASS lcl_app IMPLEMENTATION.


  METHOD get_instance.

    IF go_app IS INITIAL.
      CREATE OBJECT go_app.
    ENDIF.

    ro_instance = go_app.

  ENDMETHOD.

  METHOD constructor.

*   Get initial directory
    cl_gui_frontend_services=>get_desktop_directory(
      CHANGING
        desktop_directory    = gv_initial_directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
      gv_initial_directory = gc_default_init_dir.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD main.

    CASE abap_true.

      WHEN download.
        download_tables_to_file( ).

      WHEN upload.
        upload_tab_data_from_file( ).
    ENDCASE.

  ENDMETHOD.

  METHOD download_tables_to_file.

    DATA: lv_tables_data TYPE xstring.


    lv_tables_data = get_tables_data( ).

    IF lv_tables_data IS INITIAL.
      MESSAGE 'No data to download' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    download_data_to_file( lv_tables_data ).

  ENDMETHOD.

  METHOD get_tables_data.

    DATA: ls_header     TYPE ty_header,
          lr_table_data TYPE REF TO data,
          lr_wa_data    TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data>          TYPE STANDARD TABLE,
                   <ls_data>          TYPE any,
                   <lv_hex_container> TYPE x.




    LOOP AT tables ASSIGNING FIELD-SYMBOL(<ls_table>).

*     Create ITAB
      CREATE DATA lr_table_data TYPE STANDARD TABLE OF (<ls_table>-low).
      ASSIGN lr_table_data->* TO <lt_data>.

*     Get data from DBTAB
      SELECT *
        FROM (<ls_table>-low)
        INTO TABLE <lt_data>.

      ls_header-table = <ls_table>-low.
      ls_header-size  = lines( <lt_data> ).
      ASSIGN ls_header TO <lv_hex_container> CASTING.
      CONCATENATE rv_tables_data <lv_hex_container> INTO rv_tables_data IN BYTE MODE.

*     Crate WA
      CREATE DATA lr_wa_data TYPE (<ls_table>-low).
      ASSIGN lr_wa_data->* TO <ls_data>.
      LOOP AT <lt_data> ASSIGNING <ls_data>.
        ASSIGN <ls_data> TO <lv_hex_container> CASTING.
        CONCATENATE rv_tables_data <lv_hex_container> INTO rv_tables_data IN BYTE MODE.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD download_data_to_file.

    DATA: lv_user_action   TYPE i,
          lv_file_fullname TYPE string,
          lv_file_size     TYPE i,
          lt_tables_data   TYPE solix_tab.




    conv_xstring_to_binary( EXPORTING iv_xstring = iv_tables_data
                            IMPORTING ev_outlen  = lv_file_size
                                      et_binary  = lt_tables_data[] ).

    show_save_dialog( IMPORTING ev_fullpath    = lv_file_fullname
                                ev_user_action = lv_user_action ).
**********************************************************************
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL AND sy-msgty IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE 'Download failed' TYPE 'E'.
      ENDIF.
    ELSEIF lv_user_action <> cl_gui_frontend_services=>action_ok.
      MESSAGE 'Download aborted' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
**********************************************************************

    download_file_to_frontend( iv_bin_filesize = lv_file_size
                               iv_filename     = lv_file_fullname
                               it_data_tab     = lt_tables_data[] ).
**********************************************************************
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL AND sy-msgty IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE 'Download failed' TYPE 'E'.
      ENDIF.
    ENDIF.
**********************************************************************

  ENDMETHOD.

  METHOD conv_xstring_to_binary.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstring
*       APPEND_TO_TABLE       = ' '
      IMPORTING
        output_length = ev_outlen
      TABLES
        binary_tab    = et_binary.

  ENDMETHOD.

  METHOD show_save_dialog.

    DATA: lv_filename TYPE string,
          lv_path     TYPE string.


    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
*    window_title              =     " Window Title
        default_extension         = gc_default_extension    " Default Extension
        default_file_name         = gc_default_file_name    " Default File Name
*    with_encoding             = 'UTF-8'
*    file_filter               =     " File Type Filter Table
        initial_directory         = gv_initial_directory    " Initial Directory
*    prompt_on_overwrite       = 'X'
      CHANGING
        filename                  = lv_filename    " File Name to Save
        path                      = lv_path    " Path to File
        fullpath                  = ev_fullpath    " Path + File Name
        user_action               = ev_user_action    " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*    file_encoding             =
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).

  ENDMETHOD.

  METHOD download_file_to_frontend.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = iv_bin_filesize    " File length for binary files
        filename                  = iv_filename    " Name of file
        filetype                  = 'BIN'    " File type (ASCII, binary ...)
*      append                    = SPACE    " Character Field Length 1
*      write_field_separator     = SPACE    " Separate Columns by Tabs in Case of ASCII Download
*      header                    = '00'    " Byte Chain Written to Beginning of File in Binary Mode
*      trunc_trailing_blanks     = SPACE    " Do not Write Blank at the End of Char Fields
*      write_lf                  = 'X'    " Insert CR/LF at End of Line in Case of Char Download
*      col_select                = SPACE    " Copy Only Selected Columns of the Table
*      col_select_mask           = SPACE    " Vector Containing an 'X' for the Column To Be Copied
*      dat_mode                  = SPACE    " Numeric and date fields are in DAT format in WS_DOWNLOAD
*      confirm_overwrite         = SPACE    " Overwrite File Only After Confirmation
*      no_auth_check             = SPACE    " Switch off Check for Access Rights
*      codepage                  = '4110'    " Character Representation for Output
*      ignore_cerr               = ABAP_TRUE    " Ignore character set conversion errors?
*      replacement               = '#'    " Replacement Character for Non-Convertible Characters
*      write_bom                 = SPACE    " If set, writes a Unicode byte order mark
*      trunc_trailing_blanks_eol = 'X'    " Remove Trailing Blanks in Last Column
*      wk1_n_format              = SPACE
*      wk1_n_size                = SPACE
*      wk1_t_format              = SPACE
*      wk1_t_size                = SPACE
*      show_transfer_status      = 'X'    " Enables suppression of transfer status message
*      fieldnames                =     " Table Field Names
*      write_lf_after_last_line  = 'X'    " Writes a CR/LF after final data record
*      virus_scan_profile        = '/SCET/GUI_DOWNLOAD'    " Virus Scan Profile
*    IMPORTING
*      filelength                =     " Number of bytes transferred
      CHANGING
        data_tab                  = it_data_tab[]
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).

  ENDMETHOD.

  METHOD upload_tab_data_from_file.

    DATA: lv_tables_data TYPE xstring.


    lv_tables_data = upload_data_from_file( ).

    IF lv_tables_data IS INITIAL.
      MESSAGE 'No data to upload' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    update_tables_data( lv_tables_data ).

    IF show_res = abap_true.
      display_results( ).
    ENDIF.

  ENDMETHOD.

  METHOD upload_data_from_file.

    DATA: lt_file_table  TYPE filetable,
          lv_user_action TYPE i,
          lv_outlen      TYPE i,
          lt_bin         TYPE solix_tab.

    FIELD-SYMBOLS: <ls_file_table> TYPE file_table.




    show_open_file_dialog(
      IMPORTING
        et_file_table  = lt_file_table[]
        ev_user_action = lv_user_action
    ).
**********************************************************************
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL AND sy-msgty IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE 'Upload failed' TYPE 'E'.
      ENDIF.
    ELSEIF lv_user_action <> cl_gui_frontend_services=>action_ok.
      MESSAGE 'Upload aborted' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
**********************************************************************


    READ TABLE lt_file_table INDEX 1 ASSIGNING <ls_file_table>.
**********************************************************************
    IF sy-subrc <> 0.
      MESSAGE 'No file was found' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
**********************************************************************


    upload_file_from_frontend(
      EXPORTING
        iv_filename = <ls_file_table>-filename
      IMPORTING
        ev_outlen   = lv_outlen
        et_data_tab = lt_bin[]
    ).
**********************************************************************
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
**********************************************************************


    rv_tables_data = conv_binary_to_xstring(
      EXPORTING
        iv_outlen      = lv_outlen
        it_tables_data = lt_bin[]
    ).
**********************************************************************
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
**********************************************************************

  ENDMETHOD.

  METHOD show_open_file_dialog.

    DATA: lv_rc          TYPE i.


    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
*        window_title            =     " Title Of File Open Dialog
        default_extension       = gc_default_extension    " Default Extension
        default_filename        = gc_default_file_name    " Default File Name
*        file_filter             =     " File Extension Filter String
*        with_encoding           =     " File Encoding
        initial_directory       = gv_initial_directory
*        multiselection          =     " Multiple selections poss.
      CHANGING
        file_table              = et_file_table[]    " Table Holding Selected Files
        rc                      = lv_rc    " Return Code, Number of Files or -1 If Error Occurred
        user_action             = ev_user_action    " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*        file_encoding           =
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).

  ENDMETHOD.

  METHOD upload_file_from_frontend.

    DATA: lv_filename TYPE string.


    lv_filename = iv_filename.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_filename    " Name of file
        filetype                = 'BIN'    " File Type (ASCII, Binary)
*          has_field_separator     = SPACE    " Columns Separated by Tabs in Case of ASCII Upload
*          header_length           = 0    " Length of Header for Binary Data
*          read_by_line            = 'X'    " File Written Line-By-Line to the Internal Table
*          dat_mode                = SPACE    " Numeric and date fields are in DAT format in WS_DOWNLOAD
*          codepage                =     " Character Representation for Output
*          ignore_cerr             = ABAP_TRUE    " Ignore character set conversion errors?
*          replacement             = '#'    " Replacement Character for Non-Convertible Characters
*          virus_scan_profile      =     " Virus Scan Profile
      IMPORTING
        filelength              = ev_outlen    " File Length
*          header                  =     " File Header in Case of Binary Upload
      CHANGING
        data_tab                = et_data_tab[]    " Transfer table for file contents
*          isscanperformed         = SPACE    " File already scanned
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).

  ENDMETHOD.

  METHOD conv_binary_to_xstring.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = iv_outlen
      IMPORTING
        buffer       = rv_tables_data
      TABLES
        binary_tab   = it_tables_data
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

  ENDMETHOD.

  METHOD update_tables_data.

    DATA: ls_header         TYPE ty_header,
          lr_table_data     TYPE REF TO data,
          lr_wa_data        TYPE REF TO data,
          lv_curr_pos       TYPE i,
          lv_len            TYPE i,
          lv_last_pos       TYPE i,
          lv_is_header      TYPE flag VALUE abap_true,
          lx_sy_open_sql_db TYPE REF TO cx_sy_open_sql_db,
          lv_expt_text      TYPE string,
          ls_result         TYPE ty_result.

    FIELD-SYMBOLS: <lt_data>          TYPE STANDARD TABLE,
                   <ls_data>          TYPE any,
                   <lv_hex_container> TYPE x.




*   Init
    ASSIGN ls_header TO <lv_hex_container> CASTING.
    lv_last_pos = xstrlen( iv_tables_data ).


*   Start reading datastream
    WHILE lv_curr_pos < lv_last_pos.
      DESCRIBE FIELD <lv_hex_container> LENGTH lv_len IN BYTE MODE.
      <lv_hex_container> = iv_tables_data+lv_curr_pos(lv_len).
      ADD lv_len TO lv_curr_pos.

      IF sy-subrc = 0.

        CASE lv_is_header.

          WHEN abap_true.
            CLEAR ls_result.
            MOVE-CORRESPONDING ls_header TO ls_result.

            CREATE DATA lr_table_data TYPE STANDARD TABLE OF (ls_header-table).
            ASSIGN lr_table_data->* TO <lt_data>.
            CREATE DATA lr_wa_data TYPE (ls_header-table).
            ASSIGN lr_wa_data->* TO <ls_data>.
            ASSIGN <ls_data> TO <lv_hex_container> CASTING.
            lv_is_header = abap_false.

          WHEN abap_false.
            SUBTRACT 1 FROM ls_header-size.
            APPEND <ls_data> TO <lt_data>.
            IF ls_header-size = 0.
              TRY .
                  CASE abap_true.
                    WHEN insert.
                      INSERT (ls_header-table) FROM TABLE <lt_data>.
                    WHEN modify.
                      MODIFY (ls_header-table) FROM TABLE <lt_data>.
                  ENDCASE.
                CATCH cx_sy_open_sql_db INTO lx_sy_open_sql_db.
                  lv_expt_text = lx_sy_open_sql_db->get_text( ).
                  ls_result-err_txt = lv_expt_text.
              ENDTRY.
              ASSIGN ls_header TO <lv_hex_container> CASTING.
              lv_is_header = abap_true.

              IF show_res = abap_true.
                APPEND ls_result TO gt_result.
              ENDIF.
            ENDIF.

        ENDCASE.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.


    MESSAGE 'Data was saved in the DB tables' TYPE 'S'.

  ENDMETHOD.

  METHOD display_results.

    cl_salv_table=>factory(
*      EXPORTING
*        list_display   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode
*        r_container    =     " Abstract Container for GUI Controls
*        container_name =
      IMPORTING
        r_salv_table   = go_alv
      CHANGING
        t_table        = gt_result
    ).
*      CATCH cx_salv_msg.    "

    go_alv->get_columns( )->set_optimize( ).

    go_alv->display( ).

  ENDMETHOD.


ENDCLASS.
