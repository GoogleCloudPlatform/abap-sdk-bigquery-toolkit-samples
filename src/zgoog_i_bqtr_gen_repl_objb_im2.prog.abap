**********************************************************************
*  Copyright 2025 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************
CLASS lcl_file_handler IMPLEMENTATION.

  METHOD csv_to_itab.

    TYPES: BEGIN OF lty_tab,
             rec(1000) TYPE c,
           END OF lty_tab.
    DATA: lt_tab  TYPE TABLE OF lty_tab,
          ls_data TYPE lcl_tool=>mty_data.

    FIELD-SYMBOLS:  <ls_tab> TYPE lty_tab.


    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_file
      CHANGING
        data_tab                = lt_tab
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Front-End Function Cannot Be Executed in Backgrnd
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No Upload Authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to File Denied
        dp_out_of_memory        = 14               " Not Enough Memory in DataProvider
        disk_full               = 15               " Storage Medium full
        dp_timeout              = 16               " Timeout of DataProvider
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DELETE lt_tab INDEX 1.

    LOOP AT lt_tab ASSIGNING <ls_tab>.

      CLEAR ls_data.

      SPLIT <ls_tab>-rec AT ',' INTO  ls_data-trkey
                                      ls_data-infotype
                                      ls_data-infoprovider
                                      ls_data-tabname
                                      ls_data-ext_mode
                                      ls_data-chain
                                      ls_data-trig
                                      ls_data-info.
      APPEND ls_data TO ct_table.

    ENDLOOP.

  ENDMETHOD.

  METHOD f4_file.

    DATA: lv_file TYPE rlgrap-filename.
    lv_file = cv_file.

    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        static    = 'X'
      CHANGING
        file_name = lv_file.

    cv_file = lv_file.
  ENDMETHOD.

  METHOD f4_directory.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Select directory to save template to'
*       initial_folder       =                  " Start Browsing Here
      CHANGING
        selected_folder      = cv_file
      EXCEPTIONS
        cntl_error           = 1                " Control error
        error_no_gui         = 2                " No GUI available
        not_supported_by_gui = 3                " GUI does not support this
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.

  METHOD download_template.
    TYPES: BEGIN OF lty_template,
             trkey        TYPE string,
             infotype     TYPE string,
             infoprovider TYPE string,
             tabname      TYPE string,
             ext_mode     TYPE string,
             chain        TYPE string,
             trig         TYPE string,
             info         TYPE string,
           END OF lty_template.
    DATA: lv_file     TYPE string,
          lv_csv_file TYPE rlgrap-filename,
          lv_msg      TYPE string,
          ls_template TYPE lty_template,
          lt_template TYPE STANDARD TABLE OF lty_template.

    ls_template-trkey = 'Transfer Key'.
    ls_template-infotype = 'Infoprovider Type'.
    ls_template-infoprovider = 'Infoprovider name'.
    ls_template-tabname = 'Infoprovider Table Name'.
    ls_template-ext_mode = 'Extraction Mode'.
    ls_template-chain = 'Process Chain Name'.
    ls_template-trig  = 'Trigger Name'.
    ls_template-info  = 'Infoarea'.

    APPEND ls_template TO lt_template.

    TYPES:
      t_text_line  TYPE c LENGTH 4096,
      tt_text_data TYPE STANDARD TABLE OF t_text_line WITH EMPTY KEY.

    DATA: lt_csv_converted_table TYPE tt_text_data.
    lv_csv_file = p_dpath && '\' && 'bw_gen_template.csv'.

    DATA: lv_csv_line TYPE c LENGTH 4096.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_template TO FIELD-SYMBOL(<lv_field_name>).
      IF sy-subrc IS NOT INITIAL.
        EXIT.
      ENDIF.

      IF lv_csv_line IS INITIAL.
        lv_csv_line = <lv_field_name>.
      ELSE.
        lv_csv_line = lv_csv_line && ',' && <lv_field_name>.
      ENDIF.

    ENDDO.
    APPEND lv_csv_line TO lt_csv_converted_table.

    lv_file = lv_csv_file.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                = lv_file                     " Name of file
      CHANGING
        data_tab                = lt_csv_converted_table                    " Transfer table
      EXCEPTIONS
        file_write_error        = 1                    " Cannot write to file
        no_batch                = 2                    " Front-End Function Cannot Be Executed in Backgrnd
        gui_refuse_filetransfer = 3                    " Incorrect Front End
        invalid_type            = 4                    " Invalid value for parameter FILETYPE
        no_authority            = 5                    " No Download Authorization
        unknown_error           = 6                    " Unknown error
        header_not_allowed      = 7                    " Invalid header
        separator_not_allowed   = 8                    " Invalid separator
        filesize_not_allowed    = 9                    " Invalid file size
        header_too_long         = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create         = 11                   " Cannot create DataProvider
        dp_error_send           = 12                   " Error Sending Data with DataProvider
        dp_error_write          = 13                   " Error Writing Data with DataProvider
        unknown_dp_error        = 14                   " Error when calling data provider
        access_denied           = 15                   " Access to File Denied
        dp_out_of_memory        = 16                   " Not Enough Memory in DataProvider
        disk_full               = 17                   " Storage Medium full
        dp_timeout              = 18                   " Timeout of DataProvider
        file_not_found          = 19                   " Could not find file
        dataprovider_exception  = 20                   " General Exception Error in DataProvider
        control_flush_error     = 21                   " Error in Control Framework
        not_supported_by_gui    = 22                   " GUI does not support this
        error_no_gui            = 23                   " GUI not available
        OTHERS                  = 24
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_msg = |Template { lv_file } was successfully downloaded!|.
    MESSAGE lv_msg TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
