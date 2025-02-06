**********************************************************************
*  Copyright 2024 Google LLC                                         *
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

CLASS lcl_tool IMPLEMENTATION.

  METHOD get_data.
    DATA: ls_data TYPE mty_data.

    IF p_psel = abap_true.
      ls_data-trkey = p_trkey.
      ls_data-dsname = p_dsnam.
      ls_data-cds   = p_cds.
      ls_data-logsy = p_logsy.
      ls_data-appl  = p_appl.
      ls_data-adso  = p_adso.
      ls_data-chain = p_chain.
      ls_data-trig  = p_trig.
      ls_data-info  = p_info.
      ls_data-active = p_activ.
      IF p_mandt = abap_true.
        ls_data-mfnam = p_mfnam.
      ENDIF.
      APPEND ls_data TO mt_data.
      CLEAR: ls_data.
    ELSE.
      lcl_file_handler=>csv_to_itab( EXPORTING iv_file  = p_upath
                                     CHANGING  ct_table = mt_data
                                             ).
    ENDIF.


  ENDMETHOD.

  METHOD create.

    DATA: lt_return TYPE bapiret2_t.
    DATA: lv_sy_subrc TYPE sy-subrc.
    DATA: ls_tlogo_adso TYPE zgoog_cl_bqtr_gen_bw_object=>t_tlogo.
    DATA: lv_adso_name TYPE rsoadsonm.
    DATA: ls_tlogo_ds TYPE zgoog_cl_bqtr_gen_bw_object=>t_tlogo.
    DATA: lv_ds_name TYPE string.
    DATA: ls_tlogo_trnf TYPE zgoog_cl_bqtr_gen_bw_object=>t_tlogo.

    DATA: lv_dtp TYPE rsbkdtpnm.

    lv_ds_name = iv_cds.
    zgoog_cl_bqtr_gen_bw_object=>create_bw_ds(
      EXPORTING
        iv_cds_odp  = lv_ds_name
        iv_dsnam    = iv_dsname
        iv_logsys   = iv_logsy
        iv_applnm   = iv_appl
      IMPORTING
        es_tlogo    = ls_tlogo_ds
        ev_sy_subrc = lv_sy_subrc
        et_return   = lt_return ).

    lv_adso_name = iv_adso.

    add_status( iv_cds         = iv_cds
                iv_description = 'BW Data Source'
                iv_value       = |{ iv_dsname } with ODP Provider { lv_ds_name } |
                it_return      = lt_return ).

    IF ls_tlogo_ds IS NOT INITIAL.

      zgoog_cl_bqtr_gen_bw_object=>create_adso(
        EXPORTING
          iv_adsonm   = lv_adso_name
          iv_odpname  = ls_tlogo_ds-odpname
          iv_logsys   = iv_logsy
        IMPORTING
          es_tlogo    = ls_tlogo_adso
          ev_sy_subrc = lv_sy_subrc
          et_return   = lt_return ).

      add_status( iv_cds         = iv_cds
                  iv_description = 'Advanced Data Store Object (ADSO)'
                  iv_value       = CONV #( lv_adso_name )
                  it_return      = lt_return ).

      IF ls_tlogo_adso IS NOT INITIAL.

        zgoog_cl_bqtr_gen_bw_object=>create_trnf(
          EXPORTING
            iv_mass_tr_key = iv_trkey
            iv_data_source = lv_ds_name
            iv_tlogo_ds    = ls_tlogo_ds
            iv_tlogo_adso  = ls_tlogo_adso
            iv_mandt_fval  = iv_mfnam
          IMPORTING
            es_tlogo       = ls_tlogo_trnf
            ev_sy_subrc    = lv_sy_subrc
            et_return      = lt_return ).

        add_status( iv_cds         = iv_cds
                    iv_description = 'Data Transformation (RSDS)'
                    iv_value       = |RSDS { lv_ds_name }/{ iv_logsy } -> ADSO { lv_adso_name }|
                    it_return      = lt_return ).

        IF ls_tlogo_trnf IS NOT INITIAL.


          zgoog_cl_bqtr_gen_bw_object=>create_dtp(
            EXPORTING
              iv_tlogo_ds   = ls_tlogo_ds
              iv_tlogo_adso = ls_tlogo_adso
              iv_tlogo_trnf = ls_tlogo_trnf
            IMPORTING
              ev_dtpnm      = lv_dtp
              ev_sy_subrc   = lv_sy_subrc
              et_return     = lt_return ).

          add_status( iv_cds         = iv_cds
                      iv_description = 'Data Transfer Process'
                      iv_value       = |{ lv_ds_name }/{ iv_logsy } -> { lv_dtp }|
                      it_return      = lt_return ).

          IF lv_dtp IS NOT INITIAL.

            zgoog_cl_bqtr_gen_bw_object=>create_trigger(
              EXPORTING
                iv_trigger  = CONV #( iv_trig )
              IMPORTING
                ev_sy_subrc = lv_sy_subrc
                et_return   = lt_return
            ).
            add_status( iv_cds         = iv_cds
                        iv_description = 'Process Chain Trigger'
                        iv_value       = |{ iv_trig }|
                        it_return      = lt_return ).

            IF lv_sy_subrc = 0.

              zgoog_cl_bqtr_gen_bw_object=>create_process_chain(
                EXPORTING
                  iv_chain    = iv_chain
                  iv_infoara  = iv_info
                  iv_active   = iv_active
                  iv_trigger  = CONV #( iv_trig )
                  iv_dtp_load = CONV #( lv_dtp )
                  iv_adso     = CONV #( ls_tlogo_adso-objnm )
                IMPORTING
                  ev_sy_subrc = lv_sy_subrc
                  et_return   = lt_return
              ).
              add_status( iv_cds         = iv_cds
                          iv_description = 'Process Chain'
                          iv_value       = |{ iv_chain }|
                          it_return      = lt_return ).

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD cleanup.
    DATA: lt_return TYPE bapiret2_t.
    DATA: lv_sy_subrc TYPE sy-subrc.
    CONSTANTS: lc_delete TYPE c VALUE 'D'.



    IF p_schai = abap_true OR p_sall = abap_true.
      zgoog_cl_bqtr_gen_bw_object=>delete_process_chain(
        EXPORTING
          iv_chain    = iv_chain
        IMPORTING
          ev_sy_subrc = lv_sy_subrc
          et_return   = lt_return ).

      add_status( iv_cds         = iv_cds
                  iv_description = 'Process Chain'
                  iv_value       = |{ iv_chain }|
                  iv_category    = lc_delete
                  it_return      = lt_return ).
    ENDIF.
    IF p_strig = abap_true OR p_sall = abap_true.
      zgoog_cl_bqtr_gen_bw_object=>delete_trigger(
        EXPORTING
          iv_trigger  = iv_trig
        IMPORTING
          ev_sy_subrc = lv_sy_subrc
          et_return   = lt_return ).

      add_status( iv_cds         = iv_cds
                  iv_description = 'Process Chain Trigger'
                  iv_value       = |{ iv_trig }|
                  iv_category    = lc_delete
                  it_return      = lt_return ).

    ENDIF.


    IF p_sbwds = abap_true OR p_sall = abap_true.
      zgoog_cl_bqtr_gen_bw_object=>delete_bw_ds(
        EXPORTING
          iv_name     = CONV #( iv_dsname )
          iv_logsys   = iv_logsy
        IMPORTING
          ev_sy_subrc = lv_sy_subrc
          et_return   = lt_return ).

      add_status( iv_cds         = iv_cds
                  iv_description = 'BW Data Source (including Dependencies)'
                  iv_value       = CONV #( iv_cds )
                  iv_category    = lc_delete
                  it_return      = lt_return ).


    ENDIF.
    IF p_sadso = abap_true OR p_sall = abap_true.
      zgoog_cl_bqtr_gen_bw_object=>delete_adso(
        EXPORTING
          iv_adsonm   = CONV #( iv_adso )
        IMPORTING
          ev_sy_subrc = lv_sy_subrc
          et_return   = lt_return ).

      add_status( iv_cds         = iv_cds
                  iv_description = 'Advanced Data Store Object (ADSO)'
                  iv_value       = CONV #( iv_adso )
                  iv_category    = lc_delete
                  it_return      = lt_return ).
    ENDIF.

  ENDMETHOD.

  METHOD add_status.
    DATA: ls_output TYPE mty_output,
          lv_error  TYPE boolean.

    ls_output-description = iv_description.
    ls_output-value       = iv_value.
    ls_output-cds         = iv_cds.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>) WHERE type IS NOT INITIAL.
      IF     <ls_return>-type = 'E'.
        lv_error = abap_true.
        ls_output-status      = icon_red_light.
      ELSEIF <ls_return>-type = 'S' OR <ls_return>-type = 'I'.
        ls_output-status      = icon_green_light.
      ELSEIF <ls_return>-type = 'W'.
        ls_output-status      = icon_yellow_light.
      ENDIF.
      ls_output-type        = <ls_return>-type.
      IF <ls_return>-type IS NOT INITIAL.
        MESSAGE ID <ls_return>-id TYPE <ls_return>-type NUMBER <ls_return>-number
                                                          WITH <ls_return>-message_v1
                                                               <ls_return>-message_v2
                                                               <ls_return>-message_v3
                                                               <ls_return>-message_v4
                                                               INTO ls_output-error.
        APPEND ls_output TO mt_output.
      ENDIF.
    ENDLOOP.

    IF lv_error IS INITIAL.
      ls_output-type   = 'S'.
      ls_output-status = icon_green_light.
      IF iv_category = 'C'.
        ls_output-error  = 'Successfully created!'.
      ELSEIF iv_category = 'D'.
        ls_output-error  = 'Successfully deleted!'.
      ENDIF.

      APPEND ls_output TO mt_output.
    ENDIF.
  ENDMETHOD.

  METHOD display.

    INCLUDE: <icon>.

    DATA: lo_alv  TYPE REF TO cl_salv_table.
    DATA: lo_settings TYPE REF TO cl_salv_tree_settings.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv                          " Basis Class Simple ALV Tables
          CHANGING
            t_table      = mt_output
        ).
        DATA: lo_functions            TYPE REF TO cl_salv_functions_list.
        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        DATA: lo_columns TYPE REF TO cl_salv_columns_table,
              lo_column  TYPE REF TO cl_salv_column_table.

        lo_columns = lo_alv->get_columns( ).
        lo_columns->set_optimize( 'X' ).

        lo_column ?= lo_columns->get_column( 'DESCRIPTION' ).
        lo_column->set_short_text( 'Desc' ).
        lo_column->set_long_text( 'Description' ).
        lo_column ?= lo_columns->get_column( 'VALUE' ).
        lo_column->set_short_text( 'Name' ).
        lo_column->set_long_text( 'Artifact Name' ).
        lo_column ?= lo_columns->get_column( 'CDS' ).
        lo_column->set_short_text( 'Source' ).


        lo_column ?= lo_columns->get_column( 'STATUS' ).
        lo_column->set_short_text( 'Status' ).
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
        lo_column->set_alignment( if_salv_c_alignment=>centered ).
        lo_column->set_output_length( 20 ).



        lo_column ?= lo_columns->get_column( 'ERROR' ).
        lo_column->set_short_text( 'Error' ).



        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

        lo_alv->display( ).

      CATCH cx_salv_msg.
      CATCH cx_salv_not_found.
      CATCH cx_root INTO DATA(lo_exp).
        DATA(lv_msg) = lo_exp->get_text( ).
        MESSAGE lv_msg TYPE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

  ENDMETHOD.

  METHOD run.
    FIELD-SYMBOLS: <ls_data> TYPE mty_data.
    get_data( ).
    LOOP AT mt_data ASSIGNING <ls_data>.
      IF p_clean = abap_true.
        cleanup( iv_trkey  = <ls_data>-trkey
                 iv_dsname = <ls_data>-dsname
                 iv_cds    = <ls_data>-cds
                 iv_logsy  = <ls_data>-logsy
                 iv_appl   = <ls_data>-appl
                 iv_adso   = <ls_data>-adso
                 iv_chain  = <ls_data>-chain
                 iv_trig   = <ls_data>-trig
                 iv_info   = <ls_data>-info
                 ).
      ELSE.
        create( iv_trkey  = <ls_data>-trkey
                iv_dsname = <ls_data>-dsname
                iv_cds    = <ls_data>-cds
                iv_logsy  = <ls_data>-logsy
                iv_appl   = <ls_data>-appl
                iv_adso   = <ls_data>-adso
                iv_chain  = <ls_data>-chain
                iv_trig   = <ls_data>-trig
                iv_info   = <ls_data>-info
                iv_mfnam  = <ls_data>-mfnam
                iv_active = <ls_data>-active
                ).
      ENDIF.
    ENDLOOP.
    display( ).

  ENDMETHOD.

ENDCLASS.
