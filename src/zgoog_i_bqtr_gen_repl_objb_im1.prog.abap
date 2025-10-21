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

CLASS lcl_tool IMPLEMENTATION.

  METHOD get_data.
    DATA: ls_data TYPE mty_data.

    IF p_psel = abap_true.
      ls_data-trkey   = p_trkey.
      ls_data-infotype = p_infot.
      ls_data-infoprovider = p_infop.
      ls_data-tabname = p_inftb.
      ls_data-ext_mode = p_extmd.
      ls_data-chain   = p_chain.
      ls_data-trig    = p_trig.
      ls_data-info    = p_info.
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
    DATA: lv_ds_name TYPE string,
          lv_durmi   TYPE btcpmin,
          lv_durho   TYPE btcphour.
    DATA: ls_tlogo_trnf TYPE zgoog_cl_bqtr_gen_bw_object=>t_tlogo.

    DATA: lv_dtp TYPE rsbkdtpnm.

    ls_tlogo_adso-tlogo = iv_infotype.
    ls_tlogo_adso-objnm = iv_infoprovider.

    ls_tlogo_ds-tlogo = iv_infotype.
    ls_tlogo_ds-objnm = iv_infoprovider.
    ls_tlogo_ds-updmode = iv_ext_mode.
    ls_tlogo_ds-object = iv_infoprovider.

    IF ls_tlogo_adso IS NOT INITIAL.

      zgoog_cl_bqtr_gen_bw_object=>create_trnf_bw(
        EXPORTING
          iv_mass_tr_key = iv_trkey
          iv_tlogo_ds    = ls_tlogo_ds
          iv_tlogo_adso  = ls_tlogo_adso
          iv_tabname     = iv_tabname
        IMPORTING
          es_tlogo       = ls_tlogo_trnf
          ev_sy_subrc    = lv_sy_subrc
          et_return      = lt_return ).

      add_status( iv_infoprovider = iv_infoprovider
                  iv_description = 'Data Transformation (RSDS)'
                  iv_value       = |RSDS { iv_infotype } { iv_infoprovider } -> { iv_infotype } { iv_infoprovider }|
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

        add_status( iv_infoprovider = iv_infoprovider
                    iv_description = 'Data Transfer Process'
                    iv_value       = |{ iv_infoprovider } -> { lv_dtp }|
                    it_return      = lt_return ).

        IF lv_dtp IS NOT INITIAL.

          IF p_min = abap_true.
            lv_durmi = p_durmi.
          ELSEIF p_hour = abap_true.
            lv_durho = p_durmi.
          ENDIF.

          zgoog_cl_bqtr_gen_bw_object=>create_trigger(
            EXPORTING
              iv_trigger  = CONV #( iv_trig )
              iv_prdmin   = lv_durmi
              iv_prdhours = lv_durho
            IMPORTING
              ev_sy_subrc = lv_sy_subrc
              et_return   = lt_return
          ).
          add_status( iv_infoprovider = iv_infoprovider
                      iv_description = 'Process Chain Trigger'
                      iv_value       = |{ iv_trig }|
                      it_return      = lt_return ).

          IF lv_sy_subrc = 0.

            zgoog_cl_bqtr_gen_bw_object=>create_process_chain(
              EXPORTING
                iv_chain    = iv_chain
                iv_infoara  = iv_info
                iv_active   = p_activ
                iv_streaming = space
                iv_trigger  = CONV #( iv_trig )
                iv_dtp_load = CONV #( lv_dtp )
                iv_adso     = CONV #( ls_tlogo_adso-objnm )
              IMPORTING
                ev_sy_subrc = lv_sy_subrc
                et_return   = lt_return
            ).
            add_status( iv_infoprovider = iv_infoprovider
                        iv_description = 'Process Chain'
                        iv_value       = |{ iv_chain }|
                        it_return      = lt_return ).

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.



  ENDMETHOD.

  METHOD add_status.
    DATA: ls_output TYPE mty_output,
          lv_error  TYPE boolean.

    ls_output-description = iv_description.
    ls_output-value       = iv_value.
    ls_output-infoprovider         = iv_infoprovider.

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
        lo_column ?= lo_columns->get_column( 'INFOPROVIDER' ).
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
      create( iv_trkey  = <ls_data>-trkey
              iv_infotype = <ls_data>-infotype
              iv_infoprovider = <ls_data>-infoprovider
              iv_tabname = <ls_data>-tabname
              iv_ext_mode = <ls_data>-ext_mode
              iv_chain  = <ls_data>-chain
              iv_trig   = <ls_data>-trig
              iv_info   = <ls_data>-info
              ).
    ENDLOOP.
    display( ).

  ENDMETHOD.

ENDCLASS.
