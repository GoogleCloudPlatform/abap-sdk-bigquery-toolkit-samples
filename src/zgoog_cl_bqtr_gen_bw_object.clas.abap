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
class ZGOOG_CL_BQTR_GEN_BW_OBJECT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_tlogo,
        tlogo   TYPE rstlogo,
        objnm   TYPE sobj_name,
        subtype TYPE rso_tlogo_subtype,
        odpname TYPE rodps_odpname,
      END OF t_tlogo .

  class-data MV_OBJVER type RSOBJVERS value 'M' ##NO_TEXT.

  class-methods CREATE_BW_DS
    importing
      !IV_CDS_ODP type STRING
      !IV_LOGSYS type RSSLOGSYS
      !IV_APPLNM type RSAPPLNM
      !IV_DSNAM type RSOLTPSOURCER
    exporting
      !ES_TLOGO type T_TLOGO
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods CREATE_ADSO
    importing
      !IV_ADSONM type RSOADSONM
      !IV_ODPNAME type RODPS_ODPNAME
      !IV_LOGSYS type RSSLOGSYS
    exporting
      !ES_TLOGO type T_TLOGO
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods CREATE_TRNF
    importing
      !IV_MASS_TR_KEY type /GOOG/TRKEY
      !IV_DATA_SOURCE type STRING
      !IV_TLOGO_DS type T_TLOGO
      !IV_TLOGO_ADSO type T_TLOGO
      !IV_MANDT_FVAL type NAME_FELD optional
    exporting
      !ES_TLOGO type T_TLOGO
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods CREATE_DTP
    importing
      !IV_TLOGO_DS type T_TLOGO
      !IV_TLOGO_ADSO type T_TLOGO
      !IV_TLOGO_TRNF type T_TLOGO
    exporting
      !EV_DTPNM type RSBKDTPNM
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods CREATE_PROCESS_CHAIN
    importing
      !IV_CHAIN type RSPC_CHAIN optional
      !IV_INFOARA type RSINFOAREA default 'NODESNOTCONNECTED'
      !IV_STREAMING type BOOLEAN default ABAP_TRUE
      !IV_TRIGGER type STRING optional
      !IV_DTP_LOAD type STRING
      !IV_ADSO type STRING
      !IV_ACTIVE type BOOLEAN default ABAP_TRUE
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods CREATE_TRIGGER
    importing
      !IV_TRIGGER type STRING
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods DELETE_ADSO
    importing
      !IV_ADSONM type RSOADSONM
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods DELETE_BW_DS
    importing
      !IV_NAME type STRING
      !IV_LOGSYS type RSSLOGSYS
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods DELETE_PROCESS_CHAIN
    importing
      !IV_CHAIN type RSPC_CHAIN
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods DELETE_TRIGGER
    importing
      !IV_TRIGGER type RSPC_VARIANT
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods APPLY_ROUTINE
    importing
      !IV_MASS_TR_KEY type /GOOG/TRKEY
      !IV_DATA_SOURCE type STRING
      !IO_TRFN type ref to CL_RSTRAN_TRFN
      !IV_MANDT_FVAL type NAME_FELD optional
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  PROTECTED SECTION.
private section.

  class-methods MANAGE_ODP_DS_ACC_METHODS
    importing
      !IO_DS type ref to CL_RSDS_RSDS
      !IV_DATA_SOURCE type ROOSOURCER
      !IV_LOGSYS type RSSLOGSYS
      !IV_ODPNAME type RODPS_ODPNAME
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T
      !EO_ACCESS type ref to CL_RSDS_ACCESSMETHODS .
  class-methods CHECK_ODP_LOGSYS_EXIST
    importing
      !IV_LOGSYS type RSSLOGSYS
    exporting
      !EV_EXIST type ABAP_BOOL
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods ADD_BAPIRET2_FROM_RS_ERROR
    importing
      !IO_RS_ERROR type ref to CX_RS_ERROR_WITH_MESSAGE
    changing
      !CT_BAPIRET2 type BAPIRET2_T .
  class-methods ADD_BAPIRET2_FROM_CX_ROOT
    importing
      !IO_ERROR type ref to CX_ROOT
    changing
      !CT_BAPIRET2 type BAPIRET2_T .
  class-methods CONVERT_FREE_TEXT_TO_BAPIRET2
    importing
      !IV_TEXT type STRING
      !IV_MSG_TYPE type SYMSGTY default 'E'
    returning
      value(RS_BAPIRET2) type BAPIRET2 .
ENDCLASS.



CLASS ZGOOG_CL_BQTR_GEN_BW_OBJECT IMPLEMENTATION.


  METHOD add_bapiret2_from_cx_root.


    DATA: ls_bapiret2 TYPE bapiret2.

    DATA lv_text_exception TYPE string.
    lv_text_exception = io_error->get_text( ).
    ls_bapiret2 = convert_free_text_to_bapiret2( iv_text = lv_text_exception iv_msg_type = 'E' ).

    APPEND ls_bapiret2 TO ct_bapiret2.

  ENDMETHOD.


  METHOD add_bapiret2_from_rs_error.

    DATA: ls_bal_msg TYPE bal_s_msg.


    DATA: ls_bapiret2 TYPE bapiret2.

    ls_bal_msg = io_rs_error->get_message( ).

    ls_bapiret2-id = ls_bal_msg-msgid.
    ls_bapiret2-number = ls_bal_msg-msgno.
    ls_bapiret2-message_v1 = ls_bal_msg-msgv1.
    ls_bapiret2-message_v2 = ls_bal_msg-msgv2.
    ls_bapiret2-message_v3 = ls_bal_msg-msgv3.
    ls_bapiret2-message_v4 = ls_bal_msg-msgv4.

    APPEND ls_bapiret2 TO ct_bapiret2.

  ENDMETHOD.


  METHOD apply_routine.

    DATA: lt_abap_source TYPE rstran_t_abapsource.
    DATA: ls_source TYPE abapsource.

    DATA: lo_cmp    TYPE REF TO cl_cmp_composer,
          lo_cmpf   TYPE REF TO cx_cmp_failure,
          lo_root   TYPE REF TO cx_root,
          lt_buffer TYPE rswsourcet.

    TYPES: BEGIN OF lty_param,
             mass_tr_key   TYPE /goog/trkey,
             data_source   TYPE string,
             cdc_framework TYPE string,
             mandt_fnam    TYPE name_feld,
           END OF lty_param.
    DATA: ls_param TYPE lty_param.
    CONSTANTS: lc_start TYPE string VALUE 'ZGOOG_I_BQTR_TEMPLATE_START',
               lc_end   TYPE string VALUE 'ZGOOG_I_BQTR_TEMPLATE_END'.


    CLEAR: et_return, ev_sy_subrc.

    ls_param = VALUE #( mass_tr_key   = iv_mass_tr_key
                        data_source   = iv_data_source
                        cdc_framework = 'ODQ'
                        mandt_fnam    = iv_mandt_fval ).

    lcl_code_composer_util=>generate_code_using_cc( EXPORTING iv_template = lc_start
                                                              is_params   = ls_param
                                                    IMPORTING ev_error    = DATA(lv_error)
                                                              et_code     = lt_abap_source ).

    DATA: lo_ex TYPE REF TO cx_root.
    TRY.
*       Create routine and get reference and codeid of routine
        CALL METHOD io_trfn->create_start_routine( ).
        DATA: lo_routine_start TYPE REF TO cl_rstran_step_rout.
        lo_routine_start = io_trfn->get_start_routine( ).

        DATA: lv_codeid_start TYPE rscodeid.
        lo_routine_start->get_codeid(
          IMPORTING
            e_codid = lv_codeid_start ).

*       Set new routine code
        lo_routine_start->store_routine(
          i_codeid   = lv_codeid_start
          i_t_source = lt_abap_source ).
      CATCH cx_root INTO lo_ex.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lo_ex
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
        RETURN.
    ENDTRY.

    TRY.
*       Create routine and get reference and codeid of routine
        CALL METHOD io_trfn->create_end_routine( ).
        DATA: lo_routine_end TYPE REF TO cl_rstran_step_rout.
        lo_routine_end = io_trfn->get_end_routine( ).

        DATA: lv_codeid_end TYPE rscodeid.
        lo_routine_end->get_codeid(
          IMPORTING
            e_codid = lv_codeid_end ).

        CLEAR lt_abap_source.

        ls_source-line = | CLEAR RESULT_PACKAGE[]. |.
        APPEND ls_source TO lt_abap_source.

*       Set new routine code
        lo_routine_end->store_routine(
          i_codeid   = lv_codeid_end
          i_t_source = lt_abap_source ).
      CATCH cx_root INTO lo_ex.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lo_ex
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.


  ENDMETHOD.


  METHOD check_odp_logsys_exist.

    CLEAR: et_return, ev_sy_subrc.
    CALL FUNCTION 'RSDS_ODP_SYSTEM_PROP'
      EXPORTING
        i_logsys    = iv_logsys
        i_no_dialog = abap_true
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
    ELSE.
      ev_exist    = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD convert_free_text_to_bapiret2.

    DATA:
      BEGIN OF lv_text_converter,
        part1 TYPE symsgv,
        part2 TYPE symsgv,
        part3 TYPE symsgv,
        part4 TYPE symsgv,
      END OF lv_text_converter.

    lv_text_converter = iv_text.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = iv_msg_type
        cl     = '/GOOG/BQ'
        number = '001'
        par1   = lv_text_converter-part1
        par2   = lv_text_converter-part2
        par3   = lv_text_converter-part3
        par4   = lv_text_converter-part4
      IMPORTING
        return = rs_bapiret2.

  ENDMETHOD.


  METHOD create_adso.

    DATA: lt_field         TYPE cl_rsds_access_odp=>tt_repl_field.

    CLEAR: et_return, ev_sy_subrc.
    CALL METHOD cl_rsds_access_odp=>rodps_repl_odp_get_detail
      EXPORTING
        i_logsys  = iv_logsys
        i_odpname = iv_odpname
      IMPORTING
        et_fields = lt_field
        et_return = et_return.

    LOOP AT et_return TRANSPORTING NO FIELDS
      WHERE type CA 'AEX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS INITIAL.
      ev_sy_subrc = 4.
      RETURN.
    ENDIF.

    DATA: ls_key_field TYPE REF TO cl_rsds_access_odp=>ts_repl_field.

    READ TABLE lt_field REFERENCE INTO ls_key_field
         WITH KEY keyflag = 'X'.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA: lt_key       TYPE cl_rso_adso_api=>tn_t_key,
          lt_obj       TYPE cl_rso_adso_api=>tn_t_object,
          lt_dimension TYPE cl_rso_adso_api=>tn_t_dimension,
          lt_msg       TYPE rs_t_msg.

    DATA: ls_dimension TYPE  cl_rso_adso_api=>tn_s_dimension.


    ls_dimension-name = 'KEY'.  ls_dimension-txtsh = 'Key'.
    APPEND ls_dimension TO lt_dimension.

    ls_dimension-name = 'DATA'. ls_dimension-txtsh = 'Data'.
    APPEND ls_dimension TO lt_dimension.

    DATA:ls_key TYPE string.

    APPEND ls_key_field->name TO lt_key.

    DATA: ls_obj       TYPE cl_rso_adso_api=>tn_s_object.
    ls_obj-fieldname = ls_key_field->name.
    ls_obj-datatp = ls_key_field->type.
    ls_obj-length = ls_key_field->length.
    ls_obj-decimals = ls_key_field->decimals.
    ls_obj-dimension = 'KEY'.
    ls_obj-sid_determination_mode = 'N'.
    ls_obj-txtsh = ls_key_field->description.

    APPEND ls_obj TO lt_obj.

    TRY.

        DATA: lo_exception TYPE REF TO cx_root.
        DATA: lv_text TYPE rsoadsodescr.
        lv_text = iv_adsonm.
        cl_rso_adso_api=>create(
          EXPORTING
            i_adsonm                    = iv_adsonm
            i_text                      = lv_text
            i_infoarea                  = ''
            i_s_adsoflags               = cl_rso_adso_api=>get_adso_flags_from_model_tmpl(
                                                          cl_rso_adso_api=>tn_c_model_tmpl-standard )
            i_t_object                  = lt_obj
            i_t_key                     = lt_key
            i_t_dimension               = lt_dimension
          IMPORTING
            e_t_msg                     = lt_msg ).

      CATCH  cx_root INTO lo_exception.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lo_exception
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.

    es_tlogo-objnm = iv_adsonm.
    es_tlogo-tlogo = 'ADSO'.

  ENDMETHOD.


  METHOD create_bw_ds.

    CONSTANTS:
      lc_transactional_data TYPE rsds_requtype VALUE 'D',
      lc_odp_suffix         TYPE c LENGTH 2 VALUE '$F'.

    DATA:
      lv_data_source TYPE roosourcer,
      lo_attr        TYPE REF TO object,
      ls_t100_key    TYPE scx_t100key,
      lv_odpname     TYPE rodps_odpname,
      ls_ds          TYPE rsds,
      ls_dst         TYPE rsdst,
      ls_msg         TYPE bal_s_msg,
      lv_subrc       TYPE sysubrc,
      lt_dst         TYPE rsds_t_rsdst,
      lt_rs_msg      TYPE rs_t_msg,
      lo_ds          TYPE REF TO cl_rsds_rsds,
      lo_access_meth TYPE REF TO cl_rsds_accessmethods,
      lx_err_msg     TYPE REF TO cx_rs_error_with_message,
      lx_root        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_msg> TYPE bal_s_msg.

    DATA: lv_logsys_exists TYPE flag.


    CLEAR: et_return, ev_sy_subrc.
    check_odp_logsys_exist(
      EXPORTING
        iv_logsys   = iv_logsys
      IMPORTING
        ev_exist    = lv_logsys_exists
        ev_sy_subrc = ev_sy_subrc
        et_return   = et_return ).

    IF lv_logsys_exists <> abap_true.
      RETURN.
    ENDIF.

    lv_data_source = iv_cds_odp.

    DATA: lv_search_pattern TYPE rodps_odpname.
    DATA: lt_nodes TYPE rodps_repl_t_node.

    lv_search_pattern = iv_cds_odp && '*'.
    CALL METHOD cl_rsds_access_odp=>rodps_repl_odp_get_list
      EXPORTING
        i_logsys             = iv_logsys
        i_search_pattern     = lv_search_pattern
        i_langu              = sy-langu
      IMPORTING
        et_nodes             = lt_nodes
        et_return            = et_return
      EXCEPTIONS
        communication_failed = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

    DATA: ls_node TYPE REF TO rodps_repl_s_node.
    READ TABLE lt_nodes REFERENCE INTO ls_node
    WITH KEY display_name = iv_cds_odp.
    IF sy-subrc IS NOT INITIAL.
      ev_sy_subrc = 4.
      APPEND VALUE #( id         = '/GOOG/BQTR'
                      number     = '000'
                      type       = 'E'
                      message_v1 = 'Datasource not found!'
                      message_v2 = 'Provide SQL View Name for CDS!'

      ) TO et_return.
      RETURN.
    ENDIF.

    lv_odpname     = ls_node->nodename.
    lv_data_source = iv_dsnam.

    CALL METHOD cl_rsds_access_odp=>if_rsds_attributes_maintain~init
      EXPORTING
        i_datasource      = lv_data_source
        i_logsys          = iv_logsys
        i_r_attributes    = lo_attr
        i_r_accessmethods = lo_access_meth.

    TRY.
        CALL METHOD cl_rsds_rsds=>create_object
          EXPORTING
            i_datasource   = lv_data_source
            i_logsys       = iv_logsys
            i_new          = rs_c_true
            i_type         = lc_transactional_data
            i_modify       = rs_c_true
            i_with_cto     = rs_c_true
          RECEIVING
            r_r_datasource = lo_ds.

        CALL METHOD lo_ds->create_version
          EXPORTING
            i_objvers             = rs_c_objvers-new
            i_allow_empty_version = rs_c_true
            i_with_field          = rs_c_false
          EXCEPTIONS
            OTHERS                = 1.
        IF sy-subrc <> 0.
          ev_sy_subrc = sy-subrc.
          /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
            CHANGING
              ct_bapiret2 = et_return ).
          RETURN.
        ENDIF.

        ls_ds-datasource = lv_data_source.
        ls_ds-logsys     = iv_logsys.
        ls_ds-objvers    = rs_c_objvers-modified.
        ls_ds-primsegid  = '0001'.
        ls_ds-type       = lc_transactional_data.

        ls_dst-langu = sy-langu.
        ls_dst-txtlg = lv_data_source.
        ls_dst-txtmd = lv_data_source.
        ls_dst-txtsh = lv_data_source.
        APPEND ls_dst TO lt_dst.

        CALL METHOD lo_ds->update_head
          EXPORTING
            i_objvers = rs_c_objvers-new
            i_s_ds    = ls_ds
            i_t_dst   = lt_dst.

        CALL METHOD lo_ds->update_applnm
          EXPORTING
            i_applnm = iv_applnm.

        DATA: lo_access TYPE REF TO cl_rsds_accessmethods.

        manage_odp_ds_acc_methods(
          EXPORTING
            io_ds          = lo_ds
            iv_data_source = lv_data_source
            iv_logsys      = iv_logsys
            iv_odpname     = lv_odpname
          IMPORTING
            eo_access      = lo_access
            ev_sy_subrc    = ev_sy_subrc
            et_return      = et_return ).

        CALL METHOD lo_ds->update_head
          EXPORTING
            i_objvers = rs_c_objvers-new
            i_s_ds    = ls_ds
            i_t_dst   = lt_dst.

        CALL METHOD lo_ds->update_methods
          EXPORTING
            i_objvers   = rs_c_objvers-new
            i_r_methods = lo_access
          EXCEPTIONS
            failed      = 1
            OTHERS      = 2.
        IF sy-subrc <> 0.
          ev_sy_subrc = sy-subrc.
          /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
            CHANGING
              ct_bapiret2 = et_return ).
          RETURN.
        ENDIF.

*       As last thing save and activate data source
        TRY.
            lo_ds->if_rso_tlogo_maintain~save(
              EXPORTING
                i_with_cto = abap_false ).
          CATCH cx_rs_error_with_message INTO lx_err_msg.
            ev_sy_subrc = sy-subrc.
            add_bapiret2_from_rs_error(
              EXPORTING
                io_rs_error = lx_err_msg
              CHANGING
                ct_bapiret2 = et_return ).
            RETURN.
        ENDTRY.

*       Clear the log, to find out a potentional error in activation
        cl_rso_application_log=>appl_log_delete( ).

        lo_ds->if_rso_tlogo_maintain~activate(
          EXPORTING
            i_with_cto = abap_false
          IMPORTING
            e_subrc    = lv_subrc ).
        IF lv_subrc <> 0.
          ev_sy_subrc = lv_subrc.

          cl_rso_application_log=>appl_log_msg_read(
            IMPORTING
              e_t_msg = lt_rs_msg ).

          DATA: ls_bapiret2 TYPE bapiret2.

          LOOP AT lt_rs_msg ASSIGNING <ls_msg>
            WHERE msgty = 'E'.
            CLEAR ls_bapiret2.
            ls_bapiret2-id = <ls_msg>-msgid.
            ls_bapiret2-number = <ls_msg>-msgno.
            ls_bapiret2-message_v1 = <ls_msg>-msgv1.
            ls_bapiret2-message_v2 = <ls_msg>-msgv2.
            ls_bapiret2-message_v3 = <ls_msg>-msgv3.
            ls_bapiret2-message_v4 = <ls_msg>-msgv4.

            APPEND ls_bapiret2 TO et_return.

          ENDLOOP.
          RETURN.
        ENDIF.

      CATCH cx_root INTO lx_root.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lx_root
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.

*   Return what was generated
    es_tlogo-objnm(30)    = lv_data_source.
    es_tlogo-objnm+30(10) = iv_logsys.
    es_tlogo-tlogo = 'RSDS'.
    es_tlogo-odpname = lv_odpname.

  ENDMETHOD.


  METHOD create_dtp.

    CONSTANTS:
          lc_delta_load    TYPE rsbupdmode VALUE 'D'.

    DATA:
      lo_exception   TYPE REF TO cx_root,
      lo_dtp         TYPE REF TO cl_rsbk_dtp,
      lv_dtpnm_empty TYPE rsbkdtpnm,
      ls_dtp         TYPE rsbkdtp,
      lo_collection  TYPE REF TO cl_rsbk_dtp_collection,
      lv_objnm       TYPE sobj_name,
      ls_tlogo       TYPE rstran_s_tlogo,
      lt_tlogo       TYPE rstran_t_tlogo.


    CLEAR: et_return, ev_sy_subrc.

    MOVE-CORRESPONDING iv_tlogo_ds TO ls_tlogo.
    APPEND ls_tlogo TO lt_tlogo.

    MOVE-CORRESPONDING iv_tlogo_trnf TO ls_tlogo.
    APPEND ls_tlogo TO lt_tlogo.

    MOVE-CORRESPONDING iv_tlogo_adso TO ls_tlogo.
    APPEND ls_tlogo TO lt_tlogo.

    TRY.
        lo_dtp = cl_rsbk_dtp=>factory(
          i_dtp = lv_dtpnm_empty ).

        lo_dtp->copy_nvers_from_tlogo_list(
          i_t_tlogo = lt_tlogo
        ).

        lo_dtp->if_rsbk_dtp_maintain~set_updmode( lc_delta_load ).


        ls_dtp = lo_dtp->if_rsbk_dtp_display~get_dtp( ).
        MOVE ls_dtp-dtp TO lv_objnm.

        CREATE OBJECT lo_collection.

        lo_collection->add_tlogo(
          EXPORTING
            i_objnm = lv_objnm
        ).

        lo_collection->save(
          EXPORTING
            i_with_cto = abap_false
        ).

      CATCH cx_root INTO lo_exception.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lo_exception
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.

*   Try to activate dtp
    TRY.

        lo_collection->activate(
          EXPORTING
            i_with_cto            = abap_false
            i_force_activation    = abap_true
            i_show_check_protocol = abap_false
            i_with_check          = abap_false
        ).

      CATCH cx_root INTO lo_exception.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lo_exception
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.

*   Return generated DTP name
    ev_dtpnm = ls_dtp-dtp.

    lo_dtp->dequeue_dtp_for_request( ).

  ENDMETHOD.


  METHOD create_process_chain.

    DATA: lo_chain   TYPE REF TO cl_rspc_chain,
          lt_variant TYPE  rspc_t_variante.


    CLEAR: et_return, ev_sy_subrc.

* ==== Create Chain-Object ====
    CREATE OBJECT lo_chain
      EXPORTING
        i_chain         = iv_chain                 " Process chain
        i_objvers       = mv_objver
        i_t_variant     = lt_variant                " Type and Variant
        i_with_dialog   = ''              " With online
        i_new           = abap_true
      EXCEPTIONS
        aborted_by_user = 1                " User would actually rather not process a chain ...
        not_unique      = 2                " Selection not unique - & user requests not allowed
        wrong_name      = 3                " Incorrect Name of (New) Chain
        display_only    = 4                " Only Display Allowed
        OTHERS          = 5.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.


    lo_chain->set_streaming(
      EXPORTING
        i_streaming  = iv_streaming
      EXCEPTIONS
        not_possible = 1
        warning      = 2
        OTHERS       = 3
    ).
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

    DATA: lt_process_data TYPE rspc_t_variante,
          lv_trigger      TYPE string.
*
    IF iv_trigger IS SUPPLIED.
      lv_trigger = iv_trigger.
    ELSE.
      lv_trigger = 'ZT_' && iv_chain.
    ENDIF.

    APPEND VALUE #( type = rspc1_c_type-trigger variante = lv_trigger ) TO lt_process_data.
    APPEND VALUE #( type = rspc1_c_type-dtpload variante = iv_dtp_load ) TO lt_process_data.
    APPEND VALUE #( type = rspc1_c_type-adsoact variante = iv_adso ) TO lt_process_data.

* ==== Put processes into chain ====
    LOOP AT lt_process_data ASSIGNING FIELD-SYMBOL(<ls_process>).
      lo_chain->add_process(
        EXPORTING
          i_type          = <ls_process>-type               " Process Type
          i_variant       = <ls_process>-variante
          i_with_dialog   = ''              " With Dialog, Where Necessary
        IMPORTING
          e_variant       = <ls_process>-variante
        EXCEPTIONS
          aborted_by_user = 1                " User would actually rather not ...
          OTHERS          = 2
      ).
      IF sy-subrc <> 0.
        ev_sy_subrc = sy-subrc.
        /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
      ENDIF.

      DATA: lo_variant      TYPE REF TO cl_rspc_variant,
            lv_variant      TYPE rspc_variant,
            ls_variant      TYPE rspcvariant,
            lt_adso_variant TYPE rspc_t_variant.


      IF <ls_process>-type = rspc1_c_type-adsoact.

        lv_variant = <ls_process>-variante.

        " get an instance
        CALL METHOD cl_rspc_variant=>create
          EXPORTING
            i_type      = rspc1_c_type-adsoact
            i_variant   = lv_variant
          RECEIVING
            r_r_variant = lo_variant
          EXCEPTIONS
            locked      = 1
            OTHERS      = 2.

        .
        " fill l_t_variant
        ls_variant-type     = rspc1_c_type-adsoact.
        ls_variant-variante = lv_variant.
        ls_variant-objvers  = rs_c_objvers-active.
        ls_variant-lnr      = '0000000001'.
        ls_variant-fnam     = rsmpc_c_odsactivate-no_condense.
        ls_variant-sign     = 'I'.
        ls_variant-opt      = 'EQ'.
        ls_variant-low      = abap_false.

        APPEND ls_variant TO lt_adso_variant.
        ls_variant-fnam     = rsmpc_c_type-adso.

        ls_variant-lnr = 1.
        ls_variant-sign = 'I'.
        ls_variant-opt = 'EQ'.
        ls_variant-low = lv_variant.
        APPEND ls_variant TO lt_adso_variant.

        DATA(lv_lnr) = ls_variant-lnr + 1.
        " fill l_t_variant
        ls_variant-type     = rspc1_c_type-adsoact.
        ls_variant-variante = lv_variant.
        ls_variant-objvers  = rs_c_objvers-active.
        ls_variant-lnr      = lv_lnr.
        ls_variant-fnam     = rsmpc_c_odsactivate-noreqactwarn.
        ls_variant-sign     = 'I'.
        ls_variant-opt      = 'EQ'.
        ls_variant-low      = abap_false.
        APPEND ls_variant TO lt_adso_variant.


        DATA: ls_variantt  TYPE rspcvariantt.

        ls_variantt-langu = sy-langu.
        ls_variantt-type = rspc1_c_type-adsoact.
        ls_variantt-variante = lv_variant.
        ls_variantt-objvers = 'A'.
        ls_variantt-txtlg = lv_variant.

        CALL METHOD lo_variant->save
          EXPORTING
            i_t_rspcvariant  = lt_adso_variant
            i_s_rspcvariantt = ls_variantt
          EXCEPTIONS
            failed           = 1
            OTHERS           = 2.


      ENDIF.
    ENDLOOP.

    DATA: ls_chain   TYPE rspc_s_chain,
          ls_chain_h TYPE rspc_s_chain.

* ==== Connect processes Serially====
    LOOP AT lt_process_data ASSIGNING <ls_process>.
      ls_chain-type = <ls_process>-type.
      ls_chain-variante = <ls_process>-variante.
      SELECT SINGLE eventrange FROM rsprocesstypes INTO @DATA(ls_eventrange)
                 WHERE  type = @ls_chain_h-type.
      IF ls_eventrange > 0.
        lo_chain->connect(
          EXPORTING
            i_s_start            = ls_chain_h
            i_s_end              = ls_chain
            i_color              = 'G'
            i_no_question_double = rs_c_true
          EXCEPTIONS
            not_possible         = 1
            OTHERS               = 2 ).
      ENDIF.
      ls_chain_h = ls_chain.

    ENDLOOP.


    lo_chain->save(
      EXPORTING
        i_objvers    = mv_objver              " Object version
        i_activation = abap_true                 " Chain is being activated...
      EXCEPTIONS
        failed       = 1                " Failed
        OTHERS       = 2
    ).
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

    IF iv_active = abap_true.
      lo_chain->activate(
        IMPORTING
          e_t_conflicts = DATA(lt_co)
        EXCEPTIONS
          errors        = 1                " Errors occurred
          warnings      = 2                " Warnings occurred
          OTHERS        = 3
      ).
      IF sy-subrc <> 0.
        ev_sy_subrc = sy-subrc.
        /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_trnf.
    DATA:
      ls_source TYPE rstran_s_tlogo,
      ls_target TYPE rstran_s_tlogo,
      lv_subrc  TYPE sysubrc,
      lo_trfn   TYPE REF TO cl_rstran_trfn,
      lx_ex     TYPE REF TO cx_root.

    CLEAR: et_return, ev_sy_subrc.
    "TODO - Check existance

    MOVE-CORRESPONDING iv_tlogo_ds TO ls_source.
    MOVE-CORRESPONDING iv_tlogo_adso TO ls_target.

    TRY.
        lo_trfn = cl_rstran_trfn=>factory(
          EXPORTING
            i_s_source = ls_source
            i_s_target = ls_target
        ).
      CATCH cx_root INTO lx_ex.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lx_ex
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.

    DATA: lv_tranid TYPE rstranid.

    lo_trfn->get_tranid(
      IMPORTING
        e_tranid = lv_tranid ).

    apply_routine(
      iv_mass_tr_key = iv_mass_tr_key
      iv_data_source = iv_data_source
      io_trfn        = lo_trfn
      iv_mandt_fval  = iv_mandt_fval ).

*  Try to save object
    TRY.
        lo_trfn->if_rso_tlogo_maintain~save(
          EXPORTING
            i_with_cto = abap_false
          IMPORTING
            e_subrc    = lv_subrc ).
        IF lv_subrc <> 0.
          ev_sy_subrc = lv_subrc.
          /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
            CHANGING
              ct_bapiret2 = et_return ).
          RETURN.
        ENDIF.
      CATCH cx_root INTO lx_ex.
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lx_ex
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.

*  Try to activate object
    lo_trfn->if_rso_tlogo_maintain~activate(
      EXPORTING
        i_force_activation = abap_true
        i_with_cto         = abap_false
      IMPORTING
        e_subrc            = lv_subrc ).

    IF lv_subrc <> 0.
      ev_sy_subrc = lv_subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

    es_tlogo-objnm = lv_tranid.
    es_tlogo-tlogo    = 'TRFN'.

  ENDMETHOD.


  METHOD delete_adso.
    DATA: lt_msg       TYPE rs_t_msg.

    CLEAR: et_return, ev_sy_subrc.
    TRY.

        DATA: lo_exception TYPE REF TO cx_root.
        cl_rso_adso_api=>delete(
          EXPORTING
            i_adsonm         = iv_adsonm
            i_force_deletion = abap_true       " Delete despite inconsistencies or dependencies
          IMPORTING
            e_t_msg          = lt_msg                 " BW: Table with Messages (Application Log)
        ).
      CATCH cx_rs_all_msg INTO lo_exception. " Operation Failed
        ev_sy_subrc = 4.
        add_bapiret2_from_cx_root(
          EXPORTING
            io_error    = lo_exception
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD manage_odp_ds_acc_methods.

    CLEAR: et_return, ev_sy_subrc.
    DATA:
      lv_fieldname TYPE string,
      lo_access    TYPE REF TO cl_rsds_accessmethods,
      lt_method    TYPE rsds_t_r_accessattr,
      lt_fields    TYPE cl_rsds_access_odp=>tt_fields,
      ls_t100_key  TYPE scx_t100key.

    FIELD-SYMBOLS:
      <ls_field>  TYPE cl_rsds_access_odp=>ts_fields,
      <ls_method> LIKE LINE OF lt_method.

    CALL METHOD cl_rsds_accessmethods=>create_object
      EXPORTING
        i_datasource  = iv_data_source
        i_logsys      = iv_logsys
        i_infopackage = space
        i_objvers     = rs_c_objvers-new
      IMPORTING
        e_r_methods   = lo_access
      EXCEPTIONS
        failed        = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

    CALL METHOD lo_access->get_methods(
      EXPORTING
        i_filter    = 'X'
      RECEIVING
        r_t_methods = lt_method ).

    LOOP AT lt_method ASSIGNING <ls_method>.

      lo_access->get_methods( ).

      CALL METHOD lo_access->set_attributes
        EXPORTING
          i_accessmethod = <ls_method>-accessmethod.

      CALL FUNCTION 'RSDS_ODP_SET_GET_GLOBAL'
        EXPORTING
          i_odp        = iv_odpname
          i_logsys     = iv_logsys
          i_datasource = iv_data_source.

      CALL METHOD lo_access->get_attributes
        EXPORTING
          i_accessmethod = <ls_method>-accessmethod.

      CALL METHOD lo_access->set_structure_for_prop
        EXPORTING
          i_accessmethod = <ls_method>-accessmethod.

      CALL FUNCTION 'RSDS_ODP_SET_GET_GLOBAL'
        IMPORTING
          et_fields = lt_fields.

      LOOP AT lt_fields ASSIGNING <ls_field>
        WHERE name_ds = cl_rs_b4htapd_dp_odp_ds=>co_forbidden_fieldnm-record    OR
              name_ds = cl_rs_b4htapd_dp_odp_ds=>co_forbidden_fieldnm-request   OR
              name_ds = cl_rs_b4htapd_dp_odp_ds=>co_forbidden_fieldnm-datapakid OR
              name_ds = cl_rs_b4htapd_dp_odp_ds=>co_forbidden_fieldnm-partno.
        MOVE <ls_field>-name_ds TO lv_fieldname.
        "lv_fieldname = mo_dp_odp_cds->correct_field( lv_fieldname ). TODO
        MOVE lv_fieldname TO <ls_field>-name_ds.

      ENDLOOP.

      CALL FUNCTION 'RSDS_ODP_SET_GET_GLOBAL'
        EXPORTING
          it_fields = lt_fields.

      CALL METHOD lo_access->get_structure_for_prop
        EXPORTING
          i_accessmethod = <ls_method>-accessmethod
          i_no_popup     = rs_c_true
        EXCEPTIONS
          OTHERS         = 1.
      IF sy-subrc <> 0.
        ev_sy_subrc = sy-subrc.
        /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
          CHANGING
            ct_bapiret2 = et_return ).
        RETURN.
      ENDIF.
    ENDLOOP.

    eo_access = lo_access.

  ENDMETHOD.


  METHOD delete_process_chain.
    CLEAR: et_return, ev_sy_subrc.

    DATA: lo_chain   TYPE REF TO cl_rspc_chain,
          lt_variant TYPE  rspc_t_variante.

* ==== Create Chain-Object ====
    CREATE OBJECT lo_chain
      EXPORTING
        i_chain         = iv_chain                 " Process chain
        i_objvers       = mv_objver
        i_with_dialog   = ''              " With online
        i_new           = abap_false
      EXCEPTIONS
        aborted_by_user = 1                " User would actually rather not process a chain ...
        not_unique      = 2                " Selection not unique - & user requests not allowed
        wrong_name      = 3                " Incorrect Name of (New) Chain
        display_only    = 4                " Only Display Allowed
        OTHERS          = 5.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

    lo_chain->delete(
      EXCEPTIONS
        failed = 1                " Failed
        OTHERS = 2
    ).
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD delete_trigger.
    CLEAR: et_return, ev_sy_subrc.

    CALL FUNCTION 'RSPC_TRIGGER_DELETE'
      EXPORTING
        i_variant = iv_trigger
*       i_no_transport =                  " No Transport Connection
      EXCEPTIONS
        failed    = 1                " Failed
        OTHERS    = 2.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD create_trigger.


    CLEAR: et_return, ev_sy_subrc.
    DATA: lt_process_data TYPE rspc_t_variante,
          lv_trigger      TYPE string,
          ls_startspecs   TYPE  tbtcstrt.

    ls_startspecs-startdttyp  = 'I'.
    ls_startspecs-periodic    = abap_true.
    ls_startspecs-prdmins     = '01'.

    lv_trigger = iv_trigger.

    CALL FUNCTION 'RSPC_TRIGGER_GENERATE'
      EXPORTING
        i_variant      = CONV rspc_variant( lv_trigger )
        i_variant_text = 'Start Trigger'
        i_startspecs   = ls_startspecs                 " Start Data Description / Repetition Period of Background Job
*       i_meta         =                  " Start via meta chain
*       i_no_transport =                  " No Transport Connection
*       i_modify       =                  " No error if existence
      EXCEPTIONS
        exists         = 1                " already exists
        failed         = 2                " Failed
        OTHERS         = 3.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD delete_bw_ds.

    CLEAR: et_return, ev_sy_subrc.
    CALL METHOD cl_rsds_rsds=>delete_ds
      EXPORTING
        i_datasource = CONV #( iv_name )
        i_logsys     = iv_logsys
      EXCEPTIONS
        failed       = 1                " Failed
        not_exists   = 2                " Does Not Exist
        OTHERS       = 3.
    IF sy-subrc <> 0.
      ev_sy_subrc = sy-subrc.
      /goog/cl_bqtr_utility=>add_bapiret2_from_sy(
        CHANGING
          ct_bapiret2 = et_return ).
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
