*&---------------------------------------------------------------------*
*& Include          ZGOOG_I_BQTR_TEMPLATE_START
*&---------------------------------------------------------------------*

    DATA: lo_bq_repl    TYPE REF TO /goog/cl_bqtr_data_load,
          lv_error_code TYPE sysubrc,
          ls_textid     TYPE scx_t100key,
          lt_return     TYPE bapiret2_t.
    FIELD-SYMBOLS: <fs_return> TYPE bapiret2.

@if i_param-mandt_fnam @notinitial
    DATA: lv_logsys TYPE rsslogsys,
          lv_client TYPE rfcclient,
          lv_rfc    TYPE rfcdest.

    lv_logsys = p_r_request->get_logsys( ).
    lv_rfc = lv_logsys.

    IF lv_rfc IS NOT INITIAL.

      CALL FUNCTION 'RFC_READ_R3_DESTINATION'
        EXPORTING
          destination             = lv_rfc
        IMPORTING
          client                  = lv_client
        EXCEPTIONS
          authority_not_available = 1
          destination_not_exist   = 2
          information_failure     = 3
          internal_failure        = 4
          OTHERS                  = 5.

      IF sy-subrc <> 0.
        ls_textid-msgid = sy-msgid.
        ls_textid-msgno = sy-msgno.
        ls_textid-attr1 = sy-msgv1.
        ls_textid-attr2 = sy-msgv2.
        ls_textid-attr3 = sy-msgv3.
        ls_textid-attr4 = sy-msgv4.
        RAISE EXCEPTION TYPE cx_rsrout_abort
          EXPORTING
            textid = ls_textid.
      ELSEIF lv_client IS INITIAL.
        lv_client = sy-mandt.
      ENDIF.
    ELSE.
      lv_client = sy-mandt.
    ENDIF.
@end

    CREATE OBJECT lo_bq_repl
      EXPORTING
        iv_mass_tr_key   = '$i_param-mass_tr_key$'
        iv_data_source   = '$i_param-data_source$'
        iv_cdc_framework = '$i_param-cdc_framework$'
@if i_param-mandt_fnam @notinitial
        iv_fldnm_mandt   = '$i_param-mandt_fnam$'
        iv_mandt_value   = lv_client
@end
.
    lo_bq_repl->replicate_data(
      EXPORTING
        it_content       = source_package
      IMPORTING
        ev_error_code    = lv_error_code
        et_return        = lt_return ).
    IF lv_error_code > 0.
      LOOP AT lt_return ASSIGNING <fs_return> WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.
      IF <fs_return> IS ASSIGNED.

        DATA(lo_sdk_error) = NEW /goog/cx_sdk(
        textid =
          VALUE #(
           msgid = <fs_return>-id
           msgno = <fs_return>-number
           attr1 = 'MSGV1'
           attr2 = 'MSGV2'
           attr3 = 'MSGV3'
           attr4 = 'MSGV4' )
          msgv1 = <fs_return>-message_v1
          msgv2 = <fs_return>-message_v2
          msgv3 = <fs_return>-message_v3
          msgv4 = <fs_return>-message_v4 ).

        RAISE EXCEPTION TYPE cx_rsrout_abort
          EXPORTING
            textid   = cx_rsrout_abort=>cx_rs_error
            previous = lo_sdk_error.
      ENDIF.
    ENDIF.
