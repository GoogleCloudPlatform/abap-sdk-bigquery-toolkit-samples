" Sample Program to send data to BiqQuery using the BigQuery Toolkit for SAP
REPORT zbqtr_demo_send_data_to_bq.

" Select Data to Replicate
SELECT * FROM mara INTO TABLE @DATA(lt_mara) UP TO 100 ROWS.

IF sy-subrc = 0.
  TRY.
      DATA(lo_bq_replication) = NEW /goog/cl_bqtr_data_load(
        iv_mass_tr_key = 'DEMO_BQT'   " Transfer key from /GOOG/BQTR_MASTR
        iv_data_source = 'MARA'       " ABAP Dictionary Object Name such as Table, CDS View
      ).

      " Replicate data to BigQuery
      lo_bq_replication->replicate_data(
        EXPORTING
          it_content    = lt_mara                 " Table Content
        IMPORTING
          ev_error_code = DATA(lv_error_code)     " Error Code
          et_return     = DATA(lt_return)         " Return Table
      ).

      " Handle Errors and exceptions
      IF lv_error_code IS INITIAL AND lt_return IS INITIAL.
        DATA(lv_msg) = |Data was successfully loaded into BigQuery|.
        MESSAGE lv_msg TYPE 'S'.
      ELSE.
        MESSAGE 'An error occurred during data transfer. Please retry' TYPE 'E'.
      ENDIF.
    CATCH /goog/cx_sdk INTO DATA(lo_exp). " ABAP SDK for Google Cloud: Exception Class
      MESSAGE 'An error occurred during data transfer. Please retry' TYPE 'E'.
  ENDTRY.
ENDIF.
