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
" Sample Program to send data to BiqQuery using the BigQuery Toolkit for SAP
REPORT zgoog_r_bqtr_ex_send_data_bq.

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
        MESSAGE 'Data was successfully loaded into BigQuery' TYPE 'S'.
      ELSE.
        MESSAGE 'An error occurred during data transfer. Please retry' TYPE 'E'.
      ENDIF.
    CATCH /goog/cx_sdk INTO DATA(lo_exp). " ABAP SDK for Google Cloud: Exception Class
      MESSAGE 'An error occurred during data transfer. Please retry' TYPE 'E'.
  ENDTRY.
ENDIF.
