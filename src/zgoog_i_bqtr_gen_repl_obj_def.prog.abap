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

CLASS lcl_file_handler DEFINITION DEFERRED.

CLASS lcl_tool DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS run.

    TYPES: BEGIN OF mty_data,
             trkey  TYPE /goog/trkey,
             dsname TYPE rsoltpsourcer,
             ds     TYPE ddlname,
             logsy  TYPE rsslogsys,
             appl   TYPE rsapplnm,
             adso   TYPE char9,
             chain  TYPE rspc_chain,
             trig   TYPE rspc_variant,
             durmi  TYPE btcpmin,
             info   TYPE rsinfoarea,
             mfnam  TYPE name_feld,
             active TYPE boolean,
           END OF mty_data,
           mtt_data TYPE STANDARD TABLE OF mty_data WITH NON-UNIQUE DEFAULT KEY.

  PRIVATE SECTION.

    TYPES: BEGIN OF mty_output,
             cds         TYPE ddlname,
             status      TYPE char10,
             description TYPE string,
             value       TYPE string,
             type        TYPE bapi_mtype,
             error       TYPE bapi_msg,
           END OF mty_output.
    CLASS-DATA: mt_output TYPE STANDARD TABLE OF mty_output,
                mt_data   TYPE STANDARD TABLE OF mty_data.

    CLASS-METHODS cleanup IMPORTING iv_trkey  TYPE /goog/trkey
                                    iv_dsname TYPE rsoltpsourcer
                                    iv_ds     TYPE ddlname OPTIONAL
                                    iv_logsy  TYPE rsslogsys
                                    iv_appl   TYPE rsapplnm
                                    iv_adso   TYPE char9
                                    iv_chain  TYPE rspc_chain
                                    iv_trig   TYPE rspc_variant
                                    iv_info   TYPE rsinfoarea.

    CLASS-METHODS create IMPORTING iv_trkey  TYPE /goog/trkey
                                   iv_dsname TYPE rsoltpsourcer
                                   iv_ds     TYPE ddlname
                                   iv_logsy  TYPE rsslogsys
                                   iv_appl   TYPE rsapplnm
                                   iv_adso   TYPE char9
                                   iv_chain  TYPE rspc_chain
                                   iv_trig   TYPE rspc_variant
                                   iv_durmi  TYPE btcpmin
                                   iv_info   TYPE rsinfoarea
                                   iv_active TYPE boolean
                                   iv_mfnam  TYPE name_feld.

    CLASS-METHODS display.

    CLASS-METHODS get_data.

    CLASS-METHODS add_status IMPORTING iv_ds          TYPE ddlname
                                       iv_description TYPE string
                                       iv_value       TYPE string
                                       iv_category    TYPE c DEFAULT 'C'
                                       it_return      TYPE bapiret2_t OPTIONAL.


ENDCLASS.


CLASS lcl_file_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS download_template.
    CLASS-METHODS f4_file CHANGING cv_file TYPE string.
    CLASS-METHODS f4_directory CHANGING cv_file TYPE string.
    CLASS-METHODS csv_to_itab IMPORTING iv_file  TYPE string
                              CHANGING  ct_table TYPE lcl_tool=>mtt_data.

ENDCLASS.

CLASS lcl_output_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: f4_transport_request     CHANGING cv_trkorr TYPE trkorr.
    CLASS-METHODS: f4_bw_logical_system     CHANGING cv_logsys TYPE rsslogsys.
    CLASS-METHODS: f4_rspc_trigger     CHANGING cv_trig TYPE rspc_variant.
    CLASS-METHODS: f4_application_component CHANGING cv_appl   TYPE rsapplnm.
    CLASS-METHODS: change_screen IMPORTING iv_mandt TYPE flag.
ENDCLASS.
