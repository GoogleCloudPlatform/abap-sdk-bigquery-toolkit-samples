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

TYPES: BEGIN OF gty_excel_data,
         tablename       TYPE dd03l-tabname,
         cdsviewname(40) TYPE c,
         sqlviewname(40) TYPE c,
       END OF gty_excel_data,
       gtt_excel_data TYPE STANDARD TABLE OF gty_excel_data,

       BEGIN OF gty_tr_cds_details,
         obj_name   TYPE sobj_name,
         devclass   TYPE devclass,
         strkorr    TYPE strkorr,
         objectname TYPE objectname,
       END OF gty_tr_cds_details,

       BEGIN OF gty_output,
         tablename(30)   TYPE c,
         cdsviewname(30) TYPE c,
         sqlviewname(30) TYPE c,
         package(15)     TYPE c,
         transport(10)   TYPE c,
         status(70)      TYPE c,
       END OF gty_output.

DATA: gt_excel_data      TYPE TABLE OF gty_excel_data,
      gs_excel_data      TYPE gty_excel_data,
*      gt_dd03l_extract   TYPE STANDARD TABLE OF iuuc_s_fieldlist,
*      gs_dd03l_extract   TYPE  iuuc_s_fieldlist,
      gt_dd03l_extract   TYPE STANDARD TABLE OF dd03l,
      gs_dd03l_extract   TYPE dd03l,
      gt_output          TYPE TABLE OF gty_output,
      gs_output          TYPE gty_output,
      gt_tr_cds_details  TYPE TABLE OF gty_tr_cds_details,
      gs_tr_cds_details  TYPE gty_tr_cds_details,
      gv_keyfieldlist    TYPE string,
      gv_nonkeyfieldlist TYPE string,
      gv_input_rec_cnt   TYPE i,
      gv_output_rec_cnt  TYPE i.


INCLUDE zgoog_i_bqtr_gen_mass_cds_def. "class definitions
