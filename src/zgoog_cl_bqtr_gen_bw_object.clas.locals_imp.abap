
 CLASS lcl_code_composer_util DEFINITION.

   PUBLIC SECTION.
     CLASS-METHODS generate_code_using_cc
       IMPORTING
         !iv_template TYPE string
         !is_params   TYPE any OPTIONAL
       EXPORTING
         !ev_error    TYPE string
         !et_code     TYPE rstran_t_abapsource.
   PROTECTED SECTION.

   PRIVATE SECTION.
     CLASS-METHODS check_template_exists
       IMPORTING
         !iv_template    TYPE string
       RETURNING
         VALUE(rv_exist) TYPE abap_bool .
 ENDCLASS.

 CLASS lcl_code_composer_util IMPLEMENTATION.
   METHOD check_template_exists.

     DATA: lv_name TYPE progname,
           ls_dir  TYPE trdir.
     lv_name = iv_template.
     SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_name.
     IF sy-subrc = 0 AND ls_dir IS NOT INITIAL.
       rv_exist = abap_true.
     ENDIF.

   ENDMETHOD.

   METHOD generate_code_using_cc.

     DATA: lo_cmp    TYPE REF TO cl_cmp_composer,
           lo_cmpf   TYPE REF TO cx_cmp_failure,
           lo_root   TYPE REF TO cx_root,
           lt_code   TYPE rswsourcet,
           lt_buffer TYPE rswsourcet.

     IF NOT lo_cmp IS BOUND.
       lo_cmp = cl_cmp_composer=>s_create( ).
     ENDIF.

     lo_cmp->add_var( i_name = 'I_PARAM' i_value = is_params ).

     IF check_template_exists( iv_template ) = abap_false.
       RETURN.
     ENDIF.
     TRY.
         lt_code = lo_cmp->build_code( i_template_include = iv_template ).

         CALL FUNCTION 'PRETTY_PRINTER'
           EXPORTING
             inctoo = abap_false
           TABLES
             otext  = lt_code
             ntext  = lt_buffer
           EXCEPTIONS
             OTHERS = 1.

         IF sy-subrc = 0.

*           LOOP AT lt_buffer ASSIGNING FIELD-SYMBOL(<ls_buffer>).
*             APPEND <ls_buffer> TO et_code.
*           ENDLOOP.
*           et_code = VALUE #( FOR ls_buffer IN lt_buffer ( line = table_line ) ).
           et_code = lt_buffer.
         ENDIF.

       CATCH cx_cmp_failure INTO lo_cmpf.       " Error During Generation
         ev_error = lo_cmpf->get_text( ).
       CATCH cx_root INTO lo_root.
         ev_error = lo_root->get_text( ).
     ENDTRY.

   ENDMETHOD.
 ENDCLASS.
