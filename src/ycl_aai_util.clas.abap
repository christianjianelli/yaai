CLASS ycl_aai_util DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF importing_params_s,
             name        TYPE string,
             type        TYPE string,
             format      TYPE string,
             required    TYPE abap_bool,
             description TYPE string,
             "o_elemdescr TYPE REF TO cl_abap_elemdescr,
           END OF importing_params_s,

           importing_params_tt TYPE STANDARD TABLE OF importing_params_s WITH EMPTY KEY.

    INTERFACES if_oo_adt_classrun.

    METHODS serialize
      IMPORTING
                i_data        TYPE data
      RETURNING VALUE(r_json) TYPE string.

    METHODS deserialize
      IMPORTING
        i_json TYPE string
      EXPORTING
        e_data TYPE data.

    METHODS replace_unicode_escape_seq
      IMPORTING
                i_content        TYPE string
      RETURNING VALUE(r_content) TYPE string.

    METHODS get_method_importing_params
      IMPORTING
                i_class_name                TYPE csequence
                i_method_name               TYPE csequence
      RETURNING VALUE(r_t_importing_params) TYPE importing_params_tt.

    METHODS get_parameter_type
      IMPORTING
                i_o_type_descr TYPE REF TO cl_abap_elemdescr
      RETURNING VALUE(r_type)  TYPE string.

    METHODS get_parameter_format
      IMPORTING
                i_o_type_descr  TYPE REF TO cl_abap_datadescr
      RETURNING VALUE(r_format) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_util IMPLEMENTATION.

  METHOD serialize.

    r_json = /ui2/cl_json=>serialize(
     EXPORTING
       data = i_data
*       compress         =
*       name             =
       pretty_name      = /ui2/cl_json=>pretty_mode-low_case
*       type_descr       =
*       assoc_arrays     =
*       ts_as_iso8601    =
*       expand_includes  =
*       assoc_arrays_opt =
*       numc_as_string   =
*       name_mappings    =
*       conversion_exits =
    ).

  ENDMETHOD.

  METHOD deserialize.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = i_json          " JSON string
*      jsonx            =                  " JSON XString
*      pretty_name      =                  " Pretty Print property names
*      assoc_arrays     =                  " Deserialize associative array as tables with unique keys
*      assoc_arrays_opt =                  " Optimize rendering of name value maps
*      name_mappings    =                  " ABAP<->JSON Name Mapping Table
*      conversion_exits =                  " Use DDIC conversion exits on deserialize of values
      CHANGING
        data             = e_data          " Data to serialize
    ).

  ENDMETHOD.

  METHOD replace_unicode_escape_seq.

    r_content = i_content.

    " In order to find unicode escape sequences, the regex should have a escaped backslash like '\\u[0-9A-Fa-f]{4}',
    " but for some reason, on the ABAP WebAS version 7.52, the method deserialize of the class /ui2/cl_json removes it,
    " instead of replacing the unicode escape sequence by the corresponding unicode character. This is the reason why this method was implemented.
    FIND ALL OCCURRENCES OF REGEX 'u[0-9A-Fa-f]{4}' IN i_content RESULTS DATA(lt_matches).

    LOOP AT lt_matches ASSIGNING FIELD-SYMBOL(<ls_match>).

      DATA(l_offset) = <ls_match>-offset + 1.
      DATA(l_length) = <ls_match>-length - 1.

      DATA(l_hexa) = i_content+l_offset(l_length).

      DATA(l_character) = cl_abap_conv_in_ce=>uccp( to_upper( l_hexa ) ).

      l_offset = <ls_match>-offset.
      l_length = <ls_match>-length.

      REPLACE ALL OCCURRENCES OF i_content+l_offset(l_length) IN r_content WITH l_character.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_method_importing_params.

    DATA: lo_class_descr TYPE REF TO cl_abap_classdescr,
          lo_elem_descr  TYPE REF TO cl_abap_elemdescr,
          lt_methods     TYPE abap_methdescr_tab,
          lt_parameters  TYPE abap_parmdescr_tab,
          ls_parameter   TYPE abap_parmdescr.

    " Get the class descriptor
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = to_upper( i_class_name )     " Type name
      RECEIVING
        p_descr_ref    = DATA(lo_descr)   " Reference to description object
      EXCEPTIONS
        type_not_found = 1                " Type with name p_name could not be found
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_class_descr ?= lo_descr.

    " Get all methods of the class
    lt_methods = lo_class_descr->methods.

    READ TABLE lt_methods ASSIGNING FIELD-SYMBOL(<ls_method>) WITH KEY name = to_upper( i_method_name ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <ls_method>-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).

      IF <ls_parameter>-parm_kind <> 'I'.
        CONTINUE.
      ENDIF.

      lo_class_descr->get_method_parameter_type(
        EXPORTING
          p_method_name       = i_method_name         " Method name
          p_parameter_name    = <ls_parameter>-name   " Parameter Name
        RECEIVING
          p_descr_ref         = DATA(lo_descr_ref)    " Description object
        EXCEPTIONS
          parameter_not_found = 1                     " Parameter could not be found
          method_not_found    = 2                     " Method was not found
          OTHERS              = 3
      ).

      IF sy-subrc = 0.

        lo_elem_descr ?= lo_descr_ref.

        IF lo_elem_descr->is_ddic_type( ) = abap_true.

          lo_elem_descr->get_ddic_field(
            EXPORTING
              p_langu      = sy-langu           " Current Language
            RECEIVING
              p_flddescr   = DATA(ls_flddescr)  " Field Description
            EXCEPTIONS
              not_found    = 0                  " Type could not be found
              no_ddic_type = 0                  " Typ is not a dictionary type
              OTHERS       = 0
          ).

        ENDIF.

        APPEND VALUE #( name = <ls_parameter>-name
                        type = me->get_parameter_type( i_o_type_descr = lo_elem_descr )
                        format = me->get_parameter_format( i_o_type_descr = lo_elem_descr )
                        required = COND #( WHEN <ls_parameter>-is_optional IS INITIAL THEN abap_true ELSE abap_false )
                        description = ls_flddescr-fieldtext ) TO r_t_importing_params.

        CLEAR ls_flddescr.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_parameter_type.

    CLEAR r_type.

    CASE i_o_type_descr->type_kind.

      WHEN cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_char OR
           cl_abap_typedescr=>typekind_date OR
           cl_abap_typedescr=>typekind_csequence OR
           cl_abap_typedescr=>typekind_clike.

        r_type = 'string'.

        IF i_o_type_descr->absolute_name CS 'ABAP_BOOL' AND i_o_type_descr->output_length = 1.
          r_type = 'boolean'.
        ENDIF.

      WHEN cl_abap_typedescr=>typekind_int OR
           cl_abap_typedescr=>typekind_int1 OR
           cl_abap_typedescr=>typekind_int2 OR
           cl_abap_typedescr=>typekind_int8 OR
           cl_abap_typedescr=>typekind_decfloat OR
           cl_abap_typedescr=>typekind_decfloat16 OR
           cl_abap_typedescr=>typekind_decfloat34 OR
           cl_abap_typedescr=>typekind_float OR
           cl_abap_typedescr=>typekind_num OR
           cl_abap_typedescr=>typekind_numeric OR
           cl_abap_typedescr=>typekind_packed.

        r_type = 'number'.

    ENDCASE.

  ENDMETHOD.

  METHOD get_parameter_format.

    CLEAR r_format.

    IF i_o_type_descr->type_kind = cl_abap_typedescr=>typekind_date.
      r_format = 'date'.
    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    me->get_method_importing_params(
      EXPORTING
        i_class_name         = 'ZCL_VEHICLE'
        i_method_name        = 'GET_MAKE'
      RECEIVING
        r_t_importing_params = DATA(lt_importing_params)
    ).

    out->write( lt_importing_params ).

  ENDMETHOD.

ENDCLASS.
