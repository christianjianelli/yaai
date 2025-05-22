CLASS ycl_aai_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

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

ENDCLASS.
