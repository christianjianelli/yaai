CLASS ycl_aai_rest_llm_tool DEFINITION
  PUBLIC
  INHERITING FROM ycl_aai_rest_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_tool_s,
             class_name  TYPE string,
             method_name TYPE string,
             proxy_class TYPE string,
             description TYPE string,
           END OF ty_tool_s,

           BEGIN OF ty_response_create_s,
             created TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_create_s,

           BEGIN OF ty_response_update_s,
             updated TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_update_s,

           BEGIN OF ty_response_delete_s,
             deleted TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_delete_s,

           ty_tool_t TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY,

           BEGIN OF ty_response_read_s,
             tool TYPE ty_tool_s,
           END OF ty_response_read_s,

           BEGIN OF ty_response_query_s,
             tools TYPE ty_tool_t,
           END OF ty_response_query_s.

    METHODS yif_aai_rest_resource~create REDEFINITION.

    METHODS yif_aai_rest_resource~read REDEFINITION.

    METHODS yif_aai_rest_resource~update REDEFINITION.

    METHODS yif_aai_rest_resource~delete REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_llm_tool IMPLEMENTATION.

  METHOD yif_aai_rest_resource~create.

    DATA: ls_response TYPE ty_response_create_s.

    DATA l_json TYPE string.

    DATA(ls_tool) = VALUE yaai_tool( class_name = i_o_request->get_form_field( name = 'class_name' )
                                     method_name = i_o_request->get_form_field( name = 'method_name' )
                                     proxy_class = i_o_request->get_form_field( name = 'proxy_class' )
                                     description = i_o_request->get_form_field( name = 'description' ) ).

    IF ls_tool-class_name IS INITIAL OR
       ls_tool-method_name IS INITIAL OR
       ls_tool-description IS INITIAL.

      ls_response-error = 'Class, Method and Description are mandatory'.

    ELSE.

      INSERT yaai_tool FROM @ls_tool.

      IF sy-subrc = 0.
        ls_response-created = abap_true.
      ELSE.
        ls_response-error = 'Error while saving the tool'.
      ENDIF.

    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~read.

    DATA: lt_rng_class_name  TYPE RANGE OF yaai_tool-class_name,
          lt_rng_method_name TYPE RANGE OF yaai_tool-method_name,
          lt_rng_description TYPE RANGE OF yaai_tool-description.

    DATA: ls_response_query TYPE ty_response_query_s,
          ls_response_read  TYPE ty_response_read_s.

    DATA l_json TYPE string.

    DATA(l_class_name) = i_o_request->get_form_field( name = 'class_name' ).

    DATA(l_method_name) = i_o_request->get_form_field( name = 'method_name' ).

    DATA(l_description) = i_o_request->get_form_field( name = 'description' ).

    IF l_class_name IS NOT INITIAL AND l_method_name IS NOT INITIAL.

      SELECT SINGLE class_name, method_name, proxy_class, description
        FROM yaai_tool
          WHERE class_name = @l_class_name
            AND method_name = @l_method_name
            INTO @DATA(ls_tool).

      IF sy-subrc = 0.

        ls_response_read-tool = VALUE #( class_name = ls_tool-class_name
                                         method_name = ls_tool-method_name
                                         proxy_class = ls_tool-proxy_class
                                         description = ls_tool-description ).

      ENDIF.

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_read
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ELSE.

      IF l_class_name IS NOT INITIAL.
        lt_rng_class_name = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_class_name }*| ) ).
      ENDIF.

      IF l_method_name IS NOT INITIAL.
        lt_rng_method_name = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_method_name }*| ) ).
      ENDIF.

      IF l_description IS NOT INITIAL.
        lt_rng_description = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_description }*| ) ).
      ENDIF.

      SELECT class_name, method_name, proxy_class, description
        FROM yaai_tool
        WHERE class_name IN @lt_rng_class_name
          AND method_name IN @lt_rng_method_name
          AND description IN @lt_rng_description
          INTO TABLE @DATA(lt_tool)
          UP TO 100 ROWS.

      IF sy-subrc = 0.
        ls_response_query-tools = CORRESPONDING #( lt_tool ).
      ENDIF.

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_query
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ENDIF.

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~update.

    DATA: ls_response TYPE ty_response_update_s.

    DATA l_json TYPE string.

    DATA(ls_tool) = VALUE yaai_tool( class_name = i_o_request->get_form_field( name = 'class_name' )
                                     method_name = i_o_request->get_form_field( name = 'method_name' )
                                     proxy_class = i_o_request->get_form_field( name = 'proxy_class' )
                                     description = i_o_request->get_form_field( name = 'description' ) ).

    IF ls_tool-class_name IS INITIAL OR
       ls_tool-method_name IS INITIAL OR
       ls_tool-description IS INITIAL.

      ls_response-error = 'Class, Method and Description are mandatory'.

    ELSE.

      UPDATE yaai_tool FROM @ls_tool.

      IF sy-subrc = 0.
        ls_response-updated = abap_true.
      ELSE.
        ls_response-error = 'Error while changing the tool'.
      ENDIF.

    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~delete.

    DATA: ls_response TYPE ty_response_delete_s.

    DATA l_json TYPE string.

    DATA(ls_tool) = VALUE yaai_tool( class_name = i_o_request->get_form_field( name = 'class_name' )
                                     method_name = i_o_request->get_form_field( name = 'method_name' ) ).

    IF ls_tool-class_name IS INITIAL OR ls_tool-method_name IS INITIAL.

      ls_response-error = 'Class and Method are mandatory'.

    ELSE.

      DELETE yaai_tool FROM @ls_tool.

      IF sy-subrc = 0.
        ls_response-deleted = abap_true.
      ELSE.
        ls_response-error = 'Error while deleting the tool'.
      ENDIF.

    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

ENDCLASS.
