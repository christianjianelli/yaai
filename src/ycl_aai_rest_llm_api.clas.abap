CLASS ycl_aai_rest_llm_api DEFINITION INHERITING FROM ycl_aai_rest_base
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_model_s,
             model         TYPE string,
             default_model TYPE abap_bool,
           END OF ty_model_s,

           ty_model_t TYPE STANDARD TABLE OF ty_model_s WITH EMPTY KEY,

           BEGIN OF ty_api_s,
             id       TYPE string,
             base_url TYPE string,
             models   TYPE ty_model_t,
           END OF ty_api_s,

           ty_api_t TYPE STANDARD TABLE OF ty_api_s WITH EMPTY KEY,

           BEGIN OF ty_response_s,
             apis  TYPE ty_api_t,
             error TYPE string,
           END OF ty_response_s,

           BEGIN OF ty_response_update_s,
             updated TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_update_s.

    METHODS yif_aai_rest_resource~read REDEFINITION.

    METHODS yif_aai_rest_resource~update REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_rest_llm_api IMPLEMENTATION.

  METHOD yif_aai_rest_resource~read.

    DATA lt_models TYPE SORTED TABLE OF yaai_model
       WITH UNIQUE KEY id model.

    DATA: ls_api      TYPE ty_api_s,
          ls_response TYPE ty_response_s.

    DATA l_json TYPE string.

    DATA(l_id) = to_upper( i_o_request->get_form_field( name = 'id' ) ).

    IF l_id IS NOT INITIAL.

      APPEND INITIAL LINE TO ls_response-apis ASSIGNING FIELD-SYMBOL(<ls_api>).

      SELECT SINGLE id, base_url
        FROM yaai_api
        WHERE id = @l_id
        INTO CORRESPONDING FIELDS OF @<ls_api>.

      SELECT model, default_model
        FROM yaai_model
        WHERE id = @l_id
        INTO CORRESPONDING FIELDS OF TABLE @<ls_api>-models.

    ELSE.

      SELECT id, base_url
        FROM yaai_api
        INTO TABLE @DATA(lt_apis).                      "#EC CI_NOWHERE

      SELECT id, model, default_model
        FROM yaai_model
        ORDER BY PRIMARY KEY
        INTO CORRESPONDING FIELDS OF TABLE @lt_models.  "#EC CI_NOWHERE

      LOOP AT lt_apis ASSIGNING FIELD-SYMBOL(<ls_api_db>).

        APPEND INITIAL LINE TO ls_response-apis ASSIGNING <ls_api>.

        <ls_api>-id = <ls_api_db>-id.
        <ls_api>-base_url = <ls_api_db>-base_url.

        LOOP AT lt_models ASSIGNING FIELD-SYMBOL(<ls_model_db>)
          WHERE id = <ls_api_db>-id.

          APPEND VALUE #( model = <ls_model_db>-model
                          default_model = <ls_model_db>-default_model ) TO <ls_api>-models.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    l_json = /ui2/cl_json=>serialize(
     EXPORTING
       data = ls_response
       compress = abap_false
       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    IF ls_response-apis IS INITIAL.

      i_o_response->set_status(
        EXPORTING
          code = 404
          reason = 'Not Found'
      ).

      RETURN.

    ENDIF.

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~update.

    DATA: ls_request  TYPE ty_api_s,
          ls_response TYPE ty_response_update_s.

    DATA(l_json) = i_o_request->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = l_json
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data = ls_request
    ).

    ls_response-updated = abap_false.

    UPDATE yaai_api
      SET base_url = @ls_request-base_url
      WHERE id = @ls_request-id.

    IF sy-subrc = 0.
      ls_response-updated = abap_true.
    ELSE.
      "TODO: handle error
    ENDIF.

    DELETE FROM yaai_model
      WHERE id = @ls_request-id.

    LOOP AT ls_request-models ASSIGNING FIELD-SYMBOL(<ls_model>).

      INSERT yaai_model FROM @( VALUE yaai_model( id = ls_request-id
                                                    model = <ls_model>-model
                                                    default_model = <ls_model>-default_model ) ).

    ENDLOOP.

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
