CLASS ycl_aai_rest_stats DEFINITION INHERITING FROM ycl_aai_rest_base
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS yif_aai_rest_resource~read REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_stats IMPLEMENTATION.

  METHOD yif_aai_rest_resource~read.

    TYPES: BEGIN OF ty_response_s,
             tools     TYPE i,
             documents TYPE i,
             agents    TYPE i,
             tasks     TYPE i,
             taskflows TYPE i,
             chats     TYPE i,
           END OF ty_response_s.

    DATA lt_rng_username TYPE RANGE OF syst-uname.

    DATA ls_response TYPE ty_response_s.

    DATA: l_json   TYPE string,
          l_object TYPE ust12-objct,
          l_field1 TYPE ust12-field VALUE 'USER' ##NO_TEXT,
          l_field2 TYPE ust12-field VALUE 'ACTVT' ##NO_TEXT.

    lt_rng_username = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

    SELECT SINGLE low
      FROM tvarvc
      WHERE name = @yif_aai_const=>c_chat_auth_obj_param
        AND type = 'P'
        AND numb = '0000'
      INTO @DATA(l_authorization_object_name).

    IF l_authorization_object_name IS NOT INITIAL.

      l_object = l_authorization_object_name.

      AUTHORITY-CHECK OBJECT l_object
        ID l_field1  FIELD sy-uname
        ID l_field2  FIELD '03'.

      IF sy-subrc = 0.
        FREE lt_rng_username.
      ENDIF.

    ENDIF.

    SELECT COUNT( * ) FROM yaai_tool INTO @ls_response-tools. "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_rag INTO @ls_response-documents. "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_agent INTO @ls_response-agents. "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_task INTO @ls_response-tasks. "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_task INTO @ls_response-taskflows WHERE task_flow = @abap_true. "#EC CI_NOFIELD
    SELECT COUNT( * ) FROM yaai_chat INTO @ls_response-chats WHERE username IN @lt_rng_username. "#EC CI_NOFIELD

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
