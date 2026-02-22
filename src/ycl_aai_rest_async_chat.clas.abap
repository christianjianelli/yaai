CLASS ycl_aai_rest_async_chat DEFINITION
  PUBLIC
  INHERITING FROM ycl_aai_rest_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_request_create_s,
             chat_id  TYPE string,
             api      TYPE string,
             api_key  TYPE string,
             message  TYPE string,
             context  TYPE string,
             agent_id TYPE string,
             model    TYPE string,
           END OF ty_request_create_s,

           BEGIN OF ty_response_create_s,
             chat_id TYPE string,
             task_id TYPE string,
             created TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_create_s,

           BEGIN OF ty_request_read_s,
             chat_id TYPE string,
             task_id TYPE string,
           END OF ty_request_read_s,

           BEGIN OF ty_response_read_s,
             chat_id TYPE string,
             task_id TYPE string,
             status  TYPE string,
           END OF ty_response_read_s.

    METHODS yif_aai_rest_resource~create REDEFINITION.

    METHODS yif_aai_rest_resource~read REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_async_chat IMPLEMENTATION.

  METHOD yif_aai_rest_resource~create.

    DATA: lo_aai_db TYPE REF TO ycl_aai_db.

    DATA: ls_request  TYPE ty_request_create_s,
          ls_response TYPE ty_response_create_s.

    DATA: l_json    TYPE string,
          l_task_id TYPE yaai_async-id,
          l_debug   TYPE abap_bool.

    DATA(l_request_body) = i_o_request->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = l_request_body
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_request
    ).

    ls_request-api = condense( to_upper( ls_request-api ) ).

    ls_request-chat_id = condense( to_upper( ls_request-chat_id ) ).

    ls_request-agent_id = condense( to_upper( ls_request-agent_id ) ).

    ls_response-chat_id = ls_request-chat_id.

    IF ls_request-chat_id IS INITIAL.

      CASE ls_request-api.

        WHEN yif_aai_const=>c_openai.

          lo_aai_db = NEW ycl_aai_db( i_api = yif_aai_const=>c_openai ).

        WHEN yif_aai_const=>c_mistral.

          lo_aai_db = NEW ycl_aai_db( i_api = yif_aai_const=>c_mistral ).

        WHEN OTHERS.

          ls_response-error = |LLM API { ls_request-api } is not supported.|.

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

          RETURN.

      ENDCASE.

      ls_response-chat_id = lo_aai_db->m_id.

    ENDIF.

    " Create Async Task
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lo_aai_async) = NEW ycl_aai_async( ).

    l_task_id = lo_aai_async->create(
      EXPORTING
        i_chat_id   = CONV #( ls_request-chat_id )
        i_task_name = 'OPENAI_ASYNC_CHAT'
    ).

    ls_response-task_id = l_task_id.

    " Run Async Task
    " Change the value of l_debug to run the task synchronously
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(l_started) = lo_aai_async->yif_aai_async~run(
      EXPORTING
        i_api      = CONV #( ls_request-api )
        i_task_id  = l_task_id
        i_chat_id  = CONV #( ls_request-chat_id )
        i_api_key  = ls_request-api_key
        i_message  = ls_request-message
        i_context  = ls_request-context
        i_agent_id = CONV #( ls_request-agent_id )
        i_model    = ls_request-model
        i_log      = abap_true
        i_debug    = l_debug
    ).

    IF l_started = abap_false.
      ls_response-error = 'Error while creating Async Chat'.
    ELSE.
      ls_response-created = abap_true.
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

    DATA: ls_request  TYPE ty_request_read_s,
          ls_response TYPE ty_response_read_s.

    DATA: l_json TYPE string.

    ls_request-task_id = to_upper( i_o_request->get_form_field( name = 'task_id' ) ).

    DATA(lo_async_task) = NEW ycl_aai_async( ).

    ls_response-status = lo_async_task->get_status(
      EXPORTING
        i_task_id = CONV #( ls_request-task_id ) ).

    ls_response-task_id = ls_request-task_id.

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
