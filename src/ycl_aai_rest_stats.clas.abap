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
             tools        TYPE i,
             documents    TYPE i,
             agents       TYPE i,
             chats        TYPE i,
             log_messages TYPE i,
             async_tasks  TYPE i,
           END OF ty_response_s.

    DATA ls_response TYPE ty_response_s.

    DATA l_json TYPE string.

    SELECT COUNT( * ) FROM yaai_tool INTO @ls_response-tools.
    SELECT COUNT( * ) FROM yaai_rag INTO @ls_response-documents.
    SELECT COUNT( * ) FROM yaai_agent INTO @ls_response-agents.
    SELECT COUNT( * ) FROM yaai_chat INTO @ls_response-chats.
    SELECT COUNT( * ) FROM yaai_log INTO @ls_response-log_messages.
    SELECT COUNT( * ) FROM yaai_async INTO @ls_response-async_tasks.

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
