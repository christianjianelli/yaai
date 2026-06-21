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

    DATA ls_response TYPE ty_response_s.

    DATA l_json TYPE string.

    SELECT COUNT( * ) FROM yaai_tool INTO @ls_response-tools.                                  "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_rag INTO @ls_response-documents.                               "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_agent INTO @ls_response-agents.                                "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_task INTO @ls_response-tasks.                                  "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM yaai_task INTO @ls_response-taskflows WHERE task_flow = @abap_true. "#EC CI_NOFIELD
    SELECT COUNT( * ) FROM yaai_chat INTO @ls_response-chats.                                  "#EC CI_NOWHERE

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
