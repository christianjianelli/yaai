CLASS ycl_aai_rest_transp_config DEFINITION INHERITING FROM ycl_aai_rest_base
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    TYPES: BEGIN OF ty_request_s,
             entity    TYPE string,
             transport TYPE string,
             keys      TYPE /ui2/cl_json=>json,
           END OF ty_request_s,

           BEGIN OF ty_response_s,
             success TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_s.

    METHODS yif_aai_rest_resource~create REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _add_apis_to_transport
      IMPORTING
        i_keys      TYPE /ui2/cl_json=>json OPTIONAL
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_api_to_transport
      IMPORTING
        i_api       TYPE yaai_api-id
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_tools_to_transport
      IMPORTING
        i_keys      TYPE /ui2/cl_json=>json OPTIONAL
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_tool_to_transport
      IMPORTING
        i_class_name  TYPE yaai_tool-class_name
        i_method_name TYPE yaai_tool-method_name
        i_transport   TYPE trkorr
      CHANGING
        ch_response   TYPE ty_response_s.

    METHODS _add_rag_docs_to_transport
      IMPORTING
        i_keys      TYPE /ui2/cl_json=>json OPTIONAL
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_rag_doc_to_transport
      IMPORTING
        i_id        TYPE yaai_rag-id
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_taskflows_to_transport
      IMPORTING
        i_keys      TYPE /ui2/cl_json=>json OPTIONAL
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_taskflow_to_transport
      IMPORTING
        i_id        TYPE yaai_task_flow-id
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_agents_to_transport
      IMPORTING
        i_keys      TYPE /ui2/cl_json=>json OPTIONAL
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

    METHODS _add_agent_to_transport
      IMPORTING
        i_id        TYPE yaai_agent-id
        i_transport TYPE trkorr
      CHANGING
        ch_response TYPE ty_response_s.

ENDCLASS.



CLASS ycl_aai_rest_transp_config IMPLEMENTATION.

  METHOD yif_aai_rest_resource~create.

    DATA: ls_request  TYPE ty_request_s,
          ls_response TYPE ty_response_s.

    DATA: l_json TYPE string.

    DATA(l_body) = i_o_request->get_cdata( ).

    IF l_body IS INITIAL.

      "Bad request
      i_o_response->set_status(
        EXPORTING
          code = 400
          reason = 'Empty body'
      ).

      RETURN.

    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = l_body
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_request
    ).

    FREE l_json.

    CASE ls_request-entity.

      WHEN 'API'.

        me->_add_apis_to_transport(
          EXPORTING
            i_keys      = ls_request-keys
            i_transport = CONV #( ls_request-transport )
          CHANGING
            ch_response = ls_response
        ).

      WHEN 'TOOL'.

        me->_add_tools_to_transport(
          EXPORTING
            i_keys      = ls_request-keys
            i_transport = CONV #( ls_request-transport )
          CHANGING
            ch_response = ls_response
        ).

      WHEN 'RAG'.

        me->_add_rag_docs_to_transport(
          EXPORTING
            i_keys      = ls_request-keys
            i_transport = CONV #( ls_request-transport )
          CHANGING
            ch_response = ls_response
        ).

      WHEN 'TASKFLOW'.

        me->_add_taskflows_to_transport(
          EXPORTING
            i_keys      = ls_request-keys
            i_transport = CONV #( ls_request-transport )
          CHANGING
            ch_response = ls_response
        ).

      WHEN 'AGENT'.

        me->_add_agents_to_transport(
          EXPORTING
            i_keys      = ls_request-keys
            i_transport = CONV #( ls_request-transport )
          CHANGING
            ch_response = ls_response
        ).

    ENDCASE.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD _add_apis_to_transport.

    TYPES: BEGIN OF ty_api_table_key_s,
             id TYPE yaai_api-id,
           END OF ty_api_table_key_s.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k,
          lt_keys  TYPE STANDARD TABLE OF ty_api_table_key_s.


    IF i_keys IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = i_keys
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = lt_keys
      ).

    ENDIF.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    IF lt_keys IS INITIAL.

      lt_e071 = VALUE #( ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_API'
                           objfunc = 'K' )
                         ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_MODEL'
                           objfunc = 'K' )
                         ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_OAUTH'
                           objfunc = 'K' ) ).

      lt_e071k = VALUE #( ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_API'
                            mastertype = 'TABU'
                            mastername = 'YAAI_API'
                            tabkey = |{ sy-mandt }*| )
                          ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_MODEL'
                            mastertype = 'TABU'
                            mastername = 'YAAI_MODEL'
                            tabkey = |{ sy-mandt }*| )
                          ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_OAUTH'
                            mastertype = 'TABU'
                            mastername = 'YAAI_OAUTH'
                            tabkey = |{ sy-mandt }*| ) ).

      TRY.

          lo_cts_api->if_cts_rest_api~add_object_to_request(
            EXPORTING
              iv_trkorr = i_transport
            CHANGING
              ct_e071   = lt_e071
              ct_e071k  = lt_e071k
          ).

          ch_response-success = abap_true.

        CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

          ch_response-error = lo_ex->get_text( ).

      ENDTRY.

    ELSE.

      LOOP AT lt_keys INTO DATA(ls_key).

        me->_add_api_to_transport(
          EXPORTING
            i_api       = ls_key-id
            i_transport = i_transport
          CHANGING
            ch_response = ch_response
        ).

        IF ch_response-error IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD _add_api_to_transport.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    TRY.

        lt_e071 = VALUE #( ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_API'
                             objfunc = 'K' )
                           ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_MODEL'
                             objfunc = 'K' )
                           ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_OAUTH'
                             objfunc = 'K' ) ).

        lt_e071k = VALUE #( ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_API'
                              mastertype = 'TABU'
                              mastername = 'YAAI_API'
                              tabkey = sy-mandt && i_api )
                            ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_MODEL'
                              mastertype = 'TABU'
                              mastername = 'YAAI_MODEL'
                              tabkey = sy-mandt && i_api && '*' )
                            ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_OAUTH'
                              mastertype = 'TABU'
                              mastername = 'YAAI_OAUTH'
                              tabkey = sy-mandt && i_api ) ).

        lo_cts_api->if_cts_rest_api~add_object_to_request(
          EXPORTING
            iv_trkorr = i_transport
          CHANGING
            ct_e071   = lt_e071
            ct_e071k  = lt_e071k
        ).

        ch_response-success = abap_true.

      CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

        ch_response-error = lo_ex->get_text( ).

    ENDTRY.

  ENDMETHOD.

  METHOD _add_tools_to_transport.

    TYPES: BEGIN OF ty_tool_table_key_s,
             class_name  TYPE yaai_tool-class_name,
             method_name TYPE yaai_tool-method_name,
           END OF ty_tool_table_key_s.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k,
          lt_keys  TYPE STANDARD TABLE OF ty_tool_table_key_s.

    IF i_keys IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = i_keys
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = lt_keys
      ).

    ENDIF.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    IF lt_keys IS INITIAL.

      lt_e071 = VALUE #( ( trkorr = i_transport
                                    pgmid = 'R3TR'
                                    object = 'TABU'
                                    obj_name = 'YAAI_TOOL'
                                    objfunc = 'K' ) ).

      lt_e071k = VALUE #( ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_TOOL'
                            mastertype = 'TABU'
                            mastername = 'YAAI_TOOL'
                            tabkey = |{ sy-mandt }*| ) ).

      TRY.

          lo_cts_api->if_cts_rest_api~add_object_to_request(
            EXPORTING
              iv_trkorr = i_transport
            CHANGING
              ct_e071   = lt_e071
              ct_e071k  = lt_e071k
          ).

          ch_response-success = abap_true.

        CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

          ch_response-error = lo_ex->get_text( ).

      ENDTRY.

    ELSE.

      LOOP AT lt_keys INTO DATA(ls_key).

        me->_add_tool_to_transport(
          EXPORTING
            i_class_name  = ls_key-class_name
            i_method_name = ls_key-method_name
            i_transport   = i_transport
          CHANGING
            ch_response   = ch_response
        ).

        IF ch_response-error IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD _add_tool_to_transport.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k.

    DATA: l_class_name  TYPE yaai_tool-class_name,
          l_method_name TYPE yaai_tool-method_name.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    l_class_name = to_upper( condense( i_class_name ) ).
    l_method_name = to_upper( condense( i_method_name ) ).

    TRY.

        lt_e071 = VALUE #( ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_TOOL'
                             objfunc = 'K' ) ).

        APPEND VALUE #( trkorr = i_transport
                        pgmid = 'R3TR'
                        object = 'TABU'
                        objname = 'YAAI_TOOL'
                        mastertype = 'TABU'
                        mastername = 'YAAI_TOOL' ) TO lt_e071k ASSIGNING FIELD-SYMBOL(<ls_e071k>).

        CONCATENATE sy-mandt l_class_name l_method_name INTO <ls_e071k>-tabkey RESPECTING BLANKS.

        lo_cts_api->if_cts_rest_api~add_object_to_request(
          EXPORTING
            iv_trkorr = i_transport
          CHANGING
            ct_e071   = lt_e071
            ct_e071k  = lt_e071k
        ).

        ch_response-success = abap_true.

      CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

        ch_response-error = lo_ex->get_text( ).

    ENDTRY.

  ENDMETHOD.

  METHOD _add_rag_docs_to_transport.

    TYPES: BEGIN OF ty_api_table_key_s,
             id TYPE yaai_rag-id,
           END OF ty_api_table_key_s.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k,
          lt_keys  TYPE STANDARD TABLE OF ty_api_table_key_s.


    IF i_keys IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = i_keys
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = lt_keys
      ).

    ENDIF.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    IF lt_keys IS INITIAL.

      lt_e071 = VALUE #( ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_RAG'
                           objfunc = 'K' )
                         ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_RAG_DATA'
                           objfunc = 'K' ) ).

      lt_e071k = VALUE #( ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_RAG'
                            mastertype = 'TABU'
                            mastername = 'YAAI_RAG'
                            tabkey = |{ sy-mandt }*| )
                          ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_RAG_DATA'
                            mastertype = 'TABU'
                            mastername = 'YAAI_RAG_DATA'
                            tabkey = |{ sy-mandt }*| ) ).

      TRY.

          lo_cts_api->if_cts_rest_api~add_object_to_request(
            EXPORTING
              iv_trkorr = i_transport
            CHANGING
              ct_e071   = lt_e071
              ct_e071k  = lt_e071k
          ).

          ch_response-success = abap_true.

        CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

          ch_response-error = lo_ex->get_text( ).

      ENDTRY.

    ELSE.

      LOOP AT lt_keys INTO DATA(ls_key).

        me->_add_rag_doc_to_transport(
          EXPORTING
            i_id        = ls_key-id
            i_transport = i_transport
          CHANGING
            ch_response = ch_response
        ).

        IF ch_response-error IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD _add_rag_doc_to_transport.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    TRY.

        lt_e071 = VALUE #( ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_RAG'
                             objfunc = 'K' )
                           ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_RAG_DATA'
                             objfunc = 'K' ) ).

        lt_e071k = VALUE #( ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_RAG'
                              mastertype = 'TABU'
                              mastername = 'YAAI_RAG'
                              tabkey = sy-mandt && i_id )
                            ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_RAG_DATA'
                              mastertype = 'TABU'
                              mastername = 'YAAI_RAG_DATA'
                              tabkey = sy-mandt && i_id ) ).

        lo_cts_api->if_cts_rest_api~add_object_to_request(
          EXPORTING
            iv_trkorr = i_transport
          CHANGING
            ct_e071   = lt_e071
            ct_e071k  = lt_e071k
        ).

        ch_response-success = abap_true.

      CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

        ch_response-error = lo_ex->get_text( ).

    ENDTRY.

  ENDMETHOD.

  METHOD _add_taskflows_to_transport.

    TYPES: BEGIN OF ty_api_table_key_s,
             id TYPE yaai_rag-id,
           END OF ty_api_table_key_s.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k,
          lt_keys  TYPE STANDARD TABLE OF ty_api_table_key_s.


    IF i_keys IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = i_keys
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = lt_keys
      ).

    ENDIF.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    IF lt_keys IS INITIAL.

      lt_e071 = VALUE #( ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_TASK'
                           objfunc = 'K' )
                         ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_TASK_FLOW'
                           objfunc = 'K' ) ).

      lt_e071k = VALUE #( ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_TASK'
                            mastertype = 'TABU'
                            mastername = 'YAAI_TASK'
                            tabkey = |{ sy-mandt }*| )
                          ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_TASK_FLOW'
                            mastertype = 'TABU'
                            mastername = 'YAAI_TASK_FLOW'
                            tabkey = |{ sy-mandt }*| ) ).

      TRY.

          lo_cts_api->if_cts_rest_api~add_object_to_request(
            EXPORTING
              iv_trkorr = i_transport
            CHANGING
              ct_e071   = lt_e071
              ct_e071k  = lt_e071k
          ).

          ch_response-success = abap_true.

        CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

          ch_response-error = lo_ex->get_text( ).

      ENDTRY.

    ELSE.

      LOOP AT lt_keys INTO DATA(ls_key).

        me->_add_taskflow_to_transport(
          EXPORTING
            i_id        = ls_key-id
            i_transport = i_transport
          CHANGING
            ch_response = ch_response
        ).

        IF ch_response-error IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD _add_taskflow_to_transport.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k.

    lt_e071 = VALUE #( ( trkorr = i_transport
                         pgmid = 'R3TR'
                         object = 'TABU'
                         obj_name = 'YAAI_TASK'
                         objfunc = 'K' )
                       ( trkorr = i_transport
                         pgmid = 'R3TR'
                         object = 'TABU'
                         obj_name = 'YAAI_TASK_FLOW'
                         objfunc = 'K' ) ).

    lt_e071k = VALUE #( ( trkorr = i_transport
                          pgmid = 'R3TR'
                          object = 'TABU'
                          objname = 'YAAI_TASK'
                          mastertype = 'TABU'
                          mastername = 'YAAI_TASK'
                          tabkey = sy-mandt && i_id )
                        ( trkorr = i_transport
                          pgmid = 'R3TR'
                          object = 'TABU'
                          objname = 'YAAI_TASK_FLOW'
                          mastertype = 'TABU'
                          mastername = 'YAAI_TASK_FLOW'
                          tabkey = sy-mandt && i_id ) ).

    SELECT id, task_id
      FROM yaai_task_flow
      WHERE id = @i_id
      INTO TABLE @DATA(lt_tasks).

    LOOP AT lt_tasks ASSIGNING FIELD-SYMBOL(<ls_task>).

      APPEND VALUE #( trkorr = i_transport
                      pgmid = 'R3TR'
                      object = 'TABU'
                      objname = 'YAAI_TASK'
                      mastertype = 'TABU'
                      mastername = 'YAAI_TASK'
                      tabkey = sy-mandt && <ls_task>-task_id ) TO lt_e071k.

    ENDLOOP.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    TRY.

        lo_cts_api->if_cts_rest_api~add_object_to_request(
          EXPORTING
            iv_trkorr = i_transport
          CHANGING
            ct_e071   = lt_e071
            ct_e071k  = lt_e071k
        ).

        ch_response-success = abap_true.

      CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

        ch_response-error = lo_ex->get_text( ).

    ENDTRY.

  ENDMETHOD.

  METHOD _add_agents_to_transport.

    TYPES: BEGIN OF ty_agent_table_key_s,
             id TYPE yaai_agent-id,
           END OF ty_agent_table_key_s.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k,
          lt_keys  TYPE STANDARD TABLE OF ty_agent_table_key_s.


    IF i_keys IS NOT INITIAL.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = i_keys
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = lt_keys
      ).

    ENDIF.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    IF lt_keys IS INITIAL.

      lt_e071 = VALUE #( ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_AGENT'
                           objfunc = 'K' )
                         ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_AGENT_MDL'
                           objfunc = 'K' )
                         ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_AGENT_RAG'
                           objfunc = 'K' )
                         ( trkorr = i_transport
                           pgmid = 'R3TR'
                           object = 'TABU'
                           obj_name = 'YAAI_AGENT_TOOL'
                           objfunc = 'K' ) ).

      lt_e071k = VALUE #( ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_AGENT'
                            mastertype = 'TABU'
                            mastername = 'YAAI_AGENT'
                            tabkey = |{ sy-mandt }*| )
                          ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_AGENT_MDL'
                            mastertype = 'TABU'
                            mastername = 'YAAI_AGENT_MDL'
                            tabkey = |{ sy-mandt }*| )
                          ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_AGENT_RAG'
                            mastertype = 'TABU'
                            mastername = 'YAAI_AGENT_RAG'
                            tabkey = |{ sy-mandt }*| )
                          ( trkorr = i_transport
                            pgmid = 'R3TR'
                            object = 'TABU'
                            objname = 'YAAI_AGENT_TOOL'
                            mastertype = 'TABU'
                            mastername = 'YAAI_AGENT_TOOL'
                            tabkey = |{ sy-mandt }*| ) ).

      TRY.

          lo_cts_api->if_cts_rest_api~add_object_to_request(
            EXPORTING
              iv_trkorr = i_transport
            CHANGING
              ct_e071   = lt_e071
              ct_e071k  = lt_e071k
          ).

          ch_response-success = abap_true.

        CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

          ch_response-error = lo_ex->get_text( ).

      ENDTRY.

    ELSE.

      LOOP AT lt_keys INTO DATA(ls_key).

        me->_add_agent_to_transport(
          EXPORTING
            i_id        = ls_key-id
            i_transport = i_transport
          CHANGING
            ch_response = ch_response
        ).

        IF ch_response-error IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD _add_agent_to_transport.

    DATA: lt_e071  TYPE trwbo_t_e071,
          lt_e071k TYPE trwbo_t_e071k.

    DATA(lo_cts_api) = NEW cl_cts_rest_api_impl( ).

    TRY.

        lt_e071 = VALUE #( ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_AGENT'
                             objfunc = 'K' )
                           ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_AGENT_MDL'
                             objfunc = 'K' )
                           ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_AGENT_RAG'
                             objfunc = 'K' )
                           ( trkorr = i_transport
                             pgmid = 'R3TR'
                             object = 'TABU'
                             obj_name = 'YAAI_AGENT_TOOL'
                             objfunc = 'K' ) ).

        lt_e071k = VALUE #( ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_AGENT'
                              mastertype = 'TABU'
                              mastername = 'YAAI_AGENT'
                              tabkey = sy-mandt && i_id )
                            ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_AGENT_MDL'
                              mastertype = 'TABU'
                              mastername = 'YAAI_AGENT_MDL'
                              tabkey = sy-mandt && i_id && '*' )
                            ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_AGENT_RAG'
                              mastertype = 'TABU'
                              mastername = 'YAAI_AGENT_RAG'
                              tabkey = sy-mandt && i_id && '*' )
                            ( trkorr = i_transport
                              pgmid = 'R3TR'
                              object = 'TABU'
                              objname = 'YAAI_AGENT_TOOL'
                              mastertype = 'TABU'
                              mastername = 'YAAI_AGENT_TOOL'
                              tabkey = sy-mandt && i_id && '*' )  ).

        lo_cts_api->if_cts_rest_api~add_object_to_request(
          EXPORTING
            iv_trkorr = i_transport
          CHANGING
            ct_e071   = lt_e071
            ct_e071k  = lt_e071k
        ).

        ch_response-success = abap_true.

      CATCH cx_cts_rest_api_exception INTO DATA(lo_ex).

        ch_response-error = lo_ex->get_text( ).

    ENDTRY.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA ls_response TYPE ty_response_s.

    DATA(l_apis) = abap_false.
    DATA(l_api) = abap_true.

    CASE abap_true.

      WHEN l_apis.

        me->_add_apis_to_transport(
          EXPORTING
            i_transport = 'NPLK900153'
          CHANGING
            ch_response = ls_response
        ).

        out->write( ls_response ).

      WHEN l_api.

        me->_add_api_to_transport(
          EXPORTING
            i_api       = 'OPENAI'
            i_transport = 'NPLK900153'
          CHANGING
            ch_response = ls_response
        ).

        out->write( ls_response ).

    ENDCASE.


  ENDMETHOD.

ENDCLASS.
