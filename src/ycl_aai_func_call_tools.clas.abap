CLASS ycl_aai_func_call_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_func_call_tools.

    INTERFACES if_oo_adt_classrun.

    ALIASES get_available_tools FOR yif_aai_func_call_tools~get_available_tools.
    ALIASES request_tools FOR yif_aai_func_call_tools~request_tools.

    METHODS constructor
      IMPORTING
        i_o_agent TYPE REF TO yif_aai_agent OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _o_agent TYPE REF TO yif_aai_agent.

ENDCLASS.



CLASS YCL_aai_FUNC_CALL_TOOLS IMPLEMENTATION.


  METHOD yif_aai_func_call_tools~get_available_tools.

    FREE r_response.

    IF me->_o_agent IS NOT BOUND.

      r_response = 'Error! No Agent Instance available to retrieve the list of tools.'.

      RETURN.

    ENDIF.

    DATA(lt_tools) = me->_o_agent->get_tools( i_load_on_demand_tools = abap_true ).

    IF lt_tools IS INITIAL.

      r_response = 'No tools found.'.

      RETURN.

    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    r_response = lo_aai_util->serialize(
      EXPORTING
        i_data       = lt_tools
    ).

  ENDMETHOD.


  METHOD yif_aai_func_call_tools~request_tools.

    FREE r_response.

    IF me->_o_agent IS NOT BOUND.

      r_response = 'Error! No Agent Instance available to retrieve the list of tools.'.

      RETURN.

    ENDIF.

    DATA(lt_tools) = me->_o_agent->get_tools( i_load_on_demand_tools = abap_true ).

    IF lt_tools IS INITIAL.

      r_response = 'No tools found.'.

      RETURN.

    ENDIF.

    DATA(lt_tools_req) = lt_tools.

    LOOP AT lt_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).

      READ TABLE i_t_tools TRANSPORTING NO FIELDS
        WITH KEY
          class_name = <ls_tool>-class_name
          method_name = <ls_tool>-method_name.

      IF sy-subrc <> 0.

        DELETE lt_tools_req
          WHERE class_name = <ls_tool>-class_name
            AND method_name = <ls_tool>-method_name.

      ENDIF.

    ENDLOOP.

    FREE lt_tools.

    IF lt_tools_req IS INITIAL.

      r_response = 'No tools found.'.

      RETURN.

    ENDIF.

    SELECT SINGLE api
      FROM yaai_chat
      WHERE id = @me->_o_agent->m_chat_id
      INTO @DATA(l_api).

    DATA(lo_aai_db) = NEW ycl_aai_db( i_api = l_api
                                        i_id = me->_o_agent->m_chat_id ).

    lo_aai_db->persist_tools(
      EXPORTING
        i_t_tools   = CORRESPONDING #( lt_tools_req )
      IMPORTING
        e_persisted = DATA(l_persisted)
    ).

    IF l_persisted = abap_false.
      r_response = 'Error! Tools were found but not persisted in the database.'.
      RETURN.
    ENDIF.

    r_response = 'The JSON schema of the required tools is now available.'.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

*    me->_o_agent = NEW ycl_aai_agent(
*      i_agent_id = CONV #( 'C6111BEC04B71FE0B9DFCCB09425F70E' )
*      i_chat_id  = CONV #( ' ' )
*    ).

*    out->write( me->get_available_tools( ) ).
*
*    DATA(lo_aai_db) = NEW ycl_aai_db( i_api = yif_aai_const=>c_openai ).
*
*    me->_o_agent = NEW ycl_aai_agent(
*      i_agent_id = CONV #( 'C6111BEC04B71FE0B9DFCCB09425F70E' )
*      i_chat_id  = lo_aai_db->m_id
*    ).

*    out->write( me->request_tools( EXPORTING i_t_tools = VALUE #( ( class_name = 'YCL_aai_DATA_ELEMENT_TOOLS'
*                                                                    method_name = 'CREATE' ) ) ) ).

  ENDMETHOD.


  METHOD constructor.

    IF i_o_agent IS SUPPLIED.

      me->_o_agent = i_o_agent.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
