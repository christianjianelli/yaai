CLASS ycl_aai_func_call_approvals DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_tool_s,
             class_name  TYPE string,
             method_name TYPE string,
           END OF ty_tool_s,

           ty_tools_t TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY.

    METHODS check_tool_call_approval
      IMPORTING
        i_tool_name     TYPE csequence
        i_o_persistence TYPE REF TO yif_aai_db
        i_t_tools       TYPE ty_tools_t
      EXPORTING
        e_approved      TYPE abap_bool
        e_requested     TYPE abap_bool
        e_tool_response TYPE string.

    METHODS approve
      IMPORTING
        i_chat_id     TYPE csequence
        i_class_name  TYPE csequence
        i_method_name TYPE csequence
        i_scope       TYPE yaai_approval-scope OPTIONAL
      EXPORTING
        e_approved    TYPE abap_bool.

    METHODS set_approval_as_used
      IMPORTING
        i_tool_name     TYPE csequence
        i_o_persistence TYPE REF TO yif_aai_db
        i_t_tools       TYPE ty_tools_t OPTIONAL
      EXPORTING
        e_updated       TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _t_tools TYPE ty_tools_t.

    METHODS _get_tool_by_name
      IMPORTING
        i_tool_name TYPE string
        i_t_tools   TYPE ty_tools_t
      EXPORTING
        e_s_tool    TYPE yaai_tool.

ENDCLASS.



CLASS ycl_aai_func_call_approvals IMPLEMENTATION.

  METHOD check_tool_call_approval.

    CLEAR: e_approved,
           e_requested,
           e_tool_response.

    FREE me->_t_tools.

    me->_t_tools = i_t_tools.

    me->_get_tool_by_name(
      EXPORTING
        i_tool_name = to_upper( condense( i_tool_name ) )
        i_t_tools   = i_t_tools
      IMPORTING
        e_s_tool    = DATA(ls_tool)
    ).

    IF ls_tool-approval = abap_true.

      i_o_persistence->get_approval(
        EXPORTING
          i_class_name  = ls_tool-class_name
          i_method_name = ls_tool-method_name
        IMPORTING
          e_requested   = e_requested
          e_approved    = e_approved
      ).

      IF e_approved = abap_false AND
         e_requested = abap_false.

        i_o_persistence->request_approval(
          EXPORTING
            i_class_name  = ls_tool-class_name
            i_method_name = ls_tool-method_name
          IMPORTING
            e_created     = e_requested
        ).

      ENDIF.

    ELSE.

      e_approved = abap_true.

      RETURN.

    ENDIF.

    IF e_approved = abap_false.

      IF ls_tool-approval_text IS NOT INITIAL.
        e_tool_response = ls_tool-approval_text.
        RETURN.
      ENDIF.

      e_tool_response = |This tool requires the user's approval.|.

      IF e_requested = abap_true.
        e_tool_response = |{ e_tool_response } An approval request has been created automatically.|.
      ENDIF.

      e_tool_response = |{ e_tool_response } Inform the user that their approval is required.|.
      e_tool_response = |{ e_tool_response } Ask them to approve the request and then confirm to you that they have done so.|.
      e_tool_response = |{ e_tool_response } Once the user confirms their approval, call the tool again.|.

    ENDIF.

  ENDMETHOD.

  METHOD approve.

    SELECT SINGLE api
      FROM yaai_chat
      WHERE id = @i_chat_id
      INTO @DATA(l_api).

    NEW ycl_aai_db( i_api = l_api
                    i_id = CONV #( i_chat_id ) )->update_approval(
      EXPORTING
        i_class_name  = i_class_name
        i_method_name = i_method_name
        i_approved    = abap_true
        i_scope       = i_scope
      IMPORTING
        e_updated     = e_approved
    ).

  ENDMETHOD.

  METHOD set_approval_as_used.

    IF i_t_tools IS SUPPLIED.

      APPEND LINES OF i_t_tools TO me->_t_tools.

      SORT me->_t_tools BY class_name method_name.

      DELETE ADJACENT DUPLICATES FROM me->_t_tools COMPARING class_name method_name.

    ENDIF.

    me->_get_tool_by_name(
      EXPORTING
        i_tool_name = to_upper( condense( i_tool_name ) )
        i_t_tools   = me->_t_tools
      IMPORTING
        e_s_tool    = DATA(ls_tool)
    ).

    i_o_persistence->update_approval(
      EXPORTING
        i_class_name  = ls_tool-class_name
        i_method_name = ls_tool-method_name
        i_used        = abap_true
      IMPORTING
        e_updated     = e_updated
    ).

  ENDMETHOD.

  METHOD _get_tool_by_name.

    CLEAR e_s_tool.

    LOOP AT i_t_tools INTO DATA(ls_tool).

      ls_tool-class_name = to_upper( condense( ls_tool-class_name ) ).
      ls_tool-method_name = to_upper( condense( ls_tool-method_name ) ).

      DATA(l_name) = |{ ls_tool-class_name }_{ ls_tool-method_name }|.

      IF i_tool_name <> l_name.
        CLEAR ls_tool.
        CONTINUE.
      ENDIF.

      EXIT.

    ENDLOOP.

    IF ls_tool IS INITIAL.
      RETURN.
    ENDIF.

    e_s_tool = CORRESPONDING #( ls_tool ).

    SELECT SINGLE class_name, method_name, proxy_class, description, approval, approval_text
      FROM yaai_tool
      WHERE class_name = @e_s_tool-class_name
        AND method_name = @e_s_tool-method_name
       INTO CORRESPONDING FIELDS OF @e_s_tool.

  ENDMETHOD.

ENDCLASS.
