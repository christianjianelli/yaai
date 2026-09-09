CLASS ycl_aai_rest_approval DEFINITION
  PUBLIC
  INHERITING FROM ycl_aai_rest_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_approval_s,
             id          TYPE string,
             class_name  TYPE string,
             method_name TYPE string,
             scope       TYPE string,
             approved    TYPE abap_bool,
             approved_at TYPE string,
             used        TYPE abap_bool,
             used_at     TYPE string,
           END OF ty_approval_s,

           BEGIN OF ty_approval_update_s,
             id          TYPE string,
             class_name  TYPE string,
             method_name TYPE string,
             scope       TYPE string,
             approved    TYPE abap_bool,
             updated     TYPE abap_bool,
             error       TYPE string,
           END OF ty_approval_update_s,

           ty_approvals_t TYPE STANDARD TABLE OF ty_approval_s WITH EMPTY KEY.

    METHODS yif_aai_rest_resource~read REDEFINITION.

    METHODS yif_aai_rest_resource~update REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_approval IMPLEMENTATION.

  METHOD yif_aai_rest_resource~read.

    DATA lt_approvals_response TYPE ty_approvals_t.

    DATA(l_id) = condense( to_upper( i_o_request->get_form_field( name = 'id' ) ) ).

    SELECT id, class_name, method_name, scope, approved, approved_at, used, used_at
      FROM yaai_approval
     WHERE id = @l_id
      INTO TABLE @DATA(lt_approvals).

    lt_approvals_response = CORRESPONDING #( lt_approvals ).

    DATA(l_json) = /ui2/cl_json=>serialize(
      EXPORTING
        data = lt_approvals_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~update.

    DATA ls_response_update TYPE ty_approval_update_s.

    DATA l_json TYPE string.

    ls_response_update-id = to_upper( i_o_request->get_form_field( name = 'chat_id' ) ).
    ls_response_update-class_name  = to_upper( i_o_request->get_form_field( name = 'class_name' ) ).
    ls_response_update-method_name = to_upper( i_o_request->get_form_field( name = 'method_name' ) ).
    ls_response_update-scope = to_upper( i_o_request->get_form_field( name = 'scope' ) ).

    DATA(l_action) = to_upper( i_o_request->get_form_field( name = 'action' ) ).

    IF ls_response_update-id IS INITIAL.

      "Not Found
      i_o_response->set_status(
        EXPORTING
          code = 404
          reason = 'Not Found'
      ).

      RETURN.

    ENDIF.

    CASE l_action.

      WHEN 'APPROVE'.

        DATA(lo_aai_db) = NEW ycl_aai_db( i_api = space
                                          i_id = CONV #( ls_response_update-id ) ).

        lo_aai_db->get_approval(
          EXPORTING
            i_class_name  = ls_response_update-class_name
            i_method_name = ls_response_update-method_name
          IMPORTING
            e_scope       = DATA(l_scope)
            e_approved    = ls_response_update-approved
        ).

        IF ls_response_update-approved = abap_false.

          lo_aai_db->update_approval(
            EXPORTING
              i_class_name  = ls_response_update-class_name
              i_method_name = ls_response_update-method_name
              i_approved    = abap_true
              i_scope       = CONV #( ls_response_update-scope )
            IMPORTING
              e_updated     = ls_response_update-updated
          ).

          ls_response_update-approved = ls_response_update-updated.

          IF ls_response_update-updated = abap_false.
            ls_response_update-error = 'Error while trying to approve the tool call approval request.'.
          ENDIF.

        ENDIF.

      WHEN OTHERS.

        ls_response_update-error = |Action { l_action } is not supported|.

    ENDCASE.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response_update
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
