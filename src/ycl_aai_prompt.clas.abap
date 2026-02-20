CLASS ycl_aai_prompt DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_prompt.

    ALIASES mo_prompt_template FOR yif_aai_prompt~mo_prompt_template.
    ALIASES mr_params FOR yif_aai_prompt~mr_params.
    ALIASES generate_prompt_from_template FOR yif_aai_prompt~generate_prompt_from_template.
    ALIASES get_prompt FOR yif_aai_prompt~get_prompt.

    ALIASES m_placeholder_begin FOR yif_aai_prompt~m_placeholder_begin.
    ALIASES m_placeholder_end FOR yif_aai_prompt~m_placeholder_end.

    METHODS constructor
      IMPORTING
        i_o_prompt_template TYPE REF TO yif_aai_prompt_template OPTIONAL
        i_s_params          TYPE data OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS YCL_aai_PROMPT IMPLEMENTATION.


  METHOD constructor.

    me->m_placeholder_begin = yif_aai_const=>c_placeholder_pattern. "% (percentage sign)
    me->m_placeholder_end = me->m_placeholder_begin.

    IF i_o_prompt_template IS SUPPLIED.
      me->mo_prompt_template = i_o_prompt_template.
    ENDIF.

    IF i_s_params IS SUPPLIED.

      TRY.

          CREATE DATA me->mr_params LIKE i_s_params.

          ASSIGN me->mr_params->* TO FIELD-SYMBOL(<ls_params>).

          IF sy-subrc = 0.
            <ls_params> = CORRESPONDING #( i_s_params ).
          ENDIF.

        CATCH cx_sy_create_data_error ##NO_HANDLER.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_prompt~get_prompt.

    r_prompt = space.

    IF me->mr_params IS NOT BOUND OR me->mo_prompt_template IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN me->mr_params->* TO FIELD-SYMBOL(<ls_params>).

    me->generate_prompt_from_template(
      EXPORTING
        i_o_template = me->mo_prompt_template
        i_s_params   = <ls_params>
      RECEIVING
        r_prompt     = r_prompt
    ).

  ENDMETHOD.


  METHOD yif_aai_prompt~generate_prompt_from_template.

    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.

    FREE r_prompt.

    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( i_s_params ).

    IF lo_descr->type_kind <> cl_abap_typedescr=>typekind_struct1 AND lo_descr->type_kind <> cl_abap_typedescr=>typekind_struct2.
      "i_s_params must be a structure
      RETURN.
    ENDIF.

    TRY.

        lo_structdescr ?= lo_descr.

      CATCH cx_sy_move_cast_error ##NO_HANDLER.
        RETURN.
    ENDTRY.

    DATA(l_template_text) = i_o_template->get_template_text( ).

    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<ls_component>).

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE i_s_params TO FIELD-SYMBOL(<l_component>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      DATA(l_placeholder) = condense( me->m_placeholder_begin && <ls_component>-name && me->m_placeholder_end ).

      REPLACE ALL OCCURRENCES OF l_placeholder IN l_template_text WITH <l_component>.

    ENDLOOP.

    r_prompt = l_template_text.

  ENDMETHOD.


  METHOD yif_aai_prompt~set_placeholder_pattern.

    me->m_placeholder_begin = i_placeholder_begin.
    me->m_placeholder_end = i_placeholder_end.

  ENDMETHOD.


  METHOD yif_aai_prompt~get_user_message.

    r_user_message = space.

    IF me->mr_params IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN me->mr_params->* TO FIELD-SYMBOL(<ls_params>).

    IF sy-subrc = 0.

      ASSIGN COMPONENT 'USER_MESSAGE' OF STRUCTURE <ls_params> TO FIELD-SYMBOL(<l_user_message>).

      IF sy-subrc = 0.

        r_user_message = <l_user_message>.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_prompt~get_context.

    r_context = space.

    IF me->mr_params IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN me->mr_params->* TO FIELD-SYMBOL(<ls_params>).

    IF sy-subrc = 0.

      ASSIGN COMPONENT 'CONTEXT' OF STRUCTURE <ls_params> TO FIELD-SYMBOL(<l_context>).

      IF sy-subrc = 0.

        r_context = <l_context>.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
