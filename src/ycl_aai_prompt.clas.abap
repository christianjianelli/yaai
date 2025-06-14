CLASS ycl_aai_prompt DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_prompt.

    ALIASES generate_prompt_from_template FOR yif_aai_prompt~generate_prompt_from_template.

    ALIASES m_placeholder_begin FOR yif_aai_prompt~m_placeholder_begin.
    ALIASES m_placeholder_end FOR yif_aai_prompt~m_placeholder_end.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_prompt IMPLEMENTATION.

  METHOD constructor.

    me->m_placeholder_begin = '%'.
    me->m_placeholder_end = me->m_placeholder_begin.

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

ENDCLASS.
