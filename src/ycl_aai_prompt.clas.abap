CLASS ycl_aai_prompt DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_prompt.

    INTERFACES if_oo_adt_classrun.

    ALIASES generate_prompt_from_template FOR yif_aai_prompt~generate_prompt_from_template.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_prompt IMPLEMENTATION.

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

      "A 'Mustache' like pattern is used here
      DATA(l_placeholder) = condense( '{{' && <ls_component>-name && '}}' ).

      REPLACE ALL OCCURRENCES OF l_placeholder IN l_template_text WITH <l_component>.

    ENDLOOP.

    r_prompt = l_template_text.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

  ENDMETHOD.

ENDCLASS.
