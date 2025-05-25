CLASS ycl_aai_function_calling DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_function_calling.

    ALIASES add_methods FOR yif_aai_function_calling~add_methods.
    ALIASES get_tools FOR yif_aai_function_calling~get_tools.
    ALIASES reset_methods FOR yif_aai_function_calling~reset_methods.
    ALIASES remove_method FOR yif_aai_function_calling~remove_method.
    ALIASES mt_methods FOR yif_aai_function_calling~mt_methods.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_function_calling IMPLEMENTATION.

  METHOD yif_aai_function_calling~get_tools.

    DATA lt_tools TYPE STANDARD TABLE OF yif_aai_function_calling~tool_s.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->mt_methods ASSIGNING FIELD-SYMBOL(<ls_method>).

      DATA(lt_importing_params) = lo_aai_util->get_method_importing_params(
        EXPORTING
          i_class_name         = <ls_method>-class_name
          i_method_name        = <ls_method>-method
      ).

      APPEND INITIAL LINE TO lt_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).

      <ls_tool>-type = 'function'.
      <ls_tool>-function-name = |{ <ls_method>-class_name }_{ <ls_method>-method }|.
      <ls_tool>-function-description = <ls_method>-description.
      <ls_tool>-function-parameters-type = 'object'.

      LOOP AT lt_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_param>).

        IF <ls_importing_param>-required = abap_true.

          APPEND <ls_importing_param>-name TO <ls_tool>-function-parameters-required.

        ENDIF.

      ENDLOOP.

      DATA(l_last) = lines( lt_importing_params ).

      LOOP AT lt_importing_params ASSIGNING <ls_importing_param>.

        DATA(l_tabix) = sy-tabix.

        IF l_tabix = 1.
          <ls_tool>-function-parameters-properties = '{'.
        ENDIF.

        IF l_tabix <> l_last.

          <ls_tool>-function-parameters-properties = <ls_tool>-function-parameters-properties && '"' &&
                                                     <ls_importing_param>-name &&
                                                     '":{"type":"' &&
                                                     <ls_importing_param>-type && '","description":"' && <ls_importing_param>-description && '"},'.

        ELSE.

          <ls_tool>-function-parameters-properties = <ls_tool>-function-parameters-properties && '"' &&
                                                     <ls_importing_param>-name &&
                                                     '":{"type":"' &&
                                                     <ls_importing_param>-type && '","description":"' && <ls_importing_param>-description && '"}}'.

        ENDIF.

      ENDLOOP.

      " Example
      "<ls_tool>-function-parameters-properties = '{"location":{"type":"string","description":"City and country e.g. Bogotá, Colombia"},"unit":{"type":"string", "description":"Temperature unit like Celsius and Fahrenheit"}}'.

    ENDLOOP.

    IF lt_tools[] IS INITIAL.
      e_tools = '[]'.
      RETURN.
    ENDIF.

    e_tools = NEW ycl_aai_util( )->serialize( i_data = lt_tools ).

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    me->add_methods( i_t_methods = VALUE #( ( class_name = 'ZCL_VEHICLE' method = 'GET_MAKE' ) ) ).
    me->add_methods( i_t_methods = VALUE #( ( class_name = 'ZCL_VEHICLE' method = 'GET_MAKE_NEW' ) ) ).

    me->get_tools(
      IMPORTING
        e_tools = DATA(tools)
    ).

    out->write( tools ).

  ENDMETHOD.

  METHOD yif_aai_function_calling~add_methods.

    APPEND LINES OF i_t_methods TO me->mt_methods.

    SORT me->mt_methods BY class_name method.

    DELETE ADJACENT DUPLICATES FROM me->mt_methods COMPARING class_name method.

  ENDMETHOD.

  METHOD yif_aai_function_calling~remove_method.

    DELETE me->mt_methods WHERE class_name = i_s_method-class_name AND method = i_s_method-method.

  ENDMETHOD.

  METHOD yif_aai_function_calling~reset_methods.

    FREE me->mt_methods.

  ENDMETHOD.

ENDCLASS.
