CLASS ycl_aai_function_calling DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_function_calling.

    ALIASES add_methods   FOR yif_aai_function_calling~add_methods.
    ALIASES get_tools     FOR yif_aai_function_calling~get_tools.
    ALIASES reset_methods FOR yif_aai_function_calling~reset_methods.
    ALIASES remove_method FOR yif_aai_function_calling~remove_method.
    ALIASES get_arguments FOR yif_aai_function_calling~get_arguments.
    ALIASES call_tool     FOR yif_aai_function_calling~call_tool.

    ALIASES mt_methods    FOR yif_aai_function_calling~mt_methods.

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

  METHOD yif_aai_function_calling~get_arguments.

    DATA: lv_json_string  TYPE string,
          lt_name_value   TYPE yif_aai_function_calling~arguments_t,
          lo_deserializer TYPE REF TO /ui2/cl_json,
          lr_data         TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_name>  TYPE string,
                   <lv_value> TYPE any.

    lv_json_string = i_json.

    lo_deserializer = NEW #( ).

    " Deserialize the JSON into a generic data structure
    lo_deserializer->deserialize(
      EXPORTING
        json        = lv_json_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = lr_data ).

    ASSIGN lr_data->* TO <ls_data>.

    " Assuming the JSON is a flat structure (object with key-value pairs)
    DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr,
          lt_components TYPE abap_component_tab,
          ls_component  LIKE LINE OF lt_components.

    lo_strucdescr ?= cl_abap_typedescr=>describe_by_data_ref( lr_data ).

    lt_components = lo_strucdescr->get_components( ).

    LOOP AT lt_components INTO ls_component.

      ASSIGN COMPONENT ls_component-name OF STRUCTURE <ls_data> TO <lv_value>.

      IF sy-subrc = 0.

        APPEND INITIAL LINE TO e_t_arguments ASSIGNING FIELD-SYMBOL(<ls_argument>).

        <ls_argument>-name = ls_component-name.

        ASSIGN <lv_value>->* TO FIELD-SYMBOL(<val>).

        <ls_argument>-value = <val>.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD yif_aai_function_calling~call_tool.

    FIELD-SYMBOLS <l_data> TYPE any.

    DATA lr_data TYPE REF TO data.

    DATA lo_class TYPE REF TO object.

    DATA lo_class_descr TYPE REF TO cl_abap_classdescr.

    DATA lt_parameters TYPE abap_parmbind_tab.

    DATA ls_parameter  TYPE LINE OF abap_parmbind_tab.

    CLEAR r_response.

    LOOP AT me->mt_methods INTO DATA(ls_method).

      ls_method-class_name = to_upper( ls_method-class_name ).
      ls_method-method = to_upper( ls_method-method ).

      DATA(l_name) = |{ ls_method-class_name }_{ ls_method-method }|.

      IF i_tool_name <> l_name.
        CLEAR ls_method.
        CONTINUE.
      ENDIF.

      EXIT.

    ENDLOOP.

    IF ls_method IS INITIAL.
      r_response = 'The function/tool called is not available.'.
      RETURN.
    ENDIF.

    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = ls_method-class_name  " Type name
      RECEIVING
        p_descr_ref    = DATA(lo_descr)        " Reference to description object
      EXCEPTIONS
        type_not_found = 1                     " Type with name p_name could not be found
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_class_descr ?= lo_descr.

    LOOP AT i_t_arguments ASSIGNING FIELD-SYMBOL(<ls_argument>).

      lo_class_descr->get_method_parameter_type(
        EXPORTING
          p_method_name       = ls_method-method      " Method name
          p_parameter_name    = <ls_argument>-name    " Parameter Name
        RECEIVING
          p_descr_ref         = DATA(lo_descr_ref)    " Description object
        EXCEPTIONS
          parameter_not_found = 1                     " Parameter could not be found
          method_not_found    = 2                     " Method was not found
          OTHERS              = 3
      ).

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      CREATE DATA lr_data TYPE HANDLE lo_descr_ref.

      ASSIGN lr_data->* TO <l_data>.

      <l_data> = CONV #( <ls_argument>-value ).

      ls_parameter-name = to_upper( <ls_argument>-name ).
      ls_parameter-kind = cl_abap_objectdescr=>exporting.
      ls_parameter-value = REF #( <l_data> ).

      INSERT ls_parameter INTO TABLE lt_parameters.

    ENDLOOP.

    ls_parameter-name = 'R_RESPONSE'.
    ls_parameter-kind = cl_abap_objectdescr=>receiving.
    ls_parameter-value = REF #( r_response ).

    INSERT ls_parameter INTO TABLE lt_parameters.

    TRY.

        CREATE OBJECT lo_class TYPE (ls_method-class_name).

        CALL METHOD lo_class->(ls_method-method)
          PARAMETER-TABLE lt_parameters.

      CATCH cx_sy_create_object_error INTO DATA(lo_ex_create_object_error).

        r_response = |An error occurred while calling the function/tool. Error: { lo_ex_create_object_error->get_text( ) }|.

      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_ex_dyn_call_illegal_method).

        r_response = |'An error occurred while calling the function/tool. Error: { lo_ex_dyn_call_illegal_method->get_text( ) }|.

      CATCH cx_sy_dyn_call_illegal_type INTO DATA(lo_ex_dyn_call_illegal_type).

        r_response = |'An error occurred while calling the function/tool. Error: { lo_ex_dyn_call_illegal_type->get_text( ) }|.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
