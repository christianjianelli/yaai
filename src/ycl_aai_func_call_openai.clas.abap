CLASS ycl_aai_func_call_openai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_func_call_openai.

    ALIASES on_tool_call          FOR yif_aai_func_call_openai~on_tool_call.
    ALIASES on_tool_call_response FOR yif_aai_func_call_openai~on_tool_call_response.
    ALIASES on_tool_call_error    FOR yif_aai_func_call_openai~on_tool_call_error.

    ALIASES mt_methods FOR yif_aai_func_call_openai~mt_methods.

    ALIASES add_methods FOR yif_aai_func_call_openai~add_methods.
    ALIASES get_tools FOR yif_aai_func_call_openai~get_tools.
    ALIASES get_tools_chat_completions FOR yif_aai_func_call_openai~get_tools_chat_completions.
    ALIASES reset_methods FOR yif_aai_func_call_openai~reset_methods.
    ALIASES remove_method FOR yif_aai_func_call_openai~remove_method.
    ALIASES call_tool FOR yif_aai_func_call_openai~call_tool.


  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aai_func_call_openai IMPLEMENTATION.

  METHOD yif_aai_func_call_openai~add_methods.

    APPEND LINES OF i_t_methods TO me->mt_methods.

    SORT me->mt_methods BY class_name method_name.

    DELETE ADJACENT DUPLICATES FROM me->mt_methods COMPARING class_name method_name.

  ENDMETHOD.

  METHOD yif_aai_func_call_openai~remove_method.

    DELETE me->mt_methods WHERE class_name = i_s_method-class_name AND method_name = i_s_method-method_name.

  ENDMETHOD.

  METHOD yif_aai_func_call_openai~reset_methods.

    FREE me->mt_methods.

  ENDMETHOD.

  METHOD yif_aai_func_call_openai~get_tools.

    DATA lt_tools TYPE STANDARD TABLE OF yif_aai_func_call_openai~ty_tool_s.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->mt_methods ASSIGNING FIELD-SYMBOL(<ls_method>).

      lo_aai_util->get_method_importing_params(
        EXPORTING
          i_class_name         = <ls_method>-class_name
          i_method_name        = <ls_method>-method_name
        IMPORTING
          e_t_importing_params = DATA(lt_importing_params)
      ).

      APPEND INITIAL LINE TO lt_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).

      <ls_tool>-type = 'function'.
      <ls_tool>-name = |{ <ls_method>-class_name }_{ <ls_method>-method_name }|.
      <ls_tool>-description = <ls_method>-description.
      <ls_tool>-parameters-type = 'object'.

      LOOP AT lt_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_param>).

        IF <ls_importing_param>-required = abap_true.

          APPEND <ls_importing_param>-name TO <ls_tool>-parameters-required.

        ENDIF.

      ENDLOOP.

      <ls_tool>-parameters-properties = lo_aai_util->get_json_schema(
        EXPORTING
          i_class_name  = <ls_method>-class_name
          i_method_name = <ls_method>-method_name
      ).

    ENDLOOP.

    IF lt_tools[] IS INITIAL.
      e_tools = '[]'.
      RETURN.
    ENDIF.

    e_tools = NEW ycl_aai_util( )->serialize( i_data = lt_tools ).

  ENDMETHOD.

  METHOD yif_aai_func_call_openai~get_tools_chat_completions.

    DATA lt_tools TYPE yif_aai_func_call_openai~ty_tools_chat_completion_t.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->mt_methods ASSIGNING FIELD-SYMBOL(<ls_method>).

      lo_aai_util->get_method_importing_params(
        EXPORTING
          i_class_name         = <ls_method>-class_name
          i_method_name        = <ls_method>-method_name
        IMPORTING
          e_t_importing_params = DATA(lt_importing_params)
      ).

      APPEND INITIAL LINE TO lt_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).

      <ls_tool>-type = 'function'.
      <ls_tool>-function-name = |{ <ls_method>-class_name }_{ <ls_method>-method_name }|.
      <ls_tool>-function-description = <ls_method>-description.
      <ls_tool>-function-parameters-type = 'object'.

      LOOP AT lt_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_param>).

        IF <ls_importing_param>-required = abap_true.

          APPEND <ls_importing_param>-name TO <ls_tool>-function-parameters-required.

        ENDIF.

      ENDLOOP.

      <ls_tool>-function-parameters-properties = lo_aai_util->get_json_schema(
        EXPORTING
          i_class_name  = <ls_method>-class_name
          i_method_name = <ls_method>-method_name
      ).

    ENDLOOP.

    IF lt_tools[] IS INITIAL.
      e_tools = ''.
      RETURN.
    ENDIF.

    e_tools = NEW ycl_aai_util( )->serialize( i_data = lt_tools ).

  ENDMETHOD.

  METHOD yif_aai_func_call_openai~call_tool.

    FIELD-SYMBOLS: <ls_data> TYPE any.

    DATA: lr_data    TYPE REF TO data,
          lo_ex_root TYPE REF TO cx_root.

    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_class        TYPE REF TO object.

    DATA lt_parameters TYPE abap_parmbind_tab.

    DATA ls_parameter  TYPE LINE OF abap_parmbind_tab.

    CLEAR r_response.

    " Determine the method to be called
    LOOP AT me->mt_methods INTO DATA(ls_method).

      ls_method-class_name = to_upper( ls_method-class_name ).
      ls_method-method_name = to_upper( ls_method-method_name ).

      DATA(l_name) = |{ ls_method-class_name }_{ ls_method-method_name }|.

      IF i_tool_name <> l_name.
        CLEAR ls_method.
        CONTINUE.
      ENDIF.

      EXIT.

    ENDLOOP.

    IF ls_method IS INITIAL.

      r_response = 'The function/tool called is not available.'.

      RAISE EVENT on_tool_call_error EXPORTING error_text = r_response.

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

      r_response = 'The function/tool called is not available.'.

      RAISE EVENT on_tool_call_error EXPORTING error_text = r_response.

      RETURN.

    ENDIF.

    IF ls_method-proxy_class IS NOT INITIAL.
      ls_method-class_name = ls_method-proxy_class.
    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    lo_aai_util->get_method_importing_params(
      EXPORTING
        i_class_name         = ls_method-class_name
        i_method_name        = ls_method-method_name
      IMPORTING
        e_t_components       = DATA(lt_components)
    ).

    IF lt_components IS NOT INITIAL.

      TRY.

          " Create a structure with the importing parameters
          lo_struct_descr = cl_abap_structdescr=>create( lt_components ).

          CREATE DATA lr_data TYPE HANDLE lo_struct_descr.

          ASSIGN lr_data->* TO <ls_data>.

        CATCH cx_sy_struct_attributes INTO DATA(lo_ex_struct_attributes).

          r_response = |An error occurred while calling the function/tool. Error description: { lo_ex_struct_attributes->get_text( ) }|.

          RAISE EVENT on_tool_call_error EXPORTING error_text = r_response.

          RETURN.

        CATCH cx_sy_create_data_error INTO DATA(lo_ex_create_data_error).

          r_response = |An error occurred while calling the function/tool. Error description: { lo_ex_create_data_error->get_text( ) }|.

          RAISE EVENT on_tool_call_error EXPORTING error_text = r_response.

          RETURN.

      ENDTRY.

    ENDIF.

    IF <ls_data> IS ASSIGNED.

      " Deserialize the JSON passing its data to the corresponding importing parameters of the method that is going to be called
      /ui2/cl_json=>deserialize(
        EXPORTING
          json = i_json
        CHANGING
          data = <ls_data>
      ).

      " Fill the parameters table to dynamically pass the importing parameters in the method call
      LOOP AT lt_components INTO DATA(ls_components).

        ls_parameter-name = to_upper( ls_components-name ).
        ls_parameter-kind = cl_abap_objectdescr=>exporting.

        ASSIGN COMPONENT ls_components-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lr_param>).

        IF sy-subrc = 0.

          ls_parameter-value = REF #( <lr_param> ).          " Always pass a reference

          INSERT ls_parameter INTO TABLE lt_parameters.

        ENDIF.

      ENDLOOP.

    ENDIF.

    ls_parameter-name = 'R_RESPONSE'.
    ls_parameter-kind = cl_abap_objectdescr=>receiving.
    ls_parameter-value = REF #( r_response ).               " Always pass a reference

    INSERT ls_parameter INTO TABLE lt_parameters.

    IF lt_parameters[] IS NOT INITIAL.

      RAISE EVENT on_tool_call
        EXPORTING
          class_name = ls_method-class_name
          method_name = ls_method-method_name
          parameters_table = lt_parameters.

      TRY.

          CREATE OBJECT lo_class TYPE (ls_method-class_name).

          CALL METHOD lo_class->(ls_method-method_name)
            PARAMETER-TABLE lt_parameters.

          RAISE EVENT on_tool_call_response
            EXPORTING
              tool_response = r_response.

        CATCH cx_sy_create_object_error
              cx_sy_dyn_call_illegal_class
              cx_sy_dyn_call_illegal_method
              cx_sy_dyn_call_illegal_type
              cx_sy_dyn_call_param_missing
              cx_sy_dyn_call_param_not_found
              cx_sy_ref_is_initial INTO lo_ex_root.

          r_response = |An error occurred while calling the function/tool. Error description: { lo_ex_root->get_text( ) }|.

          RAISE EVENT on_tool_call_error EXPORTING error_text = r_response.

      ENDTRY.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
