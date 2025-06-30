# yaai - ABAP AI tools - Function Calling - Google Gemini

The ABAP AI function calling feature integrates Google Gemini's function calling capabilities with ABAP global classes, enabling large language models (LLMs) to invoke ABAP instance methods programmatically. This allows for dynamic, AI-driven workflows where LLMs can request the execution of ABAP logic and receive structured results.

## Overview

Google Gemini function calling enables LLMs to interact with external functions by describing them in a machine-readable format. In the ABAP AI context, this means exposing ABAP class methods as callable functions. The ABAP AI framework serializes method signatures and parameters, allowing the LLM to understand available methods and their expected inputs.

Supported parameter types:
- Scalar types (e.g., `STRING`, `INT4`, `P`, `D`, `C`, ...)
- One-level structures (flat structures, no nested structures)
- One-level tables (internal tables with flat line types)

## Usage Steps

1. **Define ABAP Global Class and Method**  
    Create a global class with instance methods you want to expose. Ensure method parameters are scalar, flat structures, or flat tables. The method can only have `IMPORTING` parameters; `EXPORTING` and `CHANGING` parameters are not supported. The method must have a `RETURNING` parameter named `R_RESPONSE` of type `STRING`.

2. **Register the Class/Method**  
    Use the ABAP AI framework to register the class and method for function calling.  
    The method `add_methods` of the class `ycl_aai_func_call_google` must be used to register the methods you want to expose for function calling.  
    The framework will generate a function schema compatible with Google Gemini.

    ```abap
    DATA(lo_function_calling) = NEW ycl_aai_func_call_google( ).

    lo_function_calling->add_methods( VALUE #( ( class_name = 'ycl_aai_bp_tools' method_name = 'create_person' description = 'Use this method to create a Business Partner for a Person' )
                                               ( class_name = 'ycl_aai_bp_tools' method_name = 'create_organization' description = 'Use this method to create a Business Partner for an Organization' ) ) ).
    ```    

3. **Bind Tools to Google Gemini**  
    The ABAP AI Google Gemini `bind_tools` method expects an object (instance) of the class `ycl_aai_func_call_google` as its argument. This object manages the registration and invocation of ABAP methods as callable tools for Gemini function calling.

    ```abap
    lo_aai_google->bind_tools( lo_function_calling ).
    ```

4. **Describe Functions to Google Gemini**  
    The ABAP AI framework provides a JSON schema describing available methods and their parameters. This schema is sent to the Google Gemini API as part of the function calling setup.

5. **Invoke via LLM**  
    When the LLM determines a function call is needed, it returns a function call request with parameter values. The ABAP AI framework parses this request and invokes the corresponding ABAP method.

6. **Return Result**  
    The ABAP method is executed, and its result is returned in the `R_RESPONSE` RETURNING parameter of type `STRING`. This value is then sent back to the LLM as the function call response.

**Complete Example:**

```abap
REPORT yaai_r_simple_func_call_google.

PARAMETERS: p_prompt TYPE c LENGTH 200 LOWER CASE OBLIGATORY,
            p_model  TYPE c LENGTH 30 LOWER CASE OBLIGATORY DEFAULT 'gemini-2.5-flash',
            p_tempe  TYPE p LENGTH 2 DECIMALS 1 DEFAULT 1.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    METHODS run.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    DATA l_system_instructions TYPE string.

    "This example assumes that the API base URL and the API Key are properly configured
    DATA(lo_aai_google) = NEW ycl_aai_google( ).

    l_system_instructions = |# Role\n|.
    l_system_instructions = |{ l_system_instructions }You are a knowledgeable and approachable support agent for SAP Business Partner Management.\n|.
    l_system_instructions = |{ l_system_instructions }# Instructions\n|.
    l_system_instructions = |{ l_system_instructions }1. Business Partner Creation:\n|.
    l_system_instructions = |{ l_system_instructions }    - If the user asks for the creation of a Business Partner, use the tools provided accordingly.\n|.
    l_system_instructions = |{ l_system_instructions }    - **Ensure** to include the number of the Business Partner created in your final response to the user.\n|.
    l_system_instructions = |{ l_system_instructions }2. Required Data Information:\n|.
    l_system_instructions = |{ l_system_instructions }    - If the user asks for information about the required data to create Business Partners,|.
    l_system_instructions = |{ l_system_instructions } analyze the JSON schema provided to respond to the question(s).|.
    l_system_instructions = |{ l_system_instructions }    - **User Guidance**: If the user needs further assistance or has additional questions,|.
    l_system_instructions = |{ l_system_instructions } guide them on the next steps or where to find more information|.

    lo_aai_google->set_system_instructions( l_system_instructions ).

    DATA(lo_function_calling) = NEW ycl_aai_func_call_google( ).

    lo_function_calling->add_methods( VALUE #( ( class_name = 'ycl_aai_bp_tools' method_name = 'create_person' description = 'Use this method to create a Business Partner for a Person' )
                                               ( class_name = 'ycl_aai_bp_tools' method_name = 'create_organization' description = 'Use this method to create a Business Partner for an Organization' ) ) ).

    lo_aai_google->bind_tools( lo_function_calling ).

    lo_aai_google->chat(
      EXPORTING
        i_message    = p_prompt
      IMPORTING
        e_t_response = DATA(t_response)
    ).

    LOOP AT t_response INTO DATA(l_response_line).

      WRITE: / l_response_line.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.

  "Set text for the selection screen fields
  %_p_model_%_app_%-text = 'Model'.
  %_p_prompt_%_app_%-text = 'Prompt'.
  %_p_tempe_%_app_%-text = 'Temperature'.

START-OF-SELECTION.

  NEW lcl_app( )->run( ).
```

## Limitations

- Only instance methods of global classes are supported.
- Methods can only have `IMPORTING` parameters; `EXPORTING` and `CHANGING` parameters are not supported.
- Each method must have a `RETURNING` parameter named `R_RESPONSE` of type `STRING`.
- Method parameters must be scalar, one-level structures, or one-level tables.
- Nested structures and tables are not supported.
- Complex data types (e.g., objects, deep structures) are not supported.

For more details, refer to the ABAP AI framework documentation and Google Gemini function calling API reference.