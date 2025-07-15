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

## Function Calling with Proxy Class Support

### Overview

ABAP AI Tools supports the execution of ABAP logic via Large Language Models (LLMs) by allowing methods to be called dynamically. However, because ABAP is a strongly typed language, the parameters expected by ABAP methods must be in precise formats and types.

LLMs, on the other hand, often struggle to provide perfectly formatted values. To bridge this gap and avoid runtime errors (e.g. type conversion errors, short dumps), we introduce the Proxy Class Pattern.

### Problem

When LLMs call ABAP tools, they must provide parameter values that exactly match the expected ABAP data types defined in the method signatures. For instance:

```abap
METHODS calculate_total
  IMPORTING
    value1 TYPE f
    value2 TYPE f
  RETURNING VALUE(r_response) TYPE string.
```

A minor mismatch in types or formats (e.g., a string `"10.00"` instead of a number) can lead to a short dump.

### Solution: Proxy Class Interface

To prevent type mismatch issues, ABAP AI Tools allows you to define a Proxy Class alongside your concrete implementation.

 - The **Concrete Class** is used to generate the **JSON Schema** that will be sent to the LLM, ensuring it understands the method signature and data format.

 - The **Proxy Class** is used to receive and validate/convert inputs coming from the LLM, before calling the concrete method.
 
 The developer handles type conversion, error handling, and validation inside the proxy method.

 **Example**:

1. Concrete Class (Strict Types)

```abap
CLASS zcl_calculator DEFINITION.
  
  PUBLIC SECTION.
  
    METHODS calculate_total
      IMPORTING 
        value1 TYPE f
        value2 TYPE f
      RETURNING VALUE(r_response) TYPE string.

ENDCLASS.

CLASS zcl_calculator IMPLEMENTATION.

  METHOD calculate_total.

    DATA(l_result) = value1 + value2.

    r_response = |{ value1 } + { value2 } = { l_result }|.

  ENDMETHOD.

ENDCLASS.
```

2. Proxy Class (Flexible Types)

```abap
CLASS zcl_calculator_proxy DEFINITION.
  
  PUBLIC SECTION.
    
    METHODS calculate_total
      IMPORTING 
        value1 TYPE string
        value2 TYPE string
      RETURNING VALUE(r_response) TYPE string.

ENDCLASS.

CLASS zcl_calculator_proxy IMPLEMENTATION.

  METHOD calculate_total.

    DATA: l_val1 TYPE f,
          l_val2 TYPE f.
      
    "Handle type convertions and/or execute validations before calling the concrete method
    TRY.
        
        l_val1 = value1.
        l_val2 = value2.
        
      CATCH cx_sy_conversion_no_number.
        r_response = 'Error: Invalid number format'.
    ENDTRY.
    
    "Instantiate the concrete class
    DATA(lo_calculator) = NEW zcl_calculator_proxy( ).

    "Call the concrete method
    r_response = lo_calculator->calculate_total( 
      EXPORTING value1 = l_val1
                value2 = l_val2 
    ).

  ENDMETHOD.

ENDCLASS.
```
### How It Works Internally

1. Schema Generation: ABAP AI Tools uses **RTTI (Runtime Type Information)** to inspect the concrete class method signature and generate a JSON schema.

2. Execution: When the LLM returns a tool call, ABAP AI Tools:
  - Calls the proxy class method with the LLM-provided input values.
  - Inside the proxy, the values are validated and/or converted.
  - Finally, the concrete class logic is invoked with the correctly typed parameters.

### Using the Proxy Class

To use a proxy class for function calling, simply specify its name in the `proxy_class` parameter when invoking the `add_methods` method.

**Example**:

```abap
DATA(lo_function_calling) = NEW ycl_aai_func_call_google( ).

lo_function_calling->add_methods( VALUE #( ( proxy_class = 'zcl_calculator_proxy' class_name = 'zcl_calculator' method_name = 'calculate_total' description = 'Calculate total' ) ) ).

lo_aai_google->bind_tools( lo_function_calling ).
```