# yaai - ABAP AI tools - Function Calling - Ollama

The ABAP AI function calling feature integrates Ollama's function calling capabilities with ABAP global classes, enabling large language models (LLMs) to invoke ABAP instance methods programmatically. This allows for dynamic, AI-driven workflows where LLMs can request the execution of ABAP logic and receive structured results.

## Overview

Ollama function calling enables LLMs to interact with external functions by describing them in a machine-readable format. In the ABAP AI context, this means exposing ABAP class methods as callable functions. The ABAP AI framework serializes method signatures and parameters, allowing the LLM to understand available methods and their expected inputs.

Supported parameter types:
- Scalar types (e.g., `STRING`, `INT4`, `P`, `D`, `C`, ...)
- One-level structures (flat structures, no nested structures)
- One-level tables (internal tables with flat line types)

## Usage Steps

1. **Define ABAP Global Class and Method**  
    Create a global class with instance methods you want to expose. Ensure method parameters are scalar, flat structures, or flat tables. The method can only have `IMPORTING` parameters; `EXPORTING` and `CHANGING` parameters are not supported. The method must have a `RETURNING` parameter named `R_RESPONSE` of type `STRING`.

2. **Register the Class/Method**  
    Use the ABAP AI framework to register the class and method for function calling.  
    The method `add_methods` of the class `ycl_aai_function_calling` must be used to register the methods you want to expose for function calling.  
    The framework will generate a function schema compatible with Ollama.

3. **Bind Tools to Ollama**  
    The ABAP AI OpenAI `bind_tools` method expects an object (instance) of the class `ycl_aai_function_calling` as its argument. This object manages the registration and invocation of ABAP methods as callable tools for Ollama function calling.

3. **Describe Functions to Ollama**  
    The ABAP AI framework provides a JSON schema describing available methods and their parameters. This schema is sent to the Ollama API as part of the function calling setup.

4. **Invoke via LLM**  
    When the LLM determines a function call is needed, it returns a function call request with parameter values. The ABAP AI framework parses this request and invokes the corresponding ABAP method.

5. **Return Results**  
    The ABAP method executes and returns results, which are serialized and sent back to the LLM as structured data.

## Limitations

- Only instance methods of global classes are supported.
- Method parameters must be scalar, one-level structures, or one-level tables.
- Methods can only have `IMPORTING` parameters; `EXPORTING` and `CHANGING` parameters are not supported.
- Nested structures and tables are not supported.
- Complex data types (e.g., objects, deep structures) are not supported.

For more details, refer to the ABAP AI framework documentation and Ollama function calling API reference.