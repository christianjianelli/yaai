# yaai - ABAP AI tools - System Instructions - Google

## Overview

System instructions (also known as "system prompts") are special messages provided to a Large Language Model (LLM) to guide its behavior, tone, or constraints during a conversation. In ABAP AI, you can pass system instructions to the LLM using the `set_system_instructions` method of the `ycl_aai_google` class.

## Purpose

System instructions are typically used to:

- Define the assistant's persona (e.g., "You are a helpful SAP consultant.")
- Set boundaries or rules for responses
- Specify the format or style of answers
- Provide context or background information

## How to Use

1. **Create the LLM Client Instance**

   You will have an instance of a class implementing `yif_aai_google`, such as `ycl_aai_google`.

   ```abap
   "This example assumes that the API base URL and the API Key are properly configured
   DATA(lo_aai_google) = NEW ycl_aai_google( i_model = 'gemini-2.5-flash' ).
   ```

2. **Set the System Instructions**

   Call the `set_system_instructions` method, passing your instructions as a string.

   ```abap
   DATA l_system_instructions TYPE string.

   l_system_instructions = |# Identity\n|.
   l_system_instructions = |{ l_system_instructions }You are a knowledgeable and approachable support agent for **SAP Materials Management**.\n|.
   " ... 
   " ... 

   lo_aai_google->set_system_instructions( l_system_instructions ).
   ```

3. **Continue with Message Generation**

   After setting the system instructions, you can proceed to generate messages as usual. The instructions will be included in the conversation context sent to the LLM.

   ```abap
   lo_aai_google->chat(
     EXPORTING
       i_message    = 'Before we begin, can you confirm your area of expertise? I want to ensure my question aligns with your capabilities.'
     IMPORTING
       e_t_response = DATA(t_response)
   ).
   ```

## Complete Example

```abap
REPORT yaai_r_syst_instruc_google.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    METHODS run.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    DATA l_system_instructions TYPE string.

    DATA(lo_aai_conn) = NEW ycl_aai_conn( yif_aai_const=>c_google ).

    lo_aai_conn->yif_aai_conn~set_api_key(
      EXPORTING
        i_o_api_key = NEW ycl_aai_api_key_env( )
    ).

    "This example assumes that the API base URL and the API Key are properly configured
    DATA(lo_aai_google) = NEW ycl_aai_google( i_model = 'gemini-2.5-flash' i_o_connection = lo_aai_conn ).

    l_system_instructions = |# Identity\n|.
    l_system_instructions = |{ l_system_instructions }You are a knowledgeable and approachable support agent for **SAP Materials Management**.\n|.
    l_system_instructions = |{ l_system_instructions }# Instructions\n|.
    l_system_instructions = |{ l_system_instructions }** Your goal is to assist users with **SAP Materials Management (MM)** issues, providing clear, concise, and actionable guidance.\n|.
    l_system_instructions = |{ l_system_instructions }** Maintain a polite and patient tone in all interactions.\n|.
    l_system_instructions = |{ l_system_instructions }** Offer straightforward solutions, avoiding unnecessary jargon unless the user demonstrates advanced knowledge.\n|.
    l_system_instructions = |{ l_system_instructions }** Focus exclusively on **SAP MM**-related queries (e.g., procurement, inventory management, master data, invoices). **DO NOT** address questions outside this scope.\n|.
    l_system_instructions = |{ l_system_instructions }** If you cannot resolve a query, respond **exactly** with:\n|.
    l_system_instructions = |{ l_system_instructions }*  "I'm not sure about that, but I can escalate this to an SAP MM specialist for further assistance."|.

    lo_aai_google->set_system_instructions( l_system_instructions ).

    lo_aai_google->chat(
      EXPORTING
        i_message    = 'Before we begin, can you confirm your area of expertise? I want to ensure my question aligns with your capabilities.'
      IMPORTING
        e_t_response = DATA(t_response)
    ).

    LOOP AT t_response INTO DATA(l_response_line).

      WRITE: / l_response_line.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_app( )->run( ).
```

## Notes

- The system instructions should be set before generating messages. You can update them at any time to change the assistant's behavior for subsequent requests.
- System instructions are typically included as the first message in the conversation context with the role "system".
- You can use [Prompt Templates](../prompt_templates.md) to generate dynamic or reusable system instructions.
