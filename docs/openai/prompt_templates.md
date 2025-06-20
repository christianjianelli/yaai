# ABAP AI Prompt Templates

This guide explains how to use the classes `ycl_aai_prompt` and `ycl_aai_prompt_template` to generate prompts from templates in ABAP AI.

## Overview

- `ycl_aai_prompt_template`: Represents a prompt template. You create an instance of this class with the template text.
- `ycl_aai_prompt`: Generates a prompt by replacing placeholders in the template with actual values.

## Usage Steps

1. **Prepare Parameters Structure**

   Define a structure with fields matching the placeholders in your template.

   ```abap
   TYPES: BEGIN OF ty_params,
            name TYPE string,
            id   TYPE string,
          END OF ty_params.

   DATA(ls_params) = VALUE ty_params( name = 'Alice' id = '12345' ).
   ```

2. **Create a Prompt Template Instance**

   You can create a prompt template instance in two ways:

   - **Directly from a string:**

     ```abap
     DATA(lo_template) = NEW ycl_aai_prompt_template( |Hello, %NAME%! Your ID is %ID%.| ).
     ```

   - **From a SAP standard text (SO10):**

     Create your template as a standard text in transaction SO10. Then use the `create_from_standart_text` method to instantiate the template:

     ```abap
     DATA(lo_template) = ycl_aai_prompt_template=>create_from_standart_text( i_standart_text_name = 'YOUR_TEXT_NAME' ).
     ```

3. **Generate the Prompt**

   Create an instance of `ycl_aai_prompt` and call `generate_prompt_from_template`, passing the template and the parameters.

   ```abap
   DATA(lo_prompt) = NEW ycl_aai_prompt( ).

   DATA(l_prompt) = lo_prompt->generate_prompt_from_template(
     EXPORTING
       i_o_template = lo_template
       i_s_params   = ls_params
   ).
   ```

   The result (`l_prompt`) will be:

   ```
   Hello, Alice! Your ID is 12345.
   ```

## Example

```abap
REPORT yaai_r_prompt_template_basic.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    TYPES: BEGIN OF ty_params,
             name TYPE string,
             id   TYPE string,
           END OF ty_params.

    DATA(ls_params) = VALUE ty_params( name = 'Alice' id = '12345' ).

    DATA(lo_template) = NEW ycl_aai_prompt_template( |Hello, %NAME%! Your ID is %ID%.| ).

    DATA(lo_prompt) = NEW ycl_aai_prompt( ).

    DATA(l_prompt) = lo_prompt->generate_prompt_from_template(
      EXPORTING
        i_o_template = lo_template
        i_s_params   = ls_params
    ).

    cl_demo_output=>display_text( l_prompt ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_app( )->run( ).
```

## Notes

- Placeholders in the template must match the field names in the parameters structure, surrounded by the placeholder pattern (default is `%FIELDNAME%`).
- You can customize the placeholder pattern using the `set_placeholder_pattern` method of `ycl_aai_prompt`.
- When using standard texts, ensure your text in SO10 uses the same placeholder format.


## Retrieval-Augmented Generation (RAG) style example

This is a Retrieval-Augmented Generation (RAG) style example, where the prompt template is dynamically filled with both the user's question and the retrieved context. This approach enhances the AI's ability to generate relevant and accurate responses by grounding the answer in external, up-to-date information.

This example demonstrates how to use the ABAP AI Prompt `ycl_aai_prompt` with its default parameter structure `yif_aai_prompt=>ty_params_basic_s`. The structure contains two fields: one for the user's chat message (question) and another for the context, which would be retrieved from a Vector Database (or some other source).

The default parameters structure provides a standardized way to pass both the user message and context, simplifying the integration of RAG workflows into your ABAP applications.

```abap
REPORT yaai_r_prompt_template_rag.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    DATA: l_sample_context TYPE string,
          l_template_text  TYPE string.

    "The context below is typically retrieved from a Vector Database 
    l_sample_context = |The transaction **ZFI_MASS_CREATE_FI_DOCS** allows users to create financial documents in the\n|.
    l_sample_context = |{ l_sample_context } SAP Financial Accounting (FI) module by uploading an Excel file.\n|.
    l_sample_context = |{ l_sample_context }This transaction streamlines the process of mass document creation, reducing manual entry and improving efficiency.|.

    l_template_text = |Please answer the following question using the context provided below.\nContext:\n%CONTEXT%\nQuestion:\n%USER_MESSAGE%|.


    DATA(lo_template) = NEW ycl_aai_prompt_template( l_template_text ).

    DATA(ls_params) = VALUE yif_aai_prompt=>ty_params_basic_s( user_message = 'What does this transaction do?' context = l_sample_context ).

    DATA(lo_prompt) = NEW ycl_aai_prompt( ).

    DATA(l_prompt) = lo_prompt->generate_prompt_from_template(
     EXPORTING
       i_o_template = lo_template
       i_s_params   = ls_params
   ).

    cl_demo_output=>display_text( l_prompt ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_app( )->run( ).
```

The result (`l_prompt`) will be:

```markdown
Please answer the following question using the context provided below.
Context:
The transaction **ZFI_MASS_CREATE_FI_DOCS** allows users to create financial documents in the
SAP Financial Accounting (FI) module by uploading an Excel file.
This transaction streamlines the process of mass document creation, reducing manual entry and improving efficiency.
Question:
What does this transaction do?
```

## Additional Note

The ABAP AI prompt template can also be used to create system instructions that are passed to the Large Language Model (LLM). This allows you to define the behavior, tone, or constraints for the AI's responses by generating system-level prompts in a structured and reusable way.