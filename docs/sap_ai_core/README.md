# yaai - ABAP AI tools - SAP AI Core

<p>
  <img src="../images/sap.png" alt="SAP Logo" width="200px">
</p>

The SAP AI Core API is supported by ABAP AI tools through the OpenAI Chat Completions API. For more information about using ABAP AI tools with the SAP AI Core API, please refer to the OpenAI API documentation.

**Website**: https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/enabling-service-in-cloud-foundry?locale=en-US

**API Documentation**: https://help.sap.com/docs/sap-ai-core/generative-ai/openai?locale=en-US

## Quickstart

### Running Your First ABAP AI tools SAP AI Core Application

This quickstart demonstrates how to create a simple LLM application. It shows you how to connect to the LLM and perform a basic chat interaction.

**Requirements:** 
*   You have a [SAP AI Core Deployment](https://help.sap.com/docs/sap-ai-core/generative-ai/create-deployment-for-generative-ai-model?locale=en-US).
*   Import SAP AI Core API server certificates into SAP trust manager. The [abapGit documentation](https://docs.abapgit.org/user-guide/setup/ssl-setup.html) explains in detail how to do it.
*   Maintain the OAuth settings in the table YAAI_OAUTH.

**Steps:**
1.  Create an ABAP AI tools Connection instance;
2.  Set the Base URL;
3.  Set the Resource Group;
4.  Create an ABAP AI tools OpenAI instance;
5.  Tell the ABAP AI tools OpenAI instance to use the Chat Completions API;
6.  Call the CHAT method.

**Example:**

```abap
REPORT yaai_r_llm_app_sap_ai_core.

START-OF-SELECTION.

  " Maintain the OAuth settings in the table YAAI_OAUTH
  DATA(o_aai_conn) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_sap_ai_core ).

  o_aai_conn->set_base_url( i_base_url = 'https://api.ai.prod.ap-southeast-2.aws.ml.hana.ondemand.com/v2/inference/deployments/d8f6fe1fd20b9999' ).

  "Tcode STVARV - Maintain the Resource Group parameter YAAI_SAP_AI_CORE_RESOURCE_GRP
  SELECT SINGLE low
    FROM tvarvc
    WHERE name = @yif_aai_const=>c_sap_ai_core_res_grp_param
      AND type = 'P'
      AND numb = '0000'
    INTO @DATA(resource_group).

  IF resource_group IS INITIAL.
    resource_group = yif_aai_const=>c_sap_ai_core_resource_group. "default
  ENDIF.

  o_aai_conn->yif_aai_conn~add_http_header_param(
    EXPORTING
      i_name  = 'AI-Resource-Group'
      i_value = resource_group
  ).

  DATA(o_aai_sap_ai_core) = NEW ycl_aai_openai( i_api = yif_aai_const=>c_sap_ai_core
                                                i_model = 'gpt-4.1'
                                                i_o_connection = o_aai_conn ).

  o_aai_sap_ai_core->use_completions( abap_true ).

  o_aai_sap_ai_core->chat(
    EXPORTING
      i_message    = 'What is the capital of France?'
    IMPORTING
      e_t_response = DATA(t_response)
  ).

  LOOP AT t_response INTO DATA(l_response_line).

    WRITE: / l_response_line.

  ENDLOOP.
```
