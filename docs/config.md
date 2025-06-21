# ABAP AI Tools Configuration Parameters

The ABAP AI tools require specific configuration parameters to be maintained in the SAP table `TVARVC` (accessible via transaction STVARV). These parameters define the base URLs for external AI services and are used to construct the full API endpoints for service calls.

## Required Parameters

| Parameter Name           | Description                                 |
|-------------------------|---------------------------------------------|
| YAAI_OPENAI_BASE_URL    | Base URL for OpenAI API endpoints           |
| YAAI_OLLAMA_BASE_URL    | Base URL for Ollama API endpoints           |

### Example Entries in TVARVC

| NAME                   | VALUE                                 |
|------------------------|---------------------------------------|
| YAAI_OPENAI_BASE_URL   | https://api.openai.com/               |
| YAAI_OLLAMA_BASE_URL   | http://localhost:11434                |

- The full API URL is constructed by concatenating the base URL with the specific service endpoint.
- Ensure that these parameters are maintained and up to date for the ABAP AI tools to function correctly.

> **Note:** When using the SAP NetWeaver ABAP Developer Edition to experiment with the ABAP AI tools, you may encounter SSL configuration challenges when connecting to external APIs such as OpenAI. To simplify connectivity, you can use a reverse proxy. For detailed instructions, see [Reverse Proxy Setup](reverse_proxy.md).